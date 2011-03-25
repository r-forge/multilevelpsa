#' @include package.R
NA

plotpsa.multilevel.psa <- function(multilevelPSA,
		xlab='Difference Score', ylab='Level 2', title=NULL,
		overall.col="blue", overall.ci.col='green', level2.point.size=NULL,
		level1.points=TRUE,	errorbars=TRUE, level2.rug.plot=TRUE, jitter=TRUE, reorder=TRUE
) {
	#TODO: add option for plotting effect size differences (i.e. difference / SD)
	
	multilevelPSA <<- multilevelPSA #TODO: This is a hack. Can't ggplot to plot unless I expose this variable globally
	
	if(missing(multilevelPSA)) {
		stop('Must provide multilevelPSA from multilevel.psa')
	}
	
	if(reorder) {
		multilevelPSA@level2.summary = multilevelPSA@level2.summary[order(multilevelPSA@level2.summary$diffwtd),]
		ord.level2 = multilevelPSA@level2.summary$level2[order(multilevelPSA@level2.summary$diffwtd)]
		multilevelPSA@level1.summary$level2 = factor(multilevelPSA@level1.summary$level2, levels=ord.level2)
		multilevelPSA@level2.summary$level2 = factor(multilevelPSA@level2.summary$level2, levels=ord.level2)
	}
	
	p = ggplot(multilevelPSA@level1.summary, aes(x=level2, y=Diff)) + coord_flip() + 
			geom_hline(aes(y=0), colour='black', size=1, alpha=.7) +
			geom_hline(aes(yintercept=multilevelPSA@overall.wtd), colour=overall.col, size=1) + 
			geom_hline(aes(yintercept=multilevelPSA@overall.ci), colour=overall.ci.col, size=1) + 
			opts(axis.ticks.margin=unit(0, "cm"))
	if(errorbars) {
		p = p + geom_errorbar(data=multilevelPSA@level2.summary, aes(x=level2, y=NULL, ymin=ci.min, ymax=ci.max), colour='green', alpha=.6)
	}
	if(level1.points) {
		if(jitter) {
			p = p + geom_point(stat='identity', alpha=.3, size=.8, position='jitter')
		} else {
			p = p + geom_point(stat='identity', alpha=.3, size=.8)
		}
	}
	#p = p + geom_path(data=multilevelPSA@level2.summary, aes(x=level2, y=diffwtd), colour='blue')
	p = p + geom_point(data=multilevelPSA@level2.summary, aes(x=level2, y=diffwtd, size=n), fill=ggplot.alpha('blue', .6), stat='identity', shape=21, colour='black')
	if(level2.rug.plot) {
		p = p + geom_rug(data=multilevelPSA@level2.summary, aes(x=NULL, y=diffwtd), alpha=.6, size=.5, colour='blue')
	}
	p = p + xlab(ylab) + ylab(xlab) + scale_size_continuous('Size')
	if(!is.null(title)) {
		p = p + opts(title=title)
	}
	return(p)
}

#'
#' @exportMethod plotpsa
setGeneric('plotpsa', function(multilevelPSA, ...) standardGeneric('plotpsa'))

setMethod('plotpsa', signature('multilevel.psa'), plotpsa.multilevel.psa)
