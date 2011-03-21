#' @include package.R
NA

#' Returns a heat map graphic representing missinging of variables grouped by
#' the given grouping vector.
#' 
#' TODO: Need more details
#' 
#' @param vars a data frame containing the variables to visualize missingness
#' @param grouping a vector of length nrow(vars) corresponding to how missing will be grouped by
#' @param grid whether to draw a grid between tiles
#' @param flip flip the x-axis and y-axis so that the grouping variable is on the y-axis
#' @param xlab the label to place on the x-axis
#' @param ylab the label to place on the y-axis 
#' @return a ggplot2 expression
#' @export missingPlot
missingPlot <- function(vars, grouping, grid=FALSE, flip=FALSE, xlab=NULL, ylab=NULL) {
	empty <- plyr::empty
	
	grouping = as.character(grouping)
	grouping[is.na(grouping)] = 'Unknown'
	grouping = as.factor(grouping)
	testing.NA = matrix(ncol=length(levels(grouping)), nrow=(ncol(vars)))
	for(i in 1:(dim(vars)[2])) {
		testing.NA[i,] = tapply(vars[[i]], grouping, function(x) sum(is.na(x)) / length(x))
	}
	testing.NA = testing.NA * 100
	dimnames(testing.NA) = list(names(vars), unique(grouping))
	testing.NA2 = melt(testing.NA)
	if(flip) {
		p = ggplot(testing.NA2, aes(x=X2, y=X1, fill=value))
	} else {
		p = ggplot(testing.NA2, aes(x=X1, y=X2, fill=value))
	}
	p = ggplot(testing.NA2, aes(x=X2, y=X1, fill=value))
	if(grid) {
		p = p + geom_tile(colour='grey')
	} else {
		p = p + geom_tile()
	}
	p = p + xlab(xlab) + ylab(ylab)
	p = p + opts(axis.text.y=theme_text(size=6, hjust=1, vjust=.5), axis.text.x=theme_text(size=6, angle=-90, hjust=0, vjust=.5), axis.ticks=theme_blank())
	p = p + scale_fill_gradient('Missingness', low='white', high='red', breaks=seq(0, 100, 10), labels=paste(seq(0,100,10), '%', sep=''))
	return(p)
}
