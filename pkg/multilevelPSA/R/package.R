#' This packages provides functions to perform and visualize multilvel propensity
#' score analys.
#' 
#' \tabular{ll}{
#'  Package: \tab multilevelPSA\cr
#'  Type: \tab Package\cr
#'  Version: \tab 0.9\cr
#'  Date: \tab 2011-03-18\cr
#'  License: \tab GPL (>= 2)\cr
#'  LazyLoad: \tab yes\cr
#'  Depends: \tab ggplot2 plyr psych\cr
#' }
#'
#' This package extends the principles put forth by the \code{PSAgraphics} 
#' (Helmreich, Pruzek, & Xiong, 2010) for multilevel, or clustered, data.
#' 
#' @name multilevelPSA-package
#' @aliases multilevelPSA
#' @docType package
#' @title Multilevel Propensity Score Analysis
#' @author Jason Bryer \email{jason@@bryer.org}
#' @references \url{http://cran.r-project.org/web/packages/PSAgraphics/PSAgraphics.pdf}
#' 		\url{http://www.jstatsoft.org/v29/i06/}
#' @keywords psa multilevel
#' @seealso \code{\link{PSAgraphics}}
#' @import ggplot2 party psych plyr
NA

#' This class contains the results multielevelPSA that summarizes the results.
#'
#' @exportClass multilevel.psa
setClass('multilevel.psa', representation(
				level2.summary='data.frame',
				unweighted.summary='data.frame',
				level1.summary='data.frame',
				projection.intercept='numeric',
				plot.range='numeric',
				overall.ci='numeric',
				overall.wtd='numeric',
				overall.mnxy='numeric',
				overall.mnx='numeric',
				overall.mny='numeric',
				overall.wtss='numeric',
				overall.n='numeric',
				removed='integer'))


.First.lib <- function(libname, pkgname) {
	ggplot.alpha <<- function(...) get("alpha", grep("package:ggplot2$", search()))(...)
}

.Last.lib <- function(libname, pkgname) {
}
