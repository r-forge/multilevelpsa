\name{missingPlot}
\alias{missingPlot}
\title{Returns a heat map graphic representing missinging of variables grouped by...}
\usage{missingPlot(vars, grouping, grid=FALSE, flip=FALSE, xlab, ylab)}
\description{Returns a heat map graphic representing missinging of variables grouped by
the given grouping vector.}
\details{TODO: Need more details}
\value{a ggplot2 expression}
\arguments{\item{vars}{a data frame containing the variables to visualize missingness}
\item{grouping}{a vector of length nrow(vars) corresponding to how missing will be grouped by}
\item{grid}{whether to draw a grid between tiles}
\item{flip}{flip the x-axis and y-axis so that the grouping variable is on the y-axis}
\item{xlab}{the label to place on the x-axis}
\item{ylab}{the label to place on the y-axis}}
