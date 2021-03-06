% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods.R
\name{plot.analyzerPlot}
\alias{plot.analyzerPlot}
\title{Plots a plot of class 'analyzerPlot'}
\usage{
\method{plot}{analyzerPlot}(x, ...)
}
\arguments{
\item{x}{a plot of class \code{analyzerPlot}}

\item{...}{extra arguments if required}
}
\value{
Displays the plot
}
\description{
This function plots the plot generated by the library \code{analyzer}
}
\examples{
# creating the plot
p <- plottr(mtcars)
plot(p$mpg)

}
