% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{consoleBoxplot}
\alias{consoleBoxplot}
\title{Boxplot on the console}
\usage{
consoleBoxplot(x)
}
\arguments{
\item{x}{a numeric vector of length at least 3}
}
\value{
prints a boxplot on the console which has:
\itemize{
 \item \code{|} at start and end means the minimum and maximum value respectively
 \item \code{<==*==>} The IQR region
 \item \code{*} shows the median
 \item \code{...} everything else in between
}
Gives a warning of potential outliers (if present)
}
\description{
\code{consoleBoxplot} prints the boxplot on console.
}
\details{
This function is for the numeric vectors. It prints a boxplot in a single line
on the console. It automatically adjusts for the width of the console.
The input vector must have a length of three, otherwise the function
will throw a warning and not print any plot.

In case of any potential outliers (based on 1.5*IQR criteria), this wil
give a warning.
This function is used in the \code{explainer}.
}
\examples{
consoleBoxplot(mtcars$mpg)

}
