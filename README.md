
<!-- README.md is generated from README.Rmd. Please edit that file -->

# analyzer

<!-- badges: start -->

<!-- badges: end -->

The goal of analyzer is to make data analysis easy and accessible to
everyone by automatically performing the common tasks involved in data
analysis. With one command this can also create an R notebook (.rmd)
file and convert it into html or pdf file. The notebooks can also be
made interactive.

``` r
library(analyzer)
#> analyzer loaded successfully
library(ggplot2)
```

## Example - Notebook

This is a basic example which shows you how to solve a common problem.
By running the following command a notebook can be generated:

``` r
GenerateReport(dtpath = "mtcars.csv",
               catVars = c("cyl", "vs", "am", "gear"),
               yvar = "vs", model = "binClass",
               output_format = 'html_document',
               title = "Report", tempDir = "~/Documents/temp",
               interactive.plots = FALSE)
```

In the above command we are asking to generate an HTML file
(output\_format = ‘html\_document’) for the data **mtcars.csv** stored
in the working directory (dtpath = “mtcars.csv”). We defined the
variables (“cyl”, “vs”, “am”, “gear”) as categorical through the
parameter ‘catVars’ and variable “vs” as the dependent variable.

For more details on other parameters, look into the help of
**GenerateReport** function. The generated notebook will have all the
relevant code snippents for this particular data along with the outputs.
The notebook can be used as a reference and modified as required.

## Plots

**analyzer** can also be used to automatically generate plots for all
the columns in the data. If the data has a dependent (response) variable
pass it through the **yvar** argument and its type through **yclass**
argument.

``` r
# Simple plot for one variable
p <- plottr(mtcars$mpg)
#> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
plot(p$x)
```

<img src="man/figures/README-plot_var-1.png" width="100%" />

``` r
# default plots for all the variables in mtcars
p <- plottr(mtcars, yvar = "disp", yclass = "numeric")
#> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
#> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
#> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
#> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
#> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
#> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
#> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
#> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
#> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
#> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
#> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
plot(p$mpg)
```

<img src="man/figures/README-plot_df-1.png" width="100%" />

The left plot is the univariate plot of ‘mpg’. Right plot is bivariate
plot with the dependent variable ‘disp’.

External functions can be used to create custom plots. the function
**plottr** can take 6 functions for plots of different types of
variables. Look into the help of plottr for more details.

``` r
# Define a function for plot for continuous independent and Continuous dependent variables
custom_plot_for_continuous_vars <- function(dat, xname, yname, ...) {
  
  xyplot <- ggplot(dat, aes_string(x=xname, y=yname)) +
    geom_point(alpha = 0.6, color = "#3c4fde") + geom_rug()
  xyplot <- gridExtra::arrangeGrob(xyplot,
                                   top = paste0("New plot of ",
                                                xname, " and ", yname)
  )

  return(xyplot)
  
}
```

The parameters dat, xname, yname and … must present. Additional
arguments can be added.

    #> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    #> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    #> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    #> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    #> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    #> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    #> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    #> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    #> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    #> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    #> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

<img src="man/figures/README-plot_df2-1.png" width="100%" />

FUN3 arguemnt is used for generating plots for Continuous independent
and Continuous dependent variables. This will update the right plot.
Left (Univariate) plots can also be changeed by using FUN1 (for
Continous) and FUN2 (for Categorical) variables.

``` r
# Define a function for plot for continuous independent and Continuous dependent variables
custom_plot2 <- function(dat, xname, ...) {
  
  # histogram
  p1 <- ggplot(dat, aes_string(x=xname)) +
    geom_histogram(color="black")

  return(p1)
  
}
```

    #> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    #> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    #> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    #> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    #> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    #> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    #> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    #> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    #> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    #> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    #> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

<img src="man/figures/README-plot_df3-1.png" width="100%" />

## Correlation and Association
