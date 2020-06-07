#' Generate the report
#'
#' \code{GenerateReport} generates the markdown report in one command
#'
#' This function cretes a rmarkdowwn report which can be converted to
#' html or pdf format file.
#'
#' @param dtpath dataset path
#' @param catVars vector of categorical variables names
#' @param yvar y variable name if present else \code{NULL}
#' @param yclass class of y variable, else \code{NULL}
#' @param title Title of the generated report
#' @param output_format output report format. \code{'html_documennt'} for
#' html file.
#' @param tempDir Directory where the output files needs to be stored.
#' @param interactive.plots for interactive variable exploration
#'
#' @return creates a rmarkdown and html/pdf file
#'
#' @examples
#'
#' \dontrun{
#' # Assigning the temporary folder in Documnets/temp fodler
#' GenerateReport(dtpath = "~/Documents/mtcars.csv", catVars = catVars,
#'                yvar = "cyl", yclass = "factor", output_format = "html_document",
#'                title = "Report", tempDir = "~/Documents/temp",
#'                interactive.plots = FALSE)
#' }
#' @export
GenerateReport <- function(dtpath, catVars, yvar = NULL, yclass = NULL, title = "Report",
                           output_format = "html_document", tempDir = file.path(getwd(), "temp"),
                           interactive.plots = FALSE) {

  tx <- GenerateReport_(dtpath, catVars, yvar, yclass, tempDir = tempDir, title = title,
                        interactive.plots = interactive.plots)
  cat(tx, file = file.path(tempDir, "report.rmd"))
  if (requireNamespace("rmarkdown", quietly = TRUE)) {
    rmarkdown::render(input = file.path(tempDir, "report.rmd"), output_format = output_format)
    if (interactive.plots) {
      rmarkdown::run(file = file.path(tempDir, "report.rmd"))
    }
  } else {
    stop("Please install library 'rmarkdown' to create the html/pdf file.")
  }

}


GenerateReport_ <- function(dtpath, catVars, yvar, yclass, tempDir, title, interactive.plots) {

  dtname <- basename(dtpath)

  # creating the directory if not present
  ifelse(!dir.exists(file.path(tempDir)), dir.create(file.path(tempDir)), FALSE)

  # Header
  header <- generateHeader(title, interactive.plots)

  # Introduction
  intro <- generateIntro(dtname)

  # Data
  dataInfo <- generateDataInfo(dtpath, catVars)

  # Missing Values
  missingInfo <- generateMissingInfo()

  # Variable Exploration
  variableEx <- generateVarEx(dtpath, catVars, yvar = yvar, yclass = yclass, interactive.plots = interactive.plots)

  # Correlation
  associationInfo <- getAssociation(catVars)

  # combining all the parts
  tx <- paste0(header,
               "\n\n### INTRODUCTION \n",
               intro,
               "\n\n### DATA \n",
               dataInfo,
               "\n\n#### MISSING VALUES \n",
               missingInfo,
               "\n\n####  VARIABLE EXPLORATION \n",
               variableEx,
               "\n\n#### CORRELATION & ASSOCIATION \n",
               associationInfo)

  return(tx)

}

generateHeader <- function(title, interactive.plots) {

  out <- paste0('---
title: ',
title,
'\nparams:
  consoleWidth: 80
  yclass: factor
  yvar: cyl
output:
  html_document:
    df_print: paged
    theme: journal
    toc: yes
    toc_depth: 4
    toc_float: no
  pdf_document:
    toc: yes
    toc_depth: 4
  word_document:
    toc: yes
    toc_depth: 4
')

  if (interactive.plots) {
    out <- paste0(out,
'runtime: shiny')
  }

  out <- paste0(out,
'
---

```{r global_options, include=FALSE}
knitr::opts_chunk$set(fig.width=12, fig.height=5, fig.path="Figs/",fig.fullwidth=TRUE,
                      warning=FALSE, message=FALSE, comment = "")
options(width = params$consoleWidth)
```'
)
  return(out)
}

generateIntro <- function(dtname) {
  out <- paste0('This project is a bare bone exploration of the data ',
  dtname, '. Make the changes as required.

```{r libraries_load}
# Loading the libraries
library(analyzer)
library(dplyr)
library(corrplot)
```

***')
  return(out)
}

generateDataInfo <- function(dtpath, catVars) {

  tx1 <- paste0(
"First, let's load the data and take a look at its dimension and first few rows.
```{r data_load}
tb <- read.csv('",
dtpath,
"')")

  # declaring categorical variables
  if (is.null(catVars)){
    cattx <- "# No categorical variables"
  } else {
    cattx <- paste0("factor_vars <- c('", paste0(catVars, collapse = "', '"), "')")
    cattx <- paste0(
"\n# Defining factor variables \n",
      cattx,
"\n\n# Converting categorical variables into factors
for (i in factor_vars) {
  tb[,i] <- as.factor(tb[,i])
}
```")
  }

  out <- paste0(tx1, "\n", cattx, "\n",
"\nThe dimension of data is
```{r data_dim, echo=F}
cat(paste0('Columns: ', prettyNum(ncol(tb), big.mark = ','), '\nRows: ', prettyNum(nrow(tb), big.mark = ','), '\nUnique Rows: ', prettyNum(length(unique(tb)), big.mark = ',')))
```

```{r data_head}
head(tb)
```

***
")

  return(out)
}

generateMissingInfo <- function() {

  out <- paste0(
"By plotting the proportion of missing values we can see see which variables has the maximum
counts of missing values. From the plot we can see that column 'mpg' has the highest number
of missing values. Rest columns doesn't have any missing values.

```{r NA_plots}
# Plotting the missing values
analyzer::plotNA(tb, row.level = TRUE)
```

In the right plot we can see how missing values (shown in red color) are spread across all the
columns and all the rows.

***
"
)
  return(out)

}

generateVarEx <- function(dtpath, catVars, yvar, yclass, interactive.plots) {

  # reading the data to get the column names
  tb_ <- read.csv(dtpath)
  columns <- colnames(tb_)

  tx <- paste0(
"In this section all the individual variables are being explored.
Variable **'`r params$yvar`'** is selected as the response (or dependent) variable. While,
the remaining variables are selected as the explanatory (or independent) variables.

First, let's create and save all the plots:
```{r save_plots_vars}
variable_plots <- plottrWrapper(tb, yvar = '", yvar, "',
                                yclass = '", yclass, "', inc.density = T)
```"
  )

  # for interactive report
  if (interactive.plots){
    tx <- paste0(tx,"\n\n",
                 paste(readLines(file.path("report_temp","varExp.txt")), collapse="\n")
              )
  } else {
    for (cn in columns){
      tx <- paste0(tx, "\n\n",
"##### **Variable: ", cn, "**
```{r variable_", cn, "}
explainer(tb$", cn,")

# Plot
plot(variable_plots$", cn, ")
```")
      if (!cn %in% catVars) {
        tx <- paste0(tx, "\n\n",
"**Normality test**
```{r, echo = FALSE}
# QQ plot
ggplot(tb, aes(sample = ", cn, ")) + stat_qq(color='red', alpha = 0.6) + stat_qq_line() + theme_minimal()

# Normality assumption test
nt <- norm_test_fun(tb$",cn,")
```

The `r nt$method` has a p-value of **`r nt$p.value`**.
Since `r ifelse(nt$p.value < 0.05, 'p-value is less than the significance level (0.05), we',
'p-value is not below the significance level (0.05), we do not have sufficient evidence to')` reject the null hypothesis.
Therefore, we can say that this variable **`r ifelse(nt$p.value < 0.05, 'does not follow', 'follows')` the normal distribution**.")
      }
    }
  }


  return(tx)
}

getAssociation <- function(catVars) {
  out <- paste0(
"In general there can be three types of association based on the data type of variables -
1. Between 2 continuous (numeric) variables
2. Between 2 categorical variables
3. Between 1 continuous and 1 categorical variables

In this section, each type will be analyzed separately. **association** function can be used to calculate these automatically.

```{r association}
corr_all <- association(tb, categorical = ",
paste0("c('", paste0(catVars, collapse = "', '"), "'))"),
"\n```")
  tx1 <- paste(readLines(file.path("report_temp","association_text.txt")), collapse="\n")
  out <- paste0(out, "\n\n", tx1)
  return(out)
}
