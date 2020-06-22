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
#' @param model type of model - \code{linReg} for linear regression
#' \code{binClass} for binary classification and \code{multiClass} for
#' multiclass classification
#' @param title Title of the generated report
#' @param output_format output report format. \code{'html_documennt'} for
#' html file or \code{pdf_document} for pdf file output. OR
#' \code{c("html_document", "pdf_document")} for both.
#' @param output_dir Directory where the output files needs to be stored.
#' @param normality_test_method method for normality test for a variable.
#'   Values can be \code{shapiro}
#'   for Shapiro-Wilk test or
#'   \code{'anderson'} for 'Anderson-Darling' test of normality or \code{ks} for
#'  'Kolmogorov-Smirnov'
#' @param interactive.plots for interactive variable exploration
#' @param include.vars include only these variables from the full data
#'
#' @return creates a rmarkdown and html/pdf file. Invisibly returns \code{TRUE}
#' on successful run.
#'
#' @examples
#'#' # Assigning the temporary folder in Documnets/temp fodler
#' GenerateReport(dtpath = "~/Documents/mtcars.csv",
#'                catVars = c("cyl", "vs", "am", "gear"),
#'                yvar = "vs", model = "binClass",
#'                output_format = NULL,
#'                title = "Report",
#'                output_dir = "~/Documents/temp",
#'                interactive.plots = FALSE)
#'
#' @importFrom utils read.csv
#'
#' @export
GenerateReport <- function(dtpath,
                           catVars,
                           yvar = NULL,
                           model = 'linReg',
                           title = "Report",
                           output_format = 'html_document',
                           output_dir = file.path(getwd(), "temp"),
                           normality_test_method = "ks",
                           interactive.plots = FALSE,
                           include.vars = NULL) {

  # reading the data to get the column names
  if (requireNamespace("data.table", quietly = TRUE)) {
    tb_ <- data.table::fread(dtpath)
  } else {
    tb_ <- read.csv(dtpath)
  }

  if (is.null(model)) {
    if (!is.null(yvar)) {
      stop("If yvar is not NULL then 'model' parameter
           is required and can't be NULL")
    } else {
      model <- "linReg"
    }
  }

  if (!is.null(yvar) & model != 'linReg') {
    if (!yvar %in% catVars) {
      catVars <- c(catVars, yvar)
    }
  }


  if (!requireNamespace("MASS", quietly = TRUE) & !is.null(model)) {
    stop("MASS library is required for the variable selection part")
  }

  if (!requireNamespace("nnet", quietly = TRUE) & model == "multiClass") {
    stop("nnet library is required for multiclass classification")
  }

  # Creating the .rmd file
  tx <- GenerateReport_(dtpath,
                        catVars,
                        yvar,
                        model,
                        output_dir = output_dir,
                        title = title,
                        normality_test_method = normality_test_method,
                        interactive.plots = interactive.plots,
                        df = tb_)

  cat(tx, file = file.path(output_dir, "report.rmd"))

  # Converting into html/interactive report
  if (!is.null(output_format)) {
    if (requireNamespace("rmarkdown", quietly = TRUE)) {
      rmarkdown::render(input = file.path(output_dir, "report.rmd"),
                        output_format = output_format)
    } else {
      stop("Please install library 'rmarkdown' to create the html/pdf file.")
    }
  }

  # for the interactive report
  if (interactive.plots) {
    if (requireNamespace("shiny", quietly = TRUE) & !is.null(model)) {
      rmarkdown::run(file = file.path(output_dir, "report.rmd"))
    } else {
      stop("MASS library is required for the variable selection part")
    }
  }

  invisible(TRUE)
}


GenerateReport_ <- function(dtpath,
                            catVars,
                            yvar,
                            model,
                            output_dir,
                            title,
                            normality_test_method,
                            interactive.plots,
                            df) {

  dtname <- basename(dtpath)
  columns <- colnames(df)

  # creating the directory if not present
  ifelse(!dir.exists(file.path(output_dir)),
         dir.create(file.path(output_dir)),
         FALSE)

  # Header
  header <- generateHeader(title, interactive.plots)

  # Introduction
  intro <- generateIntro(dtname, model)

  # Data
  dataInfo <- generateDataInfo(dtpath, catVars)

  # Missing Values
  missingInfo <- generateMissingInfo()

  # Variable Exploration
  variableEx <- generateVarEx(columns, catVars, yvar = yvar, model = model,
                              interactive.plots = interactive.plots,
                              normality_test_method = normality_test_method)

  # Correlation
  associationInfo <- getAssociation(columns, catVars, normality_test_method)

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

  if (!is.null(yvar)) {
    # Variable Selection using stepAIC
    stepInfo <- getStepInfo(catVars, yvar, model, columns)

    tx <- paste0(tx,  "\n\n### VARIABLE SELECTION \n",
                 stepInfo)
  }

  return(tx)

}

generateHeader <- function(title, interactive.plots) {

  out <- paste0('---
title: ',
title,
'\nparams:
  consoleWidth: 80
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
knitr::opts_chunk$set(fig.width=12, fig.height=5,
                      fig.path="Figs/",fig.fullwidth=TRUE,
                      warning=FALSE, message=FALSE, comment = "")
options(width = params$consoleWidth)
```'
)
  return(out)
}

generateIntro <- function(dtname,
                          model) {
  out <- paste0('This project is a bare bone exploration of the data ',
  dtname, '. Make the changes as required.

```{r libraries_load}
# Loading the libraries
library(analyzer)
library(dplyr)
library(corrplot)
library(ggplot2)
')

  if (!is.null(model)) {
    out <- paste0(out, 'library(MASS)')
    if (model == "multiClass") {
      out <- paste0(out, "\nlibrary(nnet)")
    }
  }
  out <- paste0(out, '
```

***')
  return(out)
}

generateDataInfo <- function(dtpath,
                             catVars) {

  tx1 <- paste0(
"First, let's load the data and take a look at its dimension and first few rows.
```{r data_load}
tb <- read.csv('",
dtpath,
"')")

  # declaring categorical variables
  if (is.null(catVars)){
    cattx <- "# No categorical variables\n```\n"
  } else {
    cattx <- paste0("factor_vars <- c('",
                    paste0(catVars, collapse = "', '"),
                    "')")
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
cat(paste0('Columns: ', prettyNum(ncol(tb), big.mark = ','), '\nRows: ', prettyNum(nrow(tb), big.mark = ','), '\nUnique Rows: ', prettyNum(nrow(unique(tb)), big.mark = ',')))
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

generateVarEx <- function(columns,
                          catVars,
                          yvar,
                          model,
                          interactive.plots,
                          normality_test_method) {

  yclass = "'numeric'"
  if (model != "linReg") yclass = "'factor'"

  if (is.null(yvar)) {
    yvarname <- "No"
    yvar <- "NULL"
    yclass <- "NULL"
  } else {
    yvarname <- yvar <- paste0("'", yvar, "'")
  }

  tx <- paste0(
"In this section all the individual variables are being explored. **",
yvarname, "**
variable is selected as the response (or dependent) variable. While
the remaining variables are selected as the explanatory (or independent) variables.

First, let's create and save all the plots:
```{r save_plots_vars}
variable_plots <- plottr(tb, yvar = ", yvar, ",
                         yclass = ", yclass, ")
```"
  )

  # for interactive report
  if (interactive.plots){

    text_path <- system.file("report_template", "varExp.txt", package = "analyzer")
    if (text_path == "") {
      warning("Could not find required template. Try re-installing 'analyzer'.
                                 If problem doesn't resolve, contact the author.")
      tx <- "**Could not find required template. Try re-installing 'analyzer'. If problem doesn't resolve, contact the author.**"
    } else {
      tx <- paste0(tx,"\n\n", paste(readLines(file.path(text_path)), collapse="\n"))
    }

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
```{r}
# QQ plot
ggplot(tb, aes(sample = ", cn, ")) + stat_qq(color='red', alpha = 0.6) + stat_qq_line() + theme_minimal()

# Normality assumption test
nt <- norm_test_fun(tb$",cn,", method = '",normality_test_method, "')
```

The `r nt$method` has a p-value of **`r round(nt$p.value, 4)`**.
Since `r ifelse(nt$p.value < 0.05, 'p-value is less than the significance level (0.05), we',
'p-value is not below the significance level (0.05), we do not have sufficient evidence to')`
reject the null hypothesis. Therefore, we can say that this variable **`r ifelse(nt$p.value < 0.05, 'does not follow', 'follows')`
the normal distribution**.")
      }
    }
  }

  tx <- paste0(tx, "

***

")

  return(tx)
}

getAssociation <- function(columns,
                           catVars,
                           normality_test_method) {

  if (normality_test_method == "ks") {
    normtest <- "Kolmogorov-Smirnov Test"
  } else if (normality_test_method == "darling") {
    normtest <- "Anderson-Darling Test"
  } else {
    normtest <- "Shapiro-Wilk Normality Test"
  }

  out <- paste0(
"In general there can be three types of association based on the data type of variables -
1. Between 2 continuous (numeric) variables
2. Between 2 categorical variables
3. Between 1 continuous and 1 categorical variables

In this section, each type will be analyzed separately. **association** function can be used to calculate these automatically.
The normality test of the variables will be done using the **'", normtest, "'**.

```{r association}
corr_all <- association(tb, categorical = ",
paste0("c('", paste0(catVars, collapse = "', '"), "'), normality_test_method = '", normality_test_method, "')"),
"\n```")

  numVars <- setdiff(columns, catVars)

  # for CC
  if (length(numVars) > 0) {
    text_path <- system.file("report_template", "QQ_text.txt", package = "analyzer")
    if (text_path == "") {
      warning("Could not find required template. Try re-installing 'analyzer'.
                                 If problem doesn't resolve, contact the author.")
      tx1 <- "**Could not find required template. Try re-installing 'analyzer'. If problem doesn't resolve, contact the author.**"
    } else {
      tx1 <- paste(readLines(file.path(text_path)), collapse="\n")
    }

    out <- paste0(out, "\n\n", tx1)
  } else {
    out <- paste0(out, "\n\n", "##### Between 2 continuous (numeric) variables\n\n",
                  "**No continuous variable present**")
  }
  # for CC
  if (length(catVars) > 0) {
    text_path <- system.file("report_template", "CC_text.txt", package = "analyzer")
    if (text_path == "") {
      warning("Could not find required template. Try re-installing 'analyzer'.
                                 If problem doesn't resolve, contact the author.")
      tx1 <- "**Could not find required template. Try re-installing 'analyzer'. If problem doesn't resolve, contact the author.**"
    } else {
      tx1 <- paste(readLines(file.path(text_path)), collapse="\n")
    }
    out <- paste0(out, "\n\n", tx1)
  } else {
    out <- paste0(out, "\n\n", "##### Between 2 categorical (factor) variables\n\n",
                  "**No continuous variable present**")
  }
  # CQ
  if (length(catVars) > 0 & length(numVars) > 0) {
    text_path <- system.file("report_template", "CQ_text.txt", package = "analyzer")
    if (text_path == "") {
      warning("Could not find required template. Try re-installing 'analyzer'.
                                 If problem doesn't resolve, contact the author.")
      tx1 = "**Could not find required template. Try re-installing 'analyzer'. If problem doesn't resolve, contact the author.**"
    } else {
      tx1 <- paste(readLines(file.path(text_path)), collapse="\n")
    }
    out <- paste0(out, "\n\n", tx1)
  } else {
    out <- paste0(out, "\n\n", "##### Between 1 continuous and 1 categorical variables\n\n",
                  "**No such combination of variables present**")
  }

  out <- paste0(out, "

***

")
  return(out)
}

getStepInfo <- function(catVars,
                        yvar,
                        model,
                        allvars) {

  # reading and creating the base template
  text_path <- system.file("report_template", "stepwise.txt", package = "analyzer")
  if (text_path == "") {
    warning("Could not find required template. Try re-installing 'analyzer'.
                                 If problem doesn't resolve, contact the author.")
    out <- "**Could not find required template. Try re-installing 'analyzer'. If problem doesn't resolve, contact the author.**"
  } else {
    out <- paste(readLines(file.path(text_path)), collapse="\n")
  }

  # updating the modeling equation for regression/classification
  if (model == "binClass") {
    out <- gsub("back_up_model", "glm(upFormula, data = tb2, family = binomial(link=logit))", out)
    out <- gsub("forward_low_model", "glm(lowFormula, data = tb2, family = binomial(link=logit))", out)
  } else if (model == "linReg") {
    out <- gsub("back_up_model", "lm(upFormula, data = tb2)", out)
    out <- gsub("forward_low_model", "glm(lowFormula, data = tb2)", out)
  } else if (model == "multiClass") {
    out <- gsub("back_up_model", "multinom(upFormula, tb2, trace = FALSE)", out)
    out <- gsub("forward_low_model", "multinom(lowFormula, tb2, trace = FALSE)", out)
  }

  # Adding code to show the coefficients based on 'model' type
  out <- gsub("back1vars_place",
              ifelse(model=="multiClass",
                     "paste0(Backward$coefnames, collapse = ', ')",
                     "paste0(names(Backward$coefficients), collapse = ', ')"),
              out)
  out <- gsub("for1vars_place",
              ifelse(model=="multiClass",
                     "paste0(Forward$coefnames, collapse = ', ')",
                     "paste0(names(Forward$coefficients), collapse = ', ')"),
              out)
  out <- gsub("both1vars_place",
              ifelse(model=="multiClass",
                     "paste0(Both$coefnames, collapse = ', ')",
                     "paste0(names(Both$coefficients), collapse = ', ')"),
              out)

  # adding the upper formula (which includes all variables)
  out <- gsub("upFormula_place",
              paste(yvar, "~", paste(setdiff(allvars, yvar), collapse = "+")),
              out)

  # updating the y variable name in text
  out <- gsub("yvariable", yvar, out)
  return(out)
}
