#' Missing value visualization using ggplot2
#'
#' \code{plotNA} returns a grob visualizing the missing values in data
#'
#' This is a function which helps in visualizing the missing values in data
#' using plots. By default a bar plot is generated which shows the count of
#' missing values in each variable.
#'
#' If \code{order} is set as \code{TRUE} then the bars are arranged in order of
#' missing values. If \code{limit} is set as \code{TRUE} then limit of axis is
#' set to [0, nrow(tb)]. If \code{add_percent} is set as \code{TRUE} then
#' percent is added as text to the bars. If \code{row.level} is set to
#' \code{TRUE} then an additional plot is generated which shows which rows have
#' missing values and in which variable (reshape2
#' (\url{https://CRAN.R-project.org/package=reshape2}) library
#' is required for this).
#'
#' @param tb a data.frame
#' @param order (logical) Whether to order the variables based on missing values
#'   in plot
#' @param limit (logical) Whether to limit the plot to maximum missing value.
#'   FALSE means the limit of axis will be [0, nrow(tb)]
#' @param add_percent (logical) Whether to add percent as data labels on bar plot
#' @param row.level (logical) Whether to create plot at rows and variables level
#'
#' @return This function returns a grob of class 'analyzePlot' which has a bar
#'   plot showing the count of missing value for each variable. \code{order,
#'   limit, add_percent} can be used to modify the bar plot. An additional plot
#'   will be created and added to the grob if \code{row.level} is set as
#'   \code{TRUE}
#'
#' @examples
#' p <- plotNA(airquality)
#' # function to show the 'analyzerPlot' class plot
#' plot(p)
#' p1 <- plotNA(airquality, order = FALSE)
#' plot(p1)
#'
#' @import ggplot2
#' @import gridExtra
#' @import grid
#'
#' @export

plotNA <- function(tb,
                  order = T,
                  limit = T,
                  add_percent = T,
                  row.level = F) {

  # plot1: number of missing
  NAcount <- data.frame(apply(tb, 2, function(x) sum(is.na(x))))
  NAcount$Variable <- row.names(NAcount)
  colnames(NAcount) <- c("Missing", "Variable")
  NAcount$Percent <- paste0(round(100*NAcount$Missing/nrow(tb),2), "%")
  NAcount$Percent <- ifelse(NAcount$Percent=="0%", "-", NAcount$Percent)

  if (order) {
    p1 <- ggplot(data=NAcount, aes(x=reorder(Variable, Missing), y=Missing))
  } else {
    p1 <- ggplot(data=NAcount, aes(x=Variable, y=Missing))
  }
  p1 <- p1 +
    geom_bar(stat="identity", aes(fill="tomato3"), color="white", alpha = 0.7) +
    theme_minimal() +
    coord_flip()

  if (limit) {
    p1 <- p1 + ylim(0, ceiling(1.2*max(NAcount$Missing)))
  } else {
    p1 <- p1 + ylim(0,nrow(tb))
  }
  if (add_percent) {
    p1 <- p1 + geom_text(aes(label=Percent), vjust=0, color = "black", size=4)
  }
  p1 <- p1 + ggtitle("Missing values per variable") +
    ylab("Count") + xlab("Variable") +
    theme(legend.position = "bottom",
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
    scale_fill_manual(name = "",
                      values = c("tomato3"),
                      labels = c("Missing"))

  # plot2: vars*rows plot
  if (row.level) {
    # check for reshape2 package. only then make this plot
    if (requireNamespace("reshape2", quietly = TRUE)) {
      level <- tryCatch(ggplot_build(p1)$layout$panel_params[[1]]$y.labels,
                        error = function(e){return(colnames(tb))})

      plot_na_ <- function(tb2, level) {
        tb2$isNA <- is.na(tb2$value)
        p2 <- ggplot(tb2, aes_string("variable", "index", fill = "isNA")) +
          geom_raster(alpha=1) +
          scale_fill_manual(name = "",
                            values = c('#ebebeb', 'tomato3'),
                            labels = c("Present", "Missing")) +
          scale_x_discrete(limits = level) +
          labs(x = "Variable",
               y = "Row Number", title = "Missing values in rows") +
          coord_flip() +
          theme_minimal() +
          theme(legend.position = "bottom",
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank())
        return(p2)
      }
      tb <- data.frame(tb)
      tb$index <- 1:nrow(tb)

      tb <- reshape2::melt(tb, id.vars = "index")
      p2 <- plot_na_(tb, level)
      p1 <- gridExtra::arrangeGrob(p1, p2, layout_matrix = cbind(1,1,2,2,2))
    } else {
      warning("To make row level plot, please install 'reshape2' library")
    }
  }

  # setting the class
  class(p1) <- c("analyzerPlot", class(p1))
  return(p1)
}
