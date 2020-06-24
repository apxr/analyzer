#' Plots for Continuous independent and dependent variables
#'
#' This function is used by [plottr()] when both the dependent and independent
#' variables are continuous. This function can be used as a template to
#' define a custom function
#'
#' @param dat a data.frame with two columns (including the dependent)
#' @param xname name of independent (x) variable in \code{dat}
#' @param yname name of dependent (y) variable in \code{dat}
#' @param ... required
#'
#' @return a grob of plot
#'
#' @export

CxCy <- function(dat, xname, yname, ...) {
  xyplot <- ggplot(dat, aes_string(x=xname, y=yname)) +
    geom_point(alpha = 0.6, color = "#FF6666") + geom_rug() +
    theme_minimal()
  xyplot <- gridExtra::arrangeGrob(xyplot,
                                   top = paste0("Scatterplot of ",
                                                xname, " and ", yname)
  )

  return(xyplot)
}


QxCy <- function(dat, xname, yname, binwidth.x = NULL, binwidth.y = NULL, ...) {
  box1 <- ggplot(dat, aes_string(y = yname,fill = xname)) +
    geom_boxplot() +
    coord_flip() +
    labs(title="Boxplot") + theme_linedraw() +
    theme(plot.title = element_text(size=12,
                                    face="bold.italic", hjust = 0.5),
          legend.position="bottom",
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    theme_minimal() +
    facet_grid(rows=xname)

  # density histogram for different levels of x
  hist1 <- ggplot(dat, aes_string(x = yname, fill = xname))+
    geom_histogram(color="black", binwidth = binwidth.y)

  hist1 <- hist1 +
    labs(title="Histogram") + theme_linedraw() +
    theme_minimal() +
    theme(plot.title = element_text(size=12,
                                    face="bold.italic", hjust = 0.5),
          legend.position="bottom") +
    facet_grid(rows = xname)

  # combining to create xyplot
  lay <- cbind(1,1,1,2,2)
  xyplot <- gridExtra::arrangeGrob(hist1, box1, layout_matrix = lay,
                                   top = paste0(yname,
                                                ": For different levels of ",
                                                xname))

  return(xyplot)
}


CxQy <- function(dat, xname, yname, binwidth.x = NULL, binwidth.y = NULL, ...) {
  box1 <- ggplot(dat, aes_string(y = xname,fill = yname)) +
    geom_boxplot() +
    coord_flip() +
    labs(title="Boxplot") + theme_linedraw() +
    theme_minimal() +
    theme(plot.title = element_text(size=12, face="bold.italic",
                                    hjust = 0.5),
          legend.position="bottom",
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    facet_grid(rows=yname)

  # density histogram for different levels of y
  hist1 <- ggplot(dat, aes_string(x = xname, fill = yname))+
    geom_histogram(color="black", binwidth = binwidth.x)

  hist1 <- hist1 +
    labs(title="Histogram") + theme_linedraw() +
    theme_minimal() +
    theme(plot.title = element_text(size=12, face="bold.italic",
                                    hjust = 0.5),
          legend.position="bottom") +
    facet_grid(rows = yname)

  # combining to create xyplot
  lay <- cbind(1,1,1,2,2)
  xyplot <- gridExtra::arrangeGrob(hist1, box1, layout_matrix = lay,
                                   top = paste0(xname,
                                                ": For different levels of ",
                                                yname))

  return(xyplot)
}


QxQy <- function(dat, xname, yname, ...) {
  # cross tab histogram and number for ratio
  x <- dat[, xname]
  y <- dat[, yname]
  crosstab <- setNames(data.frame(table(x, y)),
                       c(xname, yname,"Count"))

  xyplot <- ggplot(crosstab, aes_string(xname, yname, fill= "Count")) +
    geom_tile(color = "grey69") +
    scale_fill_gradient2(low="white", high="#FF6666") +
    geom_text(aes(label=Count))+
    theme_minimal() +
    theme(panel.grid.major=element_blank(),
          plot.title = element_text(hjust = 0.5, size = 12),
          legend.position = "blank") +
    coord_flip()

  xyplot <- gridExtra::arrangeGrob(xyplot,
                                   top = paste0("Crosstab of ",
                                                xname, " and ", yname)
  )

  return(xyplot)
}


#' Plots for Continuous independent variables
#'
#' This function is used by [plottr()] when independent
#' variable is continuous. This function can be used as a template to
#' define a custom function.
#'
#' @param dat a data.frame with only one column
#' @param xname name of independent (x) variable in \code{dat}
#' @param ... required
#' @param binwidth for the histograms (extra parameters can be added like this)
#' @param inc.density Binary. True to include the density plot on histogram
#'
#' @return a grob of plot
#'
#' @export
Cx <- function(dat, xname, binwidth = NULL, inc.density = T, ...) {
  # histogram
  quants <- quantile(dat[,1], na.rm = T)

  iqr_ <- quants[4]-quants[2]
  quantdf <- data.frame(Value = c(quants[2]-iqr_*1.5,
                                  quants[3],
                                  quants[4]+1.5*iqr_),
                        labels=c("Whiskers","Median","Whiskers"))

  p1 <- ggplot(dat, aes_string(x=xname)) +
    geom_rect(aes(xmin = quants[2], xmax = quants[4],
                  ymin = 0, ymax = Inf, fill = "IQR"),
              color=NA, alpha = 0.002)

  if (inc.density) {
    p1 <- p1 + geom_density(aes(y = ..density..),
                            alpha=.2, fill="#000000") +
      stat_bin(aes(y=..density..), colour="black",
               fill="#000000", alpha = 0.6, binwidth = binwidth)
    titl <- "Histogram & Density plot"
    ytitl <- "Density"
  } else {
    p1 <- p1 + geom_histogram(color="black", binwidth = binwidth)
    titl <- "Histogram"
    ytitl <- "Frequency"
  }
  p1 <- p1 +
    labs(title=titl, y=ytitl) +
    scale_fill_manual('',
                      values = '#FF6666',
                      guide = guide_legend(override.aes = list(alpha = 0.5))) +
    theme_minimal() +
    theme(plot.title = element_text(size=12, face="bold.italic", hjust = 0.5),
          legend.position = "bottom") +
    geom_vline(aes(xintercept=Value, color=labels),
               data=quantdf, show.legend=T) +
    scale_color_manual("", values=c("Whiskers"="grey4",
                                    "Median"="#FF6666",
                                    "Whiskers"="grey4"))

  return(p1)
}


Qx <- function(dat, xname, ...) {

  # pie chart of x distribution
  piedf <- dplyr::group_by(dat, !!as.name(xname))
  piedf <- dplyr::summarise(piedf, counts = n())
  piedf <- dplyr::arrange(piedf, desc(counts))
  piedf <- dplyr::mutate(piedf, prop = round(counts*100/sum(counts), 1),
                         lab.ypos = cumsum(prop) - 0.5*prop)

  p1 <- ggplot(piedf, aes(x = "", y = prop, fill = !!as.name(xname))) +
    geom_bar(width = 1, stat = "identity", color = "white") +
    # geom_text(aes(y = lab.ypos, label = prop), color = "white")+
    coord_polar("y", start = 0)+
    labs(title=paste0("Distribution of ", xname), x = NULL,y=NULL)+
    theme_void() + theme(legend.position="bottom") +
    theme(plot.title = element_text(size=12, face="bold.italic", hjust = 0.5))

  return(p1)
}
