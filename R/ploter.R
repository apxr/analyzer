
ploterWrapper <- function(tb, yvar = NULL) {



}


ploter <- function(x, y = NULL, xclass = NULL, yclass = NULL, callasfactor = 10, xname = 'x', yname = 'y',
                   binwidth = NULL) {

  # setting up ggplot theme
  old_theme <- ggplot2::theme_set(theme_minimal())

  if (!is.null(y) & !yclass %in% c("numeric", "factor")) stop("Pass the class of y as either 'numeric' or 'factor'")
  # finding the class of x if xclass is absent
  if (!is.null(xclass)) {
    if (!xclass %in% c("numeric", "factor")) stop("Class can only be 'numeric' or 'factor'")
    # for factor
    if (is.factor(x) | is.character(x)) xclass <- "factor"
    else if (is.numeric(x)) {
      # if unique values are very less, then call this is a factor
      if (length(unique(x)) < callasfactor) xclass <- "factor"
      else xcalss <- "numeric"
    }
  }

  # creating the dataset for plots
  xyplot <- NULL
  if (is.null(y)) {
    dat <- data.frame(x)
    colnames(dat) <- xname
  } else {
    dat <- data.frame(x, y)
    colnames(dat) <- c(xname, yname)

    # making X-Y plot
    if (yclass == "numeric"){
      if (xclass == "numeric") {
        # scatter plot
        sc <- ggplot(dat, aes_string(x=xname, y=yname)) +
          geom_point(alpha = 0.6) + geom_rug()
      } else {
        # box plot with different levels of x
        box1 <- ggplot(dat, aes_string(y = yname, x = xname, fill = xname)) +
          geom_boxplot() +
          theme(legend.position="bottom") +
          labs(title=paste0(yname, ": Boxplot")) +
          theme(plot.title = element_text(size=12, face="bold.italic", hjust = 0.5))

        # density histogram for different levels of x
        if (length(unique(x)) <= 5) {
          hist1 <- ggplot(dat, aes_string(x = yname, group = xname, fill = xname))+
            geom_density(alpha=0.3)+
            stat_bin(aes(y=..count..), position='dodge') +
            theme(legend.position="bottom") +
            labs(title=paste0(yname, ": Histogram and density plot")) +
            theme(plot.title = element_text(size=12, face="bold.italic", hjust = 0.5))
        }
      }
    } else {
      if (xclass == "numeric") {
        # box plot with different levels of y
        box1 <- ggplot(dat, aes_string(y = xname,fill = yname)) +
          geom_boxplot() +
          coord_flip() +
          labs(title=paste0(xname, ": Boxplot for all values of ", yname)) +
          theme(plot.title = element_text(size=12, face="bold.italic", hjust = 0.5),
                legend.position="bottom",
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank()) +
          facet_grid(rows=yname)

        # density histogram for different levels of y

        hist1 <- ggplot(dat, aes_string(x = xname, fill = yname))+
          geom_density(aes(y = ..count..), alpha=0.1)+
          stat_bin(aes(y=..count..), position='dodge') +
          theme(legend.position="bottom")+
          labs(title=paste0(xname, ": Histogram plot for all values of ", yname)) +
          theme(plot.title = element_text(size=12, face="bold.italic", hjust = 0.5)) +
          facet_grid(rows = yname)

      } else {
        # cross tab histogram and number for ratio
        crosstab <- setNames(data.frame(table(x, y)),
                             c(xname, yname,"Count"))

        heatmap <- ggplot(crosstab, aes_string(xname, yname, fill= "Count")) +
          geom_tile(color = "gray") +
          scale_fill_gradient2(low="white", high="red") +
          labs(title="Crosstab")+
          geom_text(aes(label=Count))+
          theme(panel.grid.major=element_blank(),
            plot.title = element_text(hjust = 0.5, size = 12))

      }
    }
  }
  # for numeric plats (univariate)
  # plot 1: density and histogram
  if (xclass == "numeric") {
    # histogram
    quants <- quantile(x, na.rm = T)
    p1 <- ggplot(dat, aes_string(x=xname)) +
      geom_rect(aes(xmin = 15, xmax = 25, ymin = 0, ymax = Inf),
                fill = "#FF6666", alpha = 0.005)+
      geom_density(aes(y = ..count..), alpha=.2, fill="#000000") +
      stat_bin(aes(y=..count..), colour="black", fill="#000000", alpha = 0.6, binwidth = binwidth) +
      labs(title=paste0(xname, ": Histogram, density and Bxsplot plot"), x = NULL, y=NULL) +
      theme(plot.title = element_text(size=12, face="bold.italic", hjust = 0.5))


    # plot 2: box plot
    # p2 <- ggplot(dat, aes_string(y = xname)) +
    #   geom_boxplot(fill = "#FF6666", alpha = 0.6) +
    #   coord_flip()
    #
    # p1g <- ggplotGrob(p1)
    # p2g <- ggplotGrob(p2)
    # p1g$widths = p2g$widths
    # g <- arrangeGrob(p1g, p2g, heights=c(4,1))
  } else if (xclass == "factor") {
    # pie chart of x distribution
    piedf <- dat %>%
      dplyr::group_by(!!as.name(xname)) %>%
      dplyr::summarise(counts = n()) %>%
      dplyr::arrange(desc(counts)) %>%
      dplyr::mutate(prop = round(counts*100/sum(counts), 1),
             lab.ypos = cumsum(prop) - 0.5*prop)

    piech <- ggplot(piedf, aes(x = "", y = prop, fill = !!as.name(xname))) +
      geom_bar(width = 1, stat = "identity", color = "white") +
      geom_text(aes(y = lab.ypos, label = prop), color = "white")+
      coord_polar("y", start = 0)+
      labs(title=paste0(xname, ": Pie chart"), x = NULL,y=NULL)+
      theme_void() + theme(legend.position="bottom") +
      theme(plot.title = element_text(size=12, face="bold.italic", hjust = 0.5))
  }

  # setting back the original theme
  ggplot2::theme_set(old_theme)

}
