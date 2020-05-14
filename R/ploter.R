
ploterWrapper <- function(tb, yvar = NULL) {



}


ploter <- function(x, y = NULL, xclass = NULL, yclass = NULL, xname = 'x', yname = 'y',
                   binwidth.x = NULL, binwidth.y = NULL) {

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
        box1 <- ggplot(dat, aes_string(y = yname,fill = xname)) +
          geom_boxplot() +
          coord_flip() +
          labs(title=paste0(yname, ": Boxplot for all values of ", xname)) +
          theme(plot.title = element_text(size=12, face="bold.italic", hjust = 0.5),
                legend.position="bottom",
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank()) +
          facet_grid(rows=xname)

        # density histogram for different levels of x
        hist1 <- ggplot(dat, aes_string(x = yname, fill = xname))+
          geom_density(aes(y = ..count..), alpha=0.1)+
          stat_bin(aes(y=..count..), position='dodge') +
          theme(legend.position="bottom")+
          labs(title=paste0(yname, ": Histogram plot for all values of ", xname)) +
          theme(plot.title = element_text(size=12, face="bold.italic", hjust = 0.5)) +
          facet_grid(rows = xname)
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
          geom_raster(color = "gray") +
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

    iqr_ <- quants[4]-quants[2]
    quantdf<-data.frame(Value=c(quants[2]-iqr_*1.5, quants[3], quants[4]+1.5*iqr_),labels=c("Whiskers","Median","Whiskers"))

    p1 <- ggplot(dat, aes_string(x=xname)) +
      geom_rect(aes(xmin = quants[2], xmax = quants[4], ymin = 0, ymax = Inf, fill = "IQR"),
                color=NA, alpha = 0.002)+
      geom_density(aes(y = ..count..), alpha=.2, fill="#000000") +
      stat_bin(aes(y=..count..), colour="black", fill="#000000", alpha = 0.6, binwidth = binwidth) +
      labs(title=paste0(xname, ": Histogram and density plot"), x = NULL, y=NULL) +
      scale_fill_manual('',
                        values = '#FF6666',
                        guide = guide_legend(override.aes = list(alpha = 0.5))) +
      theme(plot.title = element_text(size=12, face="bold.italic", hjust = 0.5), legend.position = "bottom") +
      geom_vline(aes(xintercept=Value, color=labels),data=quantdf, show.legend=T) +
      scale_color_manual("", values=c("Whiskers"="grey4","Median"="#FF6666", "Whiskers"="grey4"))

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


plotNA <- function(tb, order = T, limit = F, add_percent = T, row.level = F) {
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

  if (!limit) {
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
    # print(ggplot_build(p1)$layout$panel_params[[1]]$y.labels)
    level <- tryCatch(ggplot_build(p1)$layout$panel_params[[1]]$y.labels, error = function(e){return(colnames(tb))})

    plot_na_ <- function(tb2, level) {
      tb2$isNA <- is.na(tb2$value)
      p2 <- ggplot(tb2, aes(variable, index, fill = isNA)) +
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
    # check for reshape2 package. only then make this plot
    if (requireNamespace("reshape2", quietly = TRUE)) {
      tb <- reshape2::melt(tb, id.vars = "index")
      p2 <- plot_na_(tb, level)
    # } else if (requireNamespace("tidyr", quietly = TRUE)) {

    } else {
      warning("To make row level plot, please install 'reshape2' library")
    }
  }
  return(p1)
}
