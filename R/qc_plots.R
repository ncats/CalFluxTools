
#' Produces a grid of plate QC views for a supplied reference plate, experimental plate
#' and any transformations applied to the data.
#' @param plateSet a PlateSet object containing at least one reference and one experimental treatment plate.
#' @param platePair a vector of length 2 specifying reference and experimental plate names to QC.
#' @param statsToPlot a vector of parameters that should be visualized.
#' @param showRowColumnLabels a boolean to show or hide plate row and column labels. Larger trellises may benefit from hiding labels.
#' @param maxSatCount maximumn number of wells to allow color saturation in the plate heatmap. This sets color scale limits such that this number of wells saturate,
#' evenly at both ends of the plate's value range. This reduces the effect of outliers on the color scale limits.
#' @param showLegend a boolean that controls showing or hiding the colorbar legend next to the plate heatmaps.
#' @param tMethod is one of c('log2ratio', 'pct_change') and is used to control which data transformation to display
#' @param ggplot is a boolean to control if rendering is by ggplot2 or by ComplexHeatmap. Ggplot2 is the default.
#' @param elementDividers is a boolean controling whether boarders between heatmap elements are shown or not. Larger trellises benefit from hiding these lines.
#'
trellisPlateSetViewsPairViews <- function(plateSet, platePair, statsToPlot, showRowColumnLabels = T,
                                          maxSatCount = 10, showLegend = F, tMethod = 'log2ratio',
                                          ggplot=T, elementDividers = F) {
  plotLists <- list()
  plate = ""
  firstPlate <- T
  for(plateName in platePair) {
    plate <- plateSet@plates[[plateName]]
    pList <- aso:::trellisPlateView(plate, statsToPlot = statsToPlot, returnPlots = returnPlots,
                              showRowColumnLabels = showRowColumnLabels, maxSatCount = maxSatCount,
                              showLegend = showLegend, ggplot=ggplot, elementDividers = elementDividers,
                              plateTitle = plateName, showRowTitle = firstPlate)
    plotLists[[plateName]] <- pList
    firstPlate <- F
  }

  # wrap transformed data in plate
  for(tName in plateSet@transformationNames) {

    if(nrow(plateSet@transformedPlateData[[tName]]) > 0) {

      # wrap the tranformed data in a plate and send off for plotting
      tPlate <- aso:::clonePlate(plate)
      tPlate@plateData <- plateSet@transformedPlateData[[tName]]

      plotLists[[tName]] <- aso:::trellisPlateView(plate = tPlate, statsToPlot = statsToPlot, returnPlots = returnPlots,
                                            showRowColumnLabels = showRowColumnLabels, maxSatCount = maxSatCount,
                                             showLegend = T, ggplot=ggplot, elementDividers=elementDividers,
                                            scaleColors = c("green", "black", "red"), plateTitle = tName)
    }
  }

  fullPlateList <- c(plotLists[[1]], plotLists[[2]], plotLists[[3]])

  grid <- gridExtra::arrangeGrob(grobs=fullPlateList, widths=c(1.1,1,1.5), ncol=3, nrow=length(statsToPlot), as.table = F)

#grid <- cowplot::plot_grid(plotlist = fullPlateList, ncol=3, nrow=length(statsToPlot), rel_widths = c(1,1,1.5),
#                          rel_heights = rep(0.5, length(statsToPlot)), byrow=F)

  return(grid)
}



#' Plots plate heatmaps for one plate, for a collection of one or more statistics/parameters
#'
#' @param plate plate object with loaded data
#' @param statsToPlot a vector of parameters that should be visualized.
#' @param showRowColumnLabels a boolean to show or hide plate row and column labels. Larger trellises may benefit from hiding labels.
#' @param maxSatCount maximumn number of wells to allow color saturation in the plate heatmap. This sets color scale limits such that this number of wells saturate,
#' evenly at both ends of the plate's value range. This reduces the effect of outliers on the color scale limits.
#' @param showLegend a boolean that controls showing or hiding the colorbar legend next to the plate heatmaps.
#' @param scaleColors a vector of colors to define the color scale, two colors should be provides as low color then high color.
#' @param plateTitle a title to show over the heatmap
#' @param showRowTitle a boolean to control is the row title is shown.
#' @param rowTitle optional row title that can be used to label the plate on the left.
#' @param ggplot is a boolean to control if rendering is by ggplot2 or by ComplexHeatmap. Ggplot2 is the default.
#' @param elementDividers is a boolean controling whether boarders between heatmap elements are shown or not. Larger trellises benefit from hiding these lines.
#' @param returnPlots if true, return a list object of ggplot tile plots for visualization, false will display plots.
#'
trellisPlateView <- function(plate, statsToPlot, showRowColumnLabels = T,
                             maxSatCount = 10, showLegend = F,
                             scaleColors = c("#e8eb59", "#0740fa"),
                             plateTitle = "", showRowTitle = F, rowTitle = "row", ggplot=T, elementDividers = F,
                             returnPlots = F) {
  pList <- list()

  # get the mapping from statistic to a matrix for the stat, in plate format
  matrixMap <- aso:::rawPlateMatrixStatsToPlateFormat(plate=plate, parameter=statsToPlot)

  firstInList = T

  for(stat in statsToPlot) {

    # build a Heatmap for each statistc
    if(!ggplot) {
      p <- aso:::getPlatePlotFromMatrix(matrixMap[[stat]], plotTitle = stat,
                                        showRowColLabels = showRowColumnLabels,
                                        maxSatCount = maxSatCount,
                                        showLegend =  showLegend)
    } else {

      if(!firstInList) {
      p <- aso:::getPlateGgplotFromMatrix(matrixMap[[stat]], plotTitle = stat,
                                         showRowColLabels = showRowColumnLabels,
                                         maxSatCount = maxSatCount,
                                         showLegend =  showLegend, elementDividers=elementDividers, scaleColors=scaleColors,
                                         showColTitle = F, xTitle = plateTitle, showRowTitle = showRowTitle, yTitle = stat)
      } else {
        p <- aso:::getPlateGgplotFromMatrix(matrixMap[[stat]], plotTitle = stat,
                                           showRowColLabels = showRowColumnLabels,
                                           maxSatCount = maxSatCount,
                                           showLegend =  showLegend, elementDividers=elementDividers,
                                           scaleColors=scaleColors, showColTitle = T, xTitle = plateTitle,
                                           showRowTitle=showRowTitle, yTitle = stat)
      }
      firstInList = F
    }
    # add the plot to a list of plots
    pList[[stat]] <- p
  }

  # show plots
  return(pList)
}


#' returns a single ComplexHeatmap plate view
#' @param plateFormatData a matrix with dimensions and data in plate orientation
#' @param plotTitle a title to put over the plate heatmap
#' @param showColNames a boolean to show or hide row and column labels.
#' @param showLegend a boolean to show or hide heatmap colorbar legend
#' @param maxSatCount maximumn number of wells to allow color saturation in the plate heatmap. This sets color scale limits such that this number of wells saturate,
#' evenly at both ends of the plate's value range. This reduces the effect of outliers on the color scale limits.
getPlatePlotFromMatrix <- function(plateFormatData, plotTitle = "", showRowColNames = T,
                                   showLegend = F, maxSatCount = 10) {

  loHi <- getPlateValueCutoffs(plateFormatData, maxSaturationCount = maxSatCount)

  col_fun = circlize::colorRamp2(c(loHi[1], loHi[2]), c("blue", "yellow"))

  p <-  ComplexHeatmap::Heatmap(plateFormatData, col = col_fun, cluster_rows = FALSE, cluster_columns = FALSE,
                row_names_side = "left", column_names_side = "top", column_names_rot = 0,
                column_title = plotTitle, show_column_names = showRowColNames, show_row_names = showRowColNames, show_heatmap_legend = showLegend)

  return(p)
}



#' Title
#'
#' @param plateFormatData a matrix object with dimensions and data that conforms to plate data.
#' @param plotTitle an optional plate heatmap title
#' @param showRowColLabels boolean to show or hid column labels
#' @param showRowTitle boolean to show or hide row title
#' @param showColTitle boolean to show or hide row title
#' @param showLegend boolean to show or hide colorbar legend
#' @param maxSatCount maximumn number of wells to allow color saturation in the plate heatmap. This sets color scale limits such that this number of wells saturate,
#' evenly at both ends of the plate's value range. This reduces the effect of outliers on the color scale limits.
#' @param elementDividers boolean to hide or show borders around heatmap elements
#' @param scaleColors a vector of colors that bound the color scale
#' @param showPlateName a boolean to show or hide the name of the plate over the heatmap
#' @param xTitle a title to show on the x-axis (if showRowColLabels is true)
#' @param yTitle a title to show on the y-axis (if showRowColLabels is true)
getPlateGgplotFromMatrix <- function(plateFormatData, plotTitle = "", showRowColLabels = T,
                                     showRowTitle = F, showColTitle = F,
                                    showLegend = F, maxSatCount = 10, elementDividers = F,
                                    scaleColors = c("#e8eb59", "#0740fa"), showPlateName = F,
                                    xTitle = "col", yTitle = "row") {

  loHi <- aso:::getPlateValueCutoffs(plateFormatData, maxSaturationCount = maxSatCount)
  loEnd <- loHi[1]
  hiEnd <- loHi[2]
  colorLimits = c(loEnd, hiEnd)
  midPoint = 0.0

  if(length(scaleColors) == 3) {
    if(loHi[1] < 0 && loHi[2] > 0) {
      midPoint = 0.0
      hiEnd <- max(abs(loHi))
      loEnd <- -1 * hiEnd
    } else {
      # both midpoint takes the value closes to 0
      midPoint = loHi[which.min(abs(loHigh))]
    }
    colorLimits <- c(loEnd, hiEnd)
  }

  df <- data.frame(plateFormatData, check.names = F)
  colnames(df) <- colnames(plateFormatData)
  rownames(df) <- rownames(plateFormatData)
  df$rowLabel <- rownames(df)

  suppressMessages(  dfM <- reshape2::melt(df) )

  colnames(dfM)<- c("rowLabel", "colLabel", "signal")
  levels <- rownames(plateFormatData)

  lineWidth = 0.5

  p <- ggplot2::ggplot(dfM, ggplot2::aes(x=colLabel, y=ordered(rowLabel,levels=rev(levels)), fill=signal)) +
    ggplot2::scale_x_discrete(position='top')

  # p <- ggplot2::ggplot(dfM, ggplot2::aes(x=colLabel, y=rowLabel, fill=signal)) +
  #   ggplot2::scale_x_discrete(position='top')

  if(!elementDividers) {
    p <- p + ggplot2::geom_tile(show.legend = showLegend, ggplot2::aes(width=1.0, height=1.0))
  } else {
    p <- p + ggplot2::geom_tile(show.legend = showLegend, color="white", lwd=lineWidth, linetype = 1)
  }

  p <- p + ggplot2::theme_minimal()

  p <- p + ggplot2::xlab(xTitle) + ggplot2::ylab(yTitle)

  if(!showRowColLabels) {
    if(!showRowTitle && !showColTitle) {
      p <- p +  ggplot2::theme(axis.text.x=ggplot2::element_blank(), axis.text.y=ggplot2::element_blank(),
          axis.title.x=ggplot2::element_blank(), axis.title.y=ggplot2::element_blank())
    } else if(showRowTitle && showColTitle) {
      p <- p +  ggplot2::theme(axis.text.x=ggplot2::element_blank(), axis.text.y=ggplot2::element_blank())
    } else if(showRowTitle && !showColTitle) {
      p <- p +  ggplot2::theme(axis.text.x=ggplot2::element_blank(), axis.text.y=ggplot2::element_blank(),
                              axis.title.x=ggplot2::element_blank())
    } else if(!showRowTitle && showColTitle) {
      p <- p +  ggplot2::theme(axis.text.x=ggplot2::element_blank(), axis.text.y=ggplot2::element_blank(),
                               axis.title.y=ggplot2::element_blank())
    }
  }

  if(length(scaleColors) == 2) {
    p <- p + ggplot2::scale_fill_gradientn(colors=scaleColors,limits=colorLimits,
                                  oob=scales::squish, na.value = "#dfe8f2")
  } else {
    p <- p + ggplot2::scale_fill_gradientn(colors=scaleColors,limits=colorLimits,
                                           oob=scales::squish, na.value = "#dfe8f2")
  }

  #p <- p + ggplot2::theme(aspect.ratio = 16/24)
  #p <- p + cowplot::theme_cowplot()
  #p <- p + ggplot2::coord_fixed(ratio = 0.667)
  p <- ggplotify::as.grob(p)
  return(p)
}



getStatisticBarChartFromTransformedPlateSet <- function(plateSet, sampleName, parameter, tMethod = 'log2Ratio') {

  #clone a transformed plate, and set transformed data as plate data
  plate <- plateSet@plates[[1]]
  tPlate <- clonePlate(plate)
  tPlate@plateData <- plateSet@transformedPlateData[[1]]

  # extract the plate data for a specified sample
  data <- aso:::getPlateDataForSample(plate = tPlate, sampleName, parameter)
  # concentration should be categorical for bar chart
  data$Concentration <- as.character(data$Concentration)

  # set a temp name for the extracted parameter's value
  colnames(data)[ncol(data)] <- 'val'

  #compute mean and SD, via dplyr
  dataSum <- data.frame(group_by(data, Concentration) %>% summarize(m = mean(val)))
  dataSumSD <- data.frame(group_by(data, Concentration) %>% summarize(m = sd(val)))

  # set conc as a factor and order levels
  dataSum$Concentration <- factor(dataSum$Concentration, levels = dataSum$Concentration[order(as.numeric(dataSum$Concentration))])

  # order the data summaries
  dataSum <- dataSum[order(dataSum$Concentration),]
  dataSumSD <- dataSumSD[order(dataSumSD$Concentration),]

  dataSumSD$Concentration <- factor(dataSumSD$Concentration, levels = dataSumSD$Concentration[order(as.numeric(dataSumSD$Concentration))])

  # add sd to the summary and upper and lower limits
  dataSum$SD <- dataSumSD$m
  dataSum$sdLow <- dataSum[,2] - dataSum$SD
  dataSum$sdHigh <- dataSum[,2] + dataSum$SD

  #yTitle <- paste0(plateSet@transformationNames[1], " (", param, ")")

  #colnames(dataSum)[2] <- yTitle
  #colnames(data)[ncol(data)] <- yTitle
  title <- paste0(sampleName, " -- ", parameter)

  p <- ggplot(dataSum, aes(x=Concentration, y=dataSum[,2])) + geom_bar(stat='identity', color = 'blue', fill=rgb(0.1,0.4,0.5,0.7)) +
    geom_point(data=data, aes(x=Concentration, y=data[,ncol(data)])) +
    geom_errorbar(aes(ymin=sdLow, ymax=sdHigh), width = 0.2) +
    theme_classic() + labs(title=title, x="Concentration", y="log2FoldChange") + geom_hline(yintercept = 0.0) +
    theme(plot.title = element_text(hjust = 0.5))

  return(p)
}

getBarChartTrellis <- function(plateSet, samples, parameters, samplesIn = 'rows', tMethod = 'log2Ratio') {
  if(is.character(samples) && samples == 'all') {
    # get all compound annotations
    samples <- unique(plateSet@plates[[1]]@plateAnnotMap$Compound)
  }

  samples <- samples[samples != ""]
  samples <- sort(samples)
  plots <- list()

  for(sample in samples) {
    for(param in parameters) {
      plotHandle <- paste0(sample, ":", param)
      p <- getStatisticBarChartFromTransformedPlateSet(plateSet=plateSet, sampleName=sample, parameter = param, tMethod = tMethod)
      plots[[plotHandle]] <- p
    }
  }

  grid <- gridExtra::arrangeGrob(grobs=plots, ncol=length(parameters), nrow=length(samples), as.table = T)

  return(grid)
}



