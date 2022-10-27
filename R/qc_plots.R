
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
                                          maxSatCount = 10, showLegend = F, tMethod = 'log2ratio', ggplot=T, elementDividers = F) {
  plotLists <- list()
  plate = ""
  firstPlate <- T
  for(plateName in platePair) {
    plate <- plateSet@plates[[plateName]]
    pList <- trellisPlateView(plate, statsToPlot = statsToPlot, returnPlots = returnPlots,
                              showRowColumnLabels = showRowColumnLabels, maxSatCount = maxSatCount,
                              showLegend = showLegend, ggplot=ggplot, tileDividers = tileDividers,
                              plateName = plateName, showRowTitle = firstPlate)
    plotLists[[plateName]] <- pList
    firstPlate <- F
  }

  # wrap transformed data in plate
  for(tName in plateSet@transformationNames) {
    print(tName)
    if(nrow(plateSet@transformedPlateData[[tName]]) > 0) {
      tPlate <- new("plate")
      tPlate@plateData <- plateSet@transformedPlateData[[tName]]
      tPlate@format <- plate@format
      tPlate@statList <- plate@statList
      tPlate@wellIds <- plate@wellIds

      plotLists[[tName]] <- trellisPlateView(plate = tPlate, statsToPlot = statsToPlot, returnPlots = returnPlots,
                                            showRowColumnLabels = showRowColumnLabels, maxSatCount = maxSatCount,
                                             showLegend = T, ggplot=ggplot, tileDividers,
                                            scaleColors = c("green", "black", "red"), plateName = tName)
    }
  }

  fullPlateList <- c(plotLists[[1]], plotLists[[2]], plotLists[[3]])

  grid <- gridExtra::arrangeGrob(grobs=fullPlateList, widths=c(1,1,1.3), ncol=3, nrow=length(statsToPlot), as.table = F)


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
                             plateTitle = "", showRowTitle = F, rowTitle = "row", ggplot=T, elementDividers = F, returnPlots = F) {
  pList <- list()

  # get the mapping from statistic to a matrix for the stat, in plate format
  matrixMap <- aso:::rawPlateMatrixStatsToPlateFormat(plate=plate, stat=statsToPlot)

  firstInList = T

  for(stat in statsToPlot) {

    print(stat)

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
                                         showLegend =  showLegend, tileDividers=tileDividers, scaleColors=scaleColors,
                                         showColTitle = F, xTitle = plateTitle, showRowTitle = showRowTitle, yTitle = stat)
      } else {
        p <- aso:::getPlateGgplotFromMatrix(matrixMap[[stat]], plotTitle = stat,
                                           showRowColLabels = showRowColumnLabels,
                                           maxSatCount = maxSatCount,
                                           showLegend =  showLegend, tileDividers=tileDividers,
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

  loHi <- getPlateValueCutoffs(plateFormatData, maxSaturationCount = maxSatCount)
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

  print(colorLimits)
  print(scaleColors)

  df <- data.frame(plateFormatData, check.names = F)
  colnames(df) <- colnames(plateFormatData)
  rownames(df) <- rownames(plateFormatData)
  df$rowLabel <- rownames(df)
  dfM <- reshape2::melt(df)
  colnames(dfM)<- c("rowLabel", "colLabel", "signal")
  levels <- rownames(plateFormatData)

  lineWidth = 0.5

  p <- ggplot2::ggplot(dfM, ggplot2::aes(x=colLabel, y=ordered(rowLabel,levels=rev(levels)), fill=signal)) +
    ggplot2::scale_x_discrete(position='top')

  if(!elementDividers) {
    p <- p + ggplot2::geom_tile(show.legend = showLegend)
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

  return(p)
}







