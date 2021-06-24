#' Histogram for particle size and shape distribution
#'
#' @description
#' Plots a histogram with frequency and frequency density distribution on the y-axes.
#'
#' @param data A data frame that contains the particle measurement.
#' @param sors A character string to select the vector that contains either size or a shape descriptor.
#' @param xlim The x limits for the plot. Default is c(0,1).
#' @param ylim The y limits for the plot.
#' @param xlab Axis label for the x axis.
#' @param ylab Axis label for the first y axis.
#' @param ylab2 Axis label for the second y axis.
#' @param trans IMPORTANT: DOES NOT WORK AT THE MOMENT. Logical. TRUE: x-axis is drawn on a logarithmic scale. FALSE: x-axis is drawn on a linear scale. Default is FALSE.
#' @param breaks Defines the bin size of the histogram.
#' @param margins A numerical vector of the form c(bottom, left, top, right) which gives the number of lines of margin to be specified on the four sides of the plot. The default is c(4, 4, 1, 4). This is especalliy important for a correct representation of the second y-axis.
#' @param tick_Xaxis A sequence of numbers that will be used to draw the ticks at the x-axis.
#' @param tick_Yaxis A sequence of numbers that will be used to draw the ticks at the x-axis.
#' @param cex A numerical value giving the amount by which plotting text and symbols should be magnified relative to the default of 1.
#' @param col_normalized A single character string to pass the color in which normalized frequencies should be graphed. Default is "red".
#' @param ... Other graphical parameters
#'
#' @author
#' Uwe Schnepf
#'
#' @references
#' Filella, M., 2015. Questions of size and numbers in environmental research on microplastics: methodological and conceptual aspects. Environ. Chem. 12, 527.
#' @export
draw_dist <- function(data,
                      sors,
                      tick_Xaxis,
                      tick_Yaxis = seq(0,10000, by = 1000),
                      xlim = c(0,1),
                      ylim,
                      xlab = sors,
                      ylab = "Frequency",
                      ylab2 = expression('Normalized frequency [µm'^-1*']'),
                      margins = c(4, 4, 1, 4),
                      trans = FALSE,
                      breaks,
                      cex = 1,
                      col_normalized = "red", ...)
{
  # We do some checks
  if (!inherits(data, "data.frame"))
    stop("Data must be of class data.frame")

  if (!all(c(sors) %in% names(data)))
    stop("At least one of the specified columns is not in the data")

  if (!all(sapply(data[, sors], is.numeric)))
    stop("Particle size/shape must be numeric")

  # We decide whether particle size is converted to a logarithmic scale
  if(trans == TRUE) {
   D90_D10 <- quantile(data[[sors]], 0.9)/quantile(data[[sors]], 0.1)
   if(D90_D10 > 1.5) {
     cat("\n Data will be visualized on a logarithmic scale \n")
     data[[sors]] <- log(data[[sors]])
    } else {cat("\n Data will be visualized on a linear scale \n")}
  }

  ## Initialise plot windwow for historgram
  plot.new()
  par(mar = margins)
  plot.window(xlim = xlim, ylim = ylim)

  # Set axes
  axis(side = 1, at = tick_Xaxis)
  axis(side = 2, at = tick_Yaxis)

  ## Initialise frequency histogram
  MPAhist <- hist(data[[sors]],
                  right = FALSE,
                  plot = FALSE,
                  breaks = breaks)

  ## Plot frequency histogram
  par(new = TRUE)
  plot(MPAhist,
       main = "",
       axes = FALSE,
       xlab = "",
       ylab = "",
       xlim = xlim,
       ylim = ylim,
       col = "#F3F3F3",
       cex.lab = cex, ...
  )

  mtext(side = 2,
        text = ylab,
        line = 3,
        cex = cex)

  mtext(side = 1,
        text = xlab,
        line = 3,
        cex = cex)

  ## Initialise normalised histogram
  #  Choose only counts with more than zero counts
  MPAhist$counts <- MPAhist$counts/diff(MPAhist$breaks)
  greaterZero.counts <- MPAhist$counts[!MPAhist$counts %in% 0]

  #  Choose only midpoints of bars with more than zero counts --> otherwise the x-axis is drawn in a wrong manner
  greaterZero.mids <- MPAhist$mids[!MPAhist$counts %in% 0]

  #  Convert y-axis from frequency to normalized ticks
  Yaxis_convert <- breaks[2]-breaks[1]
  Yaxis_normal <- tick_Yaxis/Yaxis_convert

  #  Set axes
  par(new = TRUE)
  plot.new()
  par(mar = margins)
  plot.window(xlim = xlim,
              ylim = c(0, max(Yaxis_normal)))

  axis(side = 4,
       col = col_normalized,
       col.axis = col_normalized,
       at = seq(0, max(Yaxis_normal),
                by = Yaxis_normal[2]-Yaxis_normal[1]))

  ## Plot normalised histogram
  par(new = TRUE)
  plot(x = greaterZero.mids,
       y = greaterZero.counts,
       type = "p",
       pch = 21,
       col = col_normalized,
       bg = col_normalized,
       axes = FALSE,
       xlim = xlim,
       ylim = c(0, max(Yaxis_normal)),
       main = "",
       xlab = "",
       ylab = ""
  )

  mtext(side = 4,
        text = ylab2,
        col = col_normalized,
        line = 3,
        cex = cex)
} # END OF FUNCTION

#' Diagnostic plots for the Kroener & Carbo model
#'
#' @description
#' Implements an algorithm proposed by Kroener and Carbo to detect and delete falsely calculated shape descriptors
#' caused by digitization issues. It is based on a theoretical relationship between circularity/sphericity and elongtaion.
#' This relationship acts like a threshold for meausred pair of shape factor values. If they exceed the threshold, then something went
#' wrong in image processing, for which low pixel resoultion is the most prominent reason. Consequently, such pair of
#' values will be eliminated.
#'
#' @param data A data frame that contains the measurement.
#' @param cisp A character string to select the vector that contains circularity or sphericity values.
#' @param elon A character string to select the vector that contains elongation values. Default is "Elongation".
#' @param model Must be either "ce" for the Circularity-Elongation model or "se" for the Sphericity-Elongation model.
#' @param col Here you can enter character values colors as a vector of form c(valid, invalid).
#' @param col_line Here you can change the color of the theoretical curve.
#' @param pch Here you can enter numerical values for point symbols as a vector of form c(valid, invalid).
#' @param cex  Here you can enter numerical values for point size as a vector of form c(valid, invalid).
#' @param legend Logical. Should legend be drawn (TRUE) or not (FALSE)? Default is TRUE.
#' @param ... Other graphical parameters.
#'
#' @author
#' Uwe Schnepf
#'
#' @references
#' Kroener, S., Domenech Carbo, M.T., 2013. Determination of minimum pixel resolution for shape analysis: Proposal of a new data validation method for computerized images. Powder Technol. 245, 297 – 313.
#' @export
draw_KroenerCarbo <- function(data,
                              cisp,
                              elon = "Elongation",
                              model,
                              col = c("black", "black"),
                              col_line = "red",
                              pch = c(20, 4),
                              cex = c(1, 1),
                              legend = TRUE, ...)
{
  # We perform some checks
  if (!inherits(data, "data.frame"))
    stop("Data must be of class data.frame")

  if (!all(c(cisp, elon) %in% names(data)))
    stop("At least one of the specified columns is not in the data")


  if (!all(sapply(data[, cisp], is.numeric)))
    stop("Circularity/Sphericity must be numeric")

  if (!all(sapply(data[, elon], is.numeric)))
    stop("Elongation must be numeric")

  # We relate either circularity or sphericity to elongation
  if(model == "ce")
    eq <- function(x) 1-((2-sqrt(4-4*(x*100/95)^2))/(2*x*100/95))
  else if(model == "se")
    eq <- function(x) 1 - x^2

  # We plot the theoretical relationship
  par(col = "black")
  plot(eq,
       axes = FALSE,
       main = "",
       xlab = "",
       ylab = "",
       col = col_line
  )

  # We decide which values to keep
  data[["TheoreticalElongation"]] <- eq(data[[cisp]])
  data[["ThresholdCheck"]] <- with(data,
                                   ifelse(data[[elon]] > data$TheoreticalElongation, "invalid", "valid"))
  
  col_grouped <- col[as.numeric(as.factor(data$ThresholdCheck))]
  pch_grouped <- pch[as.numeric(as.factor(data$ThresholdCheck))]
  cex_grouped <- cex[as.numeric(as.factor(data$ThresholdCheck))]

  # We add the values to keep
  par(new = TRUE)
  plot(y = data[[elon]],
       x = data[[cisp]],
       xlim = c(0,1),
       ylim = c(0,1),
       xlab = cisp,
       ylab = elon,
       col = col_grouped,
       pch = pch_grouped,
       cex = cex_grouped, ...)

  if(legend == TRUE){
  legend("topright",
         legend = c("invalid", "valid"),
         col = col,
         pch = pch)
  }
} # END OF FUNCTION
