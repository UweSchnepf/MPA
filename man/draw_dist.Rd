% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/draw.R
\name{draw_dist}
\alias{draw_dist}
\title{Histogram for particle size and shape distribution}
\usage{
draw_dist(
  data,
  sors,
  tick_Xaxis,
  tick_Yaxis = seq(0, 10000, by = 1000),
  xlim = c(0, 1),
  ylim,
  xlab = sors,
  ylab = "Frequency",
  y2lab = expression("Normalized frequency [µm"^-1 * "]"),
  margins = c(4, 4, 1, 4),
  trans = FALSE,
  breaks,
  cex = 1,
  col_normalized = "#00BEFF",
  col_y2 = "#00BEFF",
  ...
)
}
\arguments{
\item{data}{A data frame that contains the particle measurement.}

\item{sors}{A character string to select the vector that contains either size or a shape descriptor.}

\item{tick_Xaxis}{A sequence of numbers that will be used to draw the ticks at the x-axis.}

\item{tick_Yaxis}{A sequence of numbers that will be used to draw the ticks at the x-axis.}

\item{xlim}{The x limits for the plot. Default is c(0,1).}

\item{ylim}{The y limits for the plot.}

\item{xlab}{Axis label for the x axis.}

\item{ylab}{Axis label for the first y axis.}

\item{y2lab}{Axis label for the second y axis.}

\item{margins}{A numerical vector of the form c(bottom, left, top, right) which gives the number of lines of margin to be specified on the four sides of the plot. The default is c(4, 4, 1, 4). This is especalliy important for a correct representation of the second y-axis.}

\item{trans}{IMPORTANT: DOES NOT WORK AT THE MOMENT. Logical. TRUE: x-axis is drawn on a logarithmic scale. FALSE: x-axis is drawn on a linear scale. Default is FALSE.}

\item{breaks}{Defines the bin size of the histogram.}

\item{cex}{A numerical value giving the amount by which plotting text and symbols should be magnified relative to the default of 1.}

\item{col_normalized}{A single character string to pass the color in which normalized frequencies should be graphed. Default is light blue.}

\item{col_y2}{A single character string to pass the color in which the second y axis should be graphed. Default is light blue.}

\item{...}{Other graphical parameters}
}
\description{
Plots a histogram with frequency and frequency density distribution on the y-axes.
}
\references{
Filella, M., 2015. Questions of size and numbers in environmental research on microplastics: methodological and conceptual aspects. Environ. Chem. 12, 527.
}
\author{
Uwe Schnepf
}
