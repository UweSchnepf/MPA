% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/draw.R
\name{draw_KroenerCarbo}
\alias{draw_KroenerCarbo}
\title{Diagnostic plots for the Kroener & Carbo model}
\usage{
draw_KroenerCarbo(
  data,
  cisp,
  elon = "Elongation",
  model,
  col = c("black", "black"),
  col_line = "#00BEFF",
  pch = c(20, 4),
  cex = c(1, 1),
  legend = TRUE,
  ...
)
}
\arguments{
\item{data}{A data frame that contains the measurement.}

\item{cisp}{A character string to select the vector that contains circularity or sphericity values.}

\item{elon}{A character string to select the vector that contains elongation values. Default is "Elongation".}

\item{model}{Must be either "ce" for the Circularity-Elongation model or "se" for the Sphericity-Elongation model.}

\item{col}{Here you can enter character values colors as a vector of form c(valid, invalid).}

\item{col_line}{Here you can change the color of the theoretical curve. Default is lightblue.}

\item{pch}{Here you can enter numerical values for point symbols as a vector of form c(valid, invalid).}

\item{cex}{Here you can enter numerical values for point size as a vector of form c(valid, invalid).}

\item{legend}{Logical. Should legend be drawn (TRUE) or not (FALSE)? Default is TRUE.}

\item{...}{Other graphical parameters.}
}
\description{
Implements an algorithm proposed by Kroener and Carbo to detect and delete falsely calculated shape descriptors
caused by digitization issues. It is based on a theoretical relationship between circularity/sphericity and elongtaion.
This relationship acts like a threshold for meausred pair of shape factor values. If they exceed the threshold, then something went
wrong in image processing, for which low pixel resoultion is the most prominent reason. Consequently, such pair of
values will be eliminated.
}
\references{
Kroener, S., Domenech Carbo, M.T., 2013. Determination of minimum pixel resolution for shape analysis: Proposal of a new data validation method for computerized images. Powder Technol. 245, 297 – 313.
}
\author{
Uwe Schnepf
}
