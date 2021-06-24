# Authors:          Uwe Schnepf (1)
# Affiliations:
# 1.                Institute of Biomaterials and biomolecular Systems
#                   Research Unit Biodiveristy and Scientific Diving
#                   University of Stuttgart
# Contact:          uwe.schnepf@bio.uni-stuttgart.de
#                   uwe.schnepf91@gmx.de
# Last modified:    07.07.2020
#----------------------------------------------------------------------------------------------------------------------------------#

#' Function to excluded particles based on their shape
#'
#' @description
#' Implements an algorithm proposed by Kroener and Carbo to detect and delete falsely calculated shape factors
#' caused by digitization issues. It is based on a theoretical relationship between circularity/sphericity and elongtaion.
#' This relationship acts like a threshold for meausred pair of values. If they exceed the threshold, then something went
#' wrong in image processing, for which low pixel resoultion is the most prominent reason. Consequently, such pair of
#' values will be eliminated.
#'
#' @param data A data frame that will be manipulated.
#' @param cisp A character string to select the vector that contains circularity or sphericity values.
#' @param elon A character string to select the vector that contains elongation values. Default is "Elongation".
#' @param model Must be either "ce" for the Circularity-Elongation model or "se" for the Sphericity-Elongation model.
#'
#' @author
#' Uwe Schnepf
#'
#' @references
#' Kroener, S., Domenech Carbo, M.T., 2013. Determination of minimum pixel resolution for shape analysis: Proposal of a new data validation method for computerized images. Powder Technol. 245, 297 – 313.
#' @export
clean_exclusion_shape <- function(data,
                                  cisp,
                                  elon = "Elongation",
                                  model
                                  )
  {
  # We check wheter our object is of class data frame or not
  if (!inherits(data, "data.frame"))
  {
    stop("Data must be of class data.frame")
  }

  if (!all(c(cisp, elon) %in% names(data))) {
    stop("At least one of the specified columns is not in the data")
  }

  if (!all(sapply(data[, cisp], is.numeric))) {
    stop("Circularity/Sphericity must be numeric")
  }

  if (!all(sapply(data[, elon], is.numeric))) {
    stop("Elongation must be numeric")
  }

  if(model == "ce")
  {
    eq <- function(x){1-((2-sqrt(4-4*(x*100/95)^2))/(2*x*100/95))}
  } else if(model == "se")
  {
    eq <- function(x){1 - x^2}
  }


  # Based on the model We first caclulate theoretical elongation for each measured circularity
  data[["TheoreticalElongation"]] <- eq(data[[cisp]])

  # We create a vector that tells us if the measured elongation exceeds the theroetical one or not
  data[["ThresholdCheck"]] <- with(data,
    ifelse(data[[elon]] > data$TheoreticalElongation, "invalid", "valid"))

  selector <- data[data$ThresholdCheck == "valid", ]
  return(selector)
}

#' Function to exclude particles based on their size
#' @description
#' All particles below a lower size limit and above an upper size limit are removed from the data set.
#'
#' @param data A data frame that will be manipulated.
#' @param lower.size The lower size that is used as cut-off value.
#' @param upper.size The upper size that is used as cut-off value.
#' @param diam A vector inside the data that contains a measure of the particles diameter.
#'
#' @author Uwe Schnepf
#' @export
clean_exclusion_size <- function(data, lower.size, upper.size,
                               diam)
{
  if (!inherits(data, "data.frame"))
  {
    stop("Data must be of class data.frame")
  }

  if (!all(c(diam) %in% names(data))) {
    stop("At least one of the specified columns is not in the data")
  }

  data <- data[which(data[[diam]] > lower.size & data[[diam]] <= upper.size),]
  return(data)
}
# END OF FUNCTION
