# Authors:          Uwe Schnepf (1)
# Affiliations:
# 1.                Institute of Biomaterials and biomolecular Systems
#                   Research Unit Biodiveristy and Scientific Diving
#                   University of Stuttgart
# Contact:          uwe.schnepf@bio.uni-stuttgart.de
#                   uwe.schnepf91@gmx.de
# Last modified:    06.012.2020
#----------------------------------------------------------------------------------------------------------------------------------#

#' Categorise particle size distributions based on their width
#' @param diam A vector containing a measure of particle size.
#'
#' @author Uwe Schnepf
#'
#' @references Merkus, H.G., 2009. Particle Size Measurements Fundamentals, Practice, Quality, 1st ed, Particle Technology Series. Springer Netherlands, Dordrecht.
#' @export
calc_width <- function(diam)
{
  D90_D10 <- quantile(diam, 0.9)/quantile(diam, 0.1)
  if(D90_D10 < 1.02)
  {
    width <- "monosized"
  } else if(D90_D10 >= 1.02 & D90_D10 < 1.05){width <- "ultra narrow"
  } else if(D90_D10 >= 1.05 & D90_D10 < 1.5){width <- "narrow"
  } else if(D90_D10 >= 1.5 & D90_D10 < 4){width <- "medium"
  } else if(D90_D10 >= 4 & D90_D10 <= 10){width <- "broad"
  } else if(D90_D10 > 10){width <- "very broad"
  }
}

#' Calculate the proportion of fibers in a microplastic particle collective
#'
#' @description
#' This functions computes the proportion of fibers in a microplastic particle collective.We use one of two different
#' shape descriptors as a criterion. For an aspect ratio larger than 3 MP is considered a fiber based on Cole (2016).
#' This is equal to an elongation larger than 2/3.Note that elongation must be calculated according to Crompton (2005).
#'
#' @param data A data frame that will be manipulated.
#' @param shape A character string to select the vector that contains either aspect ratio or elongation values.
#' @param criterion A character string indicating whether aspect ratio or elongation should be used to calculate the proportion. Must be either "asp" or "elon". Default is "elon".
#'
#' @author Uwe Schnepf
#'
#' @references
#' Crompton, C., 2005. Particle Shape An Important Parameter in Pharmaceutical Manufacturing. Pharm. Manuf. Pack. Sourcer.
#' Cole, M., 2016. A novel method for preparing microplastic fibers. Sci. Rep. 6, 34519.
#'
#' @export
calc_proportion_ofFibers <- function(shape,
                                     criterion = "elon")
{
  if(!is.numeric(shape)) stop("Shape descriptors must be numeric.")
  if(criterion == "elon" && !all(shape >= 0 | shape <= 1)) stop("Elongation values are outside the range of 0 to 1. Please check your data")
  if(criterion == "asp" && !all(shape >= 1)) stop("Aspect ratio values are outside the range of 1 to infinity. Please check your data.")

  noOfAllParticles <- length(shape)

  if(criterion == "elon"){
    noOfFibers <- length(shape[shape >= 2/3])
    proportionOfFibers <- noOfFibers/noOfAllParticles*100
  } else if(criterion == "asp"){
    noOfFibers <- length(shape[shape >= 3])
    proportionOfFibers <- noOfFibers/noOfAllParticles*100
  } else stop(cat(criterion, "is not a valid criterion. Please use either 'asp' or 'elon'."))

  return(proportionOfFibers)
}
