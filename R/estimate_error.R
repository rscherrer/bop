#' Estimate error on the means
#'
#' This function estimates errors made on measuring the mean by calculating the standard deviation of a bootstrapped distribution.
#'
#' @param speciesPC A matrix containing the PC loadings for each species.
#' @param indivPC A matrix containing the PC loadings for each individual. Should have the same number of columns as \code{speciesPC}.
#' @param metaSpecies A data frame containing species-level metadata, used to assign individuals to species.
#' @param metaIndiv A data frame containing individual-level metadata.
#' @param nPC How many PC to retain? Either an integer i, then PC 1 to i will be retained, or a vector of integers representing what PCs to retain.
#' @return A vector of estimated errors, one for each dependent variable defined by \code{nPC}.
#' @author Raphael Scherrer
#' @export

# Function to bootstrap the error made on the means
estimate_error <- function(speciesPC, indivPC, metaSpecies, metaIndiv, nPC) {

  if(!inherits(nPC, c("numeric", "integer"))) stop("nPC should be one or several integers or numeric")

  # Set the PCs to retain
  if(length(nPC) == 1)  {
    nPC <- seq_len(nPC)
  }

  nPC <- as.integer(nPC)

  # Label each species-level point
  pointNames <- with(metaSpecies, paste(species, sex))

  # Individual labels
  indivLabs <- with(metaIndiv, paste(species, sex))

  # For each species point, tell what individuals belong to it
  groupID <- lapply(seq_along(pointNames), function(i){

    # What is the current point?
    curr.point <- pointNames[i]

    # What are its dependent variables?
    curr.data <- speciesPC[i,]

    # What individuals match that point?
    curr.indiv <- indivLabs == curr.point

    return(curr.indiv)

  })

  # Make a matrix for the species-level point each individual is related to
  indivAvg <- matrix(NA, ncol = ncol(indivPC), nrow = nrow(indivPC))

  # Fill in the matrix for each species-level point
  for(i in seq_along(groupID)) {

    # What is the index of the current species-level point?
    idx <- groupID[[i]]

    # Use it to fill in the matrix with the average dependent variables for the current species
    indivAvg[idx,] <- speciesPC[i,]

  }

  # Calculate deviations from the mean
  indivDev <- indivPC - indivAvg

  # Bootstrap the error on the mean for each principal component
  meanErrors <- sapply(nPC, function(i) {

    # Current principal component
    curr.dev <- indivDev[,i]

    message(paste0("Bootstrapping variable ", i, "..."))

    # Bootrstrap the standard deviation of the mean
    curr.err <- bootstrap_error(curr.dev)

    return(curr.err)

  })

  return(meanErrors)

}
