#' Estimate error on the means
#'
#' This function estimates errors made on measuring the mean by calculating the standard deviation of a bootstrapped distribution.
#'
#' @param speciesData A data frame with PC loadings and metadata at the species level.
#' @param indivData A data frame with PC loadings and metadata at the individual level.
#' @param nPC How many PC to retain? Either an integer i, then PC 1 to i will be retained, or a vector of integers representing what PCs to retain.
#' @return A vector of estimated errors, one for each dependent variable defined by \code{nPC}.
#' @author Raphael Scherrer
#' @export

# Function to bootstrap the error made on the means
estimate_error <- function(speciesData, indivData, nPC) {

  if(!inherits(nPC, "numeric")) stop("nPC should be numeric")

  # Set the PCs to retain
  if(length(nPC) == 1)  {
    nPC <- seq_len(nPC)
  }

  # Label each species-level point
  speciesLabs <- with(speciesData, paste(species, sex))

  # Individual labels
  indivLabs <- with(indivData, paste(species, sex))

  # For each species point, tell what individuals belong to it
  groupID <- lapply(seq_along(speciesLabs), function(i){

    # What is the current point?
    curr.point <- speciesLabs[i]

    # What are its dependent variables?
    curr.data <- speciesData[i, sapply(speciesData, is.numeric)]

    # What individuals match that point?
    curr.indiv <- indivLabs == curr.point

    return(curr.indiv)

  })

  # Make a matrix for the species-level point each individual is related to
  indivAvg <- matrix(NA, ncol = ncol(indivData[,sapply(indivData, is.numeric)]), nrow = nrow(indivData))

  # Fill in the matrix for each species-level point
  for(i in seq_along(groupID)) {

    # What is the index of the current species-level point?
    idx <- groupID[[i]]

    # Use it to fill in the matrix with the average dependent variables for the current species
    indivAvg[idx,] <- speciesData[i,sapply(speciesData, is.numeric)]

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
