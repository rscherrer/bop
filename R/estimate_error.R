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

  # Pseudocode
  # Produce a data frame with the deviation from the mean for each individual
  # For each individual, look at what species it belongs to
  # What is the mean for that species?
  # Do the substraction, fill in the data frame

  # Get numerical columns
  numcols.ind <- which(sapply(indivData, is.numeric))
  numcols.sp <- which(sapply(speciesData, is.numeric))

  if(length(numcols.ind) != length(numcols.sp)) stop("indivData and speciesData should have the same number of numerical columns")

  message("Calculating deviations from the means...")

  # For each individual, get deviations from the mean...
  deviations <- lapply(seq_len(nrow(indivData)), function(i) {

    # What species and what sex?
    curr.species <- indivData[i, "species"]
    curr.sex <- indivData[i, "sex"]

    # What row contains the data for the mean of that species?
    idx <- with(speciesData, species == curr.species & sex == curr.sex,)

    # Substract the data to get deviations from the mean
    deviations <- indivData[i, numcols.ind] - speciesData[idx, numcols.sp]

    return(deviations)

  })

  deviations <- do.call("rbind", deviations)

  # Bootstrap the error on the mean for each principal component
  meanErrors <- sapply(nPC, function(i) {

    # Current principal component
    curr.dev <- deviations[,i]

    message(paste0("Bootstrapping variable ", i, "..."))

    # Bootrstrap the standard deviation of the mean
    curr.err <- bootstrap_error(curr.dev)

    return(curr.err)

  })

  return(t(meanErrors))

}
