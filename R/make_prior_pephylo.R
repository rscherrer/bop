#' Anagenetic and cladogenetic rate priors for PEPHYLO
#'
#' This function writes a table with prior rates of anagenetic and cladogenetic evolution. The prior for sa2 is calculated by dividing interspecific variance for each dependent variable by the age of the clade. The prior for sc2 is calculated by dividing the same interspecfic variance by the number of species. Saves an output file with four colums filled with zeros (unwanted parameters), plus two columns with sa2 and sc2 rate priors. One row per dependent variable.
#'
#' @param meantraits A matrix or data frame with mean traits for each species (rows), for each dependent variable (columns).
#' @param age The age of the clade (in Myr).
#' @param outpath A string. The path to the folder where the output file is to be saved.
#' @note The amount of evolutionary change along a branch in a phylogeny is assumed to be normally distributed with mean zero and variance sa2 t + sc2 S, with t the age of the clade and S the number of speciation events. sa2 represents the rate of change expected over 1 Myr. sc2 represents the rate of change upon speciation e.g. change with mean zero and variance sc2 upon speciation.
#' @return Returns 1 if succeeds. Saves the output file into the specified folder.
#' @author Raphael Scherrer
#' @export

# Function to write a table for PEPHYLO with prior rates of anagenetic and cladogenetic evolution
make_prior_pephylo <- function(meantraits, age, outpath) {

  # For each dependent variable...
  if(is.null(dim(meantraits))) meantraits <- cbind(meantraits)

  ratePriors <- apply(meantraits, 2, function(curr.meantraits) {

    # Calculate the interspecific variance
    V <- var(curr.meantraits, na.rm = T)

    # Divide it by the age of the clade to get a prior for sa2
    sa2 <- V / age

    # Divide it by the number of species to get a prior for sc2
    sc2 <- V / length(meantraits)

    return(c(sc2, sa2))

  })

  # Add zero for unwanted parameters
  zeros <- matrix(0, ncol = 4, nrow = ncol(meantraits))
  ratePriors <- cbind(zeros, t(ratePriors))

  # Path to output
  outputFile <- paste(outpath, "MetricRates.txt", sep = "/")

  # Write output
  write.table(ratePriors, outputFile, col.names = F, row.names = F, quote = F)

  return(1)

}
