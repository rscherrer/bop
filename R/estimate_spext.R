#' Esimate speciation and extinction
#'
#' This function fits Nee's likelihood function to one or more trees and returns the estimated speciation and extinction rates by maximum likelihood. If several trees are input, the average of those is returned.
#'
#' @param treepath A path to where the tree file is. The tree file should be named "Tress.txt" and be in Newick format. It may contain several trees.
#' @return A vector with a speciation and an extinction rate.
#' @note \code{ape} is a dependency.
#' @author Raphael Scherrer
#' @export

# Function to estimate speciation and extinction rates
estimate_spext <- function(treepath) {

  # Load package
  library(ape)

  # Load trees
  trees <- ape::read.tree(paste(treepath, "Trees.txt", sep = "/"))

  # Fit Nee's ML to each tree
  spext <- lapply(trees, function(tree) {

    # Fit
    spext <- ape::birthdeath(tree)

    # Extract estimates
    spext <- spext$para

    # Turnover (d/b) and net diversification (b-d)
    turnover <- spext[1]
    netdiv <- spext[2]

    # Calculate birth and death
    b <- netdiv / (1 - turnover)
    d <- b - netdiv

    # Concatenate and return
    spext <- c(b, d)
    names(spext) <- NULL
    return(spext)
  })

  # Make a table out of it
  spext <- do.call("rbind", spext)

  # Average over all trees
  spext <- colMeans(spext)

  return(spext)

}
