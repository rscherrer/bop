#' PEPHYLO speciation and extinction rate priors
#'
#' This function writes a vector of speciation and extinction rate priors into a file for input in PEPHYLO
#'
#' @param lambda The speciation rate.
#' @param mu The extinction rate.
#' @param outpath A string. The path to the folder where the output file is to be saved.
#' @return Returns 1 if succeeds. Saves the output file into the specified folder.
#' @author Raphael Scherrer
#' @export

# Function to write the speciation-extinction priors into a file for PEPHYLO
make_spext_pephylo <- function(lambda, mu, outpath) {

  # Path to output
  outputFile <- paste(outpath, "SpExt.txt", sep = "/")

  # Make a vector
  spext <- rbind(c(lambda, mu))

  # Write output
  write.table(spext, outputFile, col.names = F, row.names = F, quote = F)

  return(1)

}
