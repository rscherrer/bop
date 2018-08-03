#' Write species names for PEPHYLO
#'
#' This function writes a vector of species names into a file for input in PEPHYLO
#'
#' @param speciesNames A vector of species names.
#' @param outpath A string. The path to the folder where the output file is to be saved.
#' @return Returns 1 if succeeds. Saves the output file into the specified folder.
#' @author Raphael Scherrer
#' @export

# Function to write a table with species names
make_spname_pephylo <- function(speciesNames, outpath) {

  # Path to output
  outputFile <- paste(outpath, "MetricSpNames.txt", sep = "/")

  # Make a table
  speciesNames <- as.factor(cbind(as.character(speciesNames)))

  # Write output
  write.table(speciesNames, outputFile, col.names = F, row.names = F, quote = F)

  return(1)

}
