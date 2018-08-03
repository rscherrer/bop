#' Write PEPHYLO standard deviation input
#'
#' This function saves the standard deviations of the means required for running PEPHYLO
#'
#' @param errors A vector of standard deviations (errors on the means) for each dependent variable. The errors can be calculated with \code{bootrstrap_error}.
#' @param nrows An integer. The number of rows of the input table.
#' @param outpath A string. The path to the folder where the output file is to be saved.
#' @return Returns 1 if succeeds. Saves the output file into the specified folder.
#' @author Raphael Scherrer
#' @export

# Function to write the errors into a PEPHYLO input file
make_std_pephylo <- function(errors, nrows, outpath) {

  # Pepare output table
  errOutput <- matrix(rep(errors, nrows), ncol = length(errors), nrow = nrows, byrow = T)

  # Path to output
  outputFile <- paste(outpath, "MetricTraitStds.txt", sep = "/")

  # Write output
  write.table(errOutput, outputFile, col.names = F, row.names = F)

  return(1)

}
