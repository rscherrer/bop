#' Write PEPHYLO input mean traits
#'
#' This function saves the MetricTraitMeans.txt input file for PEPHYLO.
#'
#' @param data A data frame or matrix with dependent variables in columns and observations in rows.
#' @param outpath A string. The path to the folder where the output file is to be saved.
#' @param nPC How many PC to retain? Either an integer i, then PC 1 to i will be retained, or a vector of integers representing what PCs to retain.
#' @param whatSex A logical vector of length the number of observations in the principal component, with \code{TRUE} for each observation of the right sex.
#' @return Returns 1 if succeeds. Saves the output file into the specified folder.
#' @author Raphael Scherrer
#' @export

# Function to save the MetricTraitMeans.txt input file for pephylo
make_mean_pephylo <- function(data, outpath, nPC, whatSex) {

  if(!inherits(whatSex, "logical")) stop("whatSex should be a logical vector")

  if(!inherits(nPC, "numeric")) stop("nPC should be of class numeric")

  # Set the PCs to retain
  if(length(nPC) == 1)  {
    nPC <- seq_len(nPC)
  }

  # Retain a subset of the PCs + the sex of interest
  data <- data[whatSex, nPC]

  # Output file path
  outputFile <- paste(outpath, "MetricTraitMeans.txt", sep = "/")

  # Write output
  write.table(data, outputFile, col.names = F, row.names = F)

  return(1)

}
