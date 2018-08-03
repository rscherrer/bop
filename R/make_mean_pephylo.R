#' Write PEPHYLO input mean traits
#'
#' This function saves the MetricTraitMeans.txt input file for PEPHYLO.
#'
#' @param pcaOutput Either a list or a \code{prcomp} object. If list, the first element must be a \code{prcomp} object.
#' @param outpath A string. The path to the folder where the output file is to be saved.
#' @param nPC How many PC to retain?
#' @return Returns 1 if succeeds. Saves the output file into the specified folder.
#' @author Raphael Scherrer
#' @export

# Function to save the MetricTraitMeans.txt input file for pephylo
make_mean_pephylo <- function(pcaOutput, outpath, nPC) {

  # Input should be a list or a prcomp object
  # If list, the first element should be a PCA output
  if(inherits(pcaOutput, "list")) {
    pcaOutput <- pcaOutput[[1]]
  }

  # Now it should be a PCA output
  if(!inherits(pcaOutput, "prcomp")) stop("pcaOutput should be a prcomp object")

  # Extract the data from the PCA output
  pcaOutput <- pcaOutput$x

  # Set the number of PC to retain
  pcaOutput <- pcaOutput[,nPC]

  # Output file path
  outputFile <- paste(outpath, "MetricTraitMeans.txt", sep = "/")

  # Write output
  write.table(pcaOutput, outputFile, col.names = F, row.names = F)

  return(1)

}
