#' Write PEPHYLO input mean traits
#'
#' This function saves the MetricTraitMeans.txt input file for PEPHYLO.
#'
#' @param pcaOutput Typically the output of \code{pca_wholebird} or \code{pca_perpatch}. In the fomer case, can be a \code{prcomp} object, or a list, whose first element must be a \code{prcomp} object. In the latter case, a list of two data frames. The first one contains the PC loadings for each patch in columns and the species in rows, and the second contains the data fed ot the PCA i.e. quantum catches in columns and patches per species in rows.
#' @param outpath A string. The path to the folder where the output file is to be saved.
#' @param nPC How many PC to retain? Either an integer i, then PC 1 to i will be retained, or a vector of integers representing what PCs to retain.
#' @param whatSex A logical vector of length the number of observations in the principal component, with \code{TRUE} for each observation of the right sex.
#' @return Returns 1 if succeeds. Saves the output file into the specified folder.
#' @author Raphael Scherrer
#' @export

# Function to save the MetricTraitMeans.txt input file for pephylo
make_mean_pephylo <- function(pcaOutput, outpath, nPC, whatSex) {

  if(!inherits(whatSex, "logical")) stop("whatSex should be logical by now")

  # Input should be a list or a prcomp object
  # If list, the first element should be a PCA output
  if(inherits(pcaOutput, "list")) {
    pcaOutput <- pcaOutput[[1]]
  }

  # Now it should be a PCA output
  #if(!inherits(pcaOutput, "prcomp")) stop("pcaOutput should be a prcomp object by now")

  if(!inherits(nPC, c("numeric", "integer"))) stop("nPC should be one or several integers or numeric")

  # Extract the data from the PCA output (if prcomp object returned from pca_wholebird)
  if(inherits(pcaOutput, "prcomp")) {

    pcaOutput <- pcaOutput$x

  } else {

    # But if pcaOutput is the output of pca_perpatch, just remove the first column which contains species information
    pcaOutput <- pcaOutput[,-1]

  }

  # Set the PCs to retain
  if(length(nPC) == 1)  {
    nPC <- seq_len(nPC)
  }

  # Retain a subset of the PCs + the sex of interest
  pcaOutput <- pcaOutput[whatSex, nPC]

  # Output file path
  outputFile <- paste(outpath, "MetricTraitMeans.txt", sep = "/")

  # Write output
  write.table(pcaOutput, outputFile, col.names = F, row.names = F)

  return(1)

}
