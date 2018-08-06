#' Project individuals into PC space
#'
#' This projects individual specimens into the PC space defined by species-averages.
#'
#' @param inpath A string. The path to the folder where to find the input data file. Note: the input data file is named "specs_indiv.csv".
#' @param pca A \code{prcomp} object.
#' @param varNames A vector of strings. The names of the dependent variables in the input data frame.
#' @return A data frame of principal component coordinates and metadata.
#' @author Raphael Scherrer
#' @export

# Function to project individual specimens into the PC space defined by species-averages
project_individuals <- function(inpath, pca, varNames = c("VS.v", "S.v", "M.v", "L.v")) {

  # Check
  if(!inherits(pca, "prcomp")) stop("pca must be a prcomp object")

  message("Projecting individuals...")

  # Read individual data
  filename <- "specs_indiv.csv"
  indivData <- read.csv(paste(inpath, filename, sep = "/"))

  # Remove bad patches
  indivData <- remove_bad_patches(indivData)

  # Extract the dependent variables
  indivData <- indivData[, c("species", "sex", "patch", "specimen", varNames)]

  # Rearrange by patch
  indivData <- rearrange_indiv_by_patch(indivData)

  # Record metadata
  metadata <- indivData[,c("species", "sex", "specimen")]

  # Dependent variables
  X <- indivData[,sapply(indivData, class) == "numeric"]

  # Project the dependent variables into PC space
  Y <- scale(X, pca$center, pca$scale) %*% pca$rotation

  # Merge for output
  out <- data.frame(metadata, Y)

  return(out)
}
