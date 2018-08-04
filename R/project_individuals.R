#' Project individuals into PC space
#'
#' This projects individual specimens into the PC space defined by species-averages.
#'
#' @param inpath A string. The path to the folder where to find the input data file. Note: the input data file is named "specs_indiv.csv".
#' @param pca A list with fields \code{input} and \code{output}, where \code{output} is a \code{prcomp} object. Typically the output of \code{pca_wholebird} or \code{pca_perpatch}.
#' @param varNames A vector of strings. The names of the dependent variables in the input data frame.
#' @return A matrix of principal component coordinates.
#' @author Raphael Scherrer
#' @export

# Function to project individual specimens into the PC space defined by species-averages
project_individuals <- function(inpath, pca, varNames = c("VS.v", "S.v", "M.v", "L.v")) {

  # Check
  if(!inherits(pca, "list")) stop("pca must be a list")
  if(!all(c("input", "output") %in% names(pca))) stop("pca must have fields input and output")
  if(!inherits(pca$output, "prcomp")) stop("pca$output must be of class prcomp")

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

  # Dependent variables
  X <- indivData[,sapply(indivData, class) == "numeric"]

  # Project the dependent variables into PC space
  Y <- scale(X, pca$output$center, pca$output$scale) %*% pca$output$rotation

  # Output
  out <- list(Y, indivData)
  names(out) <- c("output", "input")

  return(out)
}
