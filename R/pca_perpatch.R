#' Project bird species in patch PC space
#'
#' This function performs a Principal Component Analysis from quantum catches measured on different patches on the plumage. Each bird is represented by several patches, so several points.
#'
#' @param inpath A string. The path to the folder where to find the input data file. Note: the input data file is named "specs.csv".
#' @param varNames A vector of strings. The names of the dependent variables in the input data frame.
#' @return A list of two elements. The first is the output of \code{prcomp} performed on the dependent variables, where each bird has coordinates. The second element a table with metadata identifying each observation in the PCA.
#' @author Raphael Scherrer
#' @export

# Function to perform PCA and create a patch-color space
pca_perpatch <- function(inpath, varNames = c("VS.v", "S.v", "M.v", "L.v")) {

  # Input file name
  filename <- "specs.csv"

  # Read input
  specTab <- read.csv(paste(inpath, filename, sep = "/"))

  # Extract the dependent variables of interest
  specTab <- specTab[, c("species", "sex", "patch", varNames)]

  # Remove the patches that are not measured in ALL species and ALL sexes
  specTab <- remove_bad_patches(specTab)

  # Matrix of dependent variables
  X <- as.matrix(specTab[,!colnames(specTab) %in% c("species", "sex", "patch")])

  # Perform Principal Component Analysis
  pca.fit <- prcomp(X, center = T, scale = T)

  # Keep track for metadata
  metadata <- specTab[,c("species", "sex", "patch")]

  return(list(res = pca.fit, metadata = metadata))

}
