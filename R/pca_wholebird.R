#' Project bird species in PC space
#'
#' This function performs a Principal Component Analysis from many dependent variables representing the color of the birds measured from different patches on the plumage.
#'
#' @param inpath A string. The path to the folder where to find the input data file. Note: the input data file is named "specs.csv".
#' @param varNames A vector of strings. The names of the dependent variables in the input data frame.
#' @return A list of two elements. The first is the output of \code{prcomp} performed on the dependent variables, where each bird has coordinates. The second element is the table of dependent variables with metadata that was used to perform the PCA.
#' @author Raphael Scherrer
#' @export

# Function to project bird species in PC space
pca_wholebird <- function(inpath, varNames) {

  # Input file name
  filename <- "specs.csv"

  # Read input
  specTab <- read.csv(paste(inpath, filename, sep = "/"))

  # Extract the dependent variables of interest
  specTab <- specTab[, c("species", "sex", "patch", varNames)]

  # Remove the patches that are not measured in ALL species and ALL sexes
  specTab <- remove_bad_patches(specTab)

  # Rearrange the dependent variables to have patches in different columns
  specTab <- rearrange_by_patch(specTab)

  # Matrix of dependent variables
  X <- as.matrix(specTab[,!colnames(specTab) %in% c("species", "sex")])

  # Perform Principal Component Analysis
  pca.fit <- prcomp(X, center = T, scale = T)

  # Return PCA output
  out <- list(pca.fit, specTab)

  return(out)

}
