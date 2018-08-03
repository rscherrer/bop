#' Create PEPHYLO input
#'
#' This function creates a table of traits per species suitable for input in Bokma's PEPHYLO
#'
#' @param inpath A string. The path to the folder where the input file "spec.csv" is to be looked for.
#' @param outpath A string. The path to the folder where the output table is to be written.
#' @param wholeBird Logical. Whether the points in color space represent whole birds (\code{TRUE}) or patches (\code{FASLE}).
#' @param varNames Vector of strings. The name(s) of the variable(s) from the reflectance data to include in the Principal Component Analysis
#' @param whatSex A character. \code{"M"} for males, \code{"F"} for females.
#' @param saveit Logical. Whether to save the output table in a file.
#' @param plotit Logical. Whether to plot PCA eigenvalues.
#' @return A data frame with each sex of each species in rows, and principal components in columns. If \code{wholeBird = TRUE}, columns represent loadings of each patch on principal components 2, 3 and 4. Otherwise, columns represent principal components 2, 3 and 4.
#' @details If \code{wholeBird = TRUE}, the different reflectance measurements for each patch are inputs of the PCA. The PC loadings describe the average position of each species in this highly multidimensional phenotypic space. If \code{wholeBird = FALSE}, the reflectance measurements are the inputs of the PCA. The PC loadings represent the position of each patch into what is closer to a true color space (a few dimensions). But PEPHYLO requires observed phenotypes on a per-species basis, so the PC loadings of each patch are converted into dependent variables.
#' @author Raphael Scherrer
#' @references Bokma, F. (2008). Detection of “punctuated equilibrium” by Bayesian estimation of speciation and extinction rates, ancestral character states, and rates of anagenetic and cladogenetic evolution on a molecular phylogeny. Evolution, 62(11), 2718–2726. https://doi.org/10.1111/j.1558-5646.2008.00492.x
#' @export

# Function to create input file for PEPHYLO
create_input <- function(inpath, outpath, wholeBird = F, varNames = c("VS.v", "S.v", "M.v", "L.v"), whatSex = "M", saveit = T, plotit = F) {

  # Read reflectance data
  specs <- read.csv(paste(inpath, "specs.csv", sep = "/"))

  # Create color data frame
  colorDF <- specs[,c("species", "sex", "patch", varNames)]

  # Make a list of all possible patches
  patchNames <- levels(specs$patch)

  # Make a list of all possible species
  speciesNames <- levels(specs$species)

  # How many species?
  nSpecies <- length(speciesNames)

  # Find out what patches are not present in ALL species and both sexes
  isData <- sapply(patchNames, function(curr.patch) {

    # What species have data for this patch?
    curr.species <- droplevels(specs$species[specs$patch == curr.patch])

    # If males and females of each species have this patch
    # The length of curr.species should be twice that of the number of species
    answer <- length(curr.species) == 2 * length(speciesNames)

  })

  # What are the patches to keep?
  goodPatches <- names(isData)[isData]

  # Remove all the data for missing patches
  colorDF <- droplevels(colorDF[colorDF$patch %in% goodPatches,])

  if(!all(levels(colorDF$patch) == goodPatches)) stop("colorDF should contain only patches without missing data")

  # If the whole bird must be analyzed...
  if(wholeBird) {

    # Split the data into multiple patches
    colorDF.split <- lapply(goodPatches, function(curr.patch) {

      subset(colorDF, patch == curr.patch)

    })

    # All split data frames should have the same dimensions
    isSameDim <- all(lapply(colorDF.split, nrow) == 2 * nSpecies)

    if(!isSameDim) stop("data frames resulting from the split into different patches should all have the sames dimensions")

    # Rearrange the data by columns into a new species-level data frame
    colorDF <- do.call("cbind", colorDF.split)

    # Remove extra species, patch and sex columns
    columns <- which(colnames(colorDF) %in% c("species", "sex", "patch"))
    columnsToRemove <- columns[columns > 3]
    colorDF <- colorDF[,-columnsToRemove]

    if(!(ncol(colorDF) - 3) / length(goodPatches) == length(varNames)) stop("the number of response variables is not a multiple of the number of patches")

    colorDF <- colorDF[,-which(colnames(colorDF) == "patch")]

    # New column names that include patch ID
    newColNames <- expand.grid(varNames, goodPatches)
    newColNames <- apply(newColNames, 1, paste, collapse = ".")
    colnames(colorDF)[3:ncol(colorDF)] <- newColNames

  }

  # Make a matrix to which apply PCA
  Y <- as.matrix(colorDF[, !colnames(colorDF) %in% c("species", "sex", "patch")])

  # Keep track of the metadata
  metaData <- colorDF[,c("species", "sex")]

  # Perform the PCA
  pca.fit <- prcomp(Y, scale = T)

  # Record rotation matrix
  pca.rotation <- pca.X$rotation

  # Visualize eigenvalues
  if(plotit) {
    plot(pca.fit)
  }

  # Extract PC coordinates
  pca.X <- pca.fit$x

  # Remove PC1 (= brightness)
  pca.X <- pca.X[,-1]

  # Remove PC > 4
  if(ncol(pca.X) > 3) pca.X <- pca.X[,1:3]

  # Attach metadata to the PCA table
  pca.X <- cbind(metaData, pca.X)

  # Split the PCA coordinates per patch (if not whole bird)
  # Note: it is important to split into patches AFTER running the PCA, this way the PCA represents the whole color space
  if(!wholeBird) {

    pca.X.split <- lapply(goodPatches, function(curr.patch) {

      pca.X[colorDF$patch == curr.patch,]

    })

    # Check that all split data frames have the same dimensions
    isSameDim <- all(lapply(pca.X.split, nrow) == 2 * nSpecies)
    if(!isSameDim) stop("data frames resulting from the split into different patches should all have the sames dimensions")

    # Remove duplicate metadata columns
    pca.X.split <- lapply(seq_along(pca.X.split), function(i) {
      if(i != 1) {
        pca.X.split[[i]] <- pca.X.split[[i]][,-c(1,2)]
      }
      return(pca.X.split[[i]])
    })

    # Merge together the data frames by column
    pca.X <- do.call("cbind", pca.X.split)

    # New column names that include patch ID
    newColNames <- expand.grid(c("PC2", "PC3","PC4"), goodPatches)
    newColNames <- apply(newColNames, 1, paste, collapse = ".")
    colnames(pca.X)[3:ncol(pca.X)] <- newColNames

  }

  # By now, pca.X should be a table with PC loadings for all species (if wholeBird is TRUE), or a table with PC loadings per patch for all species.

  # Subset the sex of interest
  pca.X <- droplevels(pca.X[pca.X$sex == whatSex,])

  # Remove metadata columns
  pca.X2 <- pca.X[,-c(1,2)]

  # Save the data
  if(saveit) {

    outputFile <- paste(outpath, "MetricTraitMeans.txt", sep = "/")
    write.table(pca.X2, outputFile, col.names = F, row.names = F)

  }

  # Return the subset of the points in PC space as well as a rotation matrix (to be able to project points into PC space)
  return(list(pca.X, pca.rotation))

}
