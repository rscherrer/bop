#' Remove patches with NAs
#'
#' This function removes the patches that are not measured in all species or all sexes from a data frame
#'
#' @param specTab A data frame. Must have a column "patch" and a column "species".
#' @return A subset of the input data frame with only patches that have no missing data.
#' @author Raphael Scherrer
#' @export

# Function to remove the patches that are not measured in all species or all sexes from a data frame
remove_bad_patches <- function(specTab) {

  # Check that the input is OK
  if(!inherits(specTab, "data.frame")) stop("specTab must be a data frame")
  if(!all(c("patch", "species") %in% colnames(specTab))) stop("specTab should have a column 'patch' and a column 'species'")

  # Record patch and species names
  patchNames <- levels(specTab$patch)
  speciesNames <- levels(specTab$species)

  # Find out what patches are not present in ALL species and both sexes
  isData <- sapply(patchNames, function(curr.patch) {

    # What species/sex have data for this patch?
    curr.data <- droplevels(specTab[specTab$patch == curr.patch,])
    curr.species <- curr.data$species
    curr.sex <- curr.data$sex

    # What combinations of species and sex are there?
    curr.combi <- paste(curr.species, curr.sex)

    # There should be as many combinations as nSexes * nSpecies
    nCombi <- length(unique(curr.combi))
    nSpecies <- length(unique(curr.species))
    nSexes <- length(unique(curr.sex))

    return(nCombi == nSpecies * nSexes)

  })

  # What are the patches to keep?
  goodPatches <- names(isData)[isData]

  specTab <- droplevels(specTab[specTab$patch %in% goodPatches,])

  if(!all(levels(specTab$patch) == goodPatches)) stop("specTab should contain only patches without missing data")

  return(specTab)

}
