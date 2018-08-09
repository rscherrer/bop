#' View PEPHYLO input files
#'
#' This function checks the correspondance between species names in the data and in the phylogeny(ies)
#'
#' @param outpath A string. The path to the folder where the output file is to be saved.
#' @return Messages summarizing what species are missing, if any.
#' @author Raphael Scherrer
#' @export

# Function to know if the names in the measurements match the names in the phylogeny
is_matching_names <- function(outpath) {

  # Check folder
  if(!"MetricSpNames.txt" %in% list.files(outpath)) stop("MetricSpNames.txt should be in outpath")
  if(!"Trees.txt" %in% list.files(outpath)) stop("Trees.txt should be in outpath")

  # Load files
  spnames.obs <- read.table(paste(outpath, "MetricSpNames.txt", sep = "/"))
  spnames.obs <- unlist(spnames.obs)
  library(ape)
  tree <- ape::read.tree(paste(outpath, "Trees.txt", sep = "/"))
  if(inherits(tree, "multiPhylo")) tree <- tree[[1]]
  spnames.tre <- tree$tip.label

  # Return if all names in obs are present in phylogeny
  isInTree <- spnames.obs %in% spnames.tre
  if(all(isInTree)) {
    message("All observed species are in tree :)")
  } else {
    missing <- paste(spnames.obs[!isInTree], collapse = ", ")
    message(paste("Species are missing from tree:", missing, sep = " "))
  }

  # Return if there are names from the phylogeny absent from obs
  isInObs <- spnames.tre %in% spnames.obs
  if(all(isInObs)) {
    message("All species in tree are observed")
  } else {
    missing <- paste(spnames.tre[!isInObs], collapse = ", ")
    message(paste("Species are not observed:", missing, sep = " "))
  }

}
