#' Function to test all 9 models on a phenotypic vector, nrepet times
#'
#' This function fits (potentially) several models of evolution on a phenotypic vector using Levy processes from the package pulsr. Each fitting can be replicated to ensure convergence to a single optimum likelihood.
#'
#' @param trait_vector A vector of phenotypic values across species.
#' @param tree A phylogeny, in ape format.
#' @param species_names A vector of species names.
#' @param model_types The names of the models to fit, in a vector. See the scripts downloaded from pulsr's github (stored in analysis2/R) for a list of models available.
#' @param nrepet The number of replicate analyses.
#' @return A list of lists of outputs of the fitting function. Levels of nestedness: model type, replicates.
#' @note This function was run on the cluster. It is made to call the functions in pulsr, which are not yet a working package but a bunch of scripts in a folder associated. So, this function won't work unless these scripts are availeble (see the source of the function to know where they should be). Also, this function runs on a single variable and not on a dataset of multiple variables because some variable cause convergence problems. This avoids the whole analysis to break down because of a single problematic variable in a multivariate dataset.
#' @author Raphael Scherrer
#' @export

# Function to test all 9 models on a phenotypic vector, nrepet times
# Arguments:
# dataset -- path to dataset
# tree -- phylogeny in ape format
# species_names -- vector of species names
# model_types -- the Levy processes to be fitted
# nrepet -- how many times to repeat each fitting?

fit_pulsr <- function(trait_vector, tree, species_names, model_types, nrepet) {

  names(trait_vector) <- species_names

  # Run each type of model
  res_levy <- lapply(model_types, function(curr_model_type) {

    res_levy <- lapply(seq_len(nrepet), function(repetition) {

      fit_reml_levy(phy = tree, dat = trait_vector, model = curr_model_type, silent = T)

    })

    return(res_levy)

  })

  return(res_levy)

}
