#' Combine output files
#'
#' This function combines output files from pulsR analyses from different variables. There is no argument. For each dataset, it combines the output lists generated for each variable into a single RDS file.
#' @param path Path to where the output files are. This is the parent directory to the folders corresponding to each dataset: "males_wholebird", "females_wholebird", "males_perpatch", "females_perpatch". They must all be present. Each contains as many folders as there are variables. Each of these variable-folders contains an RDS file, which was saved by the function \code{fit_pulsr} on the cluster and contains the results of the fitting.
#' @note This functions saves an RDS file into each dataset-folder, named "results_all_variables.rds".
#' @author Raphael Scherrer
#' @export
combine_pulsr_results <- function(path) {

  homedir <- getwd()
  setwd(path)

  dataset_folders <- c("males_wholebird", "females_wholebird", "males_perpatch", "females_perpatch")

  # For each dataset...
  lapply(dataset_folders, function(curr_folder) {

    setwd(curr_folder)

    # Extract the results for all variables
    results_all_variables <- lapply(seq_along(list.files()), function(curr_variable_id) {

      setwd(paste("variable", curr_variable_id, sep = "_"))
      if(length(grep(".rds", list.files())) == 1) {
        results <- readRDS(list.files()[grep(".rds", list.files())])
      } else {
        results <- NULL
      }
      setwd("..")
      return(results)

    })

    # Save that in an RDS object
    saveRDS(results_all_variables, "results_all_variables.rds")

    setwd("..")

  })

  setwd(homedir)


}
