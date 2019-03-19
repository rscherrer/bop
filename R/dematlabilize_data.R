#' Export Matlab output
#'
#' This function turns matlab output into a data frame handily usable in R
#'
#' @param path Path to where the output files are.
#' @return A huge data frame with MCMC variables in columns and iterations in rows.
#' @author Raphael Scherrer
#' @export

# Function to turn matlab output into a data frame handily usable in R
dematlabilize_data <- function(path = ".") {

  library(R.matlab)

  homedir <- getwd()
  setwd(path)

  data_files <- list.files()

  dematlabilized_data <- lapply(data_files, function(curr_data_file) {

    data <- readMat(curr_data_file)

    # Transform ancestral reconstructions from 3D to 2D
    ancestral_reconstructions <- do.call("cbind", lapply(seq_len(dim(data[[1]][[1]][[2]][[1]])[2]), function(i) {
      return(data[[1]][[1]][[2]][[1]][,i,])
    }))

    # Make a csv data frame out of the matlab data
    dematlabilized_data <- data.frame(
      lik = t(data[[1]][[1]][[1]]),
      lambda = data[[1]][[2]][[1]][,1],
      mu = data[[1]][[2]][[1]][,2],
      sa2 = data[[1]][[2]][[2]][[3]],
      sc2 = data[[1]][[2]][[2]][[4]],
      ace = ancestral_reconstructions
    )

    return(dematlabilized_data)

  })

  metadata <- data.frame(t(sapply(strsplit(list.files(), "_"), "[", c(2,3,4))))
  colnames(metadata) <- c("sex", "type", "mode")
  pairs_of_files <- tapply(list.files(), with(metadata, sex:mode), function(curr_files) return(c(curr_files)))

  dematlabilized_data <- lapply(pairs_of_files, function(curr_pair_of_files) {

    id_files <- sapply(curr_pair_of_files, function(curr_file) grep(curr_file, list.files()))
    combined_dematlabilized_data <- do.call("cbind", dematlabilized_data[id_files])
    return(combined_dematlabilized_data)

  })

  dematlabilized_data <- do.call("rbind", dematlabilized_data)

  metadata <- data.frame(do.call("rbind", strsplit(rep(levels(metadata$sex:metadata$mode), each = 1000), ":")))
  colnames(metadata) <- c("sex", "mode")

  dematlabilized_data <- cbind(dematlabilized_data, metadata)

  setwd(homedir)

  return(dematlabilized_data)

}
