#' View PEPHYLO input files
#'
#' This function displays the files in the input folder for PEPHYLO, to check they are alright before running the program.
#'
#' @param filename Either a string with the name of the file to be viewed, either of "MetricTraitMeans", "MetricTraitStds", "MetricSpNames", "SpExt" or "MetricRates", with or without the ".txt" extension. Or, an integer representing the position of the file to view in the list described in the previous sentence.
#' @param outpath A string. The path to the folder where the output file is to be saved.
#' @return Displays the content of the file using \code{View}.
#' @author Raphael Scherrer
#' @export

# Function to check that the output is correctly made
check_pephylo <- function(filename, outpath) {

  filenames <- c("MetricTraitMeans", "MetricTraitStds", "MetricSpNames", "SpExt", "MetricRates")

  if(inherits(filename, "character")) {
    if(length(grep("txt$", filename) == 0)) {
      filename <- paste(filename, 'txt', sep = ".")
    }
  } else if (inherits(filename, c("numeric", "integer"))){
    filename <- paste(filenames[filename], "txt", sep = ".")
  }

  d <- read.table(paste(outpath, filename, sep = "/"))
  View(d)

}
