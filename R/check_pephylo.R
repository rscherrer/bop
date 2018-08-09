#' View PEPHYLO input files
#'
#' This function displays the files in the input folder for PEPHYLO, to check they are alright before running the program.
#'
#' @param filename Either a string with the name of the file to be viewed, either of "MetricTraitMeans", "MetricTraitStds", "MetricSpNames", "SpExt", "MetricRates" or "Trees", with or without the ".txt" extension. Or, an integer representing the position of the file to view in the list described in the previous sentence.
#' @param outpath A string. The path to the folder where the output file is to be saved.
#' @param plotit Logical. Whether to plot the first phylogeny provided (applicable only if filename is Trees.txt or 6).
#' @return Displays the content of the file using \code{View}.
#' @author Raphael Scherrer
#' @export

# Function to check that the output is correctly made
check_pephylo <- function(filename, outpath, plotit = T) {

  filenames <- c("MetricTraitMeans", "MetricTraitStds", "MetricSpNames", "SpExt", "MetricRates", "Trees")

  if(inherits(filename, "character")) {
    if(length(grep("txt$", filename) == 0)) {
      filename <- paste(filename, 'txt', sep = ".")
    }
  } else if (inherits(filename, c("numeric", "integer"))){
    filename <- paste(filenames[filename], "txt", sep = ".")
  }

  if(length(grep("Trees", filename)) == 0) {
    d <- read.table(paste(outpath, filename, sep = "/"))
    View(d)
  } else {

    if(filename %in% list.files(outpath)) {
      message("Yes, Trees.txt is in the input folder")
      library(ape)
      if(plotit) {
        tree <- read.tree(paste(outpath, filename, sep = "/"))
        if(inherits(trees, "multiPhylo")) tree <- tree[[1]]
        plot(tree)
        axisPhylo()
      }
    } else {
      message("Nope, no Trees.txt here...")
    }


  }


}
