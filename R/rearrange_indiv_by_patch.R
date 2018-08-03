#' Rearrange individuals with patches in columns
#'
#' This function rearranges an individual-level dataset by patch
#'
#' @param indivData A data frame. Must have columns "patch", "species", "sex" and "specimen".
#' @return A rearranged data frame with different patches placed in different columns.
#' @note It is a separate function from \code{rearrange_by_patch} because it uses a for-loop and takes longer. For rearranging a species-level data frame, use \code{rearrange_by_patch}.
#' @author Raphael Scherrer
#' @export

# Function to rearrange an individual-level dataset by patch
# Note: it is a separate function from rearrange_by_patch because it uses a for-loop and takes longer
rearrange_indiv_by_patch <- function(indivData) {

  # How many patches and what are they?
  nPatches <- nlevels(indivData$patch)
  patchNames <- levels(indivData$patch)

  # Prepare new column names
  colNames <- expand.grid(varNames, patchNames)
  colNames <- apply(colNames, 1, paste, collapse = "_")

  # Tags for all individual specimens
  indivNames <- unique(with(indivData, paste(species, sex, specimen)))

  # Prepare a new rearranged data frame
  newTab <- matrix(NA, nrow = length(indivNames), ncol = length(colNames))

  # Go through rows and fill in the new data frame
  for(i in seq_len(nrow(indivData))) {

    # What is the patch of the current row?
    curr.patch <- indivData[i,]$patch

    # What column to paste the data in?
    whatCol <- grep(curr.patch, colNames)

    # What is the current individual?
    curr.indiv <- with(indivData[i,], paste(species, sex, specimen))

    # What row does it correspond to in the new df?
    whatRow <- curr.indiv == indivNames

    # Paste the data
    newTab[whatRow, whatCol] <- unlist(indivData[i, varNames])

  }

  # Convert the new data into a df
  newTab <- as.data.frame(newTab)
  colnames(newTab) <- colNames

  # Record metadata
  metadata <- as.data.frame(do.call("rbind", strsplit(indivNames, " ")))
  colnames(metadata) <- c("species", "sex", "specimen")

  # Merge everthing
  indivData <- cbind(metadata, newTab)

  return(indivData)

}
