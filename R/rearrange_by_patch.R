#' Rearrange with patches in columns
#'
#' This function rearranges a dataframe with patches in different columns
#'
#' @param df A data frame. Must have a column "patch" and a column "species".
#' @return A rearranged data frame with different patches placed in different columns.
#' @author Raphael Scherrer
#' @export

# Function to rearrange a dataframe with patches in different columns
rearrange_by_patch <- function(df) {

  # Check that the input is OK
  if(!inherits(df, "data.frame")) stop("df must be a data frame")
  if(!all(c("patch", "species") %in% colnames(df))) stop("df should have a column 'patch' and a column 'species'")

  # Record variable names
  varNames <- colnames(df)[!colnames(df) %in% c("species", "patch", "sex")]

  # Record patch names
  patchNames <- levels(df$patch)

  # Count how many species
  nSpecies <- nlevels(df$species)

  # Split the data frame into patches
  df.split <- lapply(patchNames, function(curr.patch) {

    subset(df, patch == curr.patch)

  })

  # All split data frames should have the same dimensions
  isSameDim <- all(lapply(df.split, nrow) == 2 * nSpecies)

  if(!isSameDim) stop("data frames resulting from the split into different patches should all have the sames dimensions")

  # Remove duplicate metadata columns
  df.split <- lapply(seq_along(df.split), function(i) {

    curr.df <- df.split[[i]]

    # Remove patch column from all data frames
    curr.df <- curr.df[, colnames(curr.df) != "patch"]

    # Remove species and sex from all df but the first one
    if(i != 1) curr.df <- curr.df[, !colnames(curr.df) %in% c("species", "sex")]

    return(curr.df)

  })

  # Rearrange the data by columns into a new level data frame
  df <- do.call("cbind", df.split)

  # New column names that include patch ID
  newColNames <- expand.grid(varNames, patchNames)
  newColNames <- apply(newColNames, 1, paste, collapse = ".")
  colnames(df)[!colnames(df) %in% c("species", "sex")] <- newColNames

  return(df)

}
