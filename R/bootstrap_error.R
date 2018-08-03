#' Bootstrap the standard deviation of the mean
#'
#' This function performs a bootstrap on a vector of deviations from means along a given variable to measure the error of the mean
#'
#' @param x A vector of deviations from the mean (or any numbers).
#' @param N An integer. The number of samplings to perform.
#' @return The standard deviation of the re-calculated means of all bootstrapped samples.
#' @author Raphael Scherrer
#' @export

# Function to compute the error (standard deviation) on the mean
bootstrap_error <- function(x, N = 999) {

  # Length of the vector to bootstrap
  n <- length(x)

  # For each permutation
  bootMeans <- sapply(seq_len(N), function(i) {

    print(i)

    # Sample a new random vector with replacement
    y <- sample(x, n, replace = T)

    # Measure the mean of the new sample
    return(mean(y, na.rm = T))

  })

  # Calculate the standard deviation of the bootstrapped means
  return(sqrt(var(bootMeans)))

}
