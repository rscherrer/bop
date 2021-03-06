% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_mean_pephylo.R
\name{make_mean_pephylo}
\alias{make_mean_pephylo}
\title{Write PEPHYLO input mean traits}
\usage{
make_mean_pephylo(data, outpath, nPC, whatSex)
}
\arguments{
\item{data}{A data frame or matrix with dependent variables in columns and observations in rows.}

\item{outpath}{A string. The path to the folder where the output file is to be saved.}

\item{nPC}{How many PC to retain? Either an integer i, then PC 1 to i will be retained, or a vector of integers representing what PCs to retain.}

\item{whatSex}{A logical vector of length the number of observations in the principal component, with \code{TRUE} for each observation of the right sex.}
}
\value{
Returns 1 if succeeds. Saves the output file into the specified folder.
}
\description{
This function saves the MetricTraitMeans.txt input file for PEPHYLO.
}
\author{
Raphael Scherrer
}
