% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_spext_pephylo.R
\name{make_spext_pephylo}
\alias{make_spext_pephylo}
\title{PEPHYLO speciation and extinction rate priors}
\usage{
make_spext_pephylo(lambda, mu, outpath)
}
\arguments{
\item{lambda}{The speciation rate.}

\item{mu}{The extinction rate.}

\item{outpath}{A string. The path to the folder where the output file is to be saved.}
}
\value{
Returns 1 if succeeds. Saves the output file into the specified folder.
}
\description{
This function writes a vector of speciation and extinction rate priors into a file for input in PEPHYLO
}
\author{
Raphael Scherrer
}
