% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{cut_probs}
\alias{cut_probs}
\title{Creates a vector if probabilities from a low a high and a step
value. Makes sure lowest is 0 and highest 1. Intended for use in cut_qs()}
\usage{
cut_probs(low = 0.01, high = 0.99, step = 0.2)
}
\arguments{
\item{low}{numeric, lowest probability which is greater 0.}

\item{high}{numeric, highest probability which is smaller 1.}

\item{step}{numeric, step size}
}
\value{
numeric vector
}
\description{
Creates a vector if probabilities from a low a high and a step
value. Makes sure lowest is 0 and highest 1. Intended for use in cut_qs()
}
\author{
Konstantin Hoffie
}
