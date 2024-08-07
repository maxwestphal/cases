% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/categorize.R
\name{categorize}
\alias{categorize}
\title{Categorize continuous values}
\usage{
categorize(
  values,
  cutoffs = rep(0, ncol(values)),
  map = 1:ncol(values),
  labels = NULL
)
}
\arguments{
\item{values}{(matrix) \cr numeric matrix of continuous values to be categorized.
Assume an (n x r) matrix with n observations (subjects) of r continuous values.}

\item{cutoffs}{(numeric | matrix) \cr numeric matrix of dimension m x k. Each row of cutoffs defines a split
into k+1 distinct categories. Each row must contain distinct values. In the simplest case (k=1),
cutoffs is a single column matrix whereby each row defines a binary split (<=t vs. >t).
In this case (k=1), cutoffs can also be a numeric vector.}

\item{map}{(numeric) \cr integer vector of length k with values in 1:r, whereby r = ncol(values).
map_l gives the value which column of values should be categorized by ...}

\item{labels}{(character) \cr character of length m (= number of prediction r)}
}
\value{
(matrix) \cr numeric (n x k) matrix with categorical outcomes after categorizing.
}
\description{
This function allows to split continuous values, e.g. (risk) scores or (bio)markers,
into two or more categories by specifying one or more cutoff values.
}
\examples{
set.seed(123)
M <- as.data.frame(mvtnorm::rmvnorm(20, mean = rep(0, 3), sigma = 2 * diag(3)))
M
categorize(M)
C <- matrix(rep(c(-1, 0, 1, -2, 0, 2), 3), ncol = 3, byrow = TRUE)
C
w <- c(1, 1, 2, 2, 3, 3)
categorize(M, C, w)
}
