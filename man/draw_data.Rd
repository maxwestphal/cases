% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/draw_data_prb.R
\name{draw_data}
\alias{draw_data}
\title{Generate binary data}
\usage{
draw_data(
  n = 200,
  prev = c(0.5, 0.5),
  random = FALSE,
  m = 10,
  method = c("roc", "lfc", "pr"),
  pars = list(),
  ...
)
}
\arguments{
\item{n}{(numeric) \cr integer, overall sample size}

\item{prev}{(numeric) \cr vector of class prevalences (adding up to 1)}

\item{random}{(logical) \cr random sampling (TRUE) or fixed group sample sizes (FALSE)}

\item{m}{(numeric) \cr integer, number of models}

\item{method}{(character) \cr either "roc", "lfc" (multiple subgroups) or "prob" (no subgroups)}

\item{pars}{(list) \cr containing further named parameters passed to \code{\link{draw_data_roc}},
\code{\link{draw_data_lfc}}}

\item{...}{(any) \cr further named parameters passed}
}
\value{
(matrix) \cr generated binary data (possibly stratified for subgroups)
}
\description{
Generate binary data
}
\examples{
draw_data()
}
