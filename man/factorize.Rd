% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/factorize.R
\name{factorize}
\alias{factorize}
\title{Factorize selection of variables}
\usage{
factorize(x, lev)
}
\arguments{
\item{x}{(data frame)}

\item{lev}{(named list) of levels. Names must be all included in
variables' names of \code{x}}
}
\value{
a new version of x, of the same class, with the required
variables mutated to factors
}
\description{
Given a data frame and a list of named factors' levels, \link{factorize}
mutate in factor the variables of the data frame corresponding to
the names of the list provided, considering the corresponding
levels/
}
\examples{
library(tibble)

df <- tibble(
  foo = c("a", "a", "b"),
  bar = 1:3,
  baz = c("one", "two", "three"),
  qux = c("random", "sample", "text")
)

fct_levels <- list(
  foo = c("a", "b", "c", "d"),
  baz = c("one", "two", "three")
)

factorize(df, fct_levels)
}
