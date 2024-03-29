% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/module-losReport.R
\name{module-losReport}
\alias{module-losReport}
\alias{losReportUI}
\alias{losReport}
\alias{losReportStatic}
\title{los module}
\usage{
losReportUI(id)

losReport(id, data, what)

losReportStatic(data, completed, what)
}
\arguments{
\item{id}{name for the specific instance of the module.}

\item{data}{database to use}

\item{completed}{(chr) "Completed" or "Not-completed"}

\item{type}{(chr) "Total" or "Proportion"}
}
\description{
General description
}
\section{Functions}{
\itemize{
\item \code{losReportUI()}: user interface

\item \code{losReport()}: server function

\item \code{losReportStatic()}: static report function

}}
