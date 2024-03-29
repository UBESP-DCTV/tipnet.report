% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/module-admissionReport.R
\name{module-admissionReport}
\alias{module-admissionReport}
\alias{admissionReportUI}
\alias{admissionReport}
\alias{admissionReportStatic}
\title{Admission module}
\usage{
admissionReportUI(id)

admissionReport(id, data, type, what = NULL, dict = NULL)

admissionReportStatic(data, completed, what)
}
\arguments{
\item{id}{name for the specific instance of the module.}

\item{data}{database to use}

\item{type}{(chr) "Total" or "Proportion"}

\item{completed}{(chr) "Completed" or "Not-completed"}
}
\description{
General description
}
\section{Functions}{
\itemize{
\item \code{admissionReportUI()}: user interface

\item \code{admissionReport()}: server function

\item \code{admissionReportStatic()}: static report function

}}
