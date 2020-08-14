% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lab_format.R
\name{submit_lab_btn}
\alias{submit_lab_btn}
\title{Add a button to submit the lab.}
\usage{
submit_lab_btn(label = "Download answers", filename = NULL)
}
\arguments{
\item{label}{button label}

\item{filename}{name of the file offered to the user for download. Defaults to the title of the Rmd document.}
}
\description{
A name input needs to be placed \emph{before} the submit button using \code{\link[=lab_name_input]{lab_name_input()}}.
}