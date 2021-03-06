% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{set_session_seed}
\alias{set_session_seed}
\alias{get_session_seed}
\title{Set/get the "master" seed for the stat305templates package.}
\usage{
set_session_seed(seed, session)

get_session_seed(session)
}
\arguments{
\item{seed}{seed.}

\item{session}{the shiny reactive domain.}
}
\value{
\code{\link[=get_session_seed]{get_session_seed()}} returns the current session seed or \code{NULL} if none was set.
}
\description{
All subsequent randomizations by the \code{stat305templates} package are based on this master seed.
}
