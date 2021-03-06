% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/psql_store.R
\name{setup_psql_event_store}
\alias{setup_psql_event_store}
\title{Record \code{learnr} User-Events in PostgreSQL}
\usage{
setup_psql_event_store(
  what = c("question_submission", "exercise_submission", "exercise_error"),
  db_table = "events",
  db_user = NULL,
  db_pass = NULL,
  db_name = NULL,
  db_host = NULL,
  db_port = NULL,
  ignore_errors = FALSE
)
}
\arguments{
\item{what}{What user-events are to be stored? See https://rstudio.github.io/learnr/publishing.html#recording_events
for available event names.}

\item{db_table}{Name of the events table.}

\item{db_user, db_pass}{User name and password. If \code{NULL}, will be retrieved from \code{PGUSER} and \code{PGPASSWORD} env vars.}

\item{db_name}{Database name. If \code{NULL}, will be retrieved from \code{PGDBNAME} or, if unset falls back to the user name.}

\item{db_host, db_port}{Hostname and port. If \code{NULL}, will be retrieved from \code{PGHOST} and \code{PGPORT} env vars.}

\item{ignore_errors}{if \code{TRUE}, ignore database connection errors.}
}
\description{
User-events are saved in a PostgreSQL table in the following format:
}
\details{
\describe{
\item{id}{primary key, generated by the database (defined as \emph{SERIAL PRIMARY KEY})}
\item{timestamp}{timestamp when the event was emitted (defined as \emph{TIMESTAMP DEFAULT CURRENT_TIMESTAMP})}
\item{tutorial_id}{string identifying the tutorial (defined as \emph{CHARACTER VARYING(255)})}
\item{tutorial_VERSION}{string identifying the version of the tutorial (defined as \emph{CHARACTER VARYING(255)})}
\item{user_id}{string identifying user (defined as \emph{CHARACTER VARYING(255)})}
\item{event}{string identifying the event (defined as \emph{CHARACTER VARYING(255)})}
\item{data}{event data, stored as JSON (defined as \emph{JSONB})}
}

The user id is determined from the current session.
See \link{resume_session} for details on how the user id is determined in the first place.
}
