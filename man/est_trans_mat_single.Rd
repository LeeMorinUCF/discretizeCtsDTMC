% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interCross.R
\name{est_trans_mat_single}
\alias{est_trans_mat_single}
\title{Estimate Transition Matrices}
\usage{
est_trans_mat_single(dt = tu)
}
\arguments{
\item{dt}{a \code{data.table} object with specific columns
\code{x_disc}, \code{id}, \code{time} and \code{sel_obsns_trans}.
It must be sorted by \code{id} then \code{time} to ensure the
consecutive listing of observations for each individual.}

\item{x_disc}{a numeric vector of observations from a discrete random variable.
If x is a matrix, it is assumed that the first two columns are the id and the time stamp.}

\item{time}{a vector of time stamps that correspond to the observations in \code{x}.}

\item{id}{a vector of labels to identify different individuals in the population.}
}
\value{
A numeric matrix of transition prbabilities in the columns.
}
\description{
\code{est_trans_mat_single} estimates a single transition matrix from a
discrete-state, discrete-time Markov process.
}
