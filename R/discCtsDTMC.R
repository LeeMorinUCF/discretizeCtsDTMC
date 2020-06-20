
#' Discretize a Continuous-state Discrete-time Markov Chain
#'
#' \code{discretizeCtsDTMC} creates a discrete approximation to a Markov
#' process defined on a continuous state space in discrete time.
#' Once the state space is discretized, \code{discretizeCtsDTMC} provides tools
#' to estimate the transition matrices and analyze the Markov process.
#'
#' @note We'll think of a better name later.
#'
#' @seealso \code{multinom} function in \code{nnet} package for estimating the
#' transition matrices.
#' \code{DTMCPack} and \code{markovchain} for analyzing the discrete-time Markov
#' model once the continuous state space is discretized.
#'
#' @docType package
#' @name aaa-discretizeCtsDTMC
NULL


#' Find Points of Discontinuity in the CDF
#'
#' \code{find_atoms} finds points of discontinuity in the CDF of a
#' continuous random variable.
#'
#' @param x a numeric vector of observations from a continuous random
#' variable with discontinuities in the distribution function.
#'
#' @return a numeric vector of points in the sample space at which there are
#' discontinuities in the distribution function, which are sometimes referred to
#' as "atoms".
#'
find_atoms <- function(x) {

  atoms <- NULL

  return(atoms)
}


#' Divide a Continuous State Space into Intervals
#'
#' \code{state_breaks} calculates a vector of thresholds for allocating
#' elements of a continuous state space to a discrete state space
#'
#' @param x a numeric vector of observations from a continuous random
#' variable with discontinuities in the distribution function.
#'
#' @return a numeric vector of thresholds for allocating elements of the
#' continuous state space to a discrete state space.
#'
state_breaks <- function(x) {

  breaks <- NULL

  return(breaks)
}


#' Discretize a Variable Defined on a Continuous State Space
#'
#' \code{cut_states} transforms a variable defined on a continuous state space
#' into a discrete variable.
#'
#' @param x_cts a numeric vector of observations from a continuous random
#' variable with discontinuities in the distribution function.
#' @param breaks a numeric vector of thresholds for allocating elements of the
#' continuous state space to discrete state space.
#'
#' @return a categorical variable with states for each variable that
#' correspond to elements in the continuous state space.
#'
cut_states <- function(x_cts, breaks) {

  x_disc <- NULL

  return(x_disc)
}


#' Estimate Transition Matrices
#'
#' \code{est_trans_mats} estimates transition matrices from a discrete-state,
#' discrete-time Markov process.
#'
#' @param x a numeric vector of observations from a discrete random variable.
#' @param time_stamp a vector of time stamps that correspond to the observations in \code{x}.
#' @param id a vector of labels to identify different individuals in the population.
#' @param n_lags an integer number of lags to define the order of the Markov process.
#' @param Hessian an indicator to specify whether the Hessian matrices are returned
#' for each column of the transition matrices (default is \code{FALSE}).
#'
#' @return trans_mats an array of transition matrices for the Markov process.
#'
est_trans_mats <- function(x, time_stamp, id, n_lags, Hessian = FALSE) {

  fmla <- NULL
  fn_of_x_etc <- NULL

  multinom_out <- nnet::multinom(formula = fmla, data = fn_of_x_etc)
  trans_mats <- multinom_out$whatever

  return(trans_mats)
}


#' Forecast a Probability Distribution
#'
#'\code{forecast_distn} calculates a forecast of a probability distribution
#'for a population governed by a discrete-state, discrete-time Markov process.
#'
#' @param trans_mats an array of transition matrices for the Markov process.
#' @param init_probs a numeric probability vector that defines the initial
#' proportions of the population in each state.
#' @param n_ahead an integer number of lags that defines the order of the
#' Markov process.
#'
#' @return a \code{n_ahead}-row numeric matrix of probability vectors that
#' define the forecasted proportion of the population in each state at each time.
#'
forecast_distn <- function(trans_mats, init_probs, n_ahead) {

  markov_thingy <- markovchain::conditionalDistribution(mcWeather, "sunny")
  Other_markov_thingy <- DTMCPack::MultDTMC(nchains, tmat, io, n)

  out_probs <- NULL

  return(out_probs)
}


#' Test for a Deviation from Forecasted Population
#'
#'\code{test_fore_dev} tests for a deviation from a forecasted population
#' by calculating the Kullback-Leibler divergence statistic
#' and p-values from the quantiles of the chi-squared distribution.
#'
#' @param x a numeric vector of observations of a discrete random variable
#' from the out-of-sample forecast period.
#' @param time_stamp a vector of time stamps that correspond to the observations in \code{x}.
#' @param id a vector of labels to identify different individuals in the population.
#' @param out_probs a \code{n_ahead}-row numeric matrix probability vector that
#' defines the forecasted proportions of the population in each state.
#'
#' @return a data frame of Kullback-Leibler divergence statistic
#' and p-values from the quantiles of the chi-squared distribution.
#'
test_fore_dev <- function(x, time_stamp, id, out_probs) {

  out <- NULL

  return(out)
}


#' Simulate a Population of Individuals following a Markov Chain
#'
#' \code{sim_pop_MC} draws a realization of a population of individuals,
#' each following a discrete-time Markov process. It can be used for a
#' parametric bootstrap procedure when the large sample properties may not hold.
#'
#' @param trans_mats an array of transition matrices for the Markov process.
#' @param init_probs a numeric probability vector that defines the initial
#' proportions of the population in each state.
#' @param n_ahead an integer number of lags that defines the order of the
#' Markov process.
#' @param n_ind an integer number of individuals in the cross-section.
#'
#' @return a data frame of realizations for individuals in the cross-section,
#' including id labels for individuals and time stamps for each time period.
#'
sim_pop_MC <- function(trans_mats, init_probs, n_ahead, n_ind) {

  out <- NULL

  return(out)
}




#' #' Function Template
#' #'
#' #'\code{function_name} does something.
#' #'
#' #' @param
#' #' @param
#' #' @param
#' #'
#' #' @return
#' #'
#' function_name <- function(x) {
#'
#'   out <- NULL
#'
#'   return(out)
#' }

