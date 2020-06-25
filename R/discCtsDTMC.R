
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
#' @param min_atom a numeric scalar on the unit interval which is the minimum
#' propability density to detect a discontinuity in the CDF of x.
#' @examples
#' find_atoms(x = c(rep(3,10), seq(1, 100), rep(26, 10)))
#' find_atoms(x = c(rep(3,10), seq(1, 100), rep(26, 3)))
#' find_atoms(x = c(rep(3,10), seq(1, 100), rep(26, 3)), min_atom = 0.025)
#' @return a numeric vector of points in the sample space at which there are
#' discontinuities in the distribution function, which are sometimes referred to
#' as "atoms".
#'
find_atoms <- function(x, min_atom = 0.05) {

  x_tab <- table(x)

  atoms <- as.numeric(names(x_tab)[x_tab/length(x) >= min_atom])

  return(atoms)
}


#' Divide a Continuous State Space into Intervals
#'
#' \code{state_breaks} calculates a vector of thresholds for allocating
#' elements of a continuous state space to a discrete state space
#'
#' @param x a numeric vector of observations from a continuous random
#' variable with discontinuities in the distribution function.
#' @param num_breaks is an integer that captures the number of break points
#' to use when dividing the support of \code{x} into intervals.
#' @examples
#' state_breaks(x = seq(-5,10), num_breaks = 4)
#'
#' @return a numeric vector of thresholds for allocating elements of the
#' continuous state space to a discrete state space.
#'
state_breaks <- function(x, num_breaks) {

  # Just like for histograms, the simplest is to set evenly spaced breaks.
  # breaks <- seq(min(x), max(x), length.out = num_breaks)
  # This leaves endpoints empty.

  # Set step size.
  step <- (max(x) - min(x))/(num_breaks + 1)
  breaks <- seq(min(x) + 0.5*step, max(x) - 0.5*step, by = step)


  # Later version should account for atoms.

  # And should divide into intervals of increasing width
  # for areas of the support more sparsely populated.
  # Strike a balance between using quantiles and using evenly-spaced intervals.
  # What about round numbers?

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
#' @param labels a character vector of labes for the discrete states.
#'
#' @return a categorical variable with states for each variable that
#' correspond to elements in the continuous state space.
#'
cut_states <- function(x_cts, breaks, labels) {

  if (is.null(labels)) {
    labels <- seq(length(breaks) + 1)
  }

  x_disc <- cut(x_cts,
                breaks = c(-1, breaks, Inf),
                labels = labels,
                # include.lowest = TRUE,
                # include.lowest = FALSE
                )
  # Later: Add a ... for passing arguments to cut().

  return(x_disc)
}


#' Reshape Data for Estimation of Transition Matrices.
#'
#' \code{reshape_markov_data} reshapes data from a discrete-state,
#' discrete-time Markov process and organizes it into a form suitable to
#' estimate transition matrices .
#'
#' @param dt a \code{data.table} object with specific columns
#' \code{x_disc}, \code{id} and \code{time}.
#' It must be sorted by \code{id} then \code{time} to ensure the
#' consecutive listing of observations for each individual.
#' @param x_disc a numeric vector of observations from a discrete random variable.
#' If x is a matrix, it is assumed that the first two columns are the id and the time stamp.
#' @param time a vector of time stamps that correspond to the observations in \code{x}.
#' @param id a vector of labels to identify different individuals in the population.
#' @param time_unit a string to identify the unit of time between consecutive
#' observations. Can be "month" or "integer".
#' @param n_lags an integer number of lags to define the order of the Markov process.
#' for each column of the transition matrices (default is \code{FALSE}).
#'
#' @return x_mat a matrix of data for estimation of transition matrices.
#' It will have 4 columns: the id, the time stamp, the observations and a boolean
#' variable to indicate valid observations for estimation.
#' It will be reordered, first by id, then time.
#'
#'
# reshape_markov_data <- function(x, time, time_unit, id, n_lags) {
reshape_markov_data <- function(dt, time_unit, n_lags) {

  # if (ncol(x) == 1) {
  #   x_mat <- cbind(id, time, x)
  # } else if (ncol(x) == 3) {
  #   x_mat <- x
  # }
  # x_mat[, 'sel_obsn'] <- TRUE
  # colnames(x_mat) <- c('id', 'time', 'x', 'sel_obsn')

  # First reorder the data.
  # x_mat <- x_mat[order(id, time)]
  # Should already be ordered.
  if (is.unsorted(dt[, id])) {
    stop('Observations are not sorted. Sort data.table by id and time. ')
  }

  # Lag the data.
  if (n_lags > 1) {
    stop('Higher lag orders not yet implemented. Try again later.')
  }
  # # Lag the data.
  # x_mat[, x_disc_lag := shift(x_disc)]
  # x_mat[, id_lag := shift(id)]
  # x_mat[, time_lag := shift(time)]
  # Lag the data.
  dt[, x_disc_lag := shift(x_disc)]
  dt[, id_lag := shift(id)]
  dt[, time_lag := shift(time)]

  # Delete Lags from first period for each individual
  # x_mat[!(id == id_lag), x_lag := NA]

  # Include only consecutive observations.
  # This allows for the possibiliy that some observations might be skipped.
  if (time_unit == 'month') {
    # x_mat[, one_lag_only := as.numeric(time - time_lag) < 32 &
    #         not_first_obsn]
    dt[, one_lag_only := as.numeric(time - time_lag) < 32]
  } else if (time_unit == 'integer') {
    # x_mat[, one_lag_only := time_lag == time - 1]
    dt[, one_lag_only := time_lag == time - 1]
  } else {
    stop('Time unit not a compatible format.')
  }

  # Complete the selection of the data.
  # x_mat[, sel_obsn := one_lag_only & (id == id_lag)]
  dt[, valid_obsn := one_lag_only & (id == id_lag) &
       !is.na(id_lag) & !is.na(x_disc) & !is.na(x_disc_lag)]



  # return(x_mat[, c('id', 'time', 'x_disc', 'sel_obsn')])
  return('Mission accomplished!')
}


#' Estimate Transition Matrices
#'
#' \code{est_trans_mat_single} estimates a single transition matrix from a
#' discrete-state, discrete-time Markov process.
#'
#' @param dt a \code{data.table} object with specific columns
#' \code{x_disc}, \code{id}, \code{time} and \code{sel_obsns_trans}.
#' It must be sorted by \code{id} then \code{time} to ensure the
#' consecutive listing of observations for each individual.
#' @param x_disc a numeric vector of observations from a discrete random variable.
#' If x is a matrix, it is assumed that the first two columns are the id and the time stamp.
#' @param time a vector of time stamps that correspond to the observations in \code{x}.
#' @param id a vector of labels to identify different individuals in the population.
#' @return A numeric matrix of transition prbabilities in the columns.
est_trans_mat_single <- function(dt = tu) {

  if (!('sel_obsns_trans' %in% colnames(dt))) {
    warning('No selection indicator supplied. Using all observations. ')
    dt[, sel_obsns_trans == TRUE]
  }

  trans_freq <- table(tu[sel_obsns_trans == TRUE, c('x_disc', 'x_disc_lag')])
  trans_mat <- prop.table(trans_freq, 2)

  return(trans_mat)
}


#' Estimate Transition Matrices
#'
#' \code{est_trans_mats} estimates transition matrices from a discrete-state,
#' discrete-time Markov process.
#'
#' @param x a numeric vector of observations from a discrete random variable.
#' @param time a vector of time stamps that correspond to the observations in \code{x}.
#' @param id a vector of labels to identify different individuals in the population.
#' @param n_lags an integer number of lags to define the order of the Markov process.
#' @param Hessian an indicator to specify whether the Hessian matrices are returned
#' for each column of the transition matrices (default is \code{FALSE}).
#'
#' @return trans_mats an array of transition matrices for the Markov process.
#'
est_trans_mats <- function(x, time, id, n_lags, Hessian = FALSE) {

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
#' @param time a vector of time stamps that correspond to the observations in \code{x}.
#' @param id a vector of labels to identify different individuals in the population.
#' @param out_probs a \code{n_ahead}-row numeric matrix probability vector that
#' defines the forecasted proportions of the population in each state.
#'
#' @return a data frame of Kullback-Leibler divergence statistics
#' and p-values from the quantiles of the chi-squared distribution.
#'
test_fore_dev <- function(x, time, id, out_probs) {

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

