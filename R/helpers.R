#' Check observation input
#'
#' @param y observations (should be vector of 0 and/or 1).
#'
#' @keywords internal
check_y <- function(y) {
 if (!is.vector(y, "numeric") || !all(y %in% c(0, 1) | is.na(y)))
  stop("'y' must be a vector containing only 0 and/or 1")
}

#' Check condition input
#'
#' @param c conditioning variable (should be vector of 0 and/or 1).
#'
#' @keywords internal
check_c <- function(c) {
 if (!is.vector(c, "numeric") || !all(c %in% c(0, 1) | is.na(c)))
  stop("'c' must be a vector containing only 0 and/or 1")
}

#' Check probability input
#'
#' @param prob probabilities (should be vector numbers in (0,1)).
#'
#' @keywords internal
check_prob <- function(prob) {
 if (!is.vector(prob, "numeric") || !all((prob >= 0 & prob <= 1) | is.na(prob)))
  stop("probability forecasts must be numbers in [0,1]")
}

#' Check lag input
#'
#' @param h forecast lag (should be positive integer).
#'
#' @keywords internal
check_h <- function(h) {
 if (!is.vector(h, "numeric") || length(h) != 1 || as.integer(h) != h || h < 1)
  stop("'h' must be a positive integer")
}

#' Check signigicance level input
#'
#' @param alpha significance level (should be single number in (0,1)).
#'
#' @keywords internal
check_alpha <- function(alpha) {
 if (!is.vector(alpha, "numeric") || length(alpha) != 1 || alpha < 0 || alpha > 1)
  stop("'alpha' must be a number in (0,1)")
}

#' Check input for stopped argument
#'
#' @param x whether e-value is stopped (should a single boolean).
#'
#' @keywords internal
check_stopped <- function(x) {
 if (!isTRUE(x) && !isFALSE(x))
  stop("'stopped' must be TRUE or FALSE")
}

#' Check equal vector lengths input
#'
#' @param ... vectors (should all have the same length).
#'
#' @keywords internal
check_lengths <- function(...) {
 vecs <- list(...)
 if (length(unique(lengths(vecs))) > 1)
  stop("lengths of input vectors must be equal")
}
