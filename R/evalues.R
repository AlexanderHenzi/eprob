#' Consistent scoring functions for probabilities
#'
#' Computes consistent scores for forecast probabilities \code{p} and
#' observations \code{y}.
#'
#' @param y observations (0 or 1).
#' @param p forecast probabilities (numbers in [0,1]).
#' @param type scoring function to be applied. Currently available are
#'   \code{"brier"} for the Brier score, \code{"log"} for the logarithmic score
#'   and \code{"spherical"} for the spherical score.
#'
#' @details
#' Smaller scores indicate better forecasts.
#'
#' @return
#' Vector of score values.
#'
#' @references
#'
#' Preprint on e-values for forecast evaluation:
#'
#' A. Henzi and J. F Ziegel. Valid sequential inference on probability forecast
#' performance. arXiv preprint arXiv:2103.08402, 2021.
#'
#' Proper scoring rules for probabilities:
#'
#' Gneiting, Tilmann, and Adrian E. Raftery. "Strictly proper scoring rules,
#' prediction, and estimation." Journal of the American statistical Association
#' 102.477 (2007): 359-378.
#'
#' @export
score <- function(y, p, type) {
 check_y(y)
 check_prob(p)
 check_lengths(p, y)
 if (identical(type, "brier")) {
  (y - p)^2
 } else if (identical(type, "log")) {
  -log(abs(1 - p - y))
 } else if (identical(type, "spherical")) {
  1 - abs(1 - y - p) / sqrt(2 * p^2 - 2 * p + 1)
 } else {
  stop("type must be 'brier', 'log', or 'spherical'")
 }
}

#' Boundary of null hypothesis of forecast dominance
#'
#' For two competing probability forecasts \code{p} and \code{q}, this function
#' computes the event probability under which \code{p} and \code{q} are
#' equally good with respect to a given scoring function.
#'
#' @param p forecast probabilities (numbers in [0,1]). The null hypothesis is
#'    that \code{p} is a better forecast than \code{q}.
#' @param q forecast probabilities (numbers in [0,1]).
#' @param type scoring function. Currently available are \code{"brier"} for the
#'   Brier score, \code{"log"} for the logarithmic score, \code{"spherical"}
#'   for the spherical score.
#'
#' @return
#' Vector of probabilities.
#'
#' @references
#'
#' Preprint on e-values for forecast evaluation:
#'
#' A. Henzi and J. F Ziegel. Valid sequential inference on probability forecast
#' performance. arXiv preprint arXiv:2103.08402, 2021.
#'
#' Proper scoring rules for probabilities:
#'
#' Gneiting, Tilmann, and Adrian E. Raftery. "Strictly proper scoring rules,
#' prediction, and estimation." Journal of the American statistical Association
#' 102.477 (2007): 359-378.
#'
#' @export
boundary <- function(p, q, type) {
 check_prob(p)
 check_prob(q)
 check_lengths(p, q)
 m <- pmin(p, q)
 M <- pmax(p, q)
 if (identical(type, "brier")) {
  out <- (m + M) / 2
 } else if (identical(type, "log")) {
  out <- (log(1 - m) - log(1 - M)) / (log(M) - log(1 - M) - log(m) + log(1 - m))
 } else if (identical(type, "spherical")) {
  nm <- sqrt(2 * m^2 - 2 * m + 1)
  nM <- sqrt(2 * M^2 - 2 * M + 1)
  out <- ((M - 1) * nm - (m - 1) * nM) / ((2 * M - 1) * nm - (2 * m - 1) * nM)
 } else if (identical(type, "dominance")) {
   out <- p
 } else {
   stop("type must be 'brier', 'log', 'spherical', or 'dominance'")
 }
 out[is.nan(out)] <- p[is.nan(out)]
 out
}

#' E-values for testing forecast dominance
#'
#' For two competing probability forecasts \code{p} and \code{q} and an
#' alternative probability \code{alt}, this function computes for every time
#' point the e-value with the null hypothesis that \code{p} is better than
#' \code{q} and maximal power against the alternative probabilities \code{alt},
#' under a given scoring function.
#'
#' @param y observations (0 or 1).
#' @param p forecast probabilities (numbers in [0,1]). The null hypothesis is
#'    that \code{p} is a better forecast than \code{q}.
#' @param q forecast probabilities (numbers in [0,1]).
#' @param alt event probabilities under alternative hypothesis
#'     (numbers in [0,1]).
#' @param c conditioning variable (0 or 1). E-values with \code{c = 0} will be
#'    ignored (set to 1), and only e-values with \code{c = 1} are computed.
#' @param type scoring function. Currently available are \code{"brier"} for the
#'   Brier score, \code{"log"} for the logarithmic score, \code{"spherical"}
#'   for the spherical score, and \code{"dominance"} for forecast dominance
#'   with respect to all scoring rules.
#'
#' @details
#' For testing forecast dominance (\code{type} equal to \code{"dominance"}),
#' rejecting the null hypothesis either means that \code{q} dominates \code{p}
#' with respect to all scoring rules, or that neither \code{p} dominates
#' \code{q} nor \code{q} dominates \code{p}.
#'
#' @return
#' Vector of e-values. Large e-values (greater than 1) indicate evidence
#' against the null hypothesis that \code{p} is a better forecast than \code{q}.
#'
#' @references
#'
#' Preprint on e-values for forecast evaluation:
#'
#' A. Henzi and J. F Ziegel. Valid sequential inference on probability forecast
#' performance. arXiv preprint arXiv:2103.08402, 2021.
#'
#' Proper scoring rules for probabilities:
#'
#' Gneiting, Tilmann, and Adrian E. Raftery. "Strictly proper scoring rules,
#' prediction, and estimation." Journal of the American statistical Association
#' 102.477 (2007): 359-378.
#'
#' @export
evalue <- function(y, p, q, alt, c = rep(1, length(y)), type) {
 check_prob(alt)
 check_c(c)
 check_lengths(p, y, alt, c)
 out <- (1 - y - alt) / (1 - y - boundary(p, q, type))
 out[c == 0] <- 1
 out
}


#' Hypothesis testing with e-values for sequential forecast dominance
#'
#' For two competing probability forecasts \code{p} and \code{q} and alternative
#' probabilities \code{alt}, this function computes the e-value with the null
#' hypothesis that \code{p} is better than \code{q} and maximal power against
#' the alternative probability \code{alt}, under a given scoring function.
#'
#' @inheritParams evalue
#' @param h forecast lag. For example, daily forecasts for the next day have lag
#'   1, daily forecasts for an event two days ahead have lag 2.
#' @param stopped compute stopped e-values for the given level \code{alpha}?
#' @param alpha significance level (number in (0,1)). Has no effect if
#'   \code{stopped} is \code{FALSE}.
#'
#' @details
#' There is no default value for the alternative \code{alt}. The alternative
#' must be greater (less) than \code{\link{boundary}(p, q, type)} if
#' \code{q > p} (if \code{q < p}).
#'
#' For testing forecast dominance (\code{type} equal to \code{"dominance"}),
#' rejecting the null hypothesis either means that \code{q} dominates \code{p}
#' with respect to all scoring rules, or that neither \code{p} dominates
#' \code{q} nor \code{q} dominates \code{p}.
#'
#' @return
#' A list containing
#'
#' \item{\code{evalue}}{e-value for the hypothesis test.}
#' \item{\code{pvalue}}{the corresponding p-value. If \code{stopped = FALSE},
#'   this is the anytime valid p-value, otherwise it is \code{1/evalue} up to a
#'   correction factor.}
#' \item{\code{alpha}}{significance level.}
#' \item{\code{h}}{forecast lag.}
#' \item{\code{stopped}}{whether the e-value is computed with optional stopping
#'  or not.}
#' \item{\code{stop_ind}}{the index at which the stopped e-value is reached.
#'   Equals the total sample size \code{n} if \code{stopped} is \code{FALSE}.}
#' \item{\code{n}}{total sample size (\code{length(y))}.}
#' \item{\code{alpha}}{significance level.}
#'
#' @note
#' Large e-values (greater than 1) give evidence against the null hypothesis and
#' indicate that \code{q} is a better forecast than \code{p}.
#'
#' If the data (\code{p}, \code{q} or \code{y}) contains \code{NA}, these must
#' not be removed! It is assumed that the time lag is always \code{h} between
#' one entry of the data and the next, for example between \code{p[i]} and
#' \code{p[i + 1]}.
#'
#' @references
#'
#' Preprint on e-values for forecast evaluation:
#'
#' A. Henzi and J. F Ziegel. Valid sequential inference on probability forecast
#' performance. arXiv preprint arXiv:2103.08402, 2021.
#'
#' Proper scoring rules for probabilities:
#'
#' Gneiting, Tilmann, and Adrian E. Raftery. "Strictly proper scoring rules,
#' prediction, and estimation." Journal of the American statistical Association
#' 102.477 (2007): 359-378.
#'
#' @export
etest <- function(p, q, y, alt, c = rep(1, length(y)), type, h, stopped = FALSE,
  alpha = NULL) {
 check_h(h)
 check_stopped(stopped)
 if (stopped) check_alpha(alpha)

 e <- evalue(y = y, p = p, q = q, c = c, alt = alt, type = type)
 e[is.na(e) | is.nan(e)] <- 1
 n <- length(e)
 stop_ind <- n

 if (h == 1) {
  e <- cumprod(e)
  if (stopped) {
   stop_ind <- which(e >= 1 / alpha)
   if (length(stop_ind) == 0) stop_ind <- n
   stop_ind <- min(stop_ind)
   statistic <- e[stop_ind]
   pval <- min(1, 1 / statistic)
  } else {
   pval <- min(1, 1 / e)
   statistic <- e[n]
  }
 } else {
  correction <- pmax(
    1,
    1/evalue(y = as.integer(p > q), p = p, q = q, c = c, alt = alt, type = type)
  )
  correction[is.na(correction) | is.nan(correction)] <- 1
  correction <- c(correction[-1], 1)
  for (j in seq_len(h - 2)) {
    correction <- pmax(
      correction,
      c(correction[-seq_len(j)], rep(1, j))
    )
  }
  ik <- rep(seq_len(h), ceiling(n / h))[seq_len(n)]
  f <- numeric(n)
  splitted <- split(e, ik)
  splitted <- lapply(splitted, function(x) diff(c(0, cumprod(x))))
  for (j in seq_len(h)) f[ik == j] <- splitted[[j]]
  f <- cumsum(f) / h
  if (stopped) {
   stop_ind <- which.max(f >= correction / alpha)
   if (stop_ind > n - h || f[stop_ind] < correction[stop_ind] / alpha) {
    stop_ind <- n
   }
   statistic <- f[stop_ind]
   pval <- min(1, correction[stop_ind] / statistic)
  } else {
    statistic <- f[n]
    pval <- min(1, 1 / f)
  }
 }
 list(
  evalue = statistic,
  pvalue = pval,
  alpha = alpha,
  h = h,
  stopped = stopped,
  stop_ind = stop_ind,
  n = n
 )
}
