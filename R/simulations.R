#' Simulation example 1
#'
#' Simulation example in Section 4.1 of the article.
#'
#' @param n sample size (single integer).
#' @param mu parameter mu (single number in (0,1), see Details).
#' @param type scoring function. Currently available are \code{"brier"} for the
#'   Brier score, \code{"log"} for the logarithmic score, \code{"spherical"}
#'   for the spherical score.
#'
#' @details
#' Returns two simulated competing forecasts \code{p}, \code{q} and observations
#' \code{y}. The forecasts are random numbers in (0,1). If \code{type} is
#' \code{"brier"}, then the \code{y} take value 1 with probability
#' \code{mu*q+(1-mu)*p}. For \code{type} equal to \code{"log"} or
#' \code{"spherical"}, \code{\link{boundary}} is called to find the number
#' \code{r} such that \code{p} and \code{q} are equally good when the true
#' probability is \code{r}. The probability that \code{y} equals 1 is then
#' \code{r+(2mu-1)(q-r)} for \code{mu > 0.5} or \code{r+(1-2mu)(p-r)}
#' if \code{mu <= 0.5}. The forecast \code{p} is better than \code{q} if and
#' only if \code{mu <= 0.5}.
#'
#' @return
#' A list of forecasts \code{p}, \code{q} and observations \code{y}, with
#' attribute \code{type}.
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
#'
#' @importFrom stats rbinom runif
sim1 <- function(n, mu, type) {
 p <- stats::runif(n)
 q <- stats::runif(n)
 r <- boundary(p, q, type)
 pi_t <- r + 2 * (pmax(mu - 0.5, 0) * (q - r) + pmax(0.5 - mu, 0) * (p - r))
 y <- stats::rbinom(n, 1, pi_t)

 list(p = p, q = q, y = y)
}

#' Simulation example 2
#'
#' Simulation example in Section 4.2 of the article.
#'
#' @param n sample size (single integer).
#' @param theta parameter theta (single number in (0,1)).
#' @param h maximal forecast lag (single integer).
#'
#' @details
#' See Section 4.2 in "" for a description of the output.
#'
#' @return
#' A list containing observations (\code{y}) and lagged forecasts (\code{pi_j}).
#'
#' @references
#'
#' Preprint on e-values for forecast evaluation:
#'
#' A. Henzi and J. F Ziegel. Valid sequential inference on probability forecast
#' performance. arXiv preprint arXiv:2103.08402, 2021.
#'
#' @export
#'
#' @importFrom stats rnorm
#' @importFrom stats pnorm
sim2 <- function(n, theta, h) {
 noise <- stats::rnorm(n + h)
 y <- noise[-seq_len(h)]
 for (j in seq_len(h)) {
  y <- y + theta * noise[(h - j + 1):(n + h - j)]
 }
 y <- as.integer(y > 0)
 out <- list(y = y)
 sds <- sqrt(cumsum(c(1, rep(theta, h)^2)))
 means <- numeric(n)
 for (j in seq_len(h)) {
  ind <- (h - j + 1):(n + h - j)
  means <- means + theta * noise[ind]
  out[[paste0("p_", h - j + 1)]] <- 1 -
   stats::pnorm(0, mean = means, sd = sds[h - j + 1])
 }
 out
}
