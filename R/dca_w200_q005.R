#' Rolling R² Connectedness (window = 200, q = 0.05)
#'
#' Pre-computed rolling connectedness results for the \code{renewable_residuals} dataset,
#' using a 200-day window, \code{nlag = 1}, and quantile level \code{q = 0.05}.
#'
#' @format A list with components matching the return of \code{R2ConnectednessQ()}, e.g.:
#' \describe{
#'   \item{CT}{Array of connectedness measures per window.}
#'   \item{TO, FROM, NET}{Matrices or arrays by window.}
#'   \item{TCI}{\code{t0 × k} (or \code{t0 × 3} if \code{nlag = 1}) matrix of total connectedness indices.}
#'   \item{TABLE}{Connectedness tables (Overall / Contemporaneous / Lagged).}
#'   \item{config}{List of configuration parameters used.}
#' }
#'
#' @details
#' Computation call:
#' \preformatted{
#' dca_w200_q005 <- R2ConnectednessQ(
#'   renewable_residuals,
#'   window.size = 200,
#'   nlag = 1,
#'   quantile = TRUE,
#'   q = 0.05,
#'   method = "pearson",
#'   shrink = TRUE,
#'   progbar = FALSE
#' )
#' }
#'
#' @examples
#' \dontrun{
#' data(dca_w200_q005)
#' head(dca_w200_q005$TCI)
#' }
"dca_w200_q005"
