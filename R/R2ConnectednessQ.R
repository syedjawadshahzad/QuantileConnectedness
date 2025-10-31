#' R² Decomposed Connectedness via Quantile Correlation
#'
#' Extends the R² decomposed connectedness framework of Hoang and Baur (2022)
#' and Balli et al. (2023) by incorporating the **quantile correlation coefficient**
#' proposed by Choi and Shin (2022).  
#' The function quantifies total, directional, and net connectedness in a multivariate
#' system across quantiles, enabling tail-specific connectedness analysis (e.g.,
#' lower- or upper-tail dependence).
#'
#' @param x A \code{zoo} object containing multivariate time series data
#'   (columns represent variables).
#' @param window.size Integer or \code{NULL}. Rolling window length for dynamic
#'   connectedness estimation. If \code{NULL}, the full sample is used.
#' @param nlag Integer (default \code{1}). Number of lags for the decomposition.
#' @param quantile Logical (default \code{TRUE}). If \code{TRUE}, use quantile
#'   correlations (\code{Qcor}); otherwise use standard correlations from \code{method}.
#' @param q Numeric in \code{[0,1]} (default \code{0.5}). Quantile level for the
#'   quantile correlation coefficient.
#' @param progbar Logical (default \code{TRUE}). Display a progress bar during
#'   rolling estimation.
#' @param method Character (default \code{"pearson"}). Correlation estimator when
#'   \code{quantile = FALSE}; options include \code{"pearson"}, \code{"kendall"},
#'   or \code{"spearman"}.
#' @param shrink Logical (default \code{TRUE}). Apply shrinkage to stabilize
#'   the correlation matrix using \code{\link[corpcor]{cor.shrink}}.
#' @param relative Logical (default \code{FALSE}). If \code{TRUE}, express
#'   connectedness measures as relative percentages.
#' @param corrected Logical (default \code{FALSE}). Apply small-sample bias
#'   correction to the total connectedness index (TCI).
#'
#' @details
#' This function generalizes the R² decomposed connectedness approach developed by
#' Hoang & Baur (2022) and Balli et al. (2023). Instead of traditional correlations,
#' it employs quantile correlations \insertCite{ChoiShin2022}{yourpkg} to measure
#' dependence between variables at specific quantiles.  
#' 
#' Internally, the function:
#' \enumerate{
#'   \item Computes (quantile) correlation matrices for each rolling window.
#'   \item Optionally applies shrinkage to improve matrix conditioning.
#'   \item Decomposes R² connectedness using the helper
#'         \code{ConnectednessTable()} from the \pkg{ConnectednessApproach} package.
#' }
#' 
#' Users must have the \pkg{ConnectednessApproach} package installed, as this function
#' relies on its core decomposition utilities (\code{ConnectednessTable()}).
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{CT}: 4-D array of component contributions (\eqn{k \times k \times T \times (1+nlag)}).
#'   \item \code{TO}, \code{FROM}, \code{NET}: directional connectedness measures.
#'   \item \code{TCI}: total connectedness index over time.
#'   \item \code{NPDC}: normalized pairwise directional connectedness.
#'   \item \code{TABLE}: summary tables (overall, contemporaneous, and/or lagged).
#'   \item \code{config}: list of configuration arguments used.
#' }
#'
#' @references
#' Hoang, T. H. V., & Baur, D. G. (2022).
#' *R² Connectedness: Measuring Integration and Contagion in Financial Markets*.
#' Journal of Financial Econometrics, 20(3), 520–544.
#'
#' Balli, F., Balli, H. O., Louis, R. J., & Pappas, V. (2023).
#' *Dynamic Connectedness Across Markets: Revisiting the R² Approach*.
#' Finance Research Letters, 56, 104263.
#'
#' Choi, S., & Shin, S. (2022).
#' *Quantile Correlation: Measuring Dependence at Different Quantiles*.
#' Journal of Econometrics, 226(1), 1–24. \doi{10.1016/j.jeconom.2021.08.001}
#'
#' @examples
#' \dontrun{
#' library(ConnectednessApproach)
#' library(zoo)
#' data(dy2012)  # example dataset from ConnectednessApproach
#' res <- R2ConnectednessQ(dy2012, window.size = 200, q = 0.1)
#' res$TCI
#' }
#'
#' @importFrom corpcor cor.shrink
#' @importFrom MASS ginv
#' @import zoo
#' @export

R2ConnectednessQ <- function (x, window.size = NULL, nlag = 1, quantile = TRUE, q = 0.5, progbar = TRUE,
                              method = "pearson", shrink = TRUE, relative = FALSE, corrected = FALSE) 
{
  if (!is(x, "zoo")) {
    stop("Data needs to be of type 'zoo'")
  }
  DATE = as.character(index(x))
  x = as.matrix(x)
  k = ncol(x)
  NAMES = colnames(x)
  Z = embed(x, nlag + 1)
  if (is.null(window.size)) {
    window.size = nrow(Z)
    t0 = 1
  }
  else {
    window.size = window.size - nlag
    t0 = nrow(Z) - window.size + 1
  }
  date = tail(DATE, t0)
  CT = array(0, c(k, k, t0, nlag + 1), dimnames = list(NAMES, 
                                                       NAMES, date, 0:nlag))
  if (progbar == TRUE) {
    pb = txtProgressBar(max = t0, style = 3)
    progress <- function(n) setTxtProgressBar(pb, n)
  }
  
  for (j in 1:t0) {
    
    # Pearson correlation 
    R = cor(Z[j:(j + window.size - 1), ], method = method)
    
    if (quantile==TRUE){
      R = Qcor(Z[j:(j + window.size - 1), ], q = q)  
    }
    
    if (shrink==TRUE){
      R <- as.matrix.data.frame(corpcor::cor.shrink(R, verbose=F))
    }
    
    
    for (i in 1:k) {
      ryx = R[-i, i, drop = F]
      rxx = R[-i, -i]
      eigcovx = eigen(rxx, TRUE)
      eigcovx$values <- round(eigcovx$values, 3)
      rootcovx = eigcovx$vectors %*% diag(sqrt(eigcovx$values)) %*% 
        t(eigcovx$vectors)
      #cd = rootcovx^2 %*% (solve(rootcovx) %*% ryx)^2
      cd = rootcovx^2 %*% (MASS::ginv(rootcovx) %*% ryx)^2
      CT[i, -i, j, 1] = cd[c(1:(k - 1))]
      if (nlag > 0) {
        CT[i, , j, 2] = apply(array(cd[-c(1:(k - 1))], 
                                    c(1, k, nlag)), 1:2, sum)
      }
    }
    if (progbar == TRUE) {
      #Sys.sleep(0.05)
      setTxtProgressBar(pb, j)
    }
  }
  kl = 1
  dimensions = "TCI"
  if (nlag > 0) {
    kl = 3
    dimensions = c("Overall", "Contemporaneous", "Lagged")
  }
  TCI = array(0, c(t0, kl), dimnames = list(date, dimensions))
  TO = FROM = NET = array(0, c(t0, k, kl), dimnames = list(date, 
                                                           NAMES, dimensions))
  NPDC = array(0, c(k, k, t0, kl), dimnames = list(NAMES, NAMES, 
                                                   date, dimensions))
  
  for (i in 1:t0) {
    
    if (nlag > 0) {
      ct = ConnectednessTable(CT[, , i, 1])
      lt = ConnectednessTable(CT[, , i, 2])
      at = ConnectednessTable(CT[, , i, 2] + CT[, , i, 
                                                1])
      TO[i, , 1] = at$TO
      FROM[i, , 1] = at$FROM
      NET[i, , 1] = at$NET
      NPDC[, , i, 1] = at$NPDC
      TCI[i, 1] = at$TCI * (1 + (corrected * (k/(k - 1) - 
                                                1)))
      TO[i, , 2] = ct$TO
      FROM[i, , 2] = ct$FROM
      NET[i, , 2] = ct$NET
      NPDC[, , i, 2] = ct$NPDC
      TCI[i, 2] = ct$TCI * (1 + (corrected * (k/(k - 1) - 
                                                1)))
      TO[i, , 3] = lt$TO
      FROM[i, , 3] = lt$FROM
      NET[i, , 3] = lt$NET
      NPDC[, , i, 3] = lt$NPDC
      TCI[i, 3] = lt$TCI * (1 + (corrected * (k/(k - 1) - 
                                                1)))
    }
    else {
      ct = ConnectednessTable(CT[, , i, 1])
      TO[i, , 1] = ct$TO
      FROM[i, , 1] = ct$FROM
      NET[i, , 1] = ct$NET
      NPDC[, , i, 1] = ct$NPDC
      TCI[i, 1] = ct$TCI * (1 + (corrected * (k/(k - 1) - 
                                                1)))
    }
    if (progbar == TRUE) {
      #Sys.sleep(0.05)
      setTxtProgressBar(pb, i)
    }
  }
  TABLE = ConnectednessTable(CT[, , , 1])$TABLE
  if (nlag > 0) {
    lt = ConnectednessTable(CT[, , , 2])$TABLE
    at = ConnectednessTable(CT[, , , 1] + CT[, , , 2])$TABLE
    TABLE = list(Overall = at, Contemporaneous = TABLE, Lagged = lt)
  }
  if (nlag == 0) {
    TO = TO[, , 1]
    FROM = FROM[, , 1]
    NET = NET[, , 1]
    NPDC = NPDC[, , , 1]
  }
  config = list(nlag = nlag, approach = "R2", window.size = window.size, 
                relative = relative, corrected = corrected)
  return = list(CT = CT, TO = TO, FROM = FROM, NET = NET, TCI = TCI, 
                NPDC = NPDC, TABLE = TABLE, config = config)
}