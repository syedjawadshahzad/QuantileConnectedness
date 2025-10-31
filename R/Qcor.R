#' Quantile Correlation Matrix
#'
#' Computes the symmetric matrix of pairwise **quantile correlations** at a given quantile level `q`,
#' following the quantile correlation coefficient proposed by Choi and Shin (2022).
#' This allows users to examine dependence between variables at specific quantiles (e.g., lower-tail,
#' median, or upper-tail dependence) rather than only at the mean.
#'
#' @param x A numeric matrix or data frame (columns represent variables).
#' @param q Numeric in `[0, 1]`; the quantile level at which the quantile correlation is evaluated.
#'
#' @return A symmetric numeric matrix of dimension `ncol(x) × ncol(x)`:
#' \itemize{
#'   \item Diagonal elements are equal to 1.
#'   \item Off-diagonal elements are pairwise quantile correlations.
#' }
#'
#' @details
#' For two variables \(X\) and \(Y\), the quantile correlation at level \(q\)
#' is based on the product of the quantile regression slopes from
#' \(Y \sim X\) and \(X \sim Y\) at quantile \(q\).
#' The sign is taken from the \(Y \sim X\) regression slope, and the
#' result is truncated to keep the correlation within \eqn{[-1, 1]}.
#'
#' @examples
#' \dontrun{
#' library(quantreg)
#' data <- matrix(rnorm(500), ncol = 5)
#' colnames(data) <- paste0("Var", 1:5)
#' Qcor(data, q = 0.1)    # lower-tail dependence
#' Qcor(data, q = 0.5)    # median correlation
#' Qcor(data, q = 0.9)    # upper-tail dependence
#' }
#'
#' @references
#' Choi, S., & Shin, S. (2022).
#' *Quantile Correlation: Measuring Dependence at Different Quantiles*.
#' Journal of Econometrics, 226(1), 1–24. \doi{10.1016/j.jeconom.2021.08.001}
#'
#' @importFrom quantreg rq
#' @export
Qcor <- function(x, q){
  names <- colnames(x)
  corQ <- matrix(NA, ncol=dim(x)[2], nrow=dim(x)[2])
  n=dim(x)[2]
  
  #Bivariate Quantile Correlation Function
  QuantileCor <- function(x, y, q){
    b2.1 <- quantreg::rq(y ~ x, tau = q, method = "fn")$coefficients[2]
    b1.2 <- quantreg::rq(x ~ y, tau = q, method = "fn")$coefficients[2]
    
    qcor <- sign(b2.1)*(ifelse((b2.1*b1.2)>0 , sqrt(b2.1*b1.2), 0))  
    qcor <- ifelse(qcor > 1, 0.999999, qcor)
    round(qcor, digits = 6)
    return(qcor)
  }
  
  # Correlation Matrix
  for (i in 1:n){
    for (j in 1:n){
      if (i < j){ 
        corQ[i,j] = QuantileCor(x[,i],x[,j], q)
      }
    }}
  
  diag(corQ) <- 1
  corQ[lower.tri(corQ)] <- t(corQ)[lower.tri(corQ)]
  colnames(corQ) <- names
  rownames(corQ) <- names
  corQ <- round(corQ, 4)
  
  return(corQ)
}
