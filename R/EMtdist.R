#' Location and scale parameters estimation of a t distribution
#'
#' EM algorithm to estimate the location and scale of a t distribution
#' for given degrees of freedom.
#'
#' @param y The data.
#' @param v Degrees of freedom.
#' @param initmu Initial value for the location parameter.
#' @param inits Initial value for the scale parameter.
#' @param tol Tolerance for the iterative procedure.
#' @author Raul Eyzaguirre.
#' @details By default the initial values are set to the sample mean and
#' standard deviation.
#' @return It returns the estimated location and scale parameters for each iteration.
#' @examples
#' y = c(10, 12, 16, 15, 15, 17, 20, 21, 16, 24, 13, 22, 14, 15, 16, 16, 17, 18, 19, 18, 23, 20, 30)
#' emtd(y, 10)
#' @importFrom stats sd
#' @export

emtd <- function(y, v, initmu = mean(y), inits = sd(y), tol = 0.0001) {
  w <- NULL
  mu <- NULL
  s2 <- NULL
  n.iter <- NULL
  mu[1] <- initmu
  s2[1] <- inits^2
  n.iter[1] <- 0
  emax <- tol + 1
  j <- 1
  while(tol < emax) {
    # E-step
    w <- sapply(y, function(x) ((v + 1) * s2[j]) / ((x - mu[j])^2 + v * s2[j]))
    # M-step
    j <- j + 1
    mu[j] <- sum(w * y) / sum(w)
    s2[j] <- sum(w * (y - mu[j])^2) / length(y)
    # count
    n.iter[j] <- j - 1
    emax <- abs((mu[j] - mu[j - 1]) / mu[j - 1])
  }
  # output
  salida <- cbind(n.iter, mu, s2^0.5)
  colnames(salida)[3] <- "sigma"
  return(salida)
}
