#' Chi-square goodness of fit test for a binomial distribution
#'
#' Performs a chi-square goodness of fit test for a binomial distribution.
#' @param x The observed values
#' @param f The frequencies for the observed values
#' @param n Binomial parameter n.
#' @param p Binomial parameter pi.
#' @author Raul Eyzaguirre.
#' @details If \code{p} is not specified, then it is estimated from the data.
#' If there are categories with expected frequencies less than 5 or less than 1 a warning
#' is shown.
#' @return It returns a table with the contribution to the chi-square statistic for each category,
#' the chi-square statistic, the degrees of freedom and the p-value.
#' @examples
#' x <- 0:6
#' f <- c(334, 369, 191, 63, 22, 12, 9)
#' chisq.bin(x, f, n = 10)
#' @export

chisq.bin <- function(x, f, n = NULL, p = NULL){

  # Compute parameter

  if (is.null(p) == 1){
    p <- sum(x*f) / sum(n*f)
    k <- 1
  }

  # Expected frequencies

  xc <- 0:n
  nc <- length(xc)
  obs <- rep(0, nc)
  for (i in 1:length(f))
    obs[x[i]+1] <- f[i]
  prob <- dbinom(xc, n, p)
  esp <- sum(f)*prob

  # Grouping categories

  lz <- which(obs != 0)

  xc <- as.character(xc[min(lz):max(lz)])
  if(min(lz) > 1)
    xc[1] <- paste("0-", xc[1], sep="")
  if(max(lz) < nc)
    xc[length(xc)] <- paste(xc[length(xc)], "+", sep="")

  obs <- obs[min(lz):max(lz)]
  esp[min(lz)] <- sum(esp[1:min(lz)])
  esp[max(lz)] <- sum(esp[max(lz):nc])
  esp <- esp[min(lz):max(lz)]

  # Chi-square statistic

  chisq <- (obs - esp)^2/esp
  chisq.t <- sum(chisq)
  dft <- length(chisq) - k
  pvt <- 1 - pchisq(chisq.t, dft)

  # Warnings

  if (sum(esp < 5) ==  1)
    warning(paste(sum(esp < 5), "expected frequency less than 5."))
  if (sum(esp < 5) > 1)
    warning(paste(sum(esp < 5), "expected frequencies less than 5."))

  if (sum(esp < 1) == 1)
    warning(paste(sum(esp < 1), "expected frequency less than 1."))
  if (sum(esp < 1) > 1)
    warning(paste(sum(esp < 1), "expected frequencies less than 1."))

  # Return

  tabla <- data.frame(x = xc, obs.f = obs, exp.f = esp, chisq.cont = chisq)
  list(Contribution_table = tabla, Chi_square_test = chisq.t,
       degrees_of_freedom = dft, p_value = pvt)
}
