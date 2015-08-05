#' Chi-square goodness of fit test for Poisson distribution
#'
#' This function performs a chi-square goodness of fit test for a Poisson distribution.
#' @param x The observed values
#' @param f The observed counts.
#' @param lambda Poisson parameter.
#' @author Raul Eyzaguirre.
#' @details If \code{lambda} is not specified, then it is estimated from the data.
#' If there are categories with expected counts less than 5 or less than 1 a warning
#' is shown.
#' @return It returns a table with the contribution to the chi-square statistic for each category,
#' the chi-square statistic, the degrees of freedom, and the p-value.
#' @examples
#' x <- 0:9
#' f <- c(6, 16, 48, 77, 72, 72, 46, 39, 15, 9)
#' chisq.pois(x, f)
#' @export

chisq.pois <- function(x, f, lambda = NULL) {

  # Compute parameter

  if (is.null(lambda)) {
    lambda <- sum(x * f) / sum(f)
    k <- 1
  } else {
    k <- 0
  }

  # Expected frequencies

  xc <- 0:max(x)
  nc <- length(xc)
  obs <- rep(0, nc)
  for (i in 1:length(f))
    obs[x[i] + 1] <- f[i]
  prob <- dpois(xc, lambda)
  prob[length(xc)] <- 1 - ppois(max(x) - 1, lambda)
  esp <- sum(f) * prob

  # Grouping categories

  lz <- which(obs != 0)

  xc <- as.character(xc[min(lz):max(lz)])
  if(min(lz) > 1)
    xc[1] <- paste("0-", xc[1], sep = "")
  xc[length(xc)] <- paste(xc[length(xc)], "+", sep = "")

  obs <- obs[min(lz):max(lz)]
  esp[min(lz)] <- sum(esp[1:min(lz)])
  esp[max(lz)] <- sum(esp[max(lz):nc])
  esp <- esp[min(lz):max(lz)]

  # Chi-square statistic

  chisq <- (obs - esp)^2 / esp
  chisq.t <- sum(chisq)
  dft <- length(chisq) - k - 1
  pvt <- 1 - pchisq(chisq.t, dft)

  # Warnings

  if (sum(esp < 5) ==  1)
    warning(paste(sum(esp < 5), "Expected frequency less than 5."))
  if (sum(esp < 5) > 1)
    warning(paste(sum(esp < 5), "Expected frequencies less than 5."))

  if (sum(esp < 1) == 1)
    warning(paste(sum(esp < 1), "Expected frequency less than 1."))
  if (sum(esp < 1) > 1)
    warning(paste(sum(esp < 1), "Expected frequencies less than 1."))

  # Return

  dist.info <- paste("Chi-square goodness of fit test for a Poisson(",
                     format(lambda, digits = 4), ") distribution", sep = "")

  tabla <- data.frame(x = xc, obs.f = obs, exp.f = esp, chisq.cont = chisq)
  tabla$x <- as.character(tabla$x)

  list(Test = dist.info, Contribution_table = tabla, Chi_square_test = chisq.t,
       Degrees_of_freedom = dft, p_value = pvt)
}
