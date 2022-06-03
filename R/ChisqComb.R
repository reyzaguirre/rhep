#' Combine categories for a chi-square goodness of fit test
#'
#' This function combines categories for a chi-square goodness of fit test.
#' @param chisq.test The output of a chi-square goodness of fit test by functions \code{chisq.bin}
#' or \code{chisq.Pois}.
#' @param combine A vector with the numbers of the categories to combine.
#' @author Raúl Eyzaguirre.
#' @details This function only cobines categories on the extremes.
#' It is recommended to combine categories when the expected counts are too low.
#' As a rule of thumb, the chi-square approximation for the test statistic can be unreliable
#' if some categories have expected counts smaller than 5 or if there is any with an expected
#' count smaller than 1.
#' @return It returns a table with the contribution to the chi-square statistic for each category,
#' the chi-square statistic, the degrees of freedom, and the p-value.
#' @examples
#' x <- 0:6
#' f <- c(334, 369, 191, 63, 22, 12, 9)
#' output <- chisq.bin(x, f, n = 10)
#' # Combine categories 5, 6, and 7
#' chisq.comb(output, combine = c(5, 6, 7))
#' @export

chisq.comb <- function(chisq.test, combine) {

  temp <- chisq.test$Contribution_table

  lower <- min(combine)
  upper <- max(combine)
  top <- dim(temp)[1]

  # Error messages
  if (lower > 1 & upper < top)
    stop("Only categories on the extremes can be combined.")

  if(lower == 1) {
    chisq.test$Contribution_table <- temp[upper:top, ]
    chisq.test$Contribution_table[1, 1] <- paste("0-", temp[upper, 1], sep = "")
    chisq.test$Contribution_table[1, 2] <- sum(temp[1:upper, 2])
    chisq.test$Contribution_table[1, 3] <- sum(temp[1:upper, 3])
    temp <- chisq.test$Contribution_table
    chisq.test$Contribution_table[, 4] <- (temp[, 2] - temp[, 3])^2 / temp[, 3]
    chisq.test$Chi_square_test <- sum(chisq.test$Contribution_table[, 4])
    chisq.test$Degrees_of_freedom <- chisq.test$Degrees_of_freedom - length(combine) + 1
  } else {
    chisq.test$Contribution_table <- temp[1:lower, ]
    chisq.test$Contribution_table[lower, 1] <- paste(temp[lower, 1], "+", sep = "")
    chisq.test$Contribution_table[lower, 2] <- sum(temp[lower:top, 2])
    chisq.test$Contribution_table[lower, 3] <- sum(temp[lower:top, 3])
    temp <- chisq.test$Contribution_table
    chisq.test$Contribution_table[, 4] <- (temp[, 2] - temp[, 3])^2 / temp[, 3]
    chisq.test$Chi_square_test <- sum(chisq.test$Contribution_table[, 4])
    chisq.test$Degrees_of_freedom <- chisq.test$Degrees_of_freedom - length(combine) + 1
  }

  chisq.test
}
