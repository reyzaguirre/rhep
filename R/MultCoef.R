#' Multinomial coefficient
#'
#' Computes the number of permutations of a multiset \eqn{M} of size \eqn{n}.
#'
#' @param n The size of \eqn{M}.
#' @param counts The counts for the repeated elements.
#' @details
#' For a set \eqn{M} with \eqn{k} unique elements with associate counts
#' \eqn{n_1, n_2, \ldots, n_k}, you only need to specify in the \code{counts}
#' argument the counts that are bigger than 1.
#' @return It returns the multinomial coefficient
#' \deqn{\frac{n!}{n_1!n_2! \ldots n_k!}}
#' where
#' \deqn{n = n_1 + n_2 + \ldots + n_k.}
#'
#' @examples
#' # The number of permutations of the letters in the set M = {A,A,A,B,B,C}
#' multcoef(6, c(3,2,1))
#' # Same result with
#' multcoef(6, c(3,2))
#' @export

multcoef <- function(n, counts){
  np <- factorial(n)
  lc <- length(counts)
  for (i in 1:lc)
    np <- np/factorial(counts[i])
  return(np)
}
