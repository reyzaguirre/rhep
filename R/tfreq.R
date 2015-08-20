#' Frequency distribution table
#'
#' Constructs a frequency distribution table for a quantitative variable.
#' @param data The observations to construct the frequency distribution table.
#' @param limits The class limits.
#' @param open Where to leave the class limits open, \code{left} or \code{right}.
#' Defaults to \code{right}.
#' @author Raul Eyzaguirre.
#' @details If class limits are not specified, the Sturges' rule is used to calculate the
#' number of class intervals \eqn{k}:
#' \deqn{k \approx 1 + 3.3 \log n}
#' Then, the left limit for the first class interval is set to the minimum value of the data,
#' the range \eqn{r} is computed and the size of the class intervals is defined by:
#' \deqn{c \approx \frac{r}{k}}
#' where \eqn{c} is rounded up with the same number of decimal places as the data.
#' @return It returns a frequency distribution table with columns for class mark,
#' absolute and relative frequencies, and cumulative absolute and relative frequencies.
#' @examples
#' # Some random data from a normal population with mean 10 and standard deviation 1
#' set.seed(1)
#' datos <- rnorm(100, 10, 1)
#' # Data with 3 decimal places
#' datos <- round(datos, 3)
#' # A summary of the data
#' summary(datos)
#' # Frequency table with 6 specified limits
#' tfreq(datos, c(7, 8, 9, 10, 11, 12, 13))
#' # Default method
#' tfreq(datos)
#' @export

tfreq <- function(data, limits = NULL, open = "right") {

  # Limits

  nd <- length(data)

  if (is.null(limits)) {
    ndp <- sapply(data, function(x) nchar(strsplit(as.character(x), ".", fixed = TRUE)[[1]])[2])
    ndpm <- max(ndp, na.rm = TRUE)
    k <- round(1 + 3.3 * log10(nd))
    r <- range(data)[2] - range(data)[1]
    tic <- ceiling(r / k * 10^ndpm) / 10^ndpm
    limits <- seq(min(data), min(data) + tic * k, tic)
  } else {
    k <- length(limits) - 1
  }

  # Table

  cl <- paste(as.character(limits)[1:k], "-", as.character(limits)[2:(k + 1)])
  cm <- ((limits)[1:k] + (limits)[2:(k + 1)]) / 2
  fi <- as.numeric(table(cut(data, limits, right = FALSE)))
  hi <- fi / nd
  Fi <- cumsum(fi)
  Hi <- cumsum(hi)

  tdf <- data.frame(cl, Mi = cm, fi, hi, Fi, Hi)
  colnames(tdf)[1] <- "Class Interval"

  # Return

  return(tdf)
}
