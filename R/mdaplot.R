#' Simulate and plot from a normal distribution
#'
#' This function simulates 1000 random samples from a skew normal distribution
#' for specified values of the mean, standard deviation and skewness parameter.
#' @author Raul Eyzaguirre.
#' @details It uses package \code{sn} to simulate the data and package \code{shiny}
#' for the web layout. Type mdaplot() in the R console to run the app.
#' @return It returns a histogram and a boxplot for the simulated data.
#' @export

mdaplot <- function () {
  dirfiles <- paste(system.file(package='rhep'), "/shinyapps/mdaplot", sep="")
  shiny::runApp(dirfiles)
}
