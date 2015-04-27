#' My first Shiny app
#'
#' A shiny user interface fragment to show an histogram.
#'
#' @export

mdaplot <- function () {
  dirfiles <- paste(system.file(package='rhep'), "/shinyapps/mdaplot", sep="")
  shiny::runApp(dirfiles)
}
