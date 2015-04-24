#' My first Shiny app
#'
#' A shiny user interface fragment to show an histogram.
#'
#' @export

histo_plot <- function () {
  dirfiles <- paste(system.file(package='rhep'), "/shinyapps/myfirstapp", sep="")
  shiny::runApp(dirfiles)
}
