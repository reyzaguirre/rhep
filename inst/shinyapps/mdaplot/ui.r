# Define UI for application that draws a histogram

shiny::shinyUI(shiny::fluidPage(

  # Application title
  shiny::titlePanel("Media, desviación estándar y asimetría (Version 0.1.1)"),

  # Sidebar with a slider input mean, variance and skew parameter

  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::sliderInput("media",
                         "Media:",
                         min = -10,
                         max = 10,
                         value = 0),
      shiny::sliderInput("dest",
                         "Desviación estándar:",
                         min = 0.5,
                         max = 3,
                         value = 1),
      shiny::sliderInput("asim",
                         "Asimetría:",
                         min = -10,
                         max = 10,
                         value = 0)
    ),

    # Show a plot of the generated distribution

    shiny::mainPanel(
      shiny::plotOutput("distPlot1", width = "70%", height = "300px"),
      shiny::plotOutput("distPlot2", width = "70%", height = "300px")
    )
  )
))

