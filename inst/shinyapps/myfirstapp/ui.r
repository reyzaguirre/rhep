# Define UI for application that draws a histogram

shiny::shinyUI(shiny::fluidPage(

  # Application title
  shiny::titlePanel("Media y varianza"),

  # Sidebar with a slider input mean, variance and skew parameter

  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::sliderInput("media",
                         "Media:",
                         min = -10,
                         max = 10,
                         value = 0),
      shiny::sliderInput("dest",
                         "Desviacion estandar:",
                         min = 0.5,
                         max = 2.5,
                         value = 1)

      ),

    # Show a plot of the generated distribution

    shiny::mainPanel(
      shiny::plotOutput("distPlot1", width = "70%", height = "300px"),
      shiny::plotOutput("distPlot2", width = "70%", height = "300px")
    )
  )
))
