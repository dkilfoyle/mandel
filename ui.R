library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Mandelbrot Explorer"),

  tagList(
    singleton(tags$head(tags$link(href="css/imgareaselect-default.css",rel="stylesheet",type="text/css"))),
    singleton(tags$head(tags$script(src="js/jquery.imgareaselect.js"))),
    tags$script(sprintf("$(function() { $('#mandelPlot').imgAreaSelect({handles: true}); });"))
  ),


  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("n", "Number of iterations:", min = 10, max = 500, value = 50),
      sliderInput("xr", "x range:", min=-2, max=2, value=c(-2,2), step=0.1),
      sliderInput("yr", "y range:", min=-2, max=2, value=c(-1.5,1.5), step=0.1),
      actionButton("zoom", "Zoom out"),
      selectInput("res", "Image size:", c("320x200","640x480","800x600","1024x768"), selected="640x480")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("mandelPlot", height="100%")
    )
  )
))
