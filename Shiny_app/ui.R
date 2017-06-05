
library(shiny)
library(dplyr)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel("Operations Characteristic", windowTitle = "Operations Charakteristic"),
  fluidRow(
    column(4,
           wellPanel(
             h4("Control Panel"),
             sliderInput(inputId = "one_min_alpha", label = "1 - alpha", value = 1, min = 0, max = 1),
             sliderInput(inputId = "AQL", label = "AQL", value = 1, min = 0, max = 1),
             sliderInput(inputId = "beta", label = "beta", value = 1, min = 0, max = 1),
             sliderInput(inputId = "RQL", label = "RQL", value = 1, min = 0, max = 1),
             numericInput(inputId = "N", label = "N", value = 400, min = 1, max = 5000)
           ),
           actionButton("action", label = "find optimal plan") 
    ),  
    column(8,
           wellPanel(
             div(strong("Author: Dennis Triepke")),
             span("Algorithmus from Guenther is used in order to find the optimal plan vor n and c for a hypergeometrical distribution. ")
           ),
           plotlyOutput("plot"),
           wellPanel(
             span(strong("Optimal Plan:"), 
                  textOutput("text"))
           )
    )
  )
)

shinyUI(ui)