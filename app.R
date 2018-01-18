# Thomas M. Massie, 11.10.2017, Zurich
rm(list = ls())



# Load libraries.
library(shiny)
library(shinythemes)
library(pdftools)
library(wordcloud2)
library(ggrepel)
library(tm)
library(stringr)
library(lsa)
library(tidyverse)
library(ggplot2)
library(ggpubr)   # devtools::install_github("kassambara/ggpubr")



# -----------------------------
# UI

ui <- fluidPage(theme = shinytheme("simplex"),
                titlePanel("Time-descrete logistic population growth"),
                sidebarLayout(
                  sidebarPanel(
                    # textInput("txt", "Text input:", "text here"),
                    sliderInput("p.scale", "Relative size:", 
                                min = 0, 
                                max = 1, 
                                value = 0.25,
                                step = 0.05),
                    textInput("text", h3("Text input"), 
                              value = "Enter text...")
                  ),
                  mainPanel(
                    plotOutput(outputId = "figure")
                  )
                )
)



server <- function(input, output) {
}










# Run the application 
shinyApp(ui = ui, server = server)




