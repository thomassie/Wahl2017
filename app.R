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

# ------------------------------------------
# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("simplex"),
                titlePanel("Analysis of manifestos in Germany's 2017 election"),
                sidebarLayout(
                  sidebarPanel(
                    # textInput("txt", "Text input:", "text here"),
                    selectInput(inputId = "selected.party", 
                                label = h3("Party:"), 
                                choices = c("CDU/CSU", "SPD", "GRUENE", "FDP", "LINKE", "AFD"), 
                                selected = NULL, multiple = FALSE,
                                selectize = TRUE, width = NULL, size = NULL),
                    textInput("txt.1", label = "1st term", value = "Which term...?"),
                    textInput("txt.1", label = "2nd term", value = "Which term...?"),
                    textInput("txt.1", label = "3rd term", value = "Which term...?"),
                    verbatimTextOutput("default")
                  ),
                  mainPanel(
                    plotOutput(outputId = "figure1"),
                    plotOutput(outputId = "figure2")
                  )
                )
)




server <- function(input, output) {
  
  # Arbeitsverzeichnis festlegen.
  setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Data analysis/Wahlprogramme")
  
  # Laden der pdf-Dateie.
  ifelse(input$selected.party = "CDU/CSU",
         download.file("https://github.com/thomassie/Wahl2017/raw/master/Data/CDUCSU.pdf",
                       "text.pdf", 
                       mode = "wb"))
         
         
         text_raw <- pdf_text("https://github.com/thomassie/Wahl2017/raw/master/Data/CDUCSU.pdf"))
  
  
  
  text_raw <- pdf_text("AFD.pdf")
  
  
  
  str(text_raw)
  
  review_source <- VectorSource(text_raw)
  
  # Removeing a couple of super frequent words.
  remove.words <- c("bundestagswahl",
                    "wahl",
                    "wahlprogramm",
                    "dass")
  
  
  
  
  
  # 1st figure: word cloud.
  output$figure1 <- renderPlot({
    wordcloud(text_process$words[1:n.words], text_process$frequency[1:n.words],
              scale = c(18, 0.5),
              # colors = c("#666666", "#1172B0")[factor(text_process$isit[1:n.words])],  # Afd
              colors = c("#666666", "#3E6AA0")[factor(text_process$isit[1:n.words])],  # SPD
              use.r.layout = FALSE,
              rot.per = .2,
              random.order = FALSE,
              ordered.colors=TRUE,
              family = "serif", font = 3)
  })
  
  # 2nd figure: bar plot.
  output$figure2 <- renderPlot({
    text_process[1:20,] %>%
      group_by(isit) %>%
      ggplot() +
      aes(x = reorder(words, frequency), 
          y = frequency,
          fill = isit) +
      scale_fill_manual(values = c("#666666", "#1172B0")) +
      geom_col() +
      theme_classic() +
      xlab("") +
      ylab("") +
      theme(axis.text = element_text(size = 50),
            axis.line = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.y = element_blank()) +
      geom_text(aes(label = frequency),
                size = 14,
                color = "white",
                position = position_dodge(width = 0.9),
                hjust = 1.2) +
      theme(legend.position="none") +
      coord_flip()
  })
}










# Run the application 
shinyApp(ui = ui, server = server)




