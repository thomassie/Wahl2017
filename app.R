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



# Create a content transformer function called 'toSpace' using 'gsub'.
# This simply replaces content by blank space.
toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})


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
                    textInput(inputId = "txt.1", label = "1st term", value = "arbeit"),
                    textInput(inputId = "txt.2", label = "2nd term", value = "familie"),
                    textInput(inputId = "txt.3", label = "3rd term", value = "bildung"),
                    # verbatimTextOutput("default"),
                    numericInput(inputId = "n.words", label = "Number of words:", 100, min = 10, max = 500)
                  ),
                  mainPanel(
                    plotOutput(outputId = "figure1"),
                    plotOutput(outputId = "figure2")
                  )
                )
)




server <- function(input, output) {
  
  dataInput <- reactive({
    
    # Reading in the specific pdf file.
    party.colour <- ifelse(input$selected.party == "CDU/CSU", "#000000",
                           ifelse(input$selected.party == "SPD", "#FF2D36",
                                  ifelse(input$selected.party == "GRUENE", "#00AB70",
                                         ifelse(input$selected.party == "FDP", "#FFD163",
                                                ifelse(input$selected.party == "LINKE", "#8C1A77",
                                                       ifelse(input$selected.party == "AFD", "#0073AB",
                                                              "#FFFFFF"))))))
    
    # Reading in the specific pdf file.
    text_raw <- ifelse(input$selected.party == "CDU/CSU",
                       pdf_text("https://github.com/thomassie/Wahl2017/raw/master/Data/CDUCSU.pdf"),
                       ifelse(input$selected.party == "SPD",
                              pdf_text("https://github.com/thomassie/Wahl2017/raw/master/Data/SPD.pdf"),
                              ifelse(input$selected.party == "GRUENE",
                                     pdf_text("https://github.com/thomassie/Wahl2017/raw/master/Data/DIEGRUENEN.pdf"),
                                     ifelse(input$selected.party == "FDP",
                                            pdf_text("https://github.com/thomassie/Wahl2017/raw/master/Data/FDP.pdf"),
                                            ifelse(input$selected.party == "LINKE",
                                                   pdf_text("https://github.com/thomassie/Wahl2017/raw/master/Data/DIELINKE.pdf"),
                                                   ifelse(input$selected.party == "AFD",
                                                          pdf_text("https://github.com/thomassie/Wahl2017/raw/master/Data/AFD.pdf"),
                                                          "https://github.com/thomassie/Wahl2017/raw/master/Data/CDUCSU.pdf"))))))
    
    review_source <- VectorSource(text_raw)
    
    # A couple of super frequent words to be removed.
    remove.words <- c("bundestagswahl",
                      "wahl",
                      "wahlprogramm",
                      "dass")
    
    # Some cleaning according to "A gentle introduction to text mining using R."
    corpus <- Corpus(review_source) %>%
      tm_map(toSpace, "-") %>%
      tm_map(toSpace, ":") %>%
      tm_map(toSpace, "'") %>%
      tm_map(toSpace, "`") %>%
      tm_map(toSpace, " -") %>%
      tm_map(removePunctuation) %>%
      tm_map(removeNumbers) %>%
      tm_map(content_transformer(tolower)) %>%
      tm_map(stripWhitespace) %>%
      tm_map(removeWords, stopwords("german")) %>%
      tm_map(removeWords, remove.words) #%>%
    # tm_map(stemDocument, language = "german")
    
    dtm <- DocumentTermMatrix(corpus)
    dtm2 <- as.matrix(dtm)
    frequency <- colSums(dtm2)
    words <- names(frequency)
    
    selected.words <- c(input$txt.1,
                        input$txt.2,
                        input$txt.3)
    
    dd.processed.text <- data.frame(words = words, frequency = frequency) %>%
      arrange(desc(frequency)) %>%
      mutate(isit = words %in% selected.words)
    
    # ------------------------------    
    # A list of output data.
    list(dd.processed.text)
    
  })
  
  
  # ------------------------------------------
  # 1st figure: word cloud.
  output$figure1 <- renderPlot({
    # Load data
    dd <- dataInput()[["dd.processed.text"]]
    # Plot figure.
    dd %>%
      # group_by(isit) %>%
      wordcloud(words[1:input$n.words], frequency[1:input$n.words],
                scale = c(2, 0.8),
                # colors = c("#666666", input$party.colour),
                use.r.layout = FALSE,
                rot.per = .2,
                random.order = FALSE,
                # ordered.colours = TRUE,
                family = "serif", font = 3)
    # wordcloud(dd.processed.text$words[1:n.words], dd.processed.text$frequency[1:input$n.words],
    #           scale = c(2, 0.8),
    #           colors = c("#666666", party.colour)[factor(dd.processed.text$isit[1:input$n.words])],
    #           use.r.layout = FALSE,
    #           rot.per = .2,
    #           random.order = FALSE,
    #           ordered.colors = TRUE,
    #           family = "serif", font = 3)
  })

  # ------------------------------------------    
  # 2nd figure: bar plot.
  output$figure2 <- renderPlot({
    # Load data
    dd <- dataInput()[["dd.processed.text"]]
    # Plot figure.
    dd[1:20,] %>%
      group_by(isit) %>%
      ggplot() +
      aes(x = reorder(words, frequency), 
          y = frequency,
          fill = isit) +
      scale_fill_manual(values = c("#666666", input$party.colour)) +
      geom_col() +
      theme_classic() +
      xlab("") +
      ylab("") +
      theme(axis.text = element_text(size = 12),
            axis.line = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.y = element_blank()) +
      geom_text(aes(label = frequency),
                size = 6,
                color = "white",
                position = position_dodge(width = 0.9),
                hjust = 1.2) +
      theme(legend.position="none") +
      coord_flip()
  })
  
}










# Run the application 
shinyApp(ui = ui, server = server)




