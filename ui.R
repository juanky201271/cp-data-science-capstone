library(shiny)
library(shinyjs)
library(dplyr)
library(NLP)
library(tm)
library(tidytext)
library(tidyr)
library(quanteda)
library(stringi)
library(MASS)

appCSS <- "
#loading-content {
  position: absolute;
  background: #555555;
  opacity: 0.8;
  z-index: 100;
  left: 0;
  right: 0;
  height: 90%;
  text-align: center;
  color: #FFFFFF;
}
#loading-bbdd-content {
  position: absolute;
  background: #555555;
  opacity: 0.8;
  z-index: 100;
  left: 0;
  right: 0;
  height: 90%;
  text-align: center;
  color: #FFFFFF;
}
#app-content {
  opacity: 1;
  z-index: 1;
}
"

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Next Word Prediction"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      useShinyjs(),
      inlineCSS(appCSS),
      div(
        id = "loading-content",
        h2("... Working ...")
      ),
      div(
        id = "loading-bbdd-content",
        h2("... loading Ngrams ...")
      ),
      div(id = "app-content",
          textAreaInput("iText", "You can write whatever you want :", width = "100%", height = "150px" ),
          actionButton("iWord1", "...", onclick = "Shiny.setInputValue('btnLabel', this.innerText);", class = "btn-info"),
          actionButton("iWord2", "...", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
          actionButton("iWord3", "...", onclick = "Shiny.setInputValue('btnLabel', this.innerText);"),
          helpText("When you write a space the App will predict the next word. If you write one or more words without a space at the end, the App will wait. If you puss one of three button, the word is going to join to your sentence with a space at the end.")
       
        )
      ,
      actionButton("iCancel", "Cancel"),
      textOutput("tError")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      strong(helpText("Data :")),
      textOutput("oPorc"),
      textOutput("oWordsNum"),
      strong(helpText("Table of predicted words :")),
      tableOutput("oTableWords"),
      helpText("freq = freq * 100000"),
      strong(helpText("Turned Text :")),
      textOutput("oTurnedText"),
      strong(helpText("The better prediction :")),
      textOutput("oPredText")
      
      
    )
  )
))
