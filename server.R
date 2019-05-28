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

jc.corp = function(x) {
  corpus(x)
}

jc.toke = function(x, ngramSize = 1) {
  
  tolower(
    quanteda::tokens_remove(
      quanteda::tokens_tolower(
        quanteda::tokens(x,
                         remove_numbers = T,
                         remove_punct = T,
                         remove_symbols = T,
                         remove_separators = T,
                         remove_twitter = T,
                         remove_hyphens = T,
                         remove_url = T,
                         ngrams = ngramSize,
                         concatenator = " "
        )
      )
      ,
      df_prof
    )
  )
  
}

jc.inpu = function(x) {
  if(is.na(x)) {
#    print("is null")
    return(tibble(words = c("","","")))
  }
  if(is.null(x)) {
#    print("is NA")
    return(tibble(words = c("","","")))
  }
  if(x == "") {
#    print("is empty")
    return(tibble(words = c("","","")))
  }
  if(length(x) == 1) {
    y = tibble(word = jc.toke(jc.corp(x)))
  }
  else
    return(tibble(words = c("","","")))
  if (nrow(y) < 1) {
    return(tibble(words = c("","","")))
  } else if (nrow(y) == 1) {
    input1 = tibble(word = "")
    input2 = tibble(word = "")
    input3 = y
    
  } else if (nrow(y) == 2) {
    input1 = tibble(word = "")
    input2 = tail(y, 2)[1, ]
    input3 = tail(y, 1)
    
  } else if (nrow(y) >= 2) {
    input1 = tail(y, 3)[1, ]
    input2 = tail(y, 2)[1, ]
    input3 = tail(y, 1)
  }
  
  return(tibble(words = unlist(rbind(input1, input2, input3))))
}

jc.pred = function(w1, w2, w3, n = 3) {
  
  if(is.na(w1) &  is.na(w2) & is.na(w3)) {
    prediction = dfTrain1 %>%
      mutate(ngram = 1) 
    
  }   else if(is.null(w1) &  is.null(w2) & is.null(w3)) {
    prediction = dfTrain1 %>%
      mutate(ngram = 1) 
    
  }   else if(w1 == "" &  w2 == "" & w3 == "") {
    prediction = dfTrain1 %>%
      mutate(ngram = 1) 
    
  }   else if(dim(dfTrain4 %>%
                  filter(word1 %in% w1 & word2 %in% w2 & word3 %in% w3))[1] > 0) {
    prediction = dfTrain4 %>%
      filter(word1 %in% w1 & word2 %in% w2 & word3 %in% w3) %>%
      mutate(ngram = 4) %>%
      dplyr::select(NextWord, freq, ngram)
    
  }   else if(dim(dfTrain3 %>%
                  filter(word1 %in% w2 & word2 %in% w3))[1] > 0) {
    prediction = dfTrain3 %>%
      filter(word1 %in% w2 & word2 %in% w3) %>%
      mutate(ngram = 3) %>%
      dplyr::select(NextWord, freq, ngram)
    
  }   else if(dim(dfTrain2 %>%
                  filter(word1 %in% w3))[1] > 0) {
    prediction = dfTrain2 %>%
      filter(word1 %in% w3) %>%
      mutate(ngram = 2) %>%
      dplyr::select(NextWord, freq, ngram)
    
  }   else {
    prediction = dfTrain1 %>%
      mutate(ngram = 1) 
  }

  if (dim(prediction)[1] >= n)  
    return(prediction[1:n, ])
  else
    return(prediction[1:dim(prediction)[1], ])
}

jc.next <- function(txt, n = 3) {
  text <- jc.inpu(txt)
#  print(str(text))
#  print(dim(text))
  out  <- jc.pred(as.character(text[1,1]), as.character(text[2,1]), as.character(text[3,1]), n)
  return(out)
}

gporc <- 0.05
dfTrain1 <- readRDS(paste0("./final/en_US/ngrams/1ng.q", gporc, ".RData"))
dfTrain2 <- readRDS(paste0("./final/en_US/ngrams/2ng.q", gporc, ".RData"))
dfTrain3 <- readRDS(paste0("./final/en_US/ngrams/3ng.q", gporc, ".RData"))
dfTrain4 <- readRDS(paste0("./final/en_US/ngrams/4ng.q", gporc, ".RData"))

conn <- file("https://www.cs.cmu.edu/~biglou/resources/bad-words.txt","r")
df_prof <- readLines(conn)
close(conn)
rm(conn)

shinyServer(function(input, output, session) {
  
  observeEvent( input$iCancel, {
    stopApp()  
  }) 
  observeEvent( input$iWord1, {
    #print(input$btnLabel)
    if (input$btnLabel != "...")
      updateTextInput(session, "iText", value = paste0(input$iText, input$btnLabel," "))
  }) 
  observeEvent( input$iWord2, {
    #print(input$btnLabel)
    if (input$btnLabel != "...")
      updateTextInput(session, "iText", value = paste0(input$iText, input$btnLabel, " "))
  }) 
  observeEvent( input$iWord3, {
    #print(input$btnLabel)
    if (input$btnLabel != "...")
      updateTextInput(session, "iText", value = paste0(input$iText, input$btnLabel, " "))
  }) 
  observeEvent( input$iSamples, {
    shinyjs::show(id = "loading-content")
    if (as.numeric(input$iSamples) > 10) {
      output$tError = renderText("shinyapp.io: loading n-grams. Out of memory!")
      shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade")
      
      return()
    }
    porc <- as.numeric(input$iSamples) / 100
    if (gporc != porc) {
      gporc <- porc
      y <- try(dfTrain1 <- readRDS(paste0("./final/en_US/ngrams/1ng.q", porc, ".RData")), silent = TRUE)
      if(inherits(y,"try-error")) {
        output$tError = renderText("shinyapp.io: 1-n-gram. Out of memory!")
        shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade")    
        return()
      }
      y <- try(dfTrain2 <- readRDS(paste0("./final/en_US/ngrams/2ng.q", porc, ".RData")), silent = TRUE)
      if(inherits(y,"try-error")) {
        output$tError = renderText("shinyapp.io: 2-n-gram. Out of memory!")
        shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade")    
        return()
      }
      y <- try(dfTrain3 <- readRDS(paste0("./final/en_US/ngrams/3ng.q", porc, ".RData")), silent = TRUE)
      if(inherits(y,"try-error")) {
        output$tError = renderText("shinyapp.io: 3-n-gram. Out of memory!")
        shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade")    
        return()
      }
      y <- try(dfTrain4 <- readRDS(paste0("./final/en_US/ngrams/4ng.q", porc, ".RData")), silent = TRUE)
      if(inherits(y,"try-error")) {
        output$tError = renderText("shinyapp.io: 4-n-gram. Out of memory!")
        shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade")    
        return()
      }
    }
    output$oPorc <- renderText({
      paste("% selected :", porc * 100)
    })
    output$oWordsNum <- renderText({
      paste("The numbers of words :", sum(stri_count_words(dfTrain1)))
    })
    shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade")    
  })
  
  observeEvent( input$iText, {
    if (!grepl(" $", input$iText) & !input$iText == "") {
      output$oTurnedText <- renderText({
        "...writing..."
      })
      output$oPredText <- renderText({
        "...writing..."
      })
    } else {
      shinyjs::show(id = "loading-content")    
      tb <- jc.next(input$iText)
      #print(tb)
      #print(tb[, 2])
      tb[ ,2] = tb[ ,2] * 100000
      output$oTableWords <- renderTable({
        tb
      })
      output$oTurnedText <- renderText({
        unlist(jc.toke(jc.corp(input$iText)))
      })
      output$oPredText <- renderText({
        unlist(c(jc.toke(jc.corp(input$iText)), as.character(tb[1,1])))
      })
      if (dim(tb)[1] < 1) 
        updateActionButton(session, "iWord1", label = "...")
      else
        updateActionButton(session, "iWord1", label = as.character(tb[1,1]))
      if (dim(tb)[1] < 2) 
        updateActionButton(session, "iWord2", label = "...")
      else
        updateActionButton(session, "iWord2", label = as.character(tb[2,1]))
      if (dim(tb)[1] < 3) 
        updateActionButton(session, "iWord3", label = "...")
      else
        updateActionButton(session, "iWord3", label = as.character(tb[3,1]))
      shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade")    
    }
  })
  
})
