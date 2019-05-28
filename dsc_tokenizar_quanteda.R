library(dplyr)
library(NLP)
library(tm)
library(tidytext)
library(tidyr)
library(quanteda)
library(wordcloud)
library(RColorBrewer)

jc.freq = function(x) {
  x = x %>%
    group_by(NextWord) %>%
    summarise(count = dplyr::n()) 
  x = x %>% 
    mutate(freq = count / sum(x$count)) %>% 
    select(-count) %>%
    arrange(desc(freq))
}

jc.corp = function(x) {
  corpus(x)
}

jc.toke = function(x, ngramSize = 1) {
  
  # Do some regex magic with quanteda
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
  # If empty input, put both words empty
  if(is.null(x) | x == "") {
    return(tibble(words = ""))
  }
  # Tokenize with same functions as training data
  if(length(x) == 1) {
    y = tibble(word = jc.toke(jc.corp(x)))
  }
  else
    return(tibble(words = ""))
  
  # If only one word, put the two first word empty
  if (nrow(y) == 1) {
    input1 = tibble(word = "")
    input2 = tibble(word = "")
    input3 = y
    
    # If two word, put first word empty
  } else if (nrow(y) == 2) {
    input1 = tibble(word = "")
    input2 = tail(y, 2)[1, ]
    input3 = tail(y, 1)
    
  } else if (nrow(y) >= 2) {
    input1 = tail(y, 3)[1, ]
    input2 = tail(y, 2)[1, ]
    input3 = tail(y, 1)
  }
  
  #  Return data frame of inputs 
  return(tibble(words = unlist(rbind(input1, input2, input3))))
}

jc.pred = function(w1, w2, w3, n = 100) {
  
  # Predict giving just the top 1-gram words, if no input given
  if((w1 == "" | is.null(w1)) &  (w2 == "" | is.null(w2)) & (w3 == "" | is.null(w3))) {
    prediction = dfTrain1 %>%
      mutate(ngram = 1) %>%
      select(NextWord, freq, ngram)

    # Predict using 4-gram model
  }   else if(dim(dfTrain4 %>%
              filter(word1 %in% w1 & word2 %in% w2 & word3 %in% w3))[1] > 0) {
    prediction = dfTrain4 %>%
      filter(word1 %in% w1 & word2 %in% w2 & word3 %in% w3) %>%
      mutate(ngram = 4) %>%
      select(NextWord, freq, ngram)
    
    # Predict using 3-gram model
  }   else if(dim(dfTrain3 %>%
                  filter(word1 %in% w2 & word2 %in% w3))[1] > 0) {
    prediction = dfTrain3 %>%
      filter(word1 %in% w2 & word2 %in% w3) %>%
      mutate(ngram = 3) %>%
      select(NextWord, freq, ngram)
    
    # Predict using 2-gram model
  }   else if(dim(dfTrain2 %>%
                  filter(word1 %in% w3))[1] > 0) {
    prediction = dfTrain2 %>%
      filter(word1 %in% w3) %>%
      mutate(ngram = 2) %>%
      select(NextWord, freq, ngram)
    
    # If no prediction found before, predict giving just the top 1-gram words
  }   else {
    prediction = dfTrain1 %>%
      mutate(ngram = 1) %>%
      select(NextWord, freq, ngram)
  }
  
  # Return predicted word in a data frame
  return(prediction[1:n, ])
}

jc.next <- function(txt, n = 100) {
  text <- jc.inpu(txt)
  #print(str(text))
  out  <- jc.pred(as.character(text[1,1]), as.character(text[2,1]), as.character(text[3,1]), n)
  return(out)
}

memory.limit(size = 16000)
set.seed(1313)

conn <- file("https://www.cs.cmu.edu/~biglou/resources/bad-words.txt","r")
df_prof <- readLines(conn)
close(conn)
rm(conn)

porc <- 0.10 # 10%
if (!file.exists(paste0("./final/en_US/sample/ne.q", porc, ".RData"))) {
  conn <- file("./final/en_US/en_US.news.txt", open = "rb")
  ne <- readLines(conn, encoding = "UTF-8", skipNul = TRUE)
  close(conn)
  rm(conn)
  ne <- iconv(ne,"latin1","ASCII",sub = "")
  ne_s <- sample(ne, round(porc * length(ne)))
  saveRDS(ne_s, file = paste0("./final/en_US/sample/ne.q", porc, ".RData"))
  rm(e, ne_s)
}

if (!file.exists(paste0("./final/en_US/sample/bl.q", porc, ".RData"))) {
  bl <- readLines("./final/en_US/en_US.blogs.txt", encoding = "UTF-8", skipNul = TRUE)
  bl <- iconv(bl,"latin1","ASCII",sub = "")
  bl_s <- sample(bl, round(porc * length(bl)))
  saveRDS(bl_s, file = paste0("./final/en_US/sample/bl.q", porc, ".RData"))
  rm(bl, bl_s)
}

if (!file.exists(paste0("./final/en_US/sample/tw.q", porc, ".RData"))) {
  tw <- readLines("./final/en_US/en_US.twitter.txt", encoding = "UTF-8", skipNul = TRUE)
  tw <- iconv(tw,"latin1","ASCII",sub = "")
  tw_s <- sample(tw, round(porc * length(tw)))
  saveRDS(tw_s, file = paste0("./final/en_US/sample/tw.q", porc, ".RData"))
  rm(tw, tw_s)
}

ne <- readRDS(paste0("./final/en_US/sample/ne.q", porc, ".RData"))
bl <- readRDS(paste0("./final/en_US/sample/bl.q", porc, ".RData"))
tw <- readRDS(paste0("./final/en_US/sample/tw.q", porc, ".RData"))

train <- c(ne,bl,tw)
rm(ne,bl,tw)

train = jc.corp(train)

if (!file.exists(paste0("./final/en_US/ngrams/1ng.q", porc, ".RData"))) {
  train1 = jc.toke(train)
  dfTrain1 = tibble(NextWord = train1)
  dfTrain1 = jc.freq(dfTrain1)
  saveRDS(dfTrain1,file = paste0("./final/en_US/ngrams/1ng.q", porc, ".RData"))
  rm(train1, dfTrain1)
}

if (!file.exists(paste0("./final/en_US/ngrams/2ng.q", porc, ".RData"))) {
  train2 = jc.toke(train, 2)
  dfTrain2= tibble(NextWord = train2)
  dfTrain2 = jc.freq(dfTrain2) %>%
    separate(NextWord, c('word1', 'NextWord'), " ")
  saveRDS(dfTrain2,file = paste0("./final/en_US/ngrams/2ng.q", porc, ".RData"))
  rm(train2, dfTrain2)
  }

if (!file.exists(paste0("./final/en_US/ngrams/3ng.q", porc, ".RData"))) {
  train3 = jc.toke(train, 3)
  dfTrain3 = tibble(NextWord = train3)
  dfTrain3 = jc.freq(dfTrain3) %>%
    separate(NextWord, c('word1', 'word2', 'NextWord'), " ")
  saveRDS(dfTrain3,file = paste0("./final/en_US/ngrams/3ng.q", porc, ".RData"))
  rm(train3, dfTrain3)
}
if (!file.exists(paste0("./final/en_US/ngrams/4ng.q", porc, ".RData"))) {
  train4 = jc.toke(train, 4)
  dfTrain4 = tibble(NextWord = train4)
  dfTrain4 = jc.freq(dfTrain4) %>%
    separate(NextWord, c('word1', 'word2', 'word3', 'NextWord'), " ")
  rm(train, train1, train2, train3, train4)
  saveRDS(dfTrain4,file = paste0("./final/en_US/ngrams/4ng.q", porc, ".RData"))
  rm(train4, dfTrain4)
}

rm(train)
dfTrain1 <- saveRDS(paste0("./final/en_US/ngrams/1ng.q", porc, ".RData"))
dfTrain2 <- saveRDS(paste0("./final/en_US/ngrams/2ng.q", porc, ".RData"))
dfTrain3 <- saveRDS(paste0("./final/en_US/ngrams/3ng.q", porc, ".RData"))
dfTrain4 <- saveRDS(paste0("./final/en_US/ngrams/4ng.q", porc, ".RData"))

jc.next("The guy in front of me just bought a pound of bacon, a bouquet, and a case of",5)
jc.next("You're the reason why I smile everyday. Can you follow me please? It would mean the",5)
jc.next("Hey sunshine, can you follow me and make me the", 5)
jc.next("Very early observations on the Bills game: Offense still struggling but the", 5)
