require(mosaic)
require(stringr)
require(tm)
require(magrittr)
require(shiny)

cname <- file.path("/home/mhc/class17/m5ttranthe", "gutenberg")
docs <- Corpus(DirSource(cname))
docs1 <- docs %>% 
  tm_map(stripWhitespace) %>%
  tm_map(removePunctuation) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeNumbers)

shinyServer(function(input, output) {
  
  getWord <- function(num) {
    poem <- as.character(docs1[[num]])
    idx <- grep("[a-z]+", poem)
    words <- str_extract_all(poem[idx], "[a-z]+")
    words <- words[-1]
    words <- unlist(words)
    words <- words[!is.na(words)]
    return(words)
  }
  
  compare2Text <- function(num1, num2) {
    poem1 <- getWord(num1)
    poem2 <- getWord(num2)
    wordlist <- c(poem1, poem2)
    wordlist <- unique(wordlist)
    c1 <- vector(mode = "numeric", length = length(wordlist))
    c2 <- vector(mode = "numeric", length = length(wordlist))
    for (i in 1: length(wordlist)) {
      c1[i] <- length(grep(paste(wordlist[i]), poem1))
      c2[i] <- length(grep(paste(wordlist[i]), poem2))
    }
    cossim <- crossprod(c1, c2)/sqrt(crossprod(c1) * crossprod(c2))
    return(as.numeric(cossim))
  }
  
  compareAllText <- function(num) {
    result <- vector("list", 2)
    cossim <- vector(mode = "numeric", length = 442)
    for (i in 1:443) {
      cossim[i] = if_else(i!= num, compare2Text(num, i), 0)
    }
    result[1] <- which.max(cossim)
    result[2] <- cossim[which.max(cossim)]
    return(result)
  }
  
  pickednum <- eventReactive(input$go, {
    pickednum <- input$pickednum
  })
  
  output$displaypoem <- renderPrint ({
    docs %>% extract2(pickednum()) %>% as.character() %>% writeLines()
    docs %>% extract2(compareAllText(pickednum())[[1]]) %>% as.character() %>% writeLines()
  })
  
  output$text <- renderPrint({paste("The cosine similarity between the two poems is", round(compareAllText(pickednum())[[2]],4))})
  
})