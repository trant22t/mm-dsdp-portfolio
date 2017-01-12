library(rjson)
library(jsonlite)
library(dplyr)
library(mosaic)
library(shinythemes)
library(tm)
library(SnowballC)
library(wordcloud)
library(cowplot)
library(ggplot2)

# we display these for the user to choose a category on first drop-down
categories <- c("Clothing, Shoes, and Jewelry", "Health and Personal Care", 
                "Sports and Outdoors")

# load dataframes with top ten reviews from each category
# files written instead of streamed from json to save time
setwd("/home/mhc/class17/m5ttranthe/Git/STAT495-Group1/Data")
load("clothing.Rda")
load("health.Rda")
load("sports.Rda")


# SENTIMENT PACAKGE FUNCTIONS BY TIMOTHY JURKA

create_matrix <- function(textColumns, language="english", minDocFreq=1, minWordLength=3, removeNumbers=TRUE, removePunctuation=TRUE, removeSparseTerms=0, removeStopwords=TRUE, stemWords=FALSE, stripWhitespace=TRUE, toLower=TRUE, weighting=weightTf) {
  
  stem_words <- function(x) {
    split <- strsplit(x," ")
    return(wordStem(split[[1]],language=language))
  }
  
  control <- list(language=language,tolower=toLower,removeNumbers=removeNumbers,removePunctuation=removePunctuation,stripWhitespace=stripWhitespace,minWordLength=minWordLength,stopwords=removeStopwords,minDocFreq=minDocFreq,weighting=weighting)
  
  if (stemWords == TRUE) control <- append(control,list(stemming=stem_words),after=6)
  
  trainingColumn <- apply(as.matrix(textColumns),1,paste,collapse=" ")
  trainingColumn <- sapply(as.vector(trainingColumn,mode="character"),iconv,to="UTF8",sub="byte")
  
  corpus <- Corpus(VectorSource(trainingColumn),readerControl=list(language=language))
  matrix <- DocumentTermMatrix(corpus,control=control);
  if (removeSparseTerms > 0) matrix <- removeSparseTerms(matrix,removeSparseTerms)
  
  gc()
  return(matrix)
}

classify_emotion <- function(textColumns,algorithm="bayes",prior=1.0,verbose=FALSE,...) {
  matrix <- create_matrix(textColumns,...)
  lexicon <- read.csv("emotions.csv", header=FALSE)
  
  counts <- list(anger=length(which(lexicon[,2]=="anger")),disgust=length(which(lexicon[,2]=="disgust")),fear=length(which(lexicon[,2]=="fear")),joy=length(which(lexicon[,2]=="joy")),sadness=length(which(lexicon[,2]=="sadness")),surprise=length(which(lexicon[,2]=="surprise")),total=nrow(lexicon))
  documents <- c()
  
  for (i in 1:nrow(matrix)) {
    if (verbose) print(paste("DOCUMENT",i))
    scores <- list(anger=0,disgust=0,fear=0,joy=0,sadness=0,surprise=0)
    doc <- matrix[i,]
    words <- findFreqTerms(doc,lowfreq=1)
    
    numanger <- 0
    numdisgust <- 0
    numfear <- 0
    numjoy <- 0
    numsad <- 0
    numsurprise <- 0
    
    for (word in words) {
      for (key in names(scores)) {
        emotions <- lexicon[which(lexicon[,2]==key),]
        index <- pmatch(word,emotions[,1],nomatch=0)
        if (index > 0) {
          entry <- emotions[index,]
          
          category <- as.character(entry[[2]])
          count <- counts[[category]]
          
          if (category=="anger") numanger <- numanger + 1
          if (category=="disgust") numdisgust <- numdisgust + 1
          if (category=="fear") numfear <- numfear + 1
          if (category=="joy") numjoy <- numjoy + 1
          if (category=="sadness") numsad <- numsad + 1
          if (category=="surprise") numsurprise <- numsurprise + 1
          
          score <- 1.0
          if (algorithm=="bayes") score <- abs(log(score*prior/count))
          
          if (verbose) {
            print(paste("WORD:",word,"CAT:",category,"SCORE:",score))
          }
          
          scores[[category]] <- scores[[category]]+score
        }
      }
    }
    
    if (algorithm=="bayes") {
      for (key in names(scores)) {
        count <- counts[[key]]
        total <- counts[["total"]]
        score <- abs(log(count/total))
        scores[[key]] <- scores[[key]]+score
      }
    } else {
      for (key in names(scores)) {
        scores[[key]] <- scores[[key]]+0.000001
      }
    }
    
    best_fit <- names(scores)[which.max(unlist(scores))]
    if (best_fit == "disgust" && as.numeric(unlist(scores[2]))-3.09234 < .01) best_fit <- NA
    documents <- rbind(documents,c(numanger,numdisgust,numfear,numjoy,numsad,numsurprise,scores$anger,scores$disgust,scores$fear,scores$joy,scores$sadness,scores$surprise,best_fit))
  }
  
  colnames(documents) <- c("# ANGER","# DISGUST","# FEAR","# JOY","# SADNESS","# SURPRISE","ANGER SCORE","DISGUST SCORE","FEAR SCORE","JOY SCORE","SADNESS SCORE","SURPRISE SCORE","BEST_FIT")
  return(documents)
}

classify_polarity <- function(textColumns,algorithm="bayes",pstrong=0.5,pweak=1.0,prior=1.0,verbose=FALSE,...) {
  matrix <- create_matrix(textColumns,...)
  lexicon <- read.csv("subjectivity.csv",header=FALSE)
  
  counts <- list(positive=length(which(lexicon[,3]=="positive")),negative=length(which(lexicon[,3]=="negative")),total=nrow(lexicon))
  documents <- c()
  
  for (i in 1:nrow(matrix)) {
    if (verbose) print(paste("DOCUMENT",i))
    scores <- list(positive=0,negative=0)
    doc <- matrix[i,]
    words <- findFreqTerms(doc,lowfreq=1)
    
    numpos <- 0
    numneg <- 0 
    
    for (word in words) {
      index <- pmatch(word,lexicon[,1],nomatch=0)
      if (index > 0) {
        entry <- lexicon[index,]
        polarity <- as.character(entry[[2]])
        category <- as.character(entry[[3]])
        count <- counts[[category]]
        
        if (category=="positive") numpos <- numpos + 1
        if (category=="negative") numneg <- numneg + 1 
        
        score <- pweak
        if (polarity == "strongsubj") score <- pstrong
        if (algorithm=="bayes") score <- abs(log(score*prior/count))
        
        if (verbose) {
          print(paste("WORD:",word,"CAT:",category,"POL:",polarity,"SCORE:",score))
        }
        
        scores[[category]] <- scores[[category]]+score
      }		
    }
    
    if (algorithm=="bayes") {
      for (key in names(scores)) {
        count <- counts[[key]]
        total <- counts[["total"]]
        score <- abs(log(count/total))
        scores[[key]] <- scores[[key]]+score
      }
    } else {
      for (key in names(scores)) {
        scores[[key]] <- scores[[key]]+0.000001
      }
    }
    
    best_fit <- names(scores)[which.max(unlist(scores))]
    ratio <- as.integer(abs(scores$positive/scores$negative))
    if (ratio==1) best_fit <- "neutral"
    documents <- rbind(documents,c(numpos,numneg,scores$positive,scores$negative,abs(scores$positive/scores$negative),best_fit))
    if (verbose) {
      print(paste("POS:",scores$positive,"NEG:",scores$negative,"RATIO:",abs(scores$positive/scores$negative)))
      cat("\n")
    }
  }
  
  colnames(documents) <- c("#POS","#NEG","POS SCORE","NEG SCORE","POS/NEG","BEST_FIT")
  return(documents)
}

# OUR HELPER FUNCTIONS

#inputs a product name and returns clean product reviews
grab_reviews <- function(category, product) {
  product_reviews <- filter(category, name == product)
  return(product_reviews$cleanText)
}

# does the sentiment analysis and returns a dataframe with results
sentimental_analysis <- function(review_txt) {
  #classify emotion 
  class_emo <- classify_emotion(review_txt, algorithm="bayes", prior=1.0)
  # get emotion best fit
  emotion <- class_emo[,13]
  # substitute NA's by "unknown"
  emotion[is.na(emotion)] <- "unknown"
  
  #classify polarity
  class_pol <- classify_polarity(review_txt, algorithm="bayes")
  # get polarity best fit
  polarity <- class_pol[,6]
  
  
  # data frame with results
  general <- data.frame(text=review_txt, emotion=emotion, 
                        polarity=polarity, stringsAsFactors=FALSE)
  
  # sort data frame
  general <- within(general,
                    emotion <- factor(emotion, 
                                      levels=names(sort(table(emotion), decreasing=TRUE))))
  
  return(general)
}



# SHINY BEGINS HERE

ui <- shinyUI(fluidPage(
  
  theme = shinytheme("cerulean"),
  
  # Application title
  titlePanel("Sentiment Analysis of Amazon Reviews"),
  
  sidebarLayout(
    sidebarPanel(
      # first drop-down menu to choose word
      selectInput("category",
                  label = "Choose an Amazon category",
                  choices = categories, 
                  selected = "Clothing, Shoes, and Jewelry")),
    mainPanel(
      # second drop-down menu to choose product
      uiOutput("productSelector"),
      
      actionButton("visualize", "Visualize!"),
      
      tabsetPanel(
        tabPanel("Bar Graph", plotOutput("bargraphs")), 
        tabPanel("Word Cloud", plotOutput("wordcloud")), 
        tabPanel("Create Your Own Review", 
                 textInput("newReview", "What do you think about this product?"),
                 actionButton("button", "Analyze"),
                 verbatimTextOutput("analyzeUser1"),
                 verbatimTextOutput("analyzeUser2"),
                 verbatimTextOutput("analyzeUser3"))
      )
    ))
))


server <- shinyServer(function(input, output) {
  # find the poems with the shakespearean word 
  output$productSelector <- renderUI({
    
    if(input$category == "Clothing, Shoes, and Jewelry") {
      # group list of products
      clothing_products <- clothing %>%
        group_by(name) %>%
        summarize(count = n())
      
      # what we want to show user
      products <- as.character(clothing_products[["name"]])
    }
    
    else if(input$category == "Health and Personal Care") {
      # group list of products
      health_products <- health %>%
        group_by(name) %>%
        summarize(count = n())
      
      # what we want to show user
      products <- as.character(health_products[["name"]])
    }
    
    else if(input$category == "Sports and Outdoors") {
      # group list of products
      sports_products <- sports %>%
        group_by(name) %>%
        summarize(count = n())
      
      # what we want to show user
      products <- as.character(sports_products[["name"]])
    }
    
    # make second drop-down menu 
    selectInput("products",
                label = "Choose a product",
                choices = products)
    
  })
  
  products <- eventReactive(input$visualize, {
    input$products
  })
  
  # show the positivity plot
  output$bargraphs <- renderPlot({
    # go from category name used in app to the dataframe name
    if(input$category == "Clothing, Shoes, and Jewelry") {
      cat <- clothing
    }
    if(input$category == "Health and Personal Care") {
      cat <- health
    }
    if(input$category == "Sports and Outdoors") {
      cat <- sports
    }
    
    #make product name back into string
    product <- as.String(products())
    
    #get the reviews and do sentimental analysis
    review_text <- grab_reviews(cat, product)
    general <- sentimental_analysis(review_text)
    
    # plot distribution of polarity
    polarityplot <- ggplot(general, aes(x=polarity)) +
      geom_bar(aes(y=..count.., fill=polarity)) +
      scale_fill_brewer(palette="RdGy") +
      labs(x="Polarity Categories", y="Number of Reviews", 
           title = "Polarity")
    
    # plot distribution of emotions
    emotionplot <- ggplot(general, aes(x=emotion)) +
      geom_bar(aes(y=..count.., fill=emotion)) +
      scale_fill_brewer(palette="Dark2") +
      labs(x="Emotion Categories", y="Number of Reviews", 
           title = "Emotions")
    
    plot_grid(polarityplot, emotionplot)
    
    
  })
  
  # show the emotions plot
  output$wordcloud <- renderPlot({
    # go from category name used in app to the dataframe name
    if(input$category == "Clothing, Shoes, and Jewelry") {
      cat <- clothing
    }
    if(input$category == "Health and Personal Care") {
      cat <- health
    }
    if(input$category == "Sports and Outdoors") {
      cat <- sports
    }
    
    #make product name back into string
    product <- as.String(input$products)
    
    #get the reviews and do sentimental analysis
    review_text <- grab_reviews(cat, product)
    general <- sentimental_analysis(review_text)
    
    # separating text by emotion
    emos <- levels(factor(general$emotion))
    nemo <- length(emos)
    emo_docs <- rep("", nemo)
    for (i in 1:nemo)
    {
      tmp <- review_text[general$emotion == emos[i]]
      emo_docs[i] <- paste(tmp, collapse=" ")
    }
    
    # remove stopwords
    emo_docs <- removeWords(emo_docs, stopwords("english"))
    # create corpus
    corpus <- Corpus(VectorSource(emo_docs))
    tdm <- TermDocumentMatrix(corpus)
    tdm <- as.matrix(tdm)
    colnames(tdm) <- emos
    
    # comparison word cloud
    comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
                     scale = c(3,.5), random.order = FALSE, title.size = 1.5)
  })
  
  # create your own review feature
  
  newGraphs <- eventReactive(input$button, {
    input$newReview
  })
  
  output$analyzeUser1 <- renderText({
    #classify emotion 
    class_emo <- classify_emotion(newGraphs(), algorithm="bayes", prior=1.0)
    print(paste("#ANGRY:",class_emo[1],"#DISGUST:",class_emo[2],"#FEAR:",class_emo[3],"#JOY:",class_emo[4],
                "#SADNESS:",class_emo[5],"#SURPRISE:",class_emo[6],"\n","ANGER SCORE:",round(as.numeric(class_emo[7]),3),
                "DISGUST SCORE:",round(as.numeric(class_emo[8]),3),"FEAR SCORE:",round(as.numeric(class_emo[9]),3),"JOY SCORE:",round(as.numeric(class_emo[10]),3),"SADNESS SCORE:",round(as.numeric(class_emo[11]),3),
                "SURPRISE SCORE:",round(as.numeric(class_emo[12]),3),"\n","BEST FIT:",class_emo[13]))
  })
  
  output$analyzeUser2 <- renderText({
    #classify polarity
    class_pol <- classify_polarity(newGraphs(), algorithm="bayes")
    print(paste("#POS:",class_pol[1],"#NEG:",class_pol[2],"\n",
                "POS SCORE:",round(as.numeric(class_pol[3]),3),"NEG SCORE:",round(as.numeric(class_pol[4]),3),
                "\n","POS/NEG:",round(as.numeric(class_pol[5]),3),"BEST FIT:",class_pol[6]))
  })
  
  output$analyzeUser3 <- renderText({
    #classify emotion 
    class_emo <- classify_emotion(newGraphs(), algorithm="bayes", prior=1.0)
    # get emotion best fit
    emotion <- class_emo[,13]
    # substitute NA's by "unknown"
    emotion[is.na(emotion)] <- "unknown"
    
    #classify polarity
    class_pol <- classify_polarity(newGraphs(), algorithm="bayes")
    # get polarity best fit
    polarity <- class_pol[,6]
    
    print(c(polarity, emotion))
    
  })
  
  
})

# Run the application 
shinyApp(ui = ui, server = server)