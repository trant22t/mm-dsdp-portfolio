library(shiny)

shinyUI(fluidPage(
  # Application title
  titlePanel("Which poem is the most similar to your chosen Emily Dickinson's poem?"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("pickednum", 
                   "Number of a Dickinson's poem (from 1 to 441)", 
                   1, min=1, max=441, step=1),
      actionButton("go", "Go!")
    ),
    # Show a plot of the generated distribution
    mainPanel(
      p("Pick a number from 1 to 441 to display the associated Dickinson's poem 
        and its most similar one in terms of cosine similarity out of the collection of 441 poems 
        by Emily Dickinson from the Gutenberg edition. Cosine similarity between two poems is displayed 
        at the bottom of the page."),
      verbatimTextOutput("displaypoem"), 
      textOutput("text"))
    )
  )
  )