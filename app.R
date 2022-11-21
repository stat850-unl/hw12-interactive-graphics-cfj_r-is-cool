


cocktail <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv")


library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(shinythemes)

#The code below creates an interactive database to choose your own ingredients, the glass type and the measurement of the ingredient that you want.
#These options are tied to each other in a such a way that the choice of glass is dependent on the ingredient chosen, and the measurement also depends on the previous selection on 
#ingredient and glass. One cool thing is, you're able also able view the image of the cocktail that you have made by clicking on the 
# link below the drink_thumb column.

is.not.null <- function(x) !is.null(x)

ui <- fluidPage(theme = shinytheme("united"),
  titlePanel("Select any of the options below to make your favorite cocktail"),
  sidebarLayout(
    sidebarPanel( width = 3,
                  uiOutput("ingredient"),
                  uiOutput("glass"),
                  uiOutput("measure")
                  
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Table", DT::dataTableOutput("table_subset"))
      )
      
    )
  )
)


################################################

server = shinyServer(function(input,output){
  cocktail$drink_thumb <- paste0("<a href='",cocktail$drink_thumb,"'>",cocktail$drink_thumb,"</a>")
  data <- cocktail
  
  output$table <- DT::renderDataTable({
    if(is.null(data)){return()}
    DT::datatable(data, options = list(scrollX = T))
  })
  
  output$ingredient <- renderUI({
    selectInput(inputId = "ingredient", "Select ingredient",choices = var_ingredient(), multiple = F)
  })
  output$glass <- renderUI({
    selectInput(inputId = "glass", "Select glass",choices = var_glass(), multiple = T)
  })
  output$measure <- renderUI({
    selectInput(inputId = "measure", "Select measure",choices = var_measure(), multiple = T)
  })
  
  # Filtered data
  data_filtered <- reactive({
    filter(cocktail, ingredient %in% ingredient(), glass %in% glass(), measure %in% measure())
  })
  
  # Get filters from inputs
  ingredient <- reactive({
    if (is.null(input$ingredient)) unique(cocktail$ingredient) else input$ingredient
  })
  
  glass <- reactive({
    if (is.null(input$glass)) unique(cocktail$glass) else input$glass
  })
  
  measure <- reactive({
    if (is.null(input$measure)) unique(cocktail$measure) else input$measure
  })
  
  # Get available categories
  var_ingredient <- reactive({
    file1 <- data
    if(is.null(data)){return()}
    as.list(unique(file1$ingredient))
  })
  
  var_glass <- reactive({
    filter(data, ingredient %in% ingredient()) %>% 
      pull(glass) %>% 
      unique()
  })
  
  var_measure <- reactive({
    filter(data, ingredient %in% ingredient(), glass %in% glass()) %>% 
      pull(measure) %>% 
      unique()
  })
  
  output$table_subset <- DT::renderDataTable({
    DT::datatable(data_filtered(), options = list(scrollX = T), escape = FALSE)
  })
  
})

shinyApp(ui, server)
