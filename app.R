
<<<<<<< HEAD
library(shiny)

# # Define UI for application that draws a histogram
# ui <- fluidPage(
# 
#     # Application title
#     titlePanel("Old Faithful Geyser Data"),
# 
#     # Sidebar with a slider input for number of bins 
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("bins",
#                         "Number of bins:",
#                         min = 1,
#                         max = 50,
#                         value = 30)
#         ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#            plotOutput("distPlot")
#         )
#     )
# )
# 
# # Define server logic required to draw a histogram
# server <- function(input, output) {
# 
#     output$distPlot <- renderPlot({
#         # generate bins based on input$bins from ui.R
#         x    <- faithful[, 2]
#         bins <- seq(min(x), max(x), length.out = input$bins + 1)
# 
#         # draw the histogram with the specified number of bins
#         hist(x, breaks = bins, col = 'darkgray', border = 'white',
#              xlab = 'Waiting time to next eruption (in mins)',
#              main = 'Histogram of waiting times')
#     })
# }
# 
# # Run the application 
# shinyApp(ui = ui, server = server)


install.packages("shinythemes")
install.packages("wordcloud")
install.packages("tm")
#Cocktail 
library(RCurl) 
=======


cocktail <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv")


>>>>>>> f5b726d2ee664c4bcd0cabfaf77d9da1c66af6f6
library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(shinythemes)

<<<<<<< HEAD
# Define UI for application that draws a histogram
ui <- fluidPage( theme = shinytheme("adventurous"),
                 titlePanel("Going Wild on Cocktails"),
                 navbarPage(
                   ">>>>>",
                   tabPanel("Overall Exploration",
                            sidebarPanel(
                              "INPUT",
                              radioButtons("button","Regular flavors in cocktails:",
                                           choices = c("Regular cocktail","Custom cocktails"),
                                           select="Custom cocktails"),
                            ),
                            mainPanel(
                              plotOutput("plot")
                            )
                            
                   ),
                   tabPanel("Indivial Exploration",
                            sidebarPanel(
                              selectInput("name","Drink name",
                                          choices=unique(cocktails$drink),
                                          selected = "ABC"),
                              selectInput("req","Requirements",choices=c("Status","Ingredients","None"),
                                          selected = "None"),
                              conditionalPanel('input.req=="Status"',
                                               checkboxGroupInput("chrac","Characteristics of the cocktail",
                                                                  choices = names(cocktails)[c(2,5,6,8)],
                                                                  selected=names(cocktails)[8])),
                              conditionalPanel('input.req =="Ingredients"',
                                               selectInput("op"," measure of the ingredients",choices=c("Yes","No"),
                                                           selected = "No")
                              )               
                              
                            ),
                            mainPanel(
                              dataTableOutput("t1")
                            )
                   ),
                   tabPanel("Order_placed",
                            sidebarPanel(
                              selectInput("name2","Drink name",
                                          choices=unique(cocktails$drink),
                                          selected = "ABC"),
                              radioButtons("pic","Picture of the cocktail",choices = c("Yes","No"),
                                           selected = "No"),
                              conditionalPanel(condition='input.pic=="Yes"',
                                               p("Have a fun night", strong("and enjoy your drink"))),
                            ),
                            mainPanel(
                              conditionalPanel(condition='input.pic=="Yes"',htmlOutput("picture")),
                              
                              conditionalPanel(condition='input.pic=="No"',
                                               p("Have a fun night", strong("Your order will be ready soon")))
                            )
                   )
                   
                 )
                 
)

# Histogram
server <- function(input, output) {
  output$plot<-renderPlot({
    if(input$butt=="Custom cocktail"){
      data<-cocktails %>%
        filter(alcoholic=="Custom") %>%
        select(ingredient)
      text <- data$ingredient
      docs <- Corpus(VectorSource(text))
      dtm <- TermDocumentMatrix(docs)
      matrix <- as.matrix(dtm)
      words <- sort(rowSums(matrix),decreasing=TRUE)
      df <- data.frame(word = names(words),freq=words)
      
      set.seed(1234) 
      wordcloud(words = df$word, freq = df$freq, min.freq = 1,max.words=200,random.order=FALSE,rot.per=0.35,
                colors=brewer.pal(8, "Egg Cream"))
    } else {
      data<-cocktails %>%
        filter(alcoholic!="Custom") %>%
        select(ingredient)
      text <- data$ingredient
      docs <- Corpus(VectorSource(text))
      dtm <- TermDocumentMatrix(docs)
      matrix <- as.matrix(dtm)
      words <- sort(rowSums(matrix),decreasing=TRUE)
      df <- data.frame(word = names(words),freq=words)
      
      set.seed(1234) 
      wordcloud(words = df$word, freq = df$freq, min.freq = 1,max.words=200,random.order=FALSE,rot.per=0.35,
                colors=brewer.pal(8, "Egg Cream"))
      
    }
=======
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
>>>>>>> f5b726d2ee664c4bcd0cabfaf77d9da1c66af6f6
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
<<<<<<< HEAD
    src = im$drink_thumb
    c('<img src="',src,'">')})
  
  output$t1<-renderDataTable({
    if (input$req=="Ingredients" & input$op=="No"){
      cocktails %>%
        filter(drink==input$name) %>%
        select(ingredient)
    }
    else if(input$req=="Ingredients" & input$op=="Yes"){
      cocktails %>%
        filter(drink==input$name) %>%
        select(ingredient,measure)
    } else if(input$req=="Status" & length(input$chrac)==1 & input$chrac[1]=='Custom'){
      cocktails %>%
        filter(drink==input$name) %>%
        select(Custom) %>%
        unique()
    } else if(input$req=="Status" & length(input$chrac)==1 & input$chrac[1]=='category'){
      cocktails %>%
        filter(drink==input$name) %>%
        select(category) %>%
        unique()
    }
    else if(input$req=="Status" & length(input$chrac)==1 & input$chrac[1]=='glass'){
      cocktails %>%
        filter(drink==input$name) %>%
        select(glass) %>%
        unique()
    } else if(input$req=="Status" & length(input$chrac)==2 & input$chrac[1]=="Custom" & input$chrac[2]=="category"){
      cocktails %>%
        filter(drink==input$name) %>%
        select(Custom,category) %>%
        unique()
    } else if(input$req=="Status" & length(input$chrac)==2 & input$chrac[1]=="category" &
              input$chrac[2]=="glass"){
      cocktails %>%
        filter(drink==input$name) %>%
        select(category,glass) %>%
        unique()
    } else if(input$req=="Status" & length(input$chrac)==2 & input$chrac[1]=="Custom" &
              input$chrac[2]=="glass"){
      cocktails %>%
        filter(drink==input$name) %>%
        select(Custom,glass) %>%
        unique()
    } else if(input$req=="Status" & length(input$chrac)==3 & input$chrac[1]=="Custom" & input$chrac[2]=="category" &
              input$chrac[3]=="glass"){
      cocktails %>%
        filter(drink==input$name) %>%
        select(Custom,category,glass) %>%
        unique()
    }
  }) 
=======
  })
  
  output$table_subset <- DT::renderDataTable({
    DT::datatable(data_filtered(), options = list(scrollX = T), escape = FALSE)
  })
>>>>>>> f5b726d2ee664c4bcd0cabfaf77d9da1c66af6f6
  
})

<<<<<<< HEAD

=======
shinyApp(ui, server)
>>>>>>> f5b726d2ee664c4bcd0cabfaf77d9da1c66af6f6
