library(shiny)
library(TAM)
library(rhandsontable)

{
  ui <- fluidPage(
        tags$p("Enter your list!"),
        tags$br(),
        rHandsontableOutput("hot"),
        tags$br(),
        actionButton(inputId="enter",label="Submit my list"),
        tags$br(),
        tags$br(),
        rHandsontableOutput("hot2"),
      )
        
  server <- function(input, output){
    data = (data.frame(words = c("word1", "word2", "word3"), stringsAsFactors = FALSE))
    output$hot <- renderRHandsontable({rhandsontable(data) %>%
        # ?hot_validate_character for red cell formatting
        hot_cols(validator = "
           function (value, callback) {
              callback(value == ('test'));
           }", allowInvalid = TRUE)
    })
    
    observeEvent(input$enter, {
      DF=hot_to_r(input$hot)
      print(DF)
      DF2 <- as.data.frame(DF[[2,1]])
      D1 <- read.csv("testfile.csv")
      print(head(D1))
      mod1 <- tam(D1)  
      print(mod1)
      output$hot2 <- renderRHandsontable({rhandsontable(DF2)})
      
    })
    
  }
  
  shinyApp(ui, server)
}
