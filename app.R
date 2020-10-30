library(shiny)
library(shinydashboard)
library(TAM)

sample1 <- unique(tolower(scan(file = "texts/sample1.txt", what="char")))
sample1 <- gsub("[[:punct:]]", "", sample1)
print(sample1)
{
  ui <- fluidPage(
          theme = "styles.css",
          fluidRow("Enter your list!"),
          fluidRow(
            column(4, offset = 8,
              wellPanel(
                fluidRow(
                  column(12, align="center",
                    textInput("text1", "Word list", value = "word1", width = '100px', placeholder = NULL),
                    textInput("text2", NULL, value = "word2", width = '100px', placeholder = NULL)
                  )
                )
              )
            )
          ),
          fluidRow(
            actionButton(inputId="enter",label="Submit my list"),
            tags$br(),
            tags$p("hello", id = "test1"),
            textOutput("textout")
          )
      )

  server <- function(input, output){
    data = (data.frame(words = c("word1", "word2", "word3"), stringsAsFactors = FALSE))

    output$text2out <- renderText({
      validate(
        need(input$text1 != '' || input$text2 != '', 'At least one word blank is empty!')
      )
      req(input$text1 == "word1" || input$text2 == "word2")
      "Test cannot be a value"
    })

    observeEvent(input$enter, {
      if(input$text1!=""&&input$text2!="")
      {
        output$textout <- renderText({"BINGO!"})
      }
      else
      {
        showModal(modalDialog(title ="Error", "Please fill all the fields before you click the 'Submit' buttion!"))
      }
    })
  }

  shinyApp(ui, server)
}
