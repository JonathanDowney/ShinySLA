library(shiny)
library(shinyjs)
library(shinydashboard)
library(TAM)

essay1 <- (scan(file = "texts/sample1.txt", what="char"))
sample1 <- gsub("[[:punct:]]", "", unique(tolower(essay1)))
print(sample1)

{
  ui <- dashboardPage(
    dashboardHeader(
      title = "SLAtools.net",
      titleWidth = 220
    ),

    dashboardSidebar(
      useShinyjs(),
      width = 220,
      sidebarMenu(
        id = "sidebar",
        
        #Menu Item: Instructions
        menuItem("Instructions", id = "instructions", tabName = "instructions", icon = icon("bullhorn")),

        #Menu Item: Step 1
                                                                               #https://stackoverflow.com/questions/36495234/conditionalpanel-around-menuitem-doesnt-display-properly
        menuItem("Step 1: Sign-in", tabName = "signIn", icon = icon("address-card")),

        #Menu Item: Step 2
        conditionalPanel(
          condition = "input.signInDone == 1",
          sidebarMenu(                                                                                 #https://stackoverflow.com/questions/36495234/conditionalpanel-around-menuitem-doesnt-display-properly
            menuItem("Step 2: Build vocab list", tabName = "building", icon = icon("clipboard-list"))
          )),

        #Menu Item: Step 3
        conditionalPanel(
          condition = "input.buildingDone == 1",
          sidebarMenu(                                                                                 #https://stackoverflow.com/questions/36495234/conditionalpanel-around-menuitem-doesnt-display-properly
              menuItem("Step 3: Computer analysis", tabName = "analysis", icon = icon("chart-line"))
          )),

        #Menu Item: Step 4
        condition = "input.enter4 == 1",
        sidebarMenu(                                                                                 #https://stackoverflow.com/questions/36495234/conditionalpanel-around-menuitem-doesnt-display-properly
          menuItem("Step 4: Human validation", tabName = "validation", icon = icon("microscope"))
        ))
    ),

    dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
      ),
      tabItems(
        tabItem(tabName = "instructions",
                h2("Instructions"),
                tags$br(),
        ),
        
        tabItem(tabName = "signIn",
                h2("Sign in!"),
                tags$br(),
                actionButton(inputId="signInDone", align="center", label="Done"),
        ),

        tabItem(tabName = "building",
                fluidRow(
                  column(8,
                         style='height:80px', align = "center",
                         tags$h2("Essay Prompt: What is the meaning of life?")
                  )
                ),
                fluidRow(
                  column(8, wellPanel(
                    tabsetPanel(
                      tabPanel(title = "Text 1",
                               div(style = 'overflow-y:scroll;height:400px;',
                                   tags$br(),
                                   textOutput("file1")
                               )
                      ),
                      tabPanel("text2", textOutput("file2")),
                      tabPanel("text3", textOutput("texts/sample1.txt"))
                    )
                  )
                  ),

                  # Word list
                  column(4,
                         wellPanel(
                           fluidRow(
                             column(4, align="center",
                                    textInput("text1", "'Easy' words", value = "word1", width = '100px', placeholder = NULL),
                                    textInput("text2", NULL, value = "word2", width = '100px', placeholder = NULL),
                                    textInput("text3", NULL, value = "word3", width = '100px', placeholder = NULL),
                                    textInput("text4", NULL, value = "word4", width = '100px', placeholder = NULL),
                                    textInput("text5", NULL, value = "word5", width = '100px', placeholder = NULL)
                             ),
                             column(4, align="center",
                                    textInput("text1", "'Mid' words", value = "word1", width = '100px', placeholder = NULL),
                                    textInput("text2", NULL, value = "word2", width = '100px', placeholder = NULL),
                                    textInput("text3", NULL, value = "word3", width = '100px', placeholder = NULL),
                                    textInput("text4", NULL, value = "word4", width = '100px', placeholder = NULL),
                                    textInput("text5", NULL, value = "word5", width = '100px', placeholder = NULL),
                                    
                                    actionButton(inputId="buildingDone", align="center", label="Submit my list"),
                                    textOutput("textout")
                             ),
                             column(4, align="center",
                                    textInput("text1", "'Hard' words", value = "word1", width = '100px', placeholder = NULL),
                                    textInput("text2", NULL, value = "word2", width = '100px', placeholder = NULL),
                                    textInput("text3", NULL, value = "word3", width = '100px', placeholder = NULL),
                                    textInput("text4", NULL, value = "word4", width = '100px', placeholder = NULL),
                                    textInput("text5", NULL, value = "word5", width = '100px', placeholder = NULL)
                             )
                           )
                         )
                      )
                ),
                fluidRow(
                  tags$br(),
                  tags$p("hello", id = "test1"),
                ),
        ),

        tabItem(tabName = "analysis",
                h2("Analysis")
         ),

        tabItem(tabName = "validation",
              h2("Validation")
        )
    )
  )
  )

  server <- function(input, output, session){

    output$file1 <- renderText(essay1)

    data = (data.frame(words = c("word1", "word2", "word3"), stringsAsFactors = FALSE))

    output$text2out <- renderText({
      validate(
        need(input$text1 != '' || input$text2 != '', 'At least one word blank is empty!')
      )
      req(input$text1 == "word1" || input$text2 == "word2")
      "Test cannot be a value"
    })

    observeEvent(input$signInDone, {
      updateTabItems(session, "sidebar", "building")
      addCssClass(selector = "a[data-value='signIn']", class = "inactiveLink")
    })

    observeEvent(input$buildingDone, {
      if(input$text1!=""&&input$text2!="")
      {
        output$textout <- renderText({"BINGO!"})
        updateTabItems(session, "sidebar", "analysis")
        addCssClass(selector = "a[data-value='building']", class = "inactiveLink")
      }
      else
      {
        showModal(modalDialog(title ="Error", "Please fill all the fields before you click the 'Submit' buttion!"))
      }
    })
    
    observeEvent(input$analysisDone, {
      updateTabItems(session, "sidebar", "building")
      addCssClass(selector = "a[data-value='analysis']", class = "inactiveLink")
    })
    
    observeEvent(input$validationDone, {
      updateTabItems(session, "sidebar", "building")
      addCssClass(selector = "a[data-value='validation']", class = "inactiveLink")
    })
  }

  shinyApp(ui, server)
}

