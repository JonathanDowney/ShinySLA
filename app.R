library(shiny)
library(shinyjs)
library(shinydashboard)
library(TAM)

working_directory <- "/home/sixohthree/1016test/ICNALE_W_CHN_B2_0_N026"

setwd(working_directory)

fileslist <- list.files(path = working_directory)

for (i in fileslist){
  #scan files in from "file list"
  corpusfile <- scan(file = i, what="char")
}



essay1 <- (scan(file = fileslist[1], what="char"))
essay2 <- (scan(file = fileslist[2], what="char"))
essay3 <- (scan(file = fileslist[3], what="char"))

# sample1 <- gsub("[[:punct:]]", "", unique(tolower(essay1)))
print(head(corpusfile))

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

        menuItem("Instructions", tabName = "instructions", icon = icon("bullhorn"), badgeColor = "red"),
        
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
              menuItem("Step 3: Essay rating", tabName = "rating", icon = icon("chart-line"))
          )),
        
        #Menu Item: Step 4
        conditionalPanel(
          condition = "input.ratingDone == 1",
          sidebarMenu(                                                                                 #https://stackoverflow.com/questions/36495234/conditionalpanel-around-menuitem-doesnt-display-properly
            menuItem("Step 4: Computer analysis", tabName = "analysis", icon = icon("chart-line"))
          )),

        #Menu Item: Step 5
        conditionalPanel(
          condition = "input.analysisDone == 1",
          sidebarMenu(                                                                                 #https://stackoverflow.com/questions/36495234/conditionalpanel-around-menuitem-doesnt-display-properly
            menuItem("Step 5: Human validation", tabName = "validation", icon = icon("microscope"))
        )),
      
      #Menu Item: Thanks!
      conditionalPanel(
        condition = "input.validationDone == 1",
        sidebarMenu(                                                                                 #https://stackoverflow.com/questions/36495234/conditionalpanel-around-menuitem-doesnt-display-properly
          menuItem("Thanks!!", tabName = "thanks", icon = icon("child"))
      ))
    )
    ),

    dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
      ),
      tabItems(
        
        ### INSTRUCTIONS ###
        
        tabItem(tabName = "instructions",
                h2("Instructions"),
                tags$br(),
        ),
        
        ### SIGN IN ###
        
        tabItem(tabName = "signIn",
                h2("Sign in!"),
                fluidRow(
                  column(4, align="center",
                    wellPanel(
                      textInput("signIn1", "Identifer", value = "Identifier", width = '100px', placeholder = NULL),
                      textInput("signIn1", "Age", value = "Age", width = '100px', placeholder = NULL),
                      textInput("signIn1", "Occupation", value = "Occupation", width = '100px', placeholder = NULL)
                    )
                  ),
                tags$br(),
                actionButton(inputId="signInDone", align="center", label="Done"),
                )
        ),
        
        ### LIST BUILDING ###
        
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
                                   textOutput("file1A")
                               )
                      ),
                      tabPanel(title = "text2",
                               tags$br(),
                               textOutput("file2A")
                               ),
                      
                      tabPanel(title = "text3",
                               tags$br(),
                               textOutput("file3A")
                      )
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

        ### ESSAY RATING ###  
        
        tabItem(tabName = "rating",
                h2("Essay Rating"),
                
                fluidRow(
                  column(8, wellPanel(
                    tabsetPanel(
                      tabPanel(title = "Text 1",
                               div(style = 'overflow-y:scroll;height:400px;',
                                   tags$br(),
                                   textOutput("file1B")
                               )
                      ),
                      tabPanel(title = "text2",
                               tags$br(),
                               textOutput("file2B")
                      ),
                      
                      tabPanel(title = "text3",
                               tags$br(),
                               textOutput("file1C")
                      )
                    )
                  )
                  ),
                  
                  column(4, align="center",
                         wellPanel(
                           "your LIST"
                         )
                  ),
                  
                  column(4, align="center",
                         wellPanel(
                           selectInput("essayA", "Rating: ESSAY A",
                                       c("Choose one" = "", 1:10)),
                           selectInput("essayB", "Rating: ESSAY B",
                                       c("Choose one" = "", 1:10)),
                           selectInput("essayC", "Rating: ESSAY C",
                                       c("Choose one" = "", 1:10)),
                         )
                  )
                ),
                actionButton(inputId="ratingDone", align="center", label="Done"),
                
        ),
        
         ### ANALYSIS ###  
        
        tabItem(tabName = "analysis",
                h2("Analysis"),
                p("Please wait until analysis is complete. This could take a couple minutes if the server is busy."),
                actionButton(inputId="analysisDone", align="center", label="Done"),
         ),
        
        ### VALIDATION ###  
        
        tabItem(tabName = "validation",
              h2("Validation"),
              actionButton(inputId="validationDone", align="center", label="Done"),
              
        ),
        
        ### THANKS ###  
        
        tabItem(tabName = "thanks",
                h2("THANKS!")

        )
    )
  )
  )

  server <- function(input, output, session){

    ### SIGN IN ###

    observeEvent(input$signInDone, {
      updateTabItems(session, "sidebar", "building")
      addCssClass(selector = "a[data-value='signIn']", class = "inactiveLink")
    })

    ### LIST BUILDING ###
    
    output$file1A <- renderText(essay1)
    output$file2A <- renderText(essay2)
    output$file3A <- renderText(essay3)
    
    output$file1B <- renderText(essay1)
    output$file2B <- renderText(essay2)
    output$file3B <- renderText(essay3)
    
    data = (data.frame(words = c("word1", "word2", "word3"), stringsAsFactors = FALSE))
    
    output$text2out <- renderText({
      validate(
        need(input$text1 != '' || input$text2 != '', 'At least one word blank is empty!')
      )
      req(input$text1 == "word1" || input$text2 == "word2")
      "Test cannot be a value"
    })
    
    observeEvent(input$buildingDone, {
      if(input$text1!=""&&input$text2!="")
      {
        output$textout <- renderText({"BINGO!"})
        updateTabItems(session, "sidebar", "rating")
        addCssClass(selector = "a[data-value='building']", class = "inactiveLink")
      }
      else
      {
        showModal(modalDialog(title ="Error", "Please fill all the fields before you click the 'Submit' buttion!"))
      }
    })
    
    ### ESSAY RATING ###
    
    observeEvent(input$ratingDone, {
      updateTabItems(session, "sidebar", "analysis")
      addCssClass(selector = "a[data-value='rating']", class = "inactiveLink")
    })
    
    ### ANALYSIS ###
    
    observeEvent(input$analysisDone, {
      updateTabItems(session, "sidebar", "validation")
      addCssClass(selector = "a[data-value='analysis']", class = "inactiveLink")
    })
    
    ### VALIDATION ###
    
    observeEvent(input$validationDone, {
      updateTabItems(session, "sidebar", "thanks")
      addCssClass(selector = "a[data-value='validation']", class = "inactiveLink")
    })
  }

  shinyApp(ui, server)
}

