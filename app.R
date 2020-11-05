library(shiny)
library(shinyjs)
library(shinydashboard)
library(TAM)

ratingList <- NULL

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
          condition = "input.buildingDone == 1 && output.listValidate == 'List Validated!'",
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
                           textInput("signIn2", "Age", value = "Age", width = '100px', placeholder = NULL),
                           textInput("signIn3", "Occupation", value = "Occupation", width = '100px', placeholder = NULL)
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
                      tabPanel(title = "Text 2",
                               tags$br(),
                               textOutput("file2A")
                      ),
                      
                      tabPanel(title = "Text 3",
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
                                    textInput("word1", "'Easy' words", value = "word1", width = '100px', placeholder = NULL),
                                    textInput("word2", NULL, value = "word2", width = '100px', placeholder = NULL),
                                    textInput("word3", NULL, value = "word3", width = '100px', placeholder = NULL),
                                    textInput("word4", NULL, value = "word4", width = '100px', placeholder = NULL),
                                    textInput("word", NULL, value = "word5", width = '100px', placeholder = NULL)
                             ),
                             column(4, align="center",
                                    textInput("word6", "'Mid' words", value = "word1", width = '100px', placeholder = NULL),
                                    textInput("word7", NULL, value = "word2", width = '100px', placeholder = NULL),
                                    textInput("word8", NULL, value = "word3", width = '100px', placeholder = NULL),
                                    textInput("word9", NULL, value = "word4", width = '100px', placeholder = NULL),
                                    textInput("word10", NULL, value = "word5", width = '100px', placeholder = NULL),
                                    
                                    actionButton(inputId="buildingDone", align="center", label="Submit my list"),
                                    
                             ),
                             column(4, align="center",
                                    textInput("word11", "'Hard' words", value = "word1", width = '100px', placeholder = NULL),
                                    textInput("word12", NULL, value = "word2", width = '100px', placeholder = NULL),
                                    textInput("word13", NULL, value = "word3", width = '100px', placeholder = NULL),
                                    textInput("word14", NULL, value = "word4", width = '100px', placeholder = NULL),
                                    textInput("word15", NULL, value = "word5", width = '100px', placeholder = NULL)
                             )
                             
                           ),
                           tags$br(),
                           textOutput("text2out"),
                           textOutput("listValidate")
                           
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
                      tabPanel(title = "Text 2",
                               tags$br(),
                               textOutput("file2B")
                      ),
                      
                      tabPanel(title = "Text 3",
                               tags$br(),
                               textOutput("file3B")
                      )
                    )
                  )
                  ),
                  
                  column(2, align="center",
                         wellPanel(
                           "YOUR LIST:",
                           textOutput("word1"),
                           textOutput("word2"),
                           textOutput("word3")
                         )
                  ),
                  
                  column(2, align="center",
                         wellPanel(
                           selectInput("rating1", "Rating: ESSAY A",
                                       c("1 = best, 10 = worst" = "", 1:10)),
                           selectInput("rating2", "Rating: ESSAY B",
                                       c("Choose one" = "", 1:10)),
                           selectInput("rating3", "Rating: ESSAY C",
                                       c("Choose one" = "", 1:10)),
                           textOutput("text3out"),
                           textOutput("ratingsValidate")
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
      SignInTimeGMT=as.character(Sys.time())
      signInInfo <<- c(SignInTimeGMT, input$signIn1,input$signIn2, input$signIn3)
      print(signInInfo)
      
      updateTabItems(session, "sidebar", "building")
      addCssClass(selector = "a[data-value='signIn']", class = "inactiveLink")
    })
    
    ### LIST BUILDING ###
    
    output$word1 <- renderText({input$word1})
    output$word2 <- renderText({input$word2})
    output$word3 <- renderText({input$word3})
    
    
    output$file1A <- renderText(essay1)
    output$file2A <- renderText(essay2)
    output$file3A <- renderText(essay3)
    
    output$file1B <- renderText(essay1)
    output$file2B <- renderText(essay2)
    output$file3B <- renderText(essay3)
    
    
    output$text2out <- renderPrint({
      wordInputs <- c(input$word1, input$word2, input$word3)
      # disable("buildingDone")
      # output$listValidate <- renderText('Invalid input!')
      # 
      # validate(
      #   need(!("" %in% wordInputs), 'At least one word blank is empty.'),
      #   need(all(grepl("word\\d+", wordInputs) == FALSE), "You can't use the default values."),
      #   need(all(duplicated(wordInputs) == FALSE), "Each word must be unique.")
      #   
      # )
      # enable("buildingDone")
      tags$br()
      output$listValidate <- renderText('List Validated!')
    })
    
    observeEvent(input$buildingDone, {
      
      test <- c(input$word1, input$word2, input$word3)
      print(test)
      
      wordList <<- data.frame(input$word1, input$word2, input$word3)
      updateTabItems(session, "sidebar", "rating")
      addCssClass(selector = "a[data-value='building']", class = "inactiveLink")
      
    })
    
    ### ESSAY RATING ###
    
    output$text3out <- renderPrint({
      essayRatings <- c(input$rating1, input$rating2, input$rating3)
      # disable("ratingDone")
      # output$ratingsValidate <- renderText('Invalid ratings!')
      # 
      # validate(
      #   need(!("" %in% essayRatings), 'At least one rating field is empty.'),
      #   need(all(duplicated(essayRatings) == FALSE), "Each rating must have a unique value.")
      # )
      # enable("ratingDone")
      output$ratingsValidate <- renderText('Ratings Validated!')
    })
    
    observeEvent(input$ratingDone, {
      ratingList <<- data.frame(input$rating1, input$rating2, input$rating3)
      updateTabItems(session, "sidebar", "analysis")
      addCssClass(selector = "a[data-value='rating']", class = "inactiveLink")
      
      ## Real analysis happens here because "wordList" is inside the "observeEvent" scope 

      tokens_analyzed_per_essay <- 100
      
      essay_scores <<- matrix(ncol = 0, nrow = length(wordList))
      essay_scores <<- as.data.frame(essay_scores)
      
      for (i in fileslist){
        
        #Read in student essays
        essayfile <- scan(file=i, what="char")
        
        #Strip out tags and punctuation
        essayfile <- gsub('<P>','',essayfile)
        essayfile <- gsub('</P>','',essayfile)
        essayfile <- gsub('[[:punct:] ]+','',essayfile)
        
        # Now remove entries that are blank (because before they were only tags or stand alone punctuation)
        essayfile[essayfile != ""]
        
        # Include only unique tokens (non-case-sensitive)
        essayfile <- unique(tolower((essayfile)))
        
        # to score essays fairly, use only an essays first X words, where X is the length of the shortest essay
        essayfile <- head(essayfile, tokens_analyzed_per_essay)
        
        #score the essay file according to how many "total_corpus" words are used
        essay_score <- as.numeric(wordList %in% essayfile)
        essay_scores <<- cbind(essay_scores, essay_score)
        
      } #End file scanning loop
      
      #name columns according to filename and name rows according to "total corpus" list
      # rownames(essay_scores) <- wordList
      
      essay1score <<- essay_score
      
      # essay1score <<- as.numeric(essay_score)
      print(essay1score)


      
  
     
      
      
      
    })
    
    ### ANALYSIS ###
    
    
    
    # submit button
    
    observeEvent(input$analysisDone, {
      updateTabItems(session, "sidebar", "validation")
      addCssClass(selector = "a[data-value='analysis']", class = "inactiveLink")
    })
    
    ### VALIDATION ###
    
    observeEvent(input$validationDone, {
      
      #submit data into new data row (with <<- you  make sure the variable is updated outside of the scope of the function)
      sessionData <- list(signin=signInInfo, words=wordList, ratings=ratingList, essay1=essay1score)
      print(sessionData)
      
      if(file.info("../responses/resultData.rds")$size != 0){
        resultData <- readRDS(file = "../responses/resultData.rds")
        resultData <- rbind(resultData, sessionData)
      } else {
        resultData <- sessionData
      }
      
      saveRDS(resultData, file = "../responses/resultData.rds")
      print("Session Data:")
      print(sessionData)
      
      resultData <- as.data.frame(resultData)
      rownames(resultData) <- NULL
      colnames(resultData) <- c("ID", "Words", "Ratings", "Score" )
      
      print("Result Data:")      
      print(resultData)
      #print(resultData$ID)
      #write.csv(resultData, "../data.csv")
      
      updateTabItems(session, "sidebar", "thanks")
      addCssClass(selector = "a[data-value='validation']", class = "inactiveLink")
    })
  }
  
  shinyApp(ui, server)
}

