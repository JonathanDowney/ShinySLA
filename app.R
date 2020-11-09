library(shiny)
library(shinyjs)
library(shinydashboard)
library(TAM)

wordInputLength <- c(1:4)

working_directory <- "/home/sixohthree/1016test/ICNALE_W_CHN_B2_0_N026"
setwd(working_directory)

fileslist <- list.files(path = working_directory)

#Data collector
if(file.info("../responses/resultData.rds")$size != 0){
  dataCollector <- as.list(readRDS(file = "../responses/resultData.rds"))
} else {
  dataCollector <<- list()
}

#Word list dataframe
wordListDF <- data.frame(wordList = character(), delta = numeric(), modelRating = numeric(), humanRating = numeric(), modelFit = numeric(), SE = numeric(), comments = character())

#Essay dataframe
essayDF <- data.frame(fileName = character(), essayText = character(), theta = numeric(), modelRating = numeric(), humanRating = numeric(), modelFit = numeric(), SE = numeric(), comments = character())

for (i in fileslist){
  #scan files in from "file list"
  corpusfile <- scan(file = i, what="char")
}

essay1 <- (scan(file = fileslist[1], what="char"))
essay2 <- (scan(file = fileslist[2], what="char"))
essay3 <- (scan(file = fileslist[3], what="char"))

# sample1 <- gsub("[[:punct:]]", "", unique(tolower(essay1)))
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
          condition = "input.buildingDone == 1 && output.validInput == 'List Validated!'",
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
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
        tags$style("#container * {display: inline;}")),
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
                                uiOutput("wordInputBlanks"),
                                actionButton(inputId="buildingDone", align="center", label="Submit my list"),
                         ),
                       ),
                       tags$br(),
                       textOutput("listValidate"),
                       tags$br(),
                       textOutput("validInput")
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
                            tags$ol(
                             uiOutput("yourList")
                            )
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
                           textOutput("ratingsValidate"),
                         )
                  )
                ),
                actionButton(inputId="ratingDone", align="center", label="Done"),
                
        ),
        
        ### ANALYSIS ###  
        
        tabItem(tabName = "analysis",
                h2("Analysis"),
                column(6, align="center",
                       wellPanel(
                         fluidRow(
                           column(4, align="center",
                                  "YOUR LIST:",
                                  div(id="container",'Word 1:', textOutput('word1Analysis')),
                                  tags$br(),
                                  div(id="container",'Word 2:', textOutput('word2Analysis')),
                                  tags$br(),
                                  div(id="container",'Word 3:', textOutput('word3Analysis'))
                           ),
                           column(4, align="center",
                                  "Your Rank:",
                                  tags$br(),
                                  div(id="container","1"),
                                  tags$br(),
                                  div(id="container","2"),
                                  tags$br(),
                                  div(id="container","3"),
                           ),
                           column(4, align="center",
                                  "Model Rank:",
                                  div(id="container", textOutput('rankDiff1'), "(", textOutput("diff1"), ")"),
                                  tags$br(),
                                  div(id="container", textOutput('rankDiff2'), "(", textOutput("diff2"), ")"),
                                  tags$br(),
                                  div(id="container", textOutput('rankDiff3'), "(", textOutput("diff3"), ")")
                                  
                           )
                         )
                        )
                ),
                column(6, align="center",
                       wellPanel(
                         fluidRow(
                           column(4, align="center",
                                  "Essays:",
                                  div(id="container",'Essay 1'),
                                  tags$br(),
                                  div(id="container",'Essay 2:'),
                                  tags$br(),
                                  div(id="container",'Essay 3:')
                           ),
                           column(4, align="center",
                                  "Your Rank:",
                                  tags$br(),
                                  div(id="container","1"),
                                  tags$br(),
                                  div(id="container","2"),
                                  tags$br(),
                                  div(id="container","3"),
                           ),
                           column(4, align="center",
                                  "Model Rank:",
                                  div(id="container", textOutput('rankAbil1'), "(", textOutput("abil1"), ")"),
                                  tags$br(),
                                  div(id="container", textOutput('rankAbil2'), "(", textOutput("abil2"), ")"),
                                  tags$br(),
                                  div(id="container", textOutput('rankAbil3'), "(", textOutput("abil3"), ")")

                           )
                         )
                       )
                ),
           
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
      updateTabItems(session, "sidebar", "building")
      addCssClass(selector = "a[data-value='signIn']", class = "inactiveLink")
    })
    
    ### LIST BUILDING ###

    #dynamically generate word input fields
    v <<- list()
    for (i in 1:length(wordInputLength)){
      v[[i]] <- textInput(paste0("word",i), label = NULL, width = '100px', placeholder = paste0("word",i))
    }
    output$wordInputBlanks <- renderUI(v)
    
    output$file1A <- renderText(essay1)
    output$file2A <- renderText(essay2)
    output$file3A <- renderText(essay3)
    
    output$file1B <- renderText(essay1)
    output$file2B <- renderText(essay2)
    output$file3B <- renderText(essay3)
    
    output$listValidate <- renderPrint({
      wordList <<- lapply(grep(pattern = "word[[:digit:]]+", x = names(input), value = TRUE), function(x) input[[x]])
      disable("buildingDone")
      output$validInput <- renderText('Invalid input!')

      validate(
        need(!("test" %in% wordList), 'Cant use test'),
        need(!(any(wordList == "")), 'At least one word blank is empty.'),
        need(all(grepl("word\\d+", wordList) == FALSE), "You can't use the default values."),
        need(all(duplicated(wordList) == FALSE), "Each word must be unique.")
      )
      
      enable("buildingDone")
      tags$br()
      output$validInput <- renderText('List Validated!')
    })
    
    observeEvent(input$buildingDone, {
      M <-  grep(pattern = "word[[:digit:]]+", x = names(input), value = FALSE)
      wOrder<- names(input)[M]
      wordList <- lapply(grep(pattern = "word[[:digit:]]+", x = wOrder, value = TRUE), function(x) input[[x]])
      wordList <<- as.data.frame(wordList)

      # Dynamically generate "wordList"
      
      output$yourList <- renderUI({
        lapply(grep(pattern = "word+[[:digit:]]+", x = wOrder, value = TRUE), function(x) tags$li(input[[x]]))
      })

      updateTabItems(session, "sidebar", "rating")
      addCssClass(selector = "a[data-value='building']", class = "inactiveLink")
      
    })
    
    ### ESSAY RATING ###
    
    output$word1 <- renderText({input$word1})
    output$word2 <- renderText({input$word2})
    output$word3 <- renderText({input$word3})

    # output$test <- renderUI(sapply(grep(pattern = "word[[:digit:]]+", x = names(input), value = TRUE), function(x) input[[x]]))
    
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
      
      ## Analysis happens before the "analysis" tab because "wordList" is inside the "observeEvent" scope 

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
        
        #score the essay file according to how many "total_corpus" words are used
        essay_score <- as.numeric(wordList %in% essayfile)
        essay_scores <<- cbind(essay_scores, essay_score)
        
      } #End file scanning loop
      
      #name columns according to filename and name rows according to "total corpus" list
      # rownames(essay_scores) <- wordList
      
      essay_scores_transposed <- data.frame(t(essay_scores))
      testTAM <- tam(essay_scores_transposed)
      
      print(head(essay_scores))
      
      diff <- testTAM$xsi$xsi
      abilEST <<- tam.wle(testTAM)
      abil <- abilEST$theta
      print(diff)
      
      rankDiff <- rank(c(diff[1], diff[2], diff[3]))
      rankAbil <- rank(c(abil[1], abil[2], abil[3]))
      
      output$diff1 <- renderPrint(cat(diff[1])) #cat() to remove row number from output
      output$diff2 <- renderPrint(cat(diff[2]))
      output$diff3 <- renderPrint(cat(diff[3]))
      output$rankDiff1 <- renderPrint(cat(rankDiff[1]))
      output$rankDiff2 <- renderPrint(cat(rankDiff[2]))
      output$rankDiff3 <- renderPrint(cat(rankDiff[3]))
      
      output$abil1 <- renderPrint(cat(abil[1])) #cat() to remove row number from output
      output$abil2 <- renderPrint(cat(abil[2]))
      output$abil3 <- renderPrint(cat(abil[3]))
      output$rankAbil1 <- renderPrint(cat(rankAbil[1]))
      output$rankAbil2 <- renderPrint(cat(rankAbil[2]))
      output$rankAbil3 <- renderPrint(cat(rankAbil[3]))
      
      output$listValidate <- renderText('List Validated!')
    })
    
    ### ANALYSIS ###
    
    output$word1Analysis <- renderText({input$word1})
    output$word2Analysis <- renderText({input$word2})
    output$word3Analysis <- renderText({input$word3})
    
    # submit button
    
    observeEvent(input$analysisDone, {
      updateTabItems(session, "sidebar", "validation")
      addCssClass(selector = "a[data-value='analysis']", class = "inactiveLink")
    })
    
    ### VALIDATION ###
    
    observeEvent(input$validationDone, {
      
      # Submit data into new collector entry
      sessionData <- list(signin=signInInfo, words=wordList, ratings=ratingList, scores=essay_scores)
      dataCollector[[length(dataCollector)+1]] <- sessionData
      saveRDS(dataCollector, file = "../responses/resultData.rds")
      
#       if(file.info("../responses/resultData.rds")$size != 0){
#         collector <- as.list(readRDS(file = "../responses/resultData.rds"))
#         # resultData <- append(resultData, sessionData)
#         collector[[length(collector)+1]] <- sessionData
#         
#       } else {
#         collector <<- list()
#         collector[[length(collector)+1]] <- sessionData
#       }
#       
#       saveRDS(collector, file = "../responses/resultData.rds")
#       print("Session Data:")
# 
#       # rownames(resultData) <- "Response"
#       # colnames(resultData) <- c("ID", "Words", "Ratings", "Score" )
#       
#       print("Result Data:")      
#       # print(resultData)
#       #print(resultData$ID)
#       #write.csv(resultData, "../data.csv")
#       
      updateTabItems(session, "sidebar", "thanks")
      addCssClass(selector = "a[data-value='validation']", class = "inactiveLink")
    })
  }
  
  shinyApp(ui, server)
}

