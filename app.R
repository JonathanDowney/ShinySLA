library(shiny)
library(shinyjs)
library(shinydashboard)
library(TAM)

# Number of word input blanks
wordInputLength <- 12

# Number of essays showing for rating
essaysShowing <- 8

# Number of essays showing for validation
validationEssays <- 2

# Number of essays showing for validation
validationWords <- 4

#Working directory is where you put the texts
working_directory <- "/home/sixohthree/1016test/ICNALE_W_CHN_B2_0_N026"
setwd(working_directory)

fileslist <<- list.files(path = working_directory)

# Main data collector
if(file.info("../responses/resultData.rds")$size != 0){
  dataCollector <<- as.list(readRDS(file = "../responses/resultData.rds"))
} else {
  dataCollector <<- list()
}

essayTexts <<- list()
for (i in 1:length(fileslist)){
  #scan files in from "file list"
  essayTexts[[i]] <<- scan(file = fileslist[[i]], what="char")
}
{ 
  #Load sidebar after 1 second delay. Otherwise, all of the sidebar content is briefly accessible before it is hidden.
  load_data <- function() {
    Sys.sleep(1)
    hide("loading_page")
    show("sidebar_content")
  }
  
  ui <- dashboardPage(
    dashboardHeader(
      title = "SLAtools.net",
      titleWidth = 220
    ),
    
    dashboardSidebar(
      useShinyjs(),
      width = 220,
      
      div(
        id = "loading_page",
        h2("Loading...")
      ),
      hidden(
        div(
          id = "sidebar_content",
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
        )
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
                  column(4,
                         style='height:80px', align = "center",
                         tags$h2("Essay Prompt: What is the meaning of life?")
                  )
                ),
                fluidRow(
                  #Dynamic essay text display
                  column(8, align="center", 
                         wellPanel(
                           do.call(tabsetPanel, c(id='tab',lapply(1:essaysShowing, function(i) {
                             tabPanel(
                               title=paste0('Text ', i),
                               tags$br(),
                               textOutput(paste0('outA',i))
                             )
                           })))                           
                         )
                  ),
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
                  #Dynamic essay text display
                  column(8, align="center", 
                         wellPanel(
                           do.call(tabsetPanel, c(id='tab',lapply(1:essaysShowing, function(i) {
                             tabPanel(
                               title=paste0('Text ', i),
                               tags$br(),
                               textOutput(paste0('outB',i))
                             )
                           })))                           
                         )
                  ),
                  column(2, align="center",
                         wellPanel(
                           "YOUR LIST:",
                            tags$ol(
                             uiOutput("yourListA")
                            )
                         )
                  ),
                  column(2, align="center",
                         wellPanel(
                           fluidRow(
                             column(12, align="center",
                                    uiOutput("ratingInputSelectors"),
                                    actionButton(inputId="ratingDone", align="center", label="Done"),
                             ),
                           ),
                           tags$br(),
                           textOutput("ratingsValidate")
                         )
                  )
                ),
                actionButton(inputId="ratingDone", align="center", label="Done"),
        ),
        
        ### ANALYSIS ###  
        
        tabItem(tabName = "analysis",
                h2("Analysis"),
                column(6, align="center",
                       "WORD ANALYSIS",
                       wellPanel(
                         fluidRow(
                           column(3, align="center",
                                  "YOUR LIST:",
                                  tags$ol(
                                    uiOutput("yourListB")
                                  ) 
                           ),
                           column(3, align="center",
                                  "Your word ranking:",
                                  lapply(1:wordInputLength, function(i) {
                                    textOutput(paste0('staticIndexYourRanking',i))
                                  })
                           ),
                           column(3, align="center",
                                  "Model Rank:",
                                  tags$ul(
                                    uiOutput("diffReport"), style = "list-style-type: none; padding-left: 0; margin: 0"
                                  )
                           ),
                           column(3, align="center",
                                  "Word Outfit:",
                                  tags$ul(
                                    uiOutput("wordFitReport"), style = "list-style-type: none; padding-left: 0; margin: 0"
                                  )
                           )
                         )
                        )
                ),
                column(6, align="center",
                       "ESSAY ANALYSIS",
                       wellPanel(
                         fluidRow(
                           column(3, align="center",
                                  "Essays:",
                                  lapply(1:essaysShowing, function(i) {
                                    textOutput(paste0('staticIndex',i))
                                  })
                                                                   
                           ),
                           column(3, align="center",
                                  "Your Rank:",
                                  tags$ol(
                                    uiOutput("rankedEssayOrder"), style = "list-style-type: none; padding-left: 0; margin: 0"
                                  ) 
                           ),
                           column(3, align="center",
                                  "Model Rank:",
                                    tags$ul(
                                      uiOutput("abilReport"), style = "list-style-type: none; padding-left: 0; margin: 0"
                                    )
                           ),
                           column(3, align="center",
                                  "Essay Outfit:",
                                  tags$ul(
                                    uiOutput("essayFitReport"), style = "list-style-type: none; padding-left: 0; margin: 0"
                                  )
                           )
                         )
                       )
                ),
               
                actionButton(inputId="analysisDone", align="center", label="Done"),
        ),
        
        ### VALIDATION ###  
        
        tabItem(tabName = "validation",
                h2("Validation"),
                fluidRow(
                  column(12, align="center",
                  wellPanel(
                    do.call(tabsetPanel, c(id='tab',lapply(1:validationEssays, function(i) {
                      tabPanel(
                        title=textOutput(paste0('validationEssayTitle',i)),
                        tags$br(),
                        fluidRow(
                        column(6, align="center",
                          textOutput(paste0('validationEssay',i))
                        ),
                        column(6, align="center",
                               uiOutput(paste0('validationEssayQ',i))
                        )
                        )
                      )
                    })))                           
                  )
                         
                  )),
                fluidRow(
                  column(12, align="center",
                         wellPanel(
                           do.call(tabsetPanel, c(id='tab',lapply(1:validationWords, function(i) {
                             tabPanel(
                               title=textOutput(paste0('validationWordTitle',i)),
                               tags$br(),
                               fluidRow(
                                 column(6, align="center",
                                        textOutput(paste0('validationWord',i))
                                 ),
                                 column(6, align="center",
                                        uiOutput(paste0('validationWordQ',i))
                                 )
                               )
                             )
                           })))                           
                         )
                         
                  )
                ),
                column(6, align="center", 
                       wellPanel(
                         do.call(tabsetPanel, c(id='tab',lapply(1:essaysShowing, function(i) {
                           tabPanel(
                             title=paste0('Text ', i),
                             tags$br(),
                             textOutput(paste0('outC',i))
                           )
                         })))                           
                       )
                ),
               
                
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
    
    # Session data collector
    sessionData <- list()
    
    sessionData$essayData$essayTexts <- essayTexts
    
    #sidebar loading
    load_data()
    
    ### SIGN IN ###
    
    observeEvent(input$signInDone, {
      SignInTimeGMT=as.character(Sys.time())
      sessionData$signInInfo <<- c(SignInTimeGMT, input$signIn1,input$signIn2, input$signIn3)
      
      updateTabItems(session, "sidebar", "building")
      addCssClass(selector = "a[data-value='signIn']", class = "inactiveLink")
    })
    
    ### LIST BUILDING ###

    #dynamically generate word input fields in UI
    v <- list()
    for (i in 1:wordInputLength){
      v[[i]] <- textInput(paste0("word",i), label = NULL, width = '100px', placeholder = paste0("word",i))
    }
    output$wordInputBlanks <- renderUI(v)
    
    #dynamically display essays to UI
    lapply(1:essaysShowing, function(j) {
      output[[paste0('outA',j)]] <- renderPrint({
        tags$br()
        cat(essayTexts[[j]], sep = " ")
      })
    })
    
    lapply(1:essaysShowing, function(j) {
      output[[paste0('outB',j)]] <- renderPrint({
        tags$br()
        cat(essayTexts[[j]], sep = " ")
      })
    })
    
    lapply(1:essaysShowing, function(j) {
      output[[paste0('outC',j)]] <- renderPrint({
        tags$br()
        cat(essayTexts[[j]], sep = " ")
      })
    })
    
    # Dynanmic index value range to UI 
    lapply(1:essaysShowing, function(j) {
      output[[paste0('staticIndex',j)]] <- renderPrint({
        tags$br()
        cat(j)
      })
    })
    
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
      wOrder<- sort(names(input)[M])
      
    
      print(names(input))
      print(M)
      print(wOrder)
      
      wordList <<- lapply(1:wordInputLength, function(j) {input[[paste0('word',j)]]})
      
      # For data collector
      sessionData$wordData$wordList <<- wordList
      
      # wordList <<- as.data.frame(wordList)

      # Dynamically generate "wordList"
      output$yourListA <- renderUI({
        lapply(1:wordInputLength, function(j) tags$li(wordList[[j]]))
      })
      output$yourListB <- renderUI({
        lapply(1:wordInputLength, function(j) tags$li(wordList[[j]]))
      })
      
      lapply(1:wordInputLength, function(j) {
        output[[paste0('staticIndexYourRanking',j)]] <- renderPrint({
          tags$br()
          cat(j)
        })
      })

      updateTabItems(session, "sidebar", "rating")
      addCssClass(selector = "a[data-value='building']", class = "inactiveLink")
      
      w <- list()
      for (i in 1:essaysShowing){
        w[[i]] <- selectInput(paste0("rating",i), c(paste0("Essay ",i,":")), c("Choose one", 1:essaysShowing)) 
      }
      output$ratingInputSelectors <- renderUI(w)

    
    ### ESSAY RATING ###
    
      # Dynamically generate "ratings list"
      output$rankedEssayOrder <- renderUI({
        N <-  grep(pattern = "rating[[:digit:]]+", x = names(input), value = FALSE)
        # Ntest <- lapply(grep(pattern = "rating[[:digit:]]+", x = names(input), value = TRUE), function(x) input[[x]])
        
        rOrder <- sort(names(input)[N])
        lapply(grep(pattern = "rating[[:digit:]]+", x = rOrder, value = TRUE), function(x) tags$li(input[[x]]))
        
        # print(Ntest)
      })
      
     
      # For data collector
 
      
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
      # ratingList <<- data.frame(input$rating1, input$rating2, input$rating3)
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
      tamObj <<- tam(essay_scores_transposed, verbose = FALSE)
      
      wordDelta <- tamObj$xsi$xsi
      abilEST <- tam.wle(tamObj)
      essayTheta <- abilEST$theta
      
      rankDiff <- rank(wordDelta, ties.method = "random")
      
      #Word fit
      wordOutfit <<- tam.fit(tamObj)
      #index 2: outfit; index 5: infit
      print(paste0('Word outfit: ', wordFit$item[2]))
      
      #index 1: outfit; index 3: infit
      essayFit <<- tam.personfit(tamObj)
      essayOutfit <- essayFit$outfitPerson
      print(paste0('Essay fit: ', essayOutfit))
      
      output$diffReport <- renderUI({
        lapply(paste0(rankDiff, " delta: (", round(wordDelta,1), ")"), function(x) tags$li(x))
      })
      
      #Only display info for the sample of essays 
      sampleAbil <- essayTheta[1:essaysShowing]
      sampleEssayOutfit <- essayOutfit[1:essaysShowing]
      
      # Word Fit report
      output$wordFitReport <- renderUI({
        lapply(round(wordOutfit$itemfit$Outfit,2), function(x) tags$li(x))
      })
      
      #essay Fit report
      output$essayFitReport <- renderUI({
        lapply(round(sampleEssayOutfit,2), function(x) tags$li(x))
      })
      
      rankAbil <- rank(sampleAbil, ties.method = "random")
    
      output$abilReport <- renderUI({
        lapply(paste0(rankAbil, " theta: (", round(sampleAbil,1), ")"), function(x) tags$li(x))
      })
     
      reactive({
        print(unlist(rankDiff)-unlist(ratingList))
      })
      
      output$listValidate <- renderText('List Validated!')
      
      # TAM stats to data collector
      sessionData$wordData$wordDifficulty <<- wordDelta
      sessionData$essayData$essayLevel <<- essayTheta
      sessionData$essayData$sampledEssayRank <<- rankAbil
      
      # Essay Ratings to data collector
      N <-  grep(pattern = "rating[[:digit:]]+", x = names(input), value = FALSE)
      rOrder <<- sort(names(input)[N])
      ratingsList <<- as.integer(sapply(grep(pattern = "rating[[:digit:]]+", x = rOrder, value = TRUE), function(x) input[[x]]))
      sessionData$essayData$ratingsList <<- ratingsList
    
      exampleEssays <- sessionData$essayData$essayTexts
      difference <- abs(sessionData$essayData$ratingsList - sessionData$essayData$sampledEssayRank)
      
      #Display the "j" essays with the largest rating differences
      
      lapply(1:validationEssays, function(j) {
        output[[paste0('validationEssay',j)]] <- renderPrint({
          tags$br()
          cat(exampleEssays[[(order(difference, decreasing = TRUE)[[j]])]], sep = " ")
        })
      })
      
      lapply(1:validationEssays, function(j) {
        output[[paste0('validationEssayTitle',j)]] <- renderPrint({
          cat(paste0("Essay ", order(difference, decreasing = TRUE)[[j]]))
        })
      })
      
      lapply(1:validationEssays, function(j) {
        output[[paste0('validationEssayQ',j)]] <- renderPrint({
          wellPanel(
            textAreaInput(paste0("validationEssayInput",j), "Response", value = "Response", width = '400px', height = '200px', placeholder = "Your response...")
          )
        })
      })
      
      #Display the "j" words with the lowest fit stats
      
      lapply(1:validationWords, function(j) {
        output[[paste0('validationWord',j)]] <- renderPrint({
          cat(paste0("The computer thinks that '",
          wordList[[(order(wordOutfit$itemfit$Outfit, decreasing = FALSE)[[j]])]],
          "' is not a good word..."))
        })
      })
    })
    
    lapply(1:validationWords, function(j) {
      output[[paste0('validationWordTitle',j)]] <- renderPrint({
        cat(wordList[[(order(wordOutfit$itemfit$Outfit, decreasing = FALSE)[[j]])]])
      })
    })
    
    lapply(1:validationWords, function(j) {
      output[[paste0('validationWordQ',j)]] <- renderPrint({
        wellPanel(
          textAreaInput(paste0("validationWordInput",j), "Response", value = "Response", width = '400px', height = '200px', placeholder = "Your response...")
        )
      })
    })
      
    ### ANALYSIS ###
    
    # submit button
    
    observeEvent(input$analysisDone, {
      updateTabItems(session, "sidebar", "validation")
      addCssClass(selector = "a[data-value='analysis']", class = "inactiveLink")
    })
    
    ### VALIDATION ###
    
    observeEvent(input$validationDone, {
      
      # Submit data into new collector entry
      # wordListData <- list(wordList = wordList, scores=essay_scores, delta = numeric(), modelRating = numeric(), humanRating = ratingsList, modelFit = numeric(), SE = numeric(), comments = character())
      # 
      # essayData <<- list(filelist = fileslist, essay_text = essayTexts, theta = numeric(), modelRating = numeric(), humanRating = numeric(), modelFit = numeric(), SE = numeric(), comments = character())
      # 
      # sessionData <<- list(signin=signInInfo, wordlist_data = wordListData, essay_data = essayData)
      
      dataCollector[[length(dataCollector)+1]] <<- sessionData
      saveRDS(dataCollector, file = "../responses/resultData.rds")
      
      updateTabItems(session, "sidebar", "thanks")
      addCssClass(selector = "a[data-value='validation']", class = "inactiveLink")
    })
  }
  
  shinyApp(ui, server)
}

