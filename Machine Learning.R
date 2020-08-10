# Load Packages

#The package does have , "Resistant Regression",
#"multiple Correspondance Analysis", "Quadratic Discriminant Analysis", "

#predict.lqs, predict.mca, predict.qda, predict.profile.glm

library(shiny)
library(shinydashboard)
library(shinyBS)
library(boastUtils)
library(caret)
library(ellipse)
library(e1071)
library(kernlab)
library(randomForest)
library(ggplot2)
library(palmerpenguins)
library(profvis)
library(MASS)
library(glmnet)
library(lars)
library(DT)
# App Meta Data----------------------------------------------------------------
APP_TITLE  <<- "[Base App for Fill in the Blank]"
APP_DESCP  <<- paste(
  "Description of the app",
  "use multiple lines to keep the description legible."
)
# End App Meta Data------------------------------------------------------------

# Load additional dependencies and setup functions
# source("global.R")

{
# Define UI for App
ui <- list(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css",
              href = "https://educationshinyappteam.github.io/Style_Guide/theme/boast.css")
    #href = "boast.css") ## This is for Neil's testing purposes
  ),
  
  #This is in the UI
  
  # Example Call: createFillInBlank("As the difference between points _word1 their reliability _word2")
  
  ## Create the app page
  dashboardPage(
    skin = "blue",
    ### Create the app header
    dashboardHeader(
      title = "ML App", # You may use a shortened form of the title here
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(class = "dropdown",
              tags$a(href='https://github.com/EducationShinyAppTeam/BOAST',
                     icon("github"))),
      tags$li(class = "dropdown",
              tags$a(href='https://shinyapps.science.psu.edu/',
                     icon("home")))
    ),
    ### Create the sidebar/left navigation menu
    dashboardSidebar(
      sidebarMenu(
        id = "tabs",
        menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
        menuItem("Prerequisites", tabName = "prerequisites", icon = icon("book")),
        menuItem("Explore ML", tabName = "expl", icon = icon("wpexplorer")),
        menuItem("ML Challenge", tabName = "challenge", icon = icon("gamepad"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::psu_eberly_logo("reversed")
      )
    ),
    ### Create the content
    dashboardBody(
      tabItems(
        
        
        tabItem(
          tabName = "prerequisites",
          withMathJax(),
          h4('Click the link to open page with basic machine learning information'),
          h4('Please refer to the', a(href = 'https://towardsdatascience.com/workflow-of-a-machine-learning-project-ec1dba419b94', 'Machine Learning Cheatsheet', target="_blank"), 'for all the information needed.'),
         ),
        
        #### Set up the Overview Page
        tabItem(
          tabName = "overview",
          withMathJax(),
          h1("Intro to Machine Learning"), # This should be the full name.
          h2("Instructions"),
          p("Each tab will bring you further through the lesson"),
          tags$ol(
            tags$li("Explore ML will display the accuracy of the machine learning based off testing and training percentages"),
            tags$li("Then you will choose the best percentage in next game"),
          ),
          ##### Go Button--location will depend on your goals
          div(
            style = "text-align: center",
            bsButton(
              inputId = "go1",
              label = "GO!",
              size = "large",
              icon = icon("bolt"),
              style = "default"
            )
          ),
          ##### Create two lines of space
          br(),br(),
          h2("Acknowledgements"),
          p(
            "This version of the app was developed and coded by Ethan Wright",
            br(),
            "We would like to extend a special thanks to the Shiny Program
            Students.",
            br(),br(),br(),
            div(class = "updated", "Last Update: 6/15/2020 by EJW")
          )
        ),
        
        #### Fill in the blank
        tabItem(
          tabName = "expl",
          withMathJax(),
          h2("Explore testing percent"),
          t("The normal amount of training data set vs validation data set is"),
          t("80% training and 20% test set"),
          br(),
          br(),
          
          
          selectInput(inputId = 'theDataSet', label = 'Dataset', choices = list('Iris','Palmer Penguins', 'Marketing', 'Heart Disease'), selected = 'Iris'),
          selectInput("theVariable", label = "Variable to predict", choices = list('Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width', 'Species'), selected = 'Sepal.Length'),
          box(
            title = strong("Output of the Graph and basic information on it"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            
            DT::dataTableOutput("dataTable"), #Shows first 10 cases in table
            textOutput("dataTableInfo") #Gives basic info on dataset
            
          ),
          selectInput('theMethod', 'Method', 
                      choices = list('Linear Discriminant Analysis', 'Classification and Regression Trees','k-Nearest Neighbors', 'Support Vector Machines',
                      'Random Forest', 'Multiple Linear Regression','Logistic Regression', 'Ridge Regression', 'LASSO'), selected = 'Linear Discriminant Analysis'),
          
          
          fluidRow(
            column(3,
                   actionButton('runTest', 'Test Accuracy'),
                   sliderInput('testingPercent','Percent tester', min = .01, max = .99, value = .80, step = .01),
                   textOutput('accuracyResult'),
                   textOutput('results')),
            column(9,
                   actionButton('outputGraph','Output Graph'),
                   plotOutput('overallPlot'),
                   plotOutput('consistencyPlot')
                   )
          ),

        ),
        
        
        #Long response
        tabItem(
          tabName = "challenge",
          withMathJax(),
          h2(inputId = "Now do it yourself",label = "test"),
        )
      )
    )
  )
)}

# Define server logic
server <- function(input, output, session) {
  ## Define what each button does
  observeEvent(input$go1, {
    updateTabItems(session, "tabs", "expl")
  })
  
  observeEvent(input$theDataSet,{
    if(input$theDataSet == "Iris")
    {
      output$dataTable <- DT::renderDataTable({
       iris
      })
      output$dataTableInfo <- renderText({
        "Classic Machine Learning Data Set primarily to predict the species described by the measurments"
      })
      updateSelectInput(session, inputId = "theVariable", label = NULL,
                        choices = list('Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width', 'Species'))
    }
    else if(input$theDataSet == "Palmer Penguins"){
      output$dataTable <- renderTable({
        penguins
      })
      output$dataTableInfo <- renderText({
        "Machine learning algorithm to try and learn the type of penguins"
      })
      updateSelectInput(session, inputId = "theVariable", label = NULL,
                        choices = list('species','island','bill_lenth_mm', 'bill_depth_mm', 'flipper_length_mm', 'body_mass_g', 'sex'))
    }
    else if(input$theDataSet == "Marketing"){
      dataset <- read.csv("marketing.csv")
      output$dataTable <- renderTable({
        dataset
      })
      output$dataTableInfo <- renderText({
        "Simple dataset to predict continous values most notably the overall sales"
      })
      updateSelectInput(session, inputId = "theVariable", label = NULL,
                        choices = list('youtube','facebook','newspaper','sales'))
    }
    else if(input$theDataSet == "Heart Disease"){
      dataset <- read.csv("HeartDiseaseData.csv")
      #Cleaning
      dataset[dataset == "?"] <- NA
      dataset[dataset$sex == 0,]$sex <- "F"
      dataset[dataset$sex == 1,]$sex <- "M"
      output$dataTable <- renderTable({
        dataset
      })
      output$dataTableInfo <- renderText({
        "This dataset adds extra challenge with its numerous continuous and categorical variables to be predicted."
      })
      updateSelectInput(session, inputId = "theVariable", label = NULL,
                        choices = list('age',	'sex', 'cp',	'trestbps',	'chol',	'fbs', 'restecg', 'thalach', 'exang', 'oldpeak',	'slope',	'ca',	'thal',	'hd'))
    }
    else{
      updateSelectInput(session,inputId = "theVariable", lable = NULL,
                        choices = list('problem','in','selection','above'))
    }
  })
  
  observeEvent(input$theVariable,{
    if(input$theVariable == "Sepal.Length")
    {
      updateSelectInput(session, inputId = "theMethod", label = NULL,
                        choices = list("Logistic Regression", "f"))
    }
    else if(input$theVariable == "Species")
    {
      updateSelectInput(session, inputId = "theMethod", label = NULL,
                        choices = list("Linear Discriminant Analysis","Logistic Regression")
                        )
    }
    else
    {
      print("test")
    }
  })

  #Input: data set, the testingPercent (average .8), method used
  #output: returns the percent accuracy of the model. 
  #percent <- calculateAccuracy(dataset, input$testingPercent, input$theMethod, predictor, predictionVariable)
  calculateAccuracy <- function(theDataSet, testingPercent, method, predictor, predictionVariable) #predictor replaces theDataSet$Species
  {
    #print(predictor)
    validation_index <- createDataPartition(predictor, p = testingPercent, list = FALSE) #p usually input$testingPercent
    
    #dataset <- dataset(na.action=na.exclude)
    validation <- theDataSet[-validation_index,]

    trainingDataset <- theDataSet[validation_index,]
    sapply(trainingDataset,class)
    #5 Run it through the models

    # # Run algorithms using 10-fold cross validation
    # control <- trainControl(method="cv", number=10)
    # metric <- "Accuracy"
    # 

    if(method == 'Linear Discriminant Analysis') {
      fit.lda <- lda(eval(parse(text = paste(predictionVariable,'~.'))), data=trainingDataset, na.action="na.omit") #, prior = c(1,1,1)/3
      predictions <- predict(object = fit.lda, newdata = validation)
      finalPredictions <- predictions$class
      #print(predictions)
      outputType <- "categorical"
      
    }
    else if(method == 'Multiple Linear Regression')
    {
      trainingDataset #<- na.omit(trainingDataset)
      # View(trainingDataset)
      # View(validation)
      fit.lm <- lm(eval(parse(text = paste(predictionVariable, '~.'))), data = trainingDataset)
      predictions <- predict(fit.lm, validation)
      #print(predictions)
      outputType <- "continuous"
    }
    else if(method == 'Logistic Regression'){
      fit.lr <- glm(hd ~ ., data = trainingDataset, family = "binomial")
      probabilities <- predict(fit.lr, validation, type = "response")
      finalPredictions <- ifelse(probabilities > 0.5, "Unhealthy", "Healthy")
      #print(finalPredictions)
      #finalPredictions <- predictions$class
      outputType <- "categorical"
    }
    else if(method == 'Classification and Regression Trees'){
      fit.cart <- train(eval(parse(text= paste(predictionVariable,'~.'))), data=trainingDataset, method="rpart", metric=metric, trControl=control)
      predictions <- predict(fit.cart, validation)
      output$results <- renderText({resamples(fit.cart)})
      }
    else if(method == 'k-Nearest Neighbors'){
      fit.knn <- knn.predict(eval(parse(text= paste(predictionVariable,'~.'))), data=trainingDataset, method="knn", metric=metric, trControl=control)
      predictions <- predict(fit.knn, validation)
      output$results <- renderText({resamples(fit.knn)})
      }
    else if(method == 'Support Vector Machines'){
      fit.svm <- train(eval(parse( text= paste(predictionVariable,'~.'))), data=trainingDataset, method="svmRadial", metric=metric, trControl=control)
      predictions <- predict(fit.svm, validation)
      output$results <- renderText({resamples(fit.svm)})
    }
    else if(method == "Ridge Regression")
    {
      x <- as.matrix(theDataSet[,1:3])
      y <- as.matrix(theDataSet[,4])
      fit.glm <- glmnet(x, y, family="gaussian", alpha=0, lambda=0.001)
      predictions <- predict(fit.glm, x, type="link") #validation, trainingDataset
      print(predictions)
      outputType <- "continuous"
    }
    else if(method == "LASSO")
    {
      # fit model
      print(theDataSet[,1:3])
      x <- as.matrix(theDataSet[,1:3])
      y <- as.matrix(theDataSet[,4])
      fit <- lars(x, y, type="lasso")
      
      # select a step with a minimum error
      best_step <- fit$df[which.min(fit$RSS)]
      # make predictions
      predictions <- predict(fit, x, s=best_step, type="fit")$fit
      outputType <- "continuous"
    }
    else {
      fit.rf <- train(eval(parse(text= paste(predictionVariable,'~.'))), data=trainingDataset, method="rf", metric=metric, trControl=control)
      predictions <- predict(fit.rf, validation)
      output$results <- renderText({resamples(fit.rf)})
      }
  
    #6 Make predictions
    #Estimate accuracy of LDA on validation dataset
    if(outputType == "categorical")
    {
      count <- 0
      correct <- 0
      #print(validation$hd)
      #print(finalPredictions)
      for(word in finalPredictions)
      {
        count <- count + 1
        if(word == eval(parse(text= paste('validation$',predictionVariable,'[',count,']'))))
          correct <- correct + 1
        #print(word)
        #print(eval(parse(text= paste('validation$',predictionVariable,'[',count,']'))))
        #print(count)
        #print(correct)
      }
      percentCorrect <- correct / count
      percentCorrect <- percentCorrect * 100
      
      return(percentCorrect)
      
      
    }
    else #For continuous
    {
      count <- 0
      MSOS <- 0
      
      for(number in predictions)
      {
        count <- count + 1
        #print(number)
        #print(eval(parse(text= paste('validation$',predictionVariable,'[',count,']'))))
        MSOS = MSOS + ((number - eval(parse(text= paste('validation$',predictionVariable,'[',count,']'))))^2)
        #print(MSOS)
      }
      MSOS <- MSOS / count
      MSOS <- MSOS ^ .5
      #print(MSOS)
      print("Gets to here")
      return(MSOS)
    }
  }
  

  
  
  observeEvent(input$runTest,{
    #print('submit detected')
    if(input$theDataSet == 'Iris'){
      dataset <- iris
      predictor <- iris$Species      #predictor is the variable that we are going to try to predict based off training data algorithm testing the validation algorithm
      predictionVariable <- 'Species'
    }
    else if(input$theDataSet == 'Marketing')
    {
      dataset <- read.csv("marketing.csv")
      predictor <- dataset$sales
      predictionVariable <- 'sales'
    }
    else if(input$theDataSet == 'Palmer Penguins'){
      dataset <- na.omit(penguins)
      predictor <- dataset$species
      predictionVariable <- 'species'
    }
    # else if(input$theDataSet == "House Data"){
    #   #dataset <- read.csv("HouseTrain.csv")
    #   dataset <- read.csv("Reduced_HouseTrain.csv")
    #   predictor <- dataset$SalePrice
    #   predictionVariable <- 'SalePrice'
    # }
    else if(input$theDataSet == "Heart Disease")
    {
      #https://www.youtube.com/watch?v=C4N3_XJJ-jU was important
      dataset <- read.csv("HeartDiseaseData.csv")
      #Cleaning
      dataset[dataset == "?"] <- NA
      dataset[dataset$sex == 0,]$sex <- "F"
      dataset[dataset$sex == 1,]$sex <- "M"
      dataset$sex <- as.factor(dataset$sex)
      dataset$cp <- as.factor(dataset$cp)
      dataset$fbs <- as.factor(dataset$fbs)
      dataset$restecg <- as.factor(dataset$restecg)
      dataset$exang <- as.factor(dataset$exang)
      dataset$slope <- as.factor(dataset$slope)
      
      dataset$ca <- as.integer(dataset$ca)
      dataset$ca <- as.factor(dataset$ca)
      dataset$thal <- as.integer(dataset$thal) # "thal" also had "?"s in it.
      dataset$thal <- as.factor(dataset$thal)
      ## This next line replaces 0 and 1 with "Healthy" and "Unhealthy"
      dataset$hd <- ifelse(test=dataset$hd == 0, yes="Healthy", no="Unhealthy") #I might remove
      dataset$hd <- as.factor(dataset$hd) # Now convert to a factor
      predictor <- dataset$hd
      predictionVariable <- 'hd'
    }
    else{
      print('ERROR')
    }
    percentCorrect <- calculateAccuracy(dataset, input$testingPercent, input$theMethod, predictor, predictionVariable)
    percentCorrect <- signif(percentCorrect,4) #
    output$accuracyResult <- renderText({paste(percentCorrect,' ', '% is the accuracy when the percent of training data is',isolate(input$testingPercent))})
    
   })
  
  observeEvent(input$outputGraph, {

    if(input$theDataSet == 'Iris'){
      dataset <- iris
      predictor <- iris$Species      #predictor is the variable that we are going to try to predict based off training data algorithm testing the validation algorithm
      predictionVariable <- 'Species'
      outputType <- "categorical"
    }
    else if(input$theDataSet == 'Marketing')
    {
      dataset <- read.csv("marketing.csv")
      predictor <- dataset$sales
      predictionVariable <- 'sales'
      outputType <- "continuous"
    }
    else if(input$theDataSet == "Heart Disease")
    {
      #https://www.youtube.com/watch?v=C4N3_XJJ-jU was important
      dataset <- read.csv("HeartDiseaseData.csv")
      #Cleaning
      dataset[dataset == "?"] <- NA
      dataset[dataset$sex == 0,]$sex <- "F"
      dataset[dataset$sex == 1,]$sex <- "M"
      dataset$sex <- as.factor(dataset$sex)
      dataset$cp <- as.factor(dataset$cp)
      dataset$fbs <- as.factor(dataset$fbs)
      dataset$restecg <- as.factor(dataset$restecg)
      dataset$exang <- as.factor(dataset$exang)
      dataset$slope <- as.factor(dataset$slope)
      
      dataset$ca <- as.integer(dataset$ca)
      dataset$ca <- as.integer(dataset$ca)
      dataset$thal <- as.integer(dataset$thal) # "thal" also had "?"s in it.
      dataset$thal <- as.factor(dataset$thal)
      ## This next line replaces 0 and 1 with "Healthy" and "Unhealthy"
      dataset$hd <- ifelse(test=dataset$hd == 0, yes="Healthy", no="Unhealthy")
      dataset$hd <- as.factor(dataset$hd) # Now convert to a factor
      
      predictor <- dataset$hd
      predictionVariable <- 'hd'
      outputType <- "categorical"
    }
    else if(input$theDataSet == 'Palmer Penguins'){
      dataset <- na.omit(penguins)
      predictor <- dataset$species
      predictionVariable <- 'species'
      outputType <- "categorical"
    }
    else if(input$theDataSet == "House Data"){
      dataset <- read.csv("HouseTrain.csv")
      predictor <- dataset$SalePrice
      predictionVariable <- 'SalePrice'
      outputType <- "categorical"
    }
    else{
      print('ERROR')
    }
    
    
    
     trainingPercents <-.5
     count <- 1
     testingPercent <- list(.5,.55,.6,.65,.7,.75,.8,.85,.9,.95)

     
     
     newAverages <- list(0,0,0,0,0,0,0,0,0,0)
     totalRuns <- 30 #Was 40 before
     count <- 1
     
     #As the probability is being calculated these lists store the values from each test and sort them by percent in training data 
     #  so for instance consistency70 is 70% testing data and 30% validation data
     consistency50 <- vector(mode = "list", length = totalRuns)
     consistency55 <- vector(mode = "list", length = totalRuns)
     consistency60 <- vector(mode = "list", length = totalRuns)
     consistency65 <- vector(mode = "list", length = totalRuns)
     consistency70 <- vector(mode = "list", length = totalRuns)
     consistency75 <- vector(mode = "list", length = totalRuns)
     consistency80 <- vector(mode = "list", length = totalRuns)
     consistency85 <- vector(mode = "list", length = totalRuns)
     consistency90 <- vector(mode = "list", length = totalRuns)
     consistency95 <- vector(mode = "list", length = totalRuns)
     
     count <- 1
     while(count <= totalRuns)
     {
       percentCorrectCalculation <- lapply(X = testingPercent, FUN = calculateAccuracy, theDataSet = dataset, method = input$theMethod, predictor = predictor, predictionVariable = predictionVariable) #Ethan
       consistency50[count] <- percentCorrectCalculation[[1]]
       consistency55[count] <- percentCorrectCalculation[[2]]
       consistency60[count] <- percentCorrectCalculation[[3]]
       consistency65[count] <- percentCorrectCalculation[[4]]
       consistency70[count] <- percentCorrectCalculation[[5]]
       consistency75[count] <- percentCorrectCalculation[[6]]
       consistency80[count] <- percentCorrectCalculation[[7]]
       consistency85[count] <- percentCorrectCalculation[[8]]
       consistency90[count] <- percentCorrectCalculation[[9]]
       consistency95[count] <- percentCorrectCalculation[[10]]
       newAverages <- list(newAverages[[1]] + percentCorrectCalculation[[1]], newAverages[[2]] + percentCorrectCalculation[[2]], newAverages[[3]] + percentCorrectCalculation[[3]], 
                                 newAverages[[4]] + percentCorrectCalculation[[4]], newAverages[[5]] + percentCorrectCalculation[[5]], newAverages[[6]] + percentCorrectCalculation[[6]], 
                                 newAverages[[7]] + percentCorrectCalculation[[7]], newAverages[[8]] + percentCorrectCalculation[[8]], newAverages[[9]] + percentCorrectCalculation[[9]],
                                 newAverages[[10]] + percentCorrectCalculation[[10]])
       count <- count + 1
     }
     percentCorrectCalculation <- list(newAverages[[1]] / totalRuns, newAverages[[2]] / totalRuns, newAverages[[3]] / totalRuns, 
                                              newAverages[[4]] / totalRuns, newAverages[[5]] / totalRuns, newAverages[[6]] / totalRuns, 
                                              newAverages[[7]] / totalRuns, newAverages[[8]] / totalRuns, newAverages[[9]] / totalRuns,
                                              newAverages[[10]] / totalRuns)
     if(outputType == "categorical")
     {
       #Calculate variation
       print("IT gets to here")
       variation = list(0,0,0,0,0,0,0,0,0,0)
       print(consistency50)
       print(variation)
       variation[[1]] <- var(unlist(x = consistency50, recursive = TRUE, use.names = TRUE), na.rm = TRUE)
       variation[[2]] <- var(unlist(x = consistency55, recursive = TRUE, use.names = TRUE), na.rm = TRUE)
       variation[[3]] <- var(unlist(x = consistency60, recursive = TRUE, use.names = TRUE), na.rm = TRUE)
       variation[[4]] <- var(unlist(x = consistency65, recursive = TRUE, use.names = TRUE), na.rm = TRUE)
       variation[[5]] <- var(unlist(x = consistency70, recursive = TRUE, use.names = TRUE), na.rm = TRUE)
       variation[[6]] <- var(unlist(x = consistency75, recursive = TRUE, use.names = TRUE), na.rm = TRUE)
       variation[[7]] <- var(unlist(x = consistency80, recursive = TRUE, use.names = TRUE), na.rm = TRUE)
       variation[[8]] <- var(unlist(x = consistency85, recursive = TRUE, use.names = TRUE), na.rm = TRUE)
       variation[[9]] <- var(unlist(x = consistency90, recursive = TRUE, use.names = TRUE), na.rm = TRUE)
       variation[[10]] <- var(unlist(x = consistency95, recursive = TRUE, use.names = TRUE), na.rm = TRUE)
       print("IT gets to here2")
       
        output$overallPlot <- renderPlot({plot(testingPercent,percentCorrectCalculation)})
        output$consistencyPlot <- renderPlot({plot(testingPercent,variation)})
     }
     else #quantitative
     {
       variation = list(0,0,0,0,0,0,0,0,0,0)
       
       variation[[1]] <- var(unlist(x = consistency50, recursive = TRUE, use.names = TRUE), na.rm = TRUE)
       variation[[2]] <- var(unlist(x = consistency55, recursive = TRUE, use.names = TRUE), na.rm = TRUE)
       variation[[3]] <- var(unlist(x = consistency60, recursive = TRUE, use.names = TRUE), na.rm = TRUE)
       variation[[4]] <- var(unlist(x = consistency65, recursive = TRUE, use.names = TRUE), na.rm = TRUE)
       variation[[5]] <- var(unlist(x = consistency70, recursive = TRUE, use.names = TRUE), na.rm = TRUE)
       variation[[6]] <- var(unlist(x = consistency75, recursive = TRUE, use.names = TRUE), na.rm = TRUE)
       variation[[7]] <- var(unlist(x = consistency80, recursive = TRUE, use.names = TRUE), na.rm = TRUE)
       variation[[8]] <- var(unlist(x = consistency85, recursive = TRUE, use.names = TRUE), na.rm = TRUE)
       variation[[9]] <- var(unlist(x = consistency90, recursive = TRUE, use.names = TRUE), na.rm = TRUE)
       variation[[10]] <- var(unlist(x = consistency95, recursive = TRUE, use.names = TRUE), na.rm = TRUE)
       
       output$overallPlot <- renderPlot({plot(testingPercent,percentCorrectCalculation, ylab = "Average Difference Squared (So low is better)", xlab = "Percent in training set")})
       output$consistencyPlot <- renderPlot({plot(testingPercent,variation)})
     }
  })
   
}


shinyApp(ui = ui, server = server)