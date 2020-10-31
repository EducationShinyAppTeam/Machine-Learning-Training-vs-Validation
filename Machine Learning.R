# Load Packages

#The package does have , "Resistant Regression",
#"multiple Correspondance Analysis", "Quadratic Discriminant Analysis", "

#predict.lqs, predict.mca, predict.qda, predict.profile.glm

#The core to this concept is that the less data in testing the more variance in performance statistic
#     Then less intrainging data the parameter estimates have greater variance. 
#https://stackoverflow.com/questions/13610074/is-there-a-rule-of-thumb-for-how-to-divide-a-dataset-into-training-and-validation#:~:text=Roughly%2017.7%25%20should%20be%20reserved,validation%20and%2082.3%25%20for%20training.&text=Well%20you%20should%20think%20about,tell%20that%20model%20works%20fine.

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
library(xgboost)
data(iris)
# App Meta Data----------------------------------------------------------------
APP_TITLE  <<- "[Base App for Fill in the Blank]"
APP_DESCP  <<- paste(
  "Description of the app",
  "use multiple lines to keep the description legible."
)
# End App Meta Data------------------------------------------------------------

# Load additional dependencies and setup functions
basePerformPlot <- ggplot(
  data = data.frame(x = seq(from = 0.5, to = 1), y = seq(from = 0, to = 1)),
  mapping = aes(x = x, y = y)
) +
  #geom_abline(slope = 1, intercept = 0) +
  theme_bw() +
  xlab("Proportion in Training Set") +
  ylab("Accuracy, (Percent for categorical, quantitative the MSOS of the difference") +
  labs(title = "Your Performance") +
  theme(
    text = element_text(size = 18)
  ) +
  scale_x_continuous(limits = c(.5, 1), expand = expansion(mult = 0.01, add = 0)) +
  scale_y_continuous(limits = c(0, .2), expand = expansion(mult = 0.01, add = 0))

#100 for categorical sample, .2 for quantitative example


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
          p('In this app you will study how the accuracy of a machine learning algorithm
            is affected by the percent of the cases in the training dataset vs the testing dataset.
            As you go pay attention to how the accuracy of the data along with the standard 
            deviation increases and decreases and to what amount')
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
            "This app was developed and coded by Ethan Wright",
            br(),
            "I would like to extend a special thanks to the Shiny Program
            Students.",
            br(),br(),br(),
            div(class = "updated", "Last Update: 6/15/2020 by EJW")
          )
        ),
        
        
        tabItem(
          tabName = "expl",
          withMathJax(),
          h2("Explore testing percent"),
          t("The normal amount of training data set vs validation data set is"),
          t("80% training and 20% test set"),
          br(), br(),
          fluidRow(
            column(2,
                   selectInput(inputId = 'theDataSet', label = 'Dataset', choices = list('Iris','Palmer Penguins', 'Marketing', 'Heart Disease'), selected = 'Iris'),
                   ),
            column(4,
                   textOutput("dataTableInfo"), #Gives basic info on dataset
                   box(
                     title = strong("Display Dataset"),
                     status = "primary",
                     collapsible = TRUE,
                     collapsed = TRUE,
                     width = '100%',
                     DT::dataTableOutput("dataTable") #Shows first 10 cases in table
                   )
                   ),
            column(2,
                   selectInput(inputId = 'theVariable', label = 'Variable to predict', choices = list('Species', 'Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width'), selected = 'Species'),
                   ),
            column(4,
                   
                   uiOutput("dataTableVariables")
                   
                  )
            
          ),
          
          #This stores and hides the dataset and the extra information the user may be interested in. 
          br(),
          br(),
          br(),
          selectInput(inputId = 'theMethod', label = 'Method', 
                      choices = list('Linear Discriminant Analysis',
                             'Multiple Linear Regression','Logistic Regression',
                             'Ridge Regression', 'LASSO', 'XGboost'), 
                      selected = 'Linear Discriminant Analysis'),
          #Ethan 10/1
          selectInput(inputId = 'repetitions', label = '# of repititions at each value',
                            choices = list(5,10,20,30,40,50,70,100),
                            selected = 20),
          
          fluidRow(
            column(4,
                   #actionButton('runTest', 'Test Accuracy'),
                   actionButton('newGraphOutput', 'Add Points to Graph'),
                   sliderInput('testingPercent','Percent tester',
                               min = .5, max = .9, value = .80, step = .1),
                   textOutput('accuracyResult')),
                   #textOutput('results')),
            column(4,
                   #actionButton('outputGraph','Output Graph'),
                   #plotOutput('overallPlot')
                   ),
            column(4,
                   #br(),
                   #plotOutput('consistencyPlot')
                   )
          ),
          plotOutput(outputId = "newAccuracy", width = "100%")
        ),
        
        
        tabItem(
          tabName = "challenge",
          withMathJax(),
          h2(inputId = "Now do it yourself",label = "test"),
          h3('Observations'),
          p('Everything is a trade off between accuracy and consistency.
            The more of the data is used for training the more accurate
            (unless oversampling occurs) the estimate. More of the data used for
            testing you will get a result that more accuratetly measures how good 
            the data algorithm is'),
          p('If the percent correct is very high you may as well just
            go with low training and high testing to make sure you get an accurate result')
        )
      )
    )
  )
)}

# Define server logic
server <- function(input, output, session) {
  ## Define what GO button does
  observeEvent(input$go1, {
    updateTabItems(session, "tabs", "expl")
  })
  
  tracking <- reactiveValues()
  tracking$DT <- data.frame(
    percentTraining = numeric(),
    accuracyValue = numeric()
  )
  

  
  
####### This changes what variables can be picked after the dataset selected changes
  #----Dataset Chosen, observeEvent(input$theDataSet), ----
  observeEvent(input$theDataSet, {
    if(input$theDataSet == "Iris")
    {
      output$dataTable <- DT::renderDataTable({
       iris
      })
      output$dataTableInfo <- renderText({
        "Machine learning dataset containing various measurments of different species of iris flowers"
      })
      output$dataTableVariables <- renderText({
        "
          </li><li><b>Sepal.Length: Length of the sepal in cm</b></li><li>
          Sepal.Width: Length of the sepal in cm</li><li>
          Petal.Length: Length of the sepal in cm</li><li>
          Petal.Width: Length of the sepal in cm</li><li>
          <b>Species: Either Setosa, Versicolour or Virginica</b>
        "
      })
      updateSelectInput(session, inputId = "theVariable", label = NULL,
                        choices = list('Sepal.Length', 'Species'))
    }
    else if(input$theDataSet == "Palmer Penguins"){
      output$dataTable <- DT::renderDataTable({
        penguins
      })
      output$dataTableInfo <- renderText({
        "Machine learning algorithm to try and learn the type of penguins"
      })
      output$dataTableVariables <- renderText({
        "<ul><li>Youtube advertising spent in thousands of dollars</li><li>
          Facebook advertising spent in thousands of dollars</li><li>
          Newspaper advertising spent in thousands of dollars</li><li>
          Total sales made in thousands of units</li><li>"
      })
      updateSelectInput(session, inputId = "theVariable", label = NULL,
                        choices = list('species','island', 'body_mass_g', 'sex'))
    }
    else if(input$theDataSet == "Marketing"){
      dataset <- read.csv("marketing.csv")
      output$dataTable <- DT::renderDataTable({
        dataset
      })
      output$dataTableInfo <- renderText({
        "Simple dataset to predict continous values most notably the overall sales"
      })
      output$dataTableVariables <- renderText({
        "<ul><li>Youtube advertising spent in thousands of dollars</li><li>
          Facebook advertising spent in thousands of dollars</li><li>
          Newspaper advertising spent in thousands of dollars</li><li>
          Total sales made in thousands of units</li><li>"
      })
      updateSelectInput(session, inputId = "theVariable", label = NULL,
                        choices = list('sales','youtube'))
    }
    else if(input$theDataSet == "Heart Disease"){
      dataset <- read.csv("HeartDiseaseData.csv")
      #Cleaning
      #dataset[dataset == "?"] <- NA
      #dataset[dataset$sex == 0,]$sex <- "F"
      #dataset[dataset$sex == 1,]$sex <- "M"
      output$dataTable <- renderTable({
        dataset
      })
      output$dataTable <-  DT::renderDataTable({dataset})
      output$dataTableInfo <- renderText({
        "This dataset adds extra challenge with its numerous continuous and categorical variables to be predicted."
      })
      output$dataTableVariables <- renderUI(
        HTML("<ul><li>Age in years</li><li>cp is chest pain type</li><li>
             sex (male or female)</li><li>
             trestbps is resting blood pressure</li><li>
             chol is serum cholestoral in mg/dl, fasting blood sugar (yes or no)</li><li>
             restecg resting electrocardiographic results</li><li>
             thalach: maximum heart rate</li><li>
             exang: exercise induced (yes or no)</li><li>
             oldpeak: ST depression induced by exercise relative to rest</li><li>
             Slope: of peak exercise ST segment,</li><li>
             ca: # vessels colored by flourosopy (0-3) </li><li>
             thal: Thalassemia level (blood disorder) the lower the better</li><li>
             hd: heart disease (healthy or unhealthy)</li></ul>")
        )
      updateSelectInput(session, inputId = "theVariable", label = NULL,
                        choices = list('age', 'sex', 'hd'))
    }
    else{
      updateSelectInput(session,inputId = "theVariable", lable = NULL,
                        choices = list('problem','in','selection','above'))
    }
  })
  
####### This changes what methods can be used after the variable selected changes
  #----Variable Selected changes observeEvent(input$theVariable), changes methods available----
  observeEvent(input$theVariable, { 
    if(input$theVariable == "Sepal.Length")
    {
      updateSelectInput(session, inputId = "theMethod", label = NULL,
                        choices = list("Multiple Linear Regression"))
    }
    else if(input$theVariable == "Species")
    {
      updateSelectInput(session, inputId = "theMethod", label = NULL,
                        choices = list("Linear Discriminant Analysis", "Logistic Regression", "XGboost"))
    }
    else if(input$theVariable == "species")
    {
      updateSelectInput(session, inputId = "theMethod", label = NULL,
                        choices = list("Linear Discriminant Analysis","Logistic Regression"))
    }
    else if(input$theVariable == "island")
    {
      updateSelectInput(session, inputId = "theMethod", label = NULL,
                        choices = list("Linear Discriminant Analysis"))
    }
    else if(input$theVariable == "body_mass_g")
    {
      updateSelectInput(session, inputId = "theMethod", label = NULL,
                        choices = list("Multiple Linear Regression"))
    }
    else if(input$theVariable == "sex")
    {
      updateSelectInput(session, inputId = "theMethod", label = NULL,
                        choices = list("Linear Discriminant Analysis","Logistic Regression"))
    }
    else if(input$theVariable == "youtube")
    {
      updateSelectInput(session, inputId = "theMethod", label = NULL,
                        choices = list("Multiple Linear Regression", "Ridge Regression", "LASSO", "XGboost"))
    }
    else if(input$theVariable == "sales")
    {
      updateSelectInput(session, inputId = "theMethod", label = NULL,
                        choices = list("Multiple Linear Regression", "Ridge Regression", "LASSO", "XGboost"))
    }
    else if(input$theVariable == "hd")
    {
      updateSelectInput(session, inputId = "theMethod", label = NULL,
                        choices = list("Logistic Regression","Linear Discriminant Analysis"))
    }
    else if(input$theVariable == "age")
    {
      updateSelectInput(session, inputId = "theMethod", label = NULL,
                        choices = list("Multiple Linear Regression", "Ridge Regression", "LASSO", "XGboost"))
    }
    else
    {
      #print("test")
    }
  })

  #Input: data set, the testingPercent (average .8), method used
  #output: returns the percent accuracy of the model. 
  #percent <- calculateAccuracy(dataset, input$testingPercent, input$theMethod, predictor, predictionVariable)
  #----calculateAccuracy()----
  calculateAccuracy <- function(theDataSet, testingPercent, method, predictor, predictionVariable) #predictor replaces theDataSet$Species, preictionVariable is just 'variableString'
  {
    validation_index <- createDataPartition(y = predictor, p = testingPercent, list = FALSE) #p usually input$testingPercent
    
    
    #dataset <- dataset(na.action=na.exclude)
    validation <- theDataSet[-validation_index,]

    trainingDataset <- theDataSet[validation_index,]
    
    sapply(trainingDataset,class)


    if(method == 'Linear Discriminant Analysis') {
      fit.lda <- lda(eval(parse(text = paste(predictionVariable,'~.'))), data=trainingDataset, na.action="na.omit") #, prior = c(1,1,1)/3
      predictions <- predict(object = fit.lda, newdata = validation)
      finalPredictions <- predictions$class
      outputType <- "categorical"
    }
    else if(method == 'Multiple Linear Regression')
    {
      trainingDataset
      fit.lm <- lm(eval(parse(text = paste(predictionVariable, '~.'))), data = trainingDataset)
      finalPredictions <- predict(fit.lm, validation)
      outputType <- "continuous" 
    }
    else if(method == 'Logistic Regression'){
      fit.lr <- glm(eval(parse(text = paste(predictionVariable, '~.'))), data = trainingDataset, family = "binomial")
      probabilities <- predict(fit.lr, validation, type = "response")
      #print(probabilities)
      if(predictionVariable ==  "sex")
        finalPredictions <- ifelse(probabilities < 0.5, "female", "male")
      else if(predictionVariable == "hd")
        finalPredictions <- ifelse(probabilities > 0.5, "Unhealthy", "Healthy")
      else
        #print(predictionVariable)
        
      #print(finalPredictions)
      #fit.lr <- glm(hd ~ ., data = trainingDataset, family = "binomial")
      #probabilities <- predict(fit.lr, validation, type = "response")
      #finalPredictions <- ifelse(probabilities > 0.5, "Unhealthy", "Healthy")
      outputType <- "categorical"
    }
    else if(method == "Ridge Regression")
    {
      #print(trainingDataset)
      #x <- model.matrix( ~ ., trainingDataset)
      
      x <- as.matrix(trainingDataset[, names(trainingDataset) != predictionVariable])
      y <- as.matrix(trainingDataset[, predictionVariable])

      fit.glm <- glmnet(x, y, family="gaussian", alpha=0, lambda=0.001)
      finalPredictions <- predict(fit.glm, data.matrix(validation[, names(trainingDataset) != predictionVariable]), type = "response") #type used to = "link"
      #print(finalPredictions)
      outputType <- "continuous"
    }
    else if(method == "LASSO")
    {
      # fit model
      #print(theDataSet[,1:3])
      x <- data.matrix(trainingDataset[, names(trainingDataset) != predictionVariable])
      y <- data.matrix(trainingDataset[, predictionVariable])
      
      fit.lars <- lars(x, y, type="lasso")
      # select a step with a minimum error
      best_step <- fit.lars$df[which.min(fit.lars$RSS)]
      # make predictions
      finalPredictions <- predict(fit.lars, data.matrix(validation[,names(trainingDataset) != predictionVariable]), s=best_step, type="fit")$fit
      
      outputType <- "continuous"
    }
    else #This is for XGboost
    {
      #Might be necessary in the future to improve the callebration of the code below
      #   Much of it is found here 
      #   https://www.analyticsvidhya.com/blog/2016/01/xgboost-algorithm-easy-steps/
      species <- iris$Species
    
      
      train.label <- as.integer(predictor) - 1
      
      #print(train.label)
      #print(class(train.label))
      xgb.train <- xgb.DMatrix(data = trainingDataset, label = train.label)
      xgb.train <- xgb.DMatrix(data = trainingDataset, label = train.label)
      
      num_class <- length(levels(species))
      
      params = list(
        booster="gbtree",
        eta=0.001,
        max_depth=5,
        gamma=3,
        subsample=0.75,
        colsample_bytree=1,
        objective="multi:softprob",
        eval_metric="mlogloss",
        num_class=num_class
      )
      xgb.train(
        params=params,
        data=xgb.train,
        nrounds=10000,
        nthreads=1,
        early_stopping_rounds=10,
        watchlist=list(val1=xgb.train,val2=xgb.test),
        verbose=0
      )
      
      xgb.pred$prediction = apply(xgb.pred,1,function(x) colnames(xgb.pred)[which.max(x)])
      xgb.pred$label = levels(species)[test.label+1]
      
      # train.index <- sample()
      # labels <- trainingDataset[,predictionVariable]
      # print(labels)
      # xgb <- xgboost(data = data.matrix(trainingDataset[, names(trainingDataset) != predictionVariable]),
      #                label = labels,
      #                eta = 0.1,
      #                max_depth = 15,
      #                nround=25,
      #                subsample = 0.5,
      #                colsample_bytree = 0.5,
      #                seed = 1,
      #                eval_metric = "merror",
      #                objective = "multi:softprob",
      #                num_class = 12,
      #                nthread = 3)
      # y_pred <- predict(xgb, data.matrix(trainingDataset[, names(trainingDataset) != predictionVariable]))
      # outputType <- "continuous"
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
      }
      percentCorrect <- correct / count
      percentCorrect <- percentCorrect * 100
      
      return(percentCorrect)
    }
    else #For continuous
    {
      count <- 0
      MSOS <- 0
      for(number in finalPredictions)
      {
        count <- count + 1
        #print(number)
        #print("THIS MARKS START OF NEXT SECTION")
        #print(eval(parse(text= paste('validation$',predictionVariable,'[',count,']'))))
        MSOS = MSOS + ((number - eval(parse(text= paste('validation$',predictionVariable,'[',count,']'))))^2)
        #print(MSOS)
      }
      MSOS <- MSOS / count
      #print(MSOS)
      return(MSOS)
    }
  }
  #----Ouput runTest 1 accuracy function----
  observeEvent(input$runTest,{
    #print('submit detected')
    if(input$theDataSet == 'Iris'){
      dataset <- iris
      predictor <- iris$Species      #predictor is the variable that we are going to try to predict based off training data algorithm testing the validation algorithm
      predictionVariable <- input$theVariable
    }
    else if(input$theDataSet == 'Marketing')
    {
      dataset <- read.csv("marketing.csv")
      predictor <- dataset$sales
      predictionVariable <- input$theVariable
    }
    else if(input$theDataSet == 'Palmer Penguins'){
      dataset <- na.omit(penguins)
      predictor <- dataset$species
      predictionVariable <- input$theVariable
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
      dataset <- na.omit(dataset)
      #dataset[dataset$sex == 0,]$sex <- "female"
      #dataset[dataset$sex == 1,]$sex <- "male"
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
      #dataset$hd <- ifelse(test=dataset$hd == 0, yes="Healthy", no="Unhealthy") #I might remove
      dataset$hd <- as.factor(dataset$hd) # Now convert to a factor
      predictor <- dataset$hd
      predictionVariable <- input$theVariable
    }
    else{
      #print('ERROR')
    }
    
    percentCorrect <- calculateAccuracy(dataset, input$testingPercent, input$theMethod, predictor, predictionVariable)
    percentCorrect <- signif(percentCorrect,4) #
    if(input$theMethod == "Linear Discriminant Analysis" || input$theMethod == "Logistic Regression")
      output$accuracyResult <- renderText({paste(percentCorrect,' ', '% is the accuracy when the percent of training data is',isolate(input$testingPercent))})
    else
      output$accuracyResult <- renderText({paste(percentCorrect,' ', ' is the mean sum of squares',isolate(input$testingPercent))})
   })
  
  #Outputs the graph
  #----OutputGraphFunction----
  observeEvent(input$outputGraph, {
    if(input$theDataSet == 'Iris'){
      dataset <- na.omit(iris)
      predictor <- iris$Species      #predictor is the variable that we are going to try to predict based off training data algorithm testing the validation algorithm
      #print(predictor)
      #print("End of first output of predictor") 
      predictionVariable <- input$theVariable
      #outputType <- "categorical"
    }
    else if(input$theDataSet == 'Marketing')
    {
      dataset <- read.csv("marketing.csv")
      predictor <- dataset$sales
      predictionVariable <- input$theVariable
      #outputType <- "continuous"
    }
    else if(input$theDataSet == "Heart Disease")
    {
      #https://www.youtube.com/watch?v=C4N3_XJJ-jU was important
      dataset <- read.csv("HeartDiseaseData.csv")
      #Cleaning
      dataset[dataset == "?"] <- NA
      dataset <- na.omit(dataset)
      dataset[dataset$sex == 0,]$sex <- "female"
      dataset[dataset$sex == 1,]$sex <- "male"
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
      #dataset$hd <- ifelse(test=dataset$hd == 0, yes="Healthy", no="Unhealthy") #I might remove
      dataset$hd <- as.factor(dataset$hd) # Now convert to a factor
      predictor <- dataset$hd
      predictionVariable <- input$theVariable
    }
    else if(input$theDataSet == 'Palmer Penguins'){
      dataset <- na.omit(penguins)
      predictor <- dataset$species
      predictionVariable <- input$theVariable
      #outputType <- "categorical"
    }
    else if(input$theDataSet == "House Data"){
      dataset <- read.csv("HouseTrain.csv")
      predictor <- dataset$SalePrice
      predictionVariable <- input$theVariable
      #outputType <- "categorical"
    }
    else{
      print('ERROR')
    }
    
    
    
     trainingPercents <-.5
     count <- 1
     testingPercent <- list(.5,.55,.6,.65,.7,.75,.8,.85,.9,.95)
     
     
     newAverages <- list(0,0,0,0,0,0,0,0,0,0)
     totalRuns <- as.numeric(input$repetitions)
     print(totalRuns)
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
     print("FINALLY GOT TO HERE")
     #Each run calculates 1 accuracy for each of the 10 percentages.
     #  It runs as many times as totalRuns and 
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
     
     
     #######Section to Calculate standard deviation
     #Used to contain these inside the 2 different if else below but don't beleive that is necessary
     stdev = list(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
     
     #The size of each consistency## is equal to the size of the totalRuns
     stdev[[1]] <- sd(unlist(x = consistency50, recursive = TRUE, use.names = TRUE), na.rm = TRUE)
     stdev[[2]] <- sd(unlist(x = consistency55, recursive = TRUE, use.names = TRUE), na.rm = TRUE)
     stdev[[3]] <- sd(unlist(x = consistency60, recursive = TRUE, use.names = TRUE), na.rm = TRUE)
     stdev[[4]] <- sd(unlist(x = consistency65, recursive = TRUE, use.names = TRUE), na.rm = TRUE)
     stdev[[5]] <- sd(unlist(x = consistency70, recursive = TRUE, use.names = TRUE), na.rm = TRUE)
     stdev[[6]] <- sd(unlist(x = consistency75, recursive = TRUE, use.names = TRUE), na.rm = TRUE)
     stdev[[7]] <- sd(unlist(x = consistency80, recursive = TRUE, use.names = TRUE), na.rm = TRUE)
     stdev[[8]] <- sd(unlist(x = consistency85, recursive = TRUE, use.names = TRUE), na.rm = TRUE)
     stdev[[9]] <- sd(unlist(x = consistency90, recursive = TRUE, use.names = TRUE), na.rm = TRUE)
     stdev[[10]] <- sd(unlist(x = consistency95, recursive = TRUE, use.names = TRUE), na.rm = TRUE)
     
     #Output the plots
     #Categorical
     if(input$theMethod == "Linear Discriminant Analysis" || input$theMethod == "Logistic Regression")
     {
        accuracyDataFrame <- do.call(rbind, Map(data.frame, testingPercent = testingPercent, percentCorrectCalculation = percentCorrectCalculation))
       
        output$overallPlot <- renderPlot({ggplot(data = accuracyDataFrame, aes(testingPercent * 100, percentCorrectCalculation)) +
            xlab("Percent In Training Set") +
            ylab("% Correct") +
            theme(text = element_text(size=20)) +
            geom_point() +
            stat_smooth(method = "lm", formula = y ~ poly(x, 3), size = 1, se = FALSE)
          })
        
        
        resultsDataFrame <- do.call(rbind, Map(data.frame, testingPercent = testingPercent, stdev = stdev))
        
        output$consistencyPlot <- renderPlot({ggplot(data = resultsDataFrame, aes(testingPercent * 100, stdev)) +
            xlab("Percent In Training Set") +
            ylab("Standard deviation of % Correct") +
            theme(text = element_text(size=20)) +
            geom_point() +
            stat_smooth(method = "lm", formula = y ~ poly(x, 3), size = 1, se = FALSE)
          })
     }
     else #quantitative
     {
       accuracyDataFrame <- do.call(rbind, Map(data.frame, testingPercent = testingPercent, percentCorrectCalculation = percentCorrectCalculation))
       
       output$overallPlot <- renderPlot({ggplot(data = accuracyDataFrame, aes(testingPercent * 100, percentCorrectCalculation)) +
           xlab("Percent In Training Set") +
           ylab("MSOS of the Distance From Answer") +
           theme(text = element_text(size=20)) +
           geom_point() +
           stat_smooth(method = "lm", formula = y ~ poly(x, 3), size = 1, se = FALSE)
         })
       
       #STD deviation
       resultsDataFrame <- do.call(rbind, Map(data.frame, testingPercent = testingPercent, stdev = stdev))
       
       output$consistencyPlot <- renderPlot({ggplot(data = resultsDataFrame, aes(testingPercent * 100, stdev)) +
           xlab("Percent In Training Set") +
           ylab("St Dev of the absolute distance") +
           geom_point() +
           theme(text = element_text(size=20)) +
           stat_smooth(method = "lm", formula = y ~ poly(x, 3), size = 1, se = FALSE)
         #+ lines(aes(testingPercent, predict(quadraticRegression), col = 2))
       })
    }
  })
  
  
  
  
  #----observeEvent(input$newGraphOutput)----
  observeEvent(input$newGraphOutput, {
    
    
    if(input$theDataSet == 'Iris'){
      dataset <- na.omit(iris)
      predictor <- iris$Species      #predictor is the variable that we are going to try to predict based off training data algorithm testing the validation algorithm
      #print(predictor)
      #print("End of first output of predictor") 
      predictionVariable <- input$theVariable
      #outputType <- "categorical"
    }
    else if(input$theDataSet == 'Marketing')
    {
      dataset <- read.csv("marketing.csv")
      predictor <- dataset$sales
      predictionVariable <- input$theVariable
      #outputType <- "continuous"
    }
    else if(input$theDataSet == "Heart Disease")
    {
      #https://www.youtube.com/watch?v=C4N3_XJJ-jU was important
      dataset <- read.csv("HeartDiseaseData.csv")
      #Cleaning
      dataset[dataset == "?"] <- NA
      dataset <- na.omit(dataset)
      dataset[dataset$sex == 0,]$sex <- "female"
      dataset[dataset$sex == 1,]$sex <- "male"
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
      #dataset$hd <- ifelse(test=dataset$hd == 0, yes="Healthy", no="Unhealthy") #I might remove
      dataset$hd <- as.factor(dataset$hd) # Now convert to a factor
      predictor <- dataset$hd
      predictionVariable <- input$theVariable
    }
    else if(input$theDataSet == 'Palmer Penguins'){
      dataset <- na.omit(penguins)
      predictor <- dataset$species
      predictionVariable <- input$theVariable
      #outputType <- "categorical"
    }
    else if(input$theDataSet == "House Data"){
      dataset <- read.csv("HouseTrain.csv")
      predictor <- dataset$SalePrice
      predictionVariable <- input$theVariable
      #outputType <- "categorical"
    }
    else{
      print('ERROR')
    }
    
    
    
    #consistency50 <- vector(mode = "list", length = totalRuns)
    count <- 1
    totalRuns <- as.numeric(input$repetitions)
    
    while(count <= totalRuns)
    {
      percentCorrect <- calculateAccuracy(dataset, input$testingPercent, input$theMethod, predictor, predictionVariable)
      percentCorrect <- signif(percentCorrect,4)
      
      print(input$testingPercent)
      print(percentCorrect)
      currentPoints <- data.frame(
        percentTraining = input$testingPercent,
        accuracyValue = percentCorrect
      )
      
      count <- count+1
      ###---- Store new values ----
      tracking$DT <- rbind(tracking$DT, currentPoints)
    }
    
    
  })
  
  
  
  
  output$newAccuracy <- renderPlot({
    if (is.null(tracking$DT) || nrow(tracking$DT) == 0) {
      basePerformPlot
    } else {
      basePerformPlot +
        geom_point(
          data = tracking$DT,
          mapping = aes(
            x = percentTraining,
            y = accuracyValue
          ),
          size = 4
        ) +
        theme(
          legend.position = "bottom"
        ) +
        stat_smooth(method = "lm", formula = y ~ poly(x, 3), size = 1, se = FALSE)
      
    }
  })
  
}

#})
shinyApp(ui = ui, server = server)


# 
# #Random Forest
#   fit.rf <- train(eval(parse(text= paste(predictionVariable,'~.'))), data=trainingDataset, method="rf", metric=metric, trControl=control)
#   predictions <- predict(fit.rf, validation)
#   output$results <- renderText({resamples(fit.rf)})
# 
# #Support Vector Machines
#   fit.svm <- train(eval(parse( text= paste(predictionVariable,'~.'))), data=trainingDataset, method="svmRadial", metric=metric, trControl=control)
#   predictions <- predict(fit.svm, validation)
#   output$results <- renderText({resamples(fit.svm)})
#   
# #k-Nearest Neighbor
#   fit.knn <- knn.predict(eval(parse(text= paste(predictionVariable,'~.'))), data=trainingDataset, method="knn", metric=metric, trControl=control)
#   predictions <- predict(fit.knn, validation)
#   output$results <- renderText({resamples(fit.knn)})
# 
# #Classification and Regression Trees
#   fit.cart <- train(eval(parse(text= paste(predictionVariable,'~.'))), data=trainingDataset, method="rpart", metric=metric, trControl=control)
#   predictions <- predict(fit.cart, validation)
#   output$results <- renderText({resamples(fit.cart)})


#This outputs the loading bar
# observeEvent(input$outputGraph,{
#   # withProgress(session, min = 1, max = 200, {
#   #   setProgress(message = 'Checking Answer',
#   #               detail = '')
#   #   for (i in 1:200) {
#   #     setProgress(value = i)
#   #     Sys.sleep(0.05)
#   #   }
#   # })
# })