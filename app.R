########################################################################################
#Author: Mallika Chakraborty
#Date: 12/13/2019
#Script: ST 558 Final Project R code

########################################################################################


check.packages <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
        install.packages(new.pkg, dependencies = TRUE)
    # sapply(pkg, require, character.only = TRUE)
}

packages<-c("rsample", "fpc", "rpart", "rpart.plot", "ipred", "summarytools","corrplot")
check.packages(packages)

library(shiny)
library(shinydashboard) 
library(summarytools)  ## to provide summary of the meta data 
library(markdown) ## to include Markdown files 
library(tidyr)
library(dplyr)
library(corrplot)
library(ggplot2) 
library(cluster) # for gower similarity and pam
#library(Rtsne) # for t-SNE plot
library(fpc)
library(tree) # for decision Tree
# library(MASS) 
library(rpart)       # performing regression trees
library(rpart.plot)  # plotting regression trees
library(ipred) # bagging
library(caret) # bagging
library(rsample)     # data splitting 


# setwd("~/StudentsMath")
mathDataSetReactive <- read.csv("./student-mat_Inp.csv")

# Make the Categorical Values represented with Numerical Counter part for Clustering and Further Analyis
Subst <- mathDataSetReactive #[sample(nrow(mathDataSetReactive), 100)]

must_convert<-sapply(Subst,is.factor)       # logical vector telling if a variable needs to be displayed as numeric
M2<-sapply(Subst[,must_convert],unclass)    # data.frame of all categorical variables now displayed as numeric
out1<-cbind(Subst[,!must_convert],M2)        # complete data.frame with all variables put together

out <- out1
out$GAvg <- rowMeans(out[,c("G1","G2","G3")])
drops <- c("G1","G2","G3")
out <- out[ , !(names(out) %in% drops)]
outSampleData <- sample_n(out, 1)

gower_dist <- daisy(out,metric = "gower",type = list(logratio = 3))
# Calculate silhouette width for many k using PAM

sil_width <- c(NA)
for(i in 2:10){
    pam_fit <- pam(gower_dist,
                   diss = TRUE,
                   k = i)
    sil_width[i] <- pam_fit$silinfo$avg.width
}

# Create training (70%) and test (30%) sets for the AmesHousing::make_ames() data.
# Use set.seed for reproducibility

set.seed(123)
out_split <- initial_split(out, prop = .7)
out_train <- training(out_split)
out_test  <- testing(out_split)
# Regression Tree
m1 <- rpart(formula=GAvg ~ .,data=out_train,method="anova")
outPutTreeModel <- rpart(
    formula = GAvg ~ .,
    data    = out_train,
    method  = "anova", 
    control = list(minsplit = 10, maxdepth = 20, xval = 10)
)

# Bagging Model
ctrl <- trainControl(method = "cv",  number = 10) 

# CV bagged model
bagged_cv <- train(
    GAvg ~ .,
    data = out_train,
    method = "treebag",
    trControl = ctrl,
    importance = TRUE
)
ctrl <- trainControl(method = "cv",  number = 10) 

## Take Inputs from User to predict Students Grade

saveData <- function(data) {
    prdInpdata <- as.data.frame(t(data))
    # ("InpStudentAge","InpMedu","InpFedu","InpStudyTimeinHours","InpFreetime","InpWorkDayAlc","InpWeekEndAlc","InpAbsence",
    #     "InpHealth","InpHigherEd","Inpfamsup" )
    outSampleData["age"] <- as.numeric(prdInpdata["InpStudentAge"])
    outSampleData["Medu"] <- as.numeric(prdInpdata["InpMedu"])
    outSampleData["Fedu"] <- as.numeric(prdInpdata["InpFedu"])
    outSampleData["studytime"] <- as.numeric(prdInpdata["InpStudyTimeinHours"])
    outSampleData["freetime"] <- as.numeric(prdInpdata["InpFreetime"])
    outSampleData["Dalc"] <- as.numeric(prdInpdata["InpWorkDayAlc"])
    outSampleData["Walc"] <- as.numeric(prdInpdata["InpWeekEndAlc"])
    outSampleData["absences"] <- as.numeric(as.character(prdInpdata["InpAbsence"]))
    outSampleData["health"] <- as.numeric(prdInpdata["InpHealth"])
    # outSampleData["higher"] <- prdInpdata["InpHigherEd"]
    # outSampleData["famsup"] <- prdInpdata["Inpfamsup"]
    
    
    responses1 <<- outSampleData
    # responses1["TreeOut"] <- predict(outPutTreeModel, newdata = responses1)
}

loadData <- function() {
    if (exists("responses1")) {
        responses1
    }
}

ui <- dashboardPage(
    dashboardHeader(title = "Student Grade Prediction"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("About Data", icon = icon("th"), tabName = "AboutData",
                     badgeLabel = "Intro", badgeColor = "green"),
            menuItem("Sample", tabName = "dashboard", icon = icon("vials")),
            menuItem("Data Exploration", tabName = "DataAnalysis", icon = icon("chart-bar")),
            menuItem("Cluster", tabName = "Clusters", icon = icon("object-group")),
            menuItem("Models for Predictions", tabName = "PredictionModels", icon = icon("graduation-cap")),
            menuItem("Predict Grades", tabName = "PredictGrades", icon = icon("superscript"))
            
        ) # end of sidebarMenu
    ), # dashboardSidebar
    dashboardBody(
        tabItems(
            tabItem(tabName = "dashboard",
                fluidRow(

                column(width = 12,
                       box(
                           title = "Sample Data", width = NULL, status = "primary",
                           div(style = 'overflow-x: scroll', DT::dataTableOutput('infoHeader')), # Div Brings Horizontal Scroll if table doesnt fits in width
                           downloadButton(outputId = "downLoadSampleCSV", label="downloadCSV")
                       ),
                       box(
                           title = "Quick Look at Metadata", width = NULL, status = "primary",
                           div(style = 'overflow-x: scroll', htmlOutput("profileSummary")) # Div Brings Horizontal Scroll if table doesnt fits in width
                       )
                     )
                 ) # end fluidRow
                ), # end of tabItem 1
            tabItem(tabName = "AboutData",
                    fluidRow(
                        infoBox("Students Count", 395, icon = icon("list-ol")),
                        infoBox("Number of Female Students", 208, icon = icon("female")),
                        infoBox("Number of Male Students", 187, icon = icon("male")),
                        infoBox("Number of features", 33, icon = icon("columns")),
                        infoBox("Number of Continous Variables", 4, icon = icon("cuttlefish")),
                        infoBox("Number of Categorical Variables", 29, icon = icon("bitbucket")),
                        
                        column(width = 12,
                               includeMarkdown("IncludeShiny.md")
                               )
                        
                    )
                    
                  ), # end of tabItem 2: AboutData
            tabItem(tabName = "DataAnalysis",
                    fluidRow(
                        column(width = 12,
                                plotOutput("corrPlot1"),      
                                HTML(print("<b>Since G1, G2 and G3 are highly corelated, we will use Average of three and use as dependent variable in next steps</b>")),
                               HTML(print("*Use of mathjax/ rubric</br>")),
                                withMathJax(), ## Rubric use MathJax or similar math expresiion funtion
                                helpText('Mean: $$GavgArith = \\frac{G1+G2+G3}{3}$$')
                            )
                        ),
                    fluidRow(
                        column(width = 12,uiOutput("choose_ScttrVariable"))
                    ),
                    fluidRow(
                        column(width = 12,plotOutput("scttrPlot",click = "plot_click",dblclick = "plot_dblclick",
                                                     hover = "plot_hover",
                                                     brush = "plot_brush"))
                        
                        ), # end of fluidRow
                    fluidRow(
                        column(width = 12,
                               HTML(print("<b>Select an area or click on the plot to check specific value</b>")),
                               verbatimTextOutput("plotInfo"),
                               radioButtons(inputId = "fileTypeChoice",label = "Select File Types to download plots",choices = c("png","pdf")),
                               downloadButton(outputId = "downloadDataAnalysisPlot", label = "Download Plot")
                            )# end of column

                    )# end of fluidRow
                    
                ), # end of tabItem 3: DataAnalysis
            tabItem(tabName = "Clusters",
                    fluidRow(
                        column(width = 12,
                               HTML(print("<b>Selecting the number of clusters with silhouette analysis on KMeans clustering</b>")),
                               plotOutput("NoOfClustersPlot"),
                               numericInput("ClustNum","Select No# of Clusters:", 2, min = 2, max = 10),
                               plotOutput("SeeClusters")

                        )
                        
                    )
                    
                ), # end of tabItem 4:Clusters
            tabItem(tabName = "PredictionModels",
                    fluidRow(
                        column(width = 12,
                               selectInput("PredictionModel","Select the Model:",c("Regression Tree"="dct","Bagging"="bag")),
                               conditionalPanel(
                                   condition = "input.PredictionModel == 'dct'",
                                   HTML(print("<b>Determine the right Size of the tree: </b></br>")),
                                   plotOutput("DecisionTreeCp"),
                                   HTML(print("<b>Select Hyperparameter for Regression Tree:</b></br>")),
                                   numericInput("RTreeMinsplit","Select Min split:", 4, min = 2, max = 10),
                                   numericInput("RMaxTreeDepth","Select Max Tree Depth:", 8, min = 2, max = 100),
                                   plotOutput("DecisionTree"),
                                   HTML(print("<b>RMSE::</b></br>")),
                                   textOutput("DecisionTreeRMSE")
                                ),
                               conditionalPanel(
                                   condition = "input.PredictionModel == 'bag'",
                                   plotOutput("BaggingModelImportance"),
                                   HTML(print("<b>Select Hyperparameter for Bagging:</b></br>")),
                                   numericInput("BTreeCV","Select number of Cross Validation:", 12, min = 10, max = 30),
                                   HTML(print("<b>RMSE::</b></br>")),
                                   textOutput(("BTreeRMSE"))
                               )
 
                              
                        )
                        
                    )
                    
                ), # end of tabItem 5:SuperVised Prediction Models
            tabItem(tabName = "PredictGrades",
                    fluidRow(
                        HTML(print("<b>Choose input for prediction:</b>")),
               
                        column(width = 4,
                                   #HTML(print("<b>Choose input for prediction:</b>")),
                                   # Select Input for Prediction
                                   
                                   sliderInput("InpStudentAge", "Age in years",15, 22, 1, ticks = FALSE),
                                   sliderInput("InpHealth", "Health: 1 is bad 5 is very good:",1, 5, 1, ticks = FALSE)
                               ),
                        
                        column(width = 4,     
                                   sliderInput("InpAbsence", "Days absent in School",0, 15, 1, ticks = FALSE),
                                   sliderInput("InpStudyTimeinHours", "Study Time in Hours",1, 10, 1, ticks = FALSE),
                                   sliderInput("InpFreetime", "FreeTime: 1 is low 5 is high:",1, 5, 1, ticks = FALSE)
                               ),
                        column(width = 4, 
                                   sliderInput("InpWeekEndAlc", "Weekend Alcohol: 1-low 5 is high:",1, 5, 1, ticks = FALSE),
                                   sliderInput("InpWorkDayAlc", "Workday Alcohol: 1-low 5 is high:",1, 5, 1, ticks = FALSE),
                                   # radioButtons("InpHigherEd", "Wants to take higher Education", c("Yes" = "yes","No" = "no")),
                                   
                                   sliderInput("InpFedu", "Father's Education: 0 is none 4 is High Education:",0, 4, 1, ticks = FALSE),
                                   sliderInput("InpMedu", "Mother's Education:0 is none 4 is High Education:",0, 4, 1, ticks = FALSE),
                                   # radioButtons("Inpfamsup", "Family Education Support", c("Yes" = "yes","No" = "no")),
                                   
                                   actionButton("submit", "Submit")
                               ),
                        fluidRow(
                            column(width=12,
                                   HTML(print("<b>User selected:</b>")),
                                   box(
                                       title = "User Input", width = NULL, status = "primary",
                                       div(style = 'overflow-x: scroll', DT::dataTableOutput("responses1"))
                                   ),
                                   # #, width = 300), tags$hr(),
                                   box(
                                       title = "Avg G1, G2, G3 Grades Prediction: Regression Tree:", width = NULL, status = "primary",
                                       textOutput("PredictStudentGradesRT")
                                       
                                   ),
                                   box(
                                       title = "Avg G1, G2, G3 Grades Prediction: Bagging:", width = NULL, status = "primary",
                                       textOutput("PredictStudentGradesBG")
                                   )
                                   
                                   )
                            
                                )
                        )
                ) # end of tabItem 6:PredictGrades
            ) # end of tabItemS 
    ) # end of dashboardBody
) # end of dashboardPage

server <- function(input, output) {
# For Summary Section
    output$infoHeader <-DT::renderDataTable({
        withProgress(message = 'Loading Data',
                     detail = 'This may take a while...', value = 0, {
                         for (i in 1:15) {
                             incProgress(1/15)
                             Sys.sleep(0.25)
                         }
                     })
        # head(mathDataSetReactive)
      mathDataSetReactive
    })
    
    output$downLoadSampleCSV <- downloadHandler(
      filename = function(){
        paste0("StudentGradeSampleFile",".","csv")
      },
      content = function(file){
          write.csv(head(mathDataSetReactive), file, row.names = FALSE)

      }
      # content = function(file) {
      #   write.table(mathDataSetReactive[10,], file, row.names = FALSE)
      # }
      
    ) # end of downloadHandler:downLoadSampleCSV
    output$profileSummary <- renderUI({
        SumProfile <- print(dfSummary(mathDataSetReactive), omit.headings = TRUE, method = 'render')
        SumProfile
    })
    
    # Data Exploration CorrPlot
    output$corrPlot1 <- renderPlot({
        withProgress(message = 'Determining Corelation',
                     detail = 'This may take a while...', value = 0, {
                         for (i in 1:15) {
                             incProgress(1/15)
                             Sys.sleep(0.25)
                         }
                     })
        corrplot(cor(out1), method = "circle")
    })
    
    output$choose_ScttrVariable <- renderUI({
        # selectizeInput("ScttrVariable", "Select y Axis", c("Age"="age","Guardian"="guardian","Higher Ed"="higher"),multiple=TRUE, selected="Age")
        selectizeInput("ScttrVariable", "Select y Axis to see how Grades Changes By", colnames(out),multiple=TRUE, selected="absences")
    })
    
    # Scatter plot to see relationship between Scores and Different Parameters
    output$scttrPlot <- renderPlot({
        req(input$ScttrVariable)
        withProgress(message = 'Calculation in progress',
                     detail = 'This may take a while...', value = 0, {
                         for (i in 1:15) {
                             incProgress(1/15)
                             Sys.sleep(0.25)
                         }
                     })
        dfFactors <- out %>% dplyr::select(!!quo(input$ScttrVariable),GAvg)
        dfFactors%>% gather(-GAvg, key = "var", value = "value") %>%
            ggplot(aes(x = value, y = GAvg)) +
            geom_point() +
            facet_wrap(~ var, scales = "free") +
            theme_bw()
    })
    ## make the Scatter plot interactive
    output$plotInfo <- renderText({
        xy_str <- function(e) {
            if(is.null(e)) return("NULL\n")
            paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
        }
        xy_range_str <- function(e) {
            if(is.null(e)) return("NULL\n")
            paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1), 
                   " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
        }
        
        paste0(
            "click: ", xy_str(input$plot_click),
            "dblclick: ", xy_str(input$plot_dblclick),
            "hover: ", xy_str(input$plot_hover),
            "brush: ", xy_range_str(input$plot_brush)
        )
        
    })
    
    # Plot to show Cluster no#s
    output$NoOfClustersPlot <- renderPlot({
        
        plot(1:10, sil_width,
             xlab = "Number of clusters",
             ylab = "Silhouette Width")
        lines(1:10, sil_width)
    })
    
    # Visualize Clusters 
    output$SeeClusters <- renderPlot({
        req(input$ClustNum)
        withProgress(message = 'Calculation in progress',
                     detail = 'This may take a while...', value = 0, {
                         for (i in 1:15) {
                             incProgress(1/15)
                             Sys.sleep(0.25)
                         }
                     })
        set.seed(20)
        clus <- kmeans(out, centers=input$ClustNum)
        # Save the cluster number in the dataset as column 'clsterCenters'
        out$clsterCenters <- as.factor(clus$cluster)
        str(clus)
        # Plot Clusters
        clusplot(out, clus$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
    })
    
    # Visualize Decision Tree 
    output$DecisionTree <- renderPlot({
        req(input$RTreeMinsplit)
        withProgress(message = 'Calculation in progress',
                     detail = 'This may take a while...', value = 0, {
                         for (i in 1:15) {
                             incProgress(1/15)
                             Sys.sleep(0.25)
                         }
                     })
        outPutTreeModel <<- rpart(
            formula = GAvg ~ .,
            data    = out_train,
            method  = "anova", 
            control = list(minsplit = input$RTreeMinsplit, maxdepth = input$RMaxTreeDepth, xval = 10)
        )
        rpart.plot(outPutTreeModel)
    })
    
    output$DecisionTreeCp <- renderPlot({
        req(input$RTreeMinsplit)
        plotcp(outPutTreeModel)
    })
    
    output$DecisionTreeRMSE <- renderPrint({
        outPutTreeModel <<- rpart(
            formula = GAvg ~ .,
            data    = out_train,
            method  = "anova", 
            control = list(minsplit = input$RTreeMinsplit, maxdepth = input$RMaxTreeDepth, xval = 10)
        )
        pred <- predict(outPutTreeModel, newdata = out_test)
        RMSE(pred = pred, obs = out_test$GAvg)
    })
    
    output$BaggingModelImportance <- renderPlot({
        # bagged_cv
        withProgress(message = 'Calculation in progress',
                     detail = 'This may take a while...', value = 0, {
                         for (i in 1:35) {
                             incProgress(1/15)
                             Sys.sleep(0.25)
                         }
                     })
        plot(varImp(bagged_cv), 20) 
    })
    
    output$BTreeRMSE <- renderPrint({
        # bagged_cv
        ctrl <- trainControl(method = "cv",  number = input$BTreeCV) 
        pred <- predict(bagged_cv, out_test)
        RMSE(pred, out_test$GAvg) 
    })
    
    # Whenever a field is filled, aggregate all form data
    formFields <- c("InpStudentAge","InpMedu","InpFedu","InpStudyTimeinHours","InpFreetime","InpWorkDayAlc","InpWeekEndAlc","InpAbsence",
                   "InpHealth") #,"InpHigherEd","Inpfamsup" )
 
    # Whenever a field is filled, aggregate all form data
    formData <- reactive({
        data <- sapply(formFields, function(x) input[[x]])
        data
    })
    
    # When the Submit button is clicked, save the form data
    observeEvent(input$submit, {
        saveData(formData())
    })
    
    # Show the previous responses
    # (update with current response when Submit is clicked)
    output$responses1 <- DT::renderDataTable({
        input$submit
        loadData()
    })     
    
    output$PredictStudentGradesRT <- renderText({
        # pred <- predict(outPutTreeModel, newdata = sample_n(out_test,1))
        pred <- predict(outPutTreeModel, newdata = responses1)
        pred
    })
    
    output$PredictStudentGradesBG <- renderText({
        # pred <- predict(bagged_cv, sample_n(out_test,1))
        pred <- predict(bagged_cv, responses1)
        pred
    })
    
    output$downloadDataAnalysisPlot <- downloadHandler(
        filename = function(){
            paste0("StudentGradeScatterPlot",".",input$fileTypeChoice)
        },
        content = function(file){
            dfFactors <- out %>% dplyr::select(!!quo(input$ScttrVariable),GAvg)
            if(input$fileTypeChoice=="png")
                png(file)
            else
                pdf(file)
            print(
                  dfFactors%>% gather(-GAvg, key = "var", value = "value") %>%
                      ggplot(aes(x = value, y = GAvg)) +
                      geom_point() +
                      facet_wrap(~ var, scales = "free") +
                      theme_bw())
            dev.off()
            
        }) # end of downloadHandler:downloadDataAnalysisPlot
    
    
}

shinyApp(ui, server)