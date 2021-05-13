library(shiny)
library(shinythemes)
library(readxl)
library(caret)
library(e1071)
library(ggplot2)


    df <-read_excel("Dry_Bean_Dataset.xlsx")
    df$Class <- factor(df$Class)
    vars <- setdiff(names(df), "Class")
    selectKernel <- c("linear","radial")
    selectModel <- c("K-Nearest Neighbors"="KNN","Support Vector Machine (SVM)"="SVM")
ui <- fluidPage(
    navbarPage(
        "DryBean",
        tabPanel("About Data",
                 includeMarkdown("about.md")),
        tabPanel("Exploratory Data Analysis (EDA)",
                 mainPanel(
                     tabsetPanel(
                         tabPanel("Overview",
                                  br(),
                                  h4("Data Preview"),
                                  tableOutput("head"),
                                  h4("Data Summary"),
                                  verbatimTextOutput("summary")),
                         tabPanel("Plot",
                                  br(),
                                  h4("Please Select Variable : "),
                                  selectInput(inputId = 'predic1',label = 'Variable(I)',vars),
                                  selectInput(inputId = 'predic2',label = 'Variable(II)',vars, selected = vars[2]),
                                  plotOutput('plot1'))
                         )
                     )
                 ),
        tabPanel("Model",
                 sidebarPanel(
                     selectInput(inputId = 'model',label = 'Please Select Model : ',selectModel),
                     conditionalPanel(condition = "input.model == 'KNN'",
                                      sliderInput(inputId = 'knum',label = "Select K",3,min = 1,max=20)),
                     conditionalPanel(condition = "input.model == 'SVM'",
                                      selectInput(inputId = 'kernel',label = 'select Kernel',selectKernel),
                                      sliderInput(inputId = 'cross',label = "Select Number of Cross-Validation",5,min = 1,max=10),
                                      sliderInput(inputId = 'cost',label = "Select Cost",1,min = 1,max=10),
                                      sliderInput(inputId = 'gamma',label = "Select Gamma",1,min = 1,max=10),
                                      )
                     ),
                 mainPanel(
                     tabsetPanel(
                         tabPanel("Summary",
                                  conditionalPanel(condition = "input.model == 'KNN'",
                                                   h4("Confusion Matrix"),
                                                   verbatimTextOutput('summaryknn'),
                                                   h4("Accuracy"),
                                                   verbatimTextOutput('predictionknn')),
                                  conditionalPanel(condition = "input.model == 'SVM'",
                                                   h4("Summary Model"),
                                                   verbatimTextOutput('summarysvm')
                                                   )
                                  )
                         )
                 )
        )
    )
)
server <- function(input, output) {
    
    output$summaryknn <- renderPrint({
        model = knn3(data=df,Class~.,k=input$knum)
        predicted  = predict(model, df, type = "class")
        conf.table <- table(predicted, df$Class)
        conf.table
    })
    
    output$predictionknn <- renderPrint({
        model = knn3(data=df,Class~.,k=input$knum)
        predicted  = predict(model, df, type = "class")
        conf.table <- table(predicted, df$Class)
        ACC <- ( (conf.table[1,1]+conf.table[2,2]+conf.table[3,3]+conf.table[4,4]+conf.table[5,5]+conf.table[6,6]) / sum(conf.table) ) * 100
        ACC
    })
    
    output$summarysvm <- renderPrint({
        model.lm <-
            svm(
                Class ~.,
                kernel = input$kernel,
                cross = input$cross,
                scale = F,
                data = df,
                cost = input$cost,
                gamma = input$gamma
            )
        summary(model.lm)
    })
        
    output$head <- renderTable({
        head(df,10)})
    
    output$summary <- renderPrint({
        summary(df)})
    
    output$plot1 <- renderPlot({
        ggplot(df, aes_string(x = input$predic1, y = input$predic2)) +
            geom_point(aes(color = Class), size = 1, alpha = .7) + 
            theme_bw()
    })
    
    
}
shinyApp(ui = ui, server = server)
