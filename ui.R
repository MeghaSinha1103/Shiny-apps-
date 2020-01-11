shinyUI(fluidPage(
  
  # Application title
  titlePanel("Assignment 3"),
  checkboxInput(inputId = "Parallel", label = "Use parallel processing", value = TRUE),
  tabsetPanel(
    tabPanel("Data",
             verbatimTextOutput("DataSummary"),
             fluidRow(
               column(width = 4,
                      sliderInput("Multiplier", "IQR multiplier", min = 0, max = 10, step = 0.1, value = 1.5)
               ),
               column(width = 3,
                      checkboxInput(inputId = "Normalise", label = "Standardise chart", value = TRUE)
               )
             ),
             plotOutput("BoxPlots"),
             plotOutput("Missing"),
             plotOutput("Corr"),
             DT::dataTableOutput("Table")
    ),
    tabPanel("Split",
             sliderInput("Split", "Train proportion", min = 0, max = 1, value = 0.8),
             verbatimTextOutput("SplitSummary")
    ),
    tabPanel("NULL Model",
             fluidRow(
               column(width = 4, selectizeInput(inputId = "NullPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = "knnimpute")),
               column(width = 1, actionButton(inputId = "NullGo", label = "Train", icon = icon("play")))
             ),
             verbatimTextOutput("NullModelSummary2")
    ),
    tabPanel("GLMnet Model",
             verbatimTextOutput("GlmnetModelSummary0"),
             fluidRow(
               column(width = 4, selectizeInput(inputId = "GlmnetPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("naomit","dummy"))),
               column(width = 1, actionButton(inputId = "GlmnetGo", label = "Train", icon = icon("play")))
             ),
             tags$h5("Best tuning parameters:"),
             tableOutput("GlmnetModelSummary1"),
             hr(),
             plotOutput("GlmnetModelPlots"),
             verbatimTextOutput("GlmnetModelSummary2")
    ),
    tabPanel("PLS Model",
             verbatimTextOutput("PlsModelSummary0"),
             fluidRow(
               column(width = 4, selectizeInput(inputId = "PlsPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy"))),
               column(width = 1, actionButton(inputId = "PlsGo", label = "Train", icon = icon("play")))
             ),
             tags$h5("Best tuning parameters:"),
             tableOutput("PlsModelSummary1"),
             hr(),
             plotOutput("PlsModelPlots"),
             verbatimTextOutput("PlsModelSummary2")
    ),
    tabPanel("Rpart Model",
             verbatimTextOutput("RpartModelSummary0"),
             fluidRow(
               column(width = 4, selectizeInput(inputId = "RpartPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c())),
               column(width = 1, actionButton(inputId = "RpartGo", label = "Train", icon = icon("play")))
             ),
             tags$h5("Best tuning parameters:"),
             tableOutput("RpartModelSummary1"),
             hr(),
             plotOutput("RpartModelPlots"),
             plotOutput("RpartModelTree"),
             verbatimTextOutput("RpartModelSummary2")
             
    ),  
    tabPanel("bayesglm",
             verbatimTextOutput("bayesglmModelSummary0"),
             fluidRow(
               column(width = 4, selectizeInput(inputId = "bayesglmModelPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c())),
               column(width = 1, actionButton(inputId = "bayesglmModelGo", label = "Train", icon = icon("play")))
             ),
             tableOutput("bayesglmModelSummary1"),
             hr(),
             plotOutput("bayesglmModelPlots"),
             verbatimTextOutput("bayesglmModelSummary2")

    ),
    tabPanel("gbm",
             verbatimTextOutput("gbmModelSummary0"),
             fluidRow(
               column(width = 4, selectizeInput(inputId = "gbmModelPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("dummy"))),
               column(width = 1, actionButton(inputId = "gbmModelGo", label = "Train", icon = icon("play")))
             ),
             tableOutput("gbmModelSummary1"),
             hr(),
             plotOutput("gbmModelPlots"),
             verbatimTextOutput("gbmModelSummary2")
             
    ),
    tabPanel("brnn",
             verbatimTextOutput("brnnModelSummary0"),
             fluidRow(
               column(width = 4, selectizeInput(inputId = "brnnModelPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("dummy","bagimpute"))),
               column(width = 1, actionButton(inputId = "brnnModelGo", label = "Train", icon = icon("play")))
             ),
             tableOutput("brnnModelSummary1"),
             hr(),
             plotOutput("brnnModelPlots"),
             verbatimTextOutput("brnnModelSummary2")
             
    ),
    tabPanel("gaussprPoly",
             verbatimTextOutput("gaussprPolyModelSummary0"),
             fluidRow(
               column(width = 4, selectizeInput(inputId = "gaussprPolyModelPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("naomit","dummy"))),
               column(width = 1, actionButton(inputId = "gaussprPolyModelGo", label = "Train", icon = icon("play")))
             ),
             tableOutput("gaussprPolyModelSummary1"),
             hr(),
             plotOutput("gaussprPolyModelPlots"),
             verbatimTextOutput("gaussprPolyModelSummary2")
             
    ),
    
    # tabPanel("earth",
    #          verbatimTextOutput("earthModelSummary0"),
    #          fluidRow(
    #            column(width = 4, selectizeInput(inputId = "earthModelPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c())),
    #            column(width = 1, actionButton(inputId = "earthModelGo", label = "Train", icon = icon("play")))
    #          ),
    #          tableOutput("earthModelSummary1"),
    #          hr(),
    #          plotOutput("earthModelPlots"),
    #          verbatimTextOutput("earthModelSummary2")
    #          
    # ),
    
    tabPanel("RandomForest",
             verbatimTextOutput("RandomForestModelSummary0"),
             fluidRow(
               column(width = 4, selectizeInput(inputId = "RandomForestModelPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute"))),
               column(width = 1, actionButton(inputId = "RandomForestModelGo", label = "Train", icon = icon("play")))
             ),
             tableOutput("RandomForestSummary1"),
             hr(),
             plotOutput("RandomForestModelPlots"),
             verbatimTextOutput("RandomForestModelSummary2")
             
    ),
    tabPanel("rvmPoly",
             verbatimTextOutput("rvmPolyModelSummary0"),
             fluidRow(
               column(width = 4, selectizeInput(inputId = "rvmPolyModelPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("bagimpute","center","scale","dummy"))),
               column(width = 1, actionButton(inputId = "rvmPolyModelGo", label = "Train", icon = icon("play")))
             ),
             tableOutput("rvmPolyModelSummary1"),
             hr(),
             plotOutput("rvmPolyModelPlots"),
             verbatimTextOutput("rvmPolyModelSummary2")
             
    ),
    tabPanel("Cubist",
             verbatimTextOutput("CubistModelSummary0"),
             fluidRow(
               column(width = 4, selectizeInput(inputId = "CubistModelPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy"))),
               column(width = 1, actionButton(inputId = "CubistModelGo", label = "Train", icon = icon("play")))
             ),
             tableOutput("CubistModelSummary1"),
             hr(),
             plotOutput("CubistModelPlots"),
             verbatimTextOutput("CubistModelSummary2")
             
    ),
    tabPanel("Model Selection",
             tags$h5("Cross validation results:"),
             checkboxInput("Notch", "Show notch", value = FALSE),
             checkboxInput("NullNormalise", "Normalise", value = TRUE),
             plotOutput("SelectionBoxPlot"),
             radioButtons("Choice", "Model choice", choices = c(""), inline = TRUE ),
             verbatimTextOutput("TimeTaken")
    ),
    tabPanel("Performance",
             htmlOutput("Title"),
             verbatimTextOutput("TestSummary"),
             plotOutput("TestPlot")
    )
    # tabPanel("Ensemble",
    #          verbatimTextOutput("EnsembleModelSummary0"),
    #          fluidRow(
    #            column(width = 4, selectizeInput(inputId = "EnsembleModelPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy"))),
    #            column(width = 1, actionButton(inputId = "ensembleGo", label = "Train", icon = icon("play")))
    #          ),
    #          hr(),
    #          verbatimTextOutput("EnsembleModelSummary2"),
    #          verbatimTextOutput("EnsembleModelSummary3")
    #         
    # )
    
  )
))
