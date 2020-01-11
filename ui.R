library(shiny)
library(DT)
library(shinythemes)

Choice <- c("Author","Priority","Price","Speed","Duration","Scarcity","Location","Agreed","State","Class","Surface")
inputchoice <- c("Y","sensor1","sensor2","sensor3","sensor4","sensor5","sensor6","sensor7","sensor8","sensor9","sensor10","sensor11","sensor12","sensor13","sensor14","sensor15","sensor16","sensor17","sensor18","sensor19","sensor20","sensor21","sensor22","sensor23","sensor24","sensor24","sensor25","sensor26","sensor27","sensor28","sensor29","sensor30")
shinyUI(fluidPage(
  theme = shinytheme("cosmo"),
  titlePanel("Assignment1 - Megha Sinha"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      # Input: select a file
      fileInput(inputId = "file1", label = "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values, text/plain",
                           ".csv")
                
      ),
      checkboxInput("rownames", "Show row names", value=T),
      checkboxInput("order", "Column ordering", value=T),
      checkboxInput("scale", "scale", value=FALSE),
      checkboxInput("center", "center", value=FALSE)
      # selectInput("selection", "Selection type", choices=c("none","single","multiple"), selected = "none"),
      # selectInput("filter", "Filter type", choices=c("none","top","bottom"), selected = "none")
      # selectInput("dom", "DOM", choices=domChoices, multiple = TRUE, selected=domChoices)
      
     
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      tabsetPanel( type = "tabs",
                   tabPanel(title = "Data Table",
                     DT::dataTableOutput("tableX"),
                     tableOutput("SelRows")
                     
                   ),
                   
                   tabPanel(
                     title = "Summary Statistics",
                     verbatimTextOutput("summary")
                   ),
                   tabPanel(
                     title = "MissingValues",
                     h3("Missing Value Distribution:"),
                     plotOutput('MissingData'),
                     h5("The above graph shows the missing values in the dataset. It shows there are more missing values in sensor3 compared to the other columns. There are no missing values till 45 observations for all the columns except sensor3.")
                     
                   ),
                   tabPanel(
                     title = "Quantitative Data",
                     selectInput("p","Choose Quantitative Variables for the plots",choices = inputchoice, multiple = TRUE, selected = c("sensor1","sensor2") ),
                     h3("Correlation Plot of Quantitative Data:"),
                     plotOutput("Plot"),
                     h5("The correlation between few of the predictors are close to 1 such as sensor2 and 5, sensor2 and 13, sensor5 and 13. Highly correlated predictors cannot be used in linear regression model as it causes an issue of multicollinearity."),
                     h3("Box Plot for Quantitative Data:"),
                     selectInput("box","Choose Quantitative Variables for the plots",choices = inputchoice, multiple = TRUE, selected = c("sensor1","sensor2") ),
                     sliderInput("obs", "Interquartile Range",
                                 min = 0, max = 5, value = 1.5
                     ),
                     plotOutput("BoxPlot"),
                     selectInput("k","Choose Any One Quantitative variable for Line graph",choices = inputchoice, multiple = FALSE, selected = c("sensor1") ),
                     h3("Line Graph for Quantitative Data:"),
                     plotOutput("LinePlot")
                     
                   ),
                   tabPanel(
                     title = "Categorical Data",
                     selectInput("m","Choose Any One Categorical variable for Bar graph",choices = Choice, multiple = FALSE, selected = c("Price") ),
                     h3("Bar Graph For Categorical Data"),
                     plotOutput("BarPlot"),
                     h5("The above graph shows the count of different levels of the cosen column.Bar graph for price column shows that we have very less data with the level costly in comparison to cheap and extravagant."),
                     selectInput("measure","Choose One Categorical variable for Mosaic graph",choices = Choice, multiple = TRUE, selected = c("Price") ),
                     h3("Mosaic Plot For Categorical Data"),
                     plotOutput("Mosaic"),
                     h5("In the above plot unusually rare data has been depicted in red colur"),
                     h5("                   ")
                     
                   ),
                   tabPanel(
                     
                     title = "Data Visualization",
                     verbatimTextOutput("str")
                     
                   ),
                   tabPanel(
                     
                     title = "Homogeneity",
                     selectInput("c","Choose Quantitative Variables for the below plot",choices = inputchoice, multiple = TRUE, selected = c("sensor1","sensor2") ),
                     plotOutput("Homo"),
                     h5("")
                     
                   ),
                   tabPanel(
                     
                     title = "Continuity",
                     h3("Continuity Plot for Numeric values"),
                     selectInput("n","Choose Quantitative Variables for the below plot",choices = inputchoice, multiple = TRUE, selected = c("sensor1","sensor2") ),
                     plotOutput("Continuity"),
                     h5("In the above plot we can see, the sensors 2,5,13,15,26 and 27 are not continuous and follows a different pattern when compared to other columns. Apart from this, plot for sensor3 is also not continuous."),
                     verbatimTextOutput("cont")
                     
                     
                   )
                
                   
      )
    )
  )))
