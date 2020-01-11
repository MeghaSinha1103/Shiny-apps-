
dashboardPage(
              title = "Dimensional Reduction",
  skin = "red",

dashboardHeader(title = "Megha-Dimensional Reduction",
                titleWidth = 450),
  dashboardSidebar(
    # Adjust the sidebar
        tags$style(".left-side, .main-sidebar {padding-top: 100px}"),
        tags$img(src = "PCAGIF.gif", height="250%", width="100%", align="left"),
    
    p("Choose mtcars dataset for PCA and Boston dataset for PLS"),
    # fileInput(inputId = "file1", label = "Choose CSV File",
    #           multiple = FALSE,
    #           accept = c("text/csv",
    #                      "text/comma-separated-values, text/plain",
    #                      ".csv"),
    # 
    # 
    # ),
    radioButtons(inputId = 'k',  
                 label = 'Dataset Choice',
                 choices = c('Boston'='boston','mtcars'='mtcars'),
                              
                 selected = 'mtcars'),
    p("Choose Options from below:"),
    menuItem("Overview Of Data", tabName = "dashboard"),
     menuSubItem("PLS- Supervised", tabName = "PLS"),
     menuSubItem("PCA- Unsupervised", tabName = "pca")
    # checkboxInput("rownames", "Show row names", value=T),
    # checkboxInput("order", "Column ordering", value=T)
    
 
  ),
  dashboardBody(
    
    tabItems(
    tabItem(tabName = "dashboard",
    fluidRow(
      tabBox(width = "1000px",
  
        tabPanel(
        title = "Datatable",
        DT::dataTableOutput("tableX"),
        tableOutput("SelRows")
      ),
      
      tabPanel(
        tags$style(type='text/css', '#summary {background-color: rgba(0,250,250,0.20); color: black;}'), 
        title = "Summary",
        p("Here is a summary of the data"),
        verbatimTextOutput("summary")
      ),
      
      tabPanel(
        tags$style(type='text/css', '#summary {background-color: rgba(0,250,250,0.20); color: black;}'), 
        title = "MissingValue Plot",
        plotOutput("missing")
      )
      
      
      
      ))),
    
    tabItem(tabName = "pca", 
    fluidRow(
    tabBox(width = "1000px",
    tabPanel(
      title = "Correlation",
      # p("Donot choose model as a column as it has 32 levels and not suitable for correlation plot"),
      uiOutput("choose_columns_biplot"),
      tags$hr(),
      p("When two variables are highly uncorrelated, then the first and second components explains the dataset with high fidelity.Setting aside variables known to be strongly correlated with others can have a substantial effect on the PCA results."),
      tags$hr(),
      downloadButton('downloadCorPlot', 'Download the plot as pdf'),
      plotOutput("corr_plot"),
      plotOutput("correlation"),
      tags$hr(),
      p("Summary of correlations"),
      DT::dataTableOutput("corr_tables")
    ),
    tabPanel(title =  "Diagnosis",
    tags$style(type='text/css', '#Diagnosis {background-color: rgba(0,250,250,0.20); color: black;}'), 
    tags$style(type='text/css', '#str {background-color: rgba(0,250,250,0.20); color: black;}'), 
    tags$style(type='text/css', '#eigen {background-color: rgba(0,50,250,0.20); color: black;}'),
    p("The PCA means creating the same number of components as there are original variables but usually only a few capture enough variance to be useful."),
    p("Carefully look at the dataset and discard the categorical data. In case of mtcars dataset columns vs and am are categorical variables "),
    p("Choose the columns of your data to include in the PCA."),
    p("Only columns containing numeric data are shown here because PCA doesn't work with non-numeric data."),
    p("Observations (ie. rows) are automatically removed if they contain any missing values."),
    p("Variables with zero variance have been automatically removed because they're not useful in a PCA."),
    uiOutput("choose_columns_pca"),
    p("Select options for the PCA computation: PCA requires that the input variables have similar scales of measurement."),
    radioButtons(inputId = 'center',  
                 label = 'Center',
                 choices = c('Shift variables to be zero centered'='Yes',
                             'Do not shift variables'='No'), 
                 selected = 'Yes'),
    
    radioButtons('scale.', 'Scale',
                 choices = c('Scale variables to have unit variance'='Yes',
                             'Do not scale variables'='No'), 
                 selected = 'Yes'),
    
    p("Summary result of different principle components of the dataset."),
    verbatimTextOutput("Diagnosis"),
    p("Taking look at PCA Objects"),
    p("center point ($center), scaling ($scale), standard deviation(sdev) of each principal component.($rotation) The relationship (correlation or anticorrelation, etc) between the initial variables and the principal components.($x) - The values of each sample in terms of the principal components "),
    verbatimTextOutput("str"),
    p("Eigen values for PCA computation:"),
    verbatimTextOutput("eigen")
    ),
    tabPanel(title = "Scree plot",
             
             p("The scree plot shows the variances of each PC, and the cumulative variance explained by each PC (in %) "),
             
             withSpinner(plotOutput("plot2", height = "300px")),
             p("Contribution Of the Variables on the components PC1 and PC2"),
             withSpinner(plotOutput("pca_bar")),
             p("The below correlation plot shows the contribution of each variable to the dimensions. Here dimensions are the components. We can see that component 1 has contribution from almost all the variables. Component4 is dominated by variable drat. Contribution from other variables are very less for the component 4."),
             withSpinner(plotOutput("corr_dim"))
             ),
    # tabPanel(title = "Bar Plot",
    #   
    #   p("Bar Plot showing two components of the PCA"),
    #   
    #   withSpinner(plotOutput("pcPlot1"))
    #   
    # ),
    
    tabPanel(title =  "PCA Plot",
             p("The below plot shows the clustering of the observations with the change in priciple components. The components with substantial variance produces better result. In this case Comp.1 and Comp.2 performs better clustering."),
             p("You can change the components using below options to see how the clustering changes with change in components."),
             selectInput("choice1","Choose only Quantitative variable for PCA Diagnosis as it works best with quantitative data",choices = inputchoice1, multiple = FALSE, selected = c("Comp.1") ),
             selectInput("choice2","Choose only Quantitative variable for PCA Diagnosis as it works best with quantitative data",choices = inputchoice1, multiple = FALSE, selected = c("Comp.2") ),
             selectInput("choice3","Choose the method for Hierarchical clustering",choices = inputchoice2, multiple = FALSE, selected = c("ward.D2")),
             withSpinner(plotlyOutput("plot"))
    ),
        
        tabPanel("Scores Plot",
            p("For the below plot, pc1 and pc2 has been taken as these two components together explains 84% of the data as shown in scree plot."),
            p("The below plot shows us how the datapoints relate to the axes. For the mtcar dataset we can see variables hp,cyl and disp contribute to pc1"),
            withSpinner(plotlyOutput("plotData"))
                ),
      
      
        # end  tab

     
     tabPanel("GroupingVariableInPCA",
     
     h2("PC plot: zoom and select points"),
     p("Select the grouping variable."),
     p("Only variables where the number of unique values is less than 10% of the total number of observations are shown here (because seeing groups with 1-2 observations is usually not very useful)."),
     uiOutput("the_grouping_variable"),
     tags$hr(),
     p("Select the PCs to plot"),
     uiOutput("the_pcs_to_plot_x"),
     uiOutput("the_pcs_to_plot_y"),
     tags$hr(),
     
     p("Click and drag on the first plot below to zoom into a region on the plot. Or you can go directly to the second plot below to select points to get more information about them."),
     p("Then select points on zoomed plot below to get more information about the points."),
     p("You can click on the 'Compute PCA' tab at any time to change the variables included in the PCA, and then come back to this tab and the plots will automatically update."),
     plotlyOutput ("z_plot1", height = 400)
    ),
      
       tabPanel("PlotDetails",

       p("Click and drag on the plot below to select points, and inspect the table of selected points below"),

       plotOutput("z_plot2", height = 400,
                    brush = brushOpts(
                      id = "plot_brush_after_zoom",
                      resetOnNew = TRUE)),
       tags$hr(),
       p("Details of the brushed points"),
       tableOutput("brush_info_after_zoom")
       ,
       brush = brushOpts(
         id = "plot_brush_after_zoom",
         resetOnNew = TRUE)

        )
    )
  )),
  
  tabItem(tabName = "PLS", 
          fluidRow(
            box(title = "PLS and PCR Comparison",
                   background = "black",
                   width = "1000px",
                  p("We have taken a dataset to explore the performance of PCR and PLS methods for dimentionality reduction."),
                  p("Performance Matrix for both the method is given below. PLS requires less number of components compared to PCR. This can be explored using the slider inputs in PLS and PCR plots"),
                tags$b("Scores Plot: "),
                p("This gives a pairwise Plot of score values for the components. It is used for finding patterns, groups or outliers in the data.We can see that for PLS plot of comp1 is following same pattern with comp3, comp4 and comp5 "),
                tags$b("Loading Plot:"),
                p("The loading plot is much used for interpretation purposes, for instance to look for known spectral peaks or profiles")
                   
                ),
            tabBox(title = "PLS",
              tabPanel(tags$style(type='text/css', '#pls_detais1 {background-color: rgba(0,100,0,0.20); color: black;}'), 
              title = "PLS Summary",
              withSpinner(verbatimTextOutput("pls_detais1"))
                     
                ),
              tabPanel(
                title = "PLS PLOT",
                sliderInput("comp1", "Choose Number of components for PLS",
                            min = 0, max = 14, value = 10
                ),
                withSpinner(plotOutput("pls_plot1"))
                
              ),
              tabPanel(
                
                title = "PLS_ScorePlot",
                sliderInput("comp2", "Choose Number of components for PLS",
                            min = 0, max = 14, value = 10
                ), 
                withSpinner(plotOutput("pls_plot2"))
                
              ),
              tabPanel(
                title = "performance Matrix",
                withSpinner(verbatimTextOutput("pls_detais2"))
                
              ),
              tabPanel(
                title = "Loading Plot",
                sliderInput("comp5", "Choose Number of components for PLS",
                            min = 0, max = 14, value = 10
                ), 
                withSpinner(plotOutput("pls_plot3"))
                
              )
            ),
            tabBox(title = "PCR", 
              tabPanel( 
                tags$style(type='text/css', '#pcr_detais1 {background-color: rgba(0,100,0,0.20); color: black;}'), 
                  title = "PCR Summary", 
                withSpinner(verbatimTextOutput("pcr_detais1"))
                  
                
              ),
              tabPanel(
                
                title = "PCR PLOT",
                sliderInput("comp", "Choose Number of components for PCR",
                            min = 0, max = 14, value = 10
                ), 
                withSpinner(plotOutput("pcr_plot1"))
                
              ),
              tabPanel(
                
                title = "PCR_ScorePlot",
                sliderInput("comp3", "Choose Number of components for PCR",
                            min = 0, max = 14, value = 10
                ), 
                withSpinner(plotOutput("pcr_plot2"))
                
              ),
              tabPanel(
                title = "performance Matrix",
                withSpinner(verbatimTextOutput("pcr_detais2"))
                
              ),
              
              tabPanel(
                title = "Loading Plot",
                sliderInput("comp6", "Choose Number of components for PCR",
                            min = 0, max = 14, value = 10
                ),
                withSpinner(plotOutput("pcr_plot3"))
                
              )
              
            ),
            tabBox(
              title = "PLS PLOT",
                width = "1000px",
              tabPanel(
                title = "PLOT",
                p("Below is the plot for PLS as, for this dataset PLS is performing better."),
                withSpinner(plotOutput("pls_plot4"))
               
                ),
              tabPanel(
                title = "Scree Plot",
              p("Below scree plot shows the principle components for the dataset and the variances explained by each components. This shows the importance of principle components for the datasets which has large number of variables.As on creating these priciple components we are able to explain the dataset with fewer number of variables.In this case if we perform PCA only two components will be able explain the dataset. However, PCA is unsupervised learning and generally PLS is preferred over PCA for classification problems."),
              plotOutput("pls_scree")
              )
              )
              
            )
            
            )
  )
))





