# install.packages(c("shiny","DT","plotly","visdat","ggplot2","Amelia","PerformanceAnalytics","lubridate","reshape2","vcd","ggmosaic","dplyr","summarytools"))
library(shiny)
library(DT)
library (plotly)
library(visdat)
library(ggplot2)
library(Amelia)
library(PerformanceAnalytics)
library(lubridate)
library(reshape2)
library(vcd)
library(ggmosaic)
library(dplyr)
library(summarytools)
data(mtcars)
mt <- mtcars
mt$vs <- as.factor(mt$vs)
mt$am <- as.factor(mt$am)

shinyServer(function(input, output, session) {
    # https://datatables.net/reference/option/dom
    
    # https://datatables.net/extensions/index
    ext <- list(Responsive = TRUE)
    
    data <- reactive({
        req(input$file1) 
        
        
        data2 <- read.csv(input$file1$datapath)
        
        
        # df <- scale(data2, center = input$center, scale = input$scale) 
        return(data2)
        
    })
    
    # scale <- reactive({
    #     data3 <- data()
    #     scale <- scale(x=dplyr::select_if(data3,is.numeric),center=input$center,scale=input$scale)
    #     return(scale)
    # })
    
    output$tableX <- DT::renderDataTable({
        mt <- data()
        
        DT::datatable(data = mt,
                      rownames = input$rownames,
                      options = list(searching = TRUE,
                                     pageLength = 10,
                                     lengthMenu = c(5, 10, 100),
                                     ordering = input$order
                      )
                      # extensions = ext
        )  %>%
            formatStyle(columns = c("sensor1"), backgroundColor = "lightblue")  %>%
            #      formatCurrency(c(2), '$') %>%
            #      formatPercentage(3, 2) %>%
            formatRound(c("sensor1"), 3)
    })
    
    output$SelRows <- renderTable({
        req(input$tableX_rows_selected)
        print(mt[input$tableX_rows_selected,"sensor1"])
    })
    
    
    
    # Generate a summary table of the data uploaded
    output$summary <- renderPrint({
        y <- data()
        
        dfSummary(y,
                  method = 'render',
                  omit.headings = TRUE,
                  bootstrap.css = FALSE)
        
        
    })
    
    output$MissingData <- renderPlot({
        
        mt <- data()
        missmap(mt, col = c("red", "skyblue"),title = " ")
        
        
    })
    
    
    output$Plot <- renderPlot({
        
        t <- data()
        d <- scale(x=dplyr::select_if(t,is.numeric),center=input$center,scale=input$scale)
        numData <- d[,input$p]
        
        chart.Correlation(numData,method = "pearson")
        
    })
    
    output$BoxPlot <- renderPlot({
        
        
        data3 <- data()
        numData <- data3[,input$box]
        d <- scale(numData,center=input$center,scale=input$scale)
        
        boxplot(d, range = input$obs, col = c("green","blue"), notch = FALSE)
        
        
    })
    
   
    output$LinePlot <- renderPlot({
        Ass1Data <- data()
        mydate <- as.data.frame.matrix(Ass1Data)
        
        
        d <- ymd(mydate$Date)
        
        
        ggplot(mydate, aes(x = d, y = mydate[, input$k])) + 
            geom_line() +
            scale_x_date(date_breaks = "1 years", date_labels = "%Y") +
            labs (title = paste("Plot of",input$k), x = "Date", y = "Values") + 
            theme(legend.title = element_blank())
    })
    
    
    output$BarPlot <- renderPlot({
        
        my_data <- data()
        #Removing the NA data 
        omit <- na.omit(my_data)
        numeric <- omit[,input$m]
        # Render a barplot
        ggplot(omit, aes(x = factor(numeric),fill = factor(numeric))) +
           
            geom_bar()+
            xlab(input$m)
    })
    
    output$str <- renderPrint({
        
        AssData <- data()
        str(AssData)
        
    })
    
    output$Continuity <- renderPlot({
        
        mt <- data()
        t <- mt[,input$n] 
        d <- scale(t,center=input$center,scale=input$scale)
        for (col in 1:ncol(d)) {
            d[,col] <- d[order(d[,col]),col] #sort each column in ascending order
        }
         # scale so they can be graphed with a shared Y axis
        mypalette <- rainbow(ncol(d))
        matplot(y = d, type = "l", xlab = "Observations", ylab = "Values", lty = 1, lwd = 1, col = mypalette)
        legend(legend = colnames(d), x = "top", y = "top", lty = 1, lwd = 1, col = mypalette, ncol = round(ncol(d)^0.3))
        
        
    })
    
    output$Mosaic <- renderPlot({
        
        df <- data()
        
        # k5 <- as.factor(input$measure)
        # k6 <- as.factor(input$measure1)
        form <- as.formula(paste(collapse = "+", "~", input$measure))
        mosaicplot(form, data = df, legend = TRUE, shade = TRUE)
    })
    
    output$Homo <- renderPlot({
        
    df <- data()
    
    values <- scale(df[,input$c], center=input$center,scale=input$scale) # Normalise so they can share a common y axis
    matplot(values, type = "l", col = alpha(rainbow(ncol(values)), 0.4) ) #use transparency so you can see into the data
    
    
    })
    
    output$cont <- renderPrint({
        data <- data()
        threshhold <- 0.2
        lines <- "Variables in dataset:"
        for (col in colnames(data)) {
            xline <- paste0("  ", col, " -", paste(collapse = ",", class(data[,col])),"- ")
            ratio <- length(unique(data[,col])) / nrow(data)
            if ( ratio == 1) {
                xline <- paste0(xline, "is continuous, ratio = ", round(ratio,1))
            } else if (ratio > threshhold) {
                xline <- paste0(xline, "is as good as continuous, ratio = ", round(ratio,3))
            } else {
                xline <- paste0(xline, "is not continuous, ratio = ", round(ratio,3))
            }
            lines <- c(lines, xline)
        }
        cat(paste(lines, collapse = "\n"))
        
        
    })
    
    output$pca <- renderPlot({

        data <- data()

        form <- as.formula(paste(collapse = "+", "~", input$z)) # convert all variables to numeric but exclude the intercept
        pca <- prcomp(~ form, center = input$center, scale = input$scale)
        plot(pca$x[,1:2]) # plot only the first two principle components

    })
    
    

    
    
    
})
