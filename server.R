
server <- function(input, output) {
  
  # data <- reactive({
  #   req(input$file1)
  #   data2 <- read.csv(input$file1$datapath)
  #   # df <- scale(data2, center = input$center, scale = input$scale)
  # 
  #   return(data2)
  # 
  # })
  
  data <- reactive({
    
    if(input$k == "mtcars"){
      data2 <- mt
    }
    else{
      data2 <- bt
    }
    
    return(data2)
  })
  
    output$tableX <- DT::renderDataTable({
    mt <- data()
    
    DT::datatable(data = mt,
                  rownames = input$rownames,
                  options = list(searching = TRUE,
                                 pageLength = 10,
                                 lengthMenu = c(5, 10, 100),
                                 ordering = input$order
                  ))
                  # extensions = ext
    # )  %>%
    #   
    #   formatStyle(columns = c("mpg"), backgroundColor = "lightblue")  %>%
    #   #      formatCurrency(c(2), '$') %>%
    #   #      formatPercentage(3, 2) %>%
    #   formatRound(c("mpg"), 3)
  })
  
  output$SelRows <- renderTable({
    req(input$tableX_rows_selected)
    print(mt[input$tableX_rows_selected])
  })
  
  output$summary <- renderPrint({
    y <- data()
    
    dfSummary(y,
              method = 'render',
              headings = FALSE,
              bootstrap.css = FALSE)
    
    })
  
  output$choose_columns_biplot <- renderUI({
    
    the_data <- data()
    
    colnames <- names(the_data[,2:12])
    
    # Create the checkboxes and select them all by default
    selectInput("columns_biplot", "Choose up to five columns to display on the correlation plot below", 
                       choices  = colnames,
                       selected = colnames[1:5],multiple=TRUE)
  })
  makecorPlot <- function(){the_data <- data()
              # Keep the selected columns
              columns_biplot <-    input$columns_biplot
              the_data_subset_biplot <- the_data[, columns_biplot, drop = FALSE]
              # ggpairs(the_data_subset_biplot)
              pairs.panels(the_data_subset_biplot)}
  
  output$corr_plot <- renderPlot({
    print(makecorPlot())
    
  })
  output$correlation <- renderPlot({
    

    the_data <- data()
    # we only want to show numeric cols
    the_data_num <- the_data[,sapply(the_data,is.numeric)]
    # exclude cols with zero variance
    y <- the_data_num[,!apply(the_data_num, MARGIN = 2, function(x) max(x, na.rm = TRUE) == min(x, na.rm = TRUE))]
    ggcorrplot(cor(y), p.mat = cor_pmat(y), hc.order=TRUE, type='lower',lab = TRUE)
  })
  
  output$downloadCorPlot <- downloadHandler(
    filename = function() {
      paste('Corplot-', Sys.Date(), '.pdf', sep='')
    },
    content = function(FILE=NULL) {
      pdf(file=FILE)
      print(makecorPlot())
      dev.off()
    })
  
  output$corr_tables <- renderDT({
    the_data <- data()
    # we only want to show numeric cols
    the_data_num <- the_data[,sapply(the_data,is.numeric)]
    # exclude cols with zero variance
    the_data_num <- the_data_num[,!apply(the_data_num, MARGIN = 2, function(x) max(x, na.rm = TRUE) == min(x, na.rm = TRUE))]
    
    
    res <- Hmisc::rcorr(as.matrix(the_data_num))
    cormat <- res$r
    pmat <- res$P
    ut <- upper.tri(cormat)
    df <- data.frame(
      row = rownames(cormat)[row(cormat)[ut]],
      column = rownames(cormat)[col(cormat)[ut]],
      cor  = (cormat)[ut],
      pValue = pmat[ut]
    )
    with(df, df[order(-cor), ])
  })
  
  
  output$Diagnosis <- renderPrint({
    
    columns <-    input$columns
    data3 <- data()
    the_data <- na.omit(data())
    the_data_subset <- na.omit(the_data[, columns, drop = FALSE])
    the_data <- the_data_subset[,input$columns]
    mtcars.pca <- prcomp(the_data, center = TRUE,scale. = TRUE)
      summary(mtcars.pca)
    })
  
  output$str <- renderPrint({
    columns <-    input$columns
    data3 <- data()
    the_data <- na.omit(data())
    the_data_subset <- na.omit(the_data[, columns, drop = FALSE])
    the_data <- the_data_subset[,input$columns]
    mtcars.pca <- prcomp(the_data, center = TRUE,scale. = TRUE)
    str(mtcars.pca)
  })
  output$plotData <- renderPlotly({
    
    columns <-    input$columns
    data3 <- data()
    the_data <- na.omit(data())
    the_data_subset <- na.omit(the_data[, columns, drop = FALSE])
    
    mtcars.pca <- prcomp(na.omit(the_data_subset),center = (input$center == 'Yes'), 
    scale. = (input$scale. == 'Yes'))
    #mtcars.pca <- pca_objects()$pca_output
    p <- ggbiplot(mtcars.pca, labels=rownames(the_data_subset))
    p
  })
  
  output$plot <- renderPlotly({
    columns <-    input$columns
    data3 <- data()
    the_data <- na.omit(data())
    the_data_subset <- na.omit(the_data[, columns, drop = FALSE])
    
    
    pcaCars <-  princomp(na.omit(the_data_subset),cor = TRUE,center = (input$center == 'Yes'), 
                         scale. = (input$scale. == 'Yes'))
    # cluster cars
    carsHC <- hclust(dist(pcaCars$scores), method = input$choice3)
    # cut the dendrogram into 3 clusters
    carsClusters <- cutree(carsHC, k = 3)
    # add cluster to data frame of scores
    carsDf <- data.frame(pcaCars$scores, "cluster" = factor(carsClusters))
    carsDf <- transform(carsDf, cluster_name = paste("Cluster",carsClusters))
    pc1 <- carsDf[,input$choice1]
    pc2 <- carsDf[,input$choice2]
    p <- plot_ly(carsDf, x = pc1 , y = pc2, text = rownames(carsDf),
                 mode = "markers", color = carsDf$cluster_name, marker = list(size = 11)) 
    
    p <- layout(p, title = "PCA Clusters from Hierarchical Clustering of Cars Data", 
                xaxis = list(title = input$choice1),
                yaxis = list(title = input$choice2))
    p
  })
  
  
  # Check boxes to choose columns
  output$choose_columns_pca <- renderUI({
    
    the_data <- data()
    
    # Get the data set with the appropriate name
    
    # we only want to show numeric cols
    the_data_num <- na.omit(the_data[,sapply(the_data,is.numeric)])
    # exclude cols with zero variance
    the_data_num <- the_data_num[,!apply(the_data_num, MARGIN = 2, function(x) max(x, na.rm = TRUE) == min(x, na.rm = TRUE))]
    
    
    colnames <- names(the_data_num)
    
    # Create the checkboxes and select them all by default
    selectInput("columns", "Choose columns", 
                       choices  = colnames,
                       selected = colnames,multiple=TRUE)
  })
  
  # choose a grouping variable
  output$the_grouping_variable <- renderUI({
    the_data <- data()
    
    
    # for grouping we want to see only cols where the number of unique values are less than 
    # 10% the number of observations
    grouping_cols <- sapply(seq(1, ncol(the_data)), function(i) length(unique(the_data[,i])) < nrow(the_data)/10 )
    
    the_data_group_cols <- the_data[, grouping_cols, drop = FALSE]
    # drop down selection
    selectInput(inputId = "the_grouping_variable", 
                label = "Grouping variable:",
                choices=c("None", names(the_data_group_cols)))
    
  })
  
  
  pca_objects <- reactive({
    # Keep the selected columns
    columns <-    input$columns
    the_data <- na.omit(data())
    the_data_subset <- na.omit(the_data[, columns, drop = FALSE])

    pca_output <- prcomp(na.omit(the_data_subset), 
                         center = (input$center == 'Yes'), 
                         scale. = (input$scale. == 'Yes'))
    # data.frame of PCs
    pcs_df <- cbind(the_data, pca_output$x)
    
    return(list(the_data = the_data, 
                the_data_subset = the_data_subset,
                pca_output = pca_output, 
                pcs_df = pcs_df))
    
  })
  
  output$the_pcs_to_plot_x <- renderUI({
    pca_output <- pca_objects()$pca_output$x
    
    # drop down selection
    selectInput(inputId = "the_pcs_to_plot_x", 
                label = "X axis:",
                choices= colnames(pca_output), 
                selected = 'PC1')
  })
  
  output$the_pcs_to_plot_y <- renderUI({
    pca_output <- pca_objects()$pca_output$x
    
    # drop down selection
    selectInput(inputId = "the_pcs_to_plot_y", 
                label = "Y axis:",
                choices= colnames(pca_output), 
                selected = 'PC2')
  })
  
  
  
  output$plot2 <- renderPlot({
    pca_output <- pca_objects()$pca_output
    eig = (pca_output$sdev)^2
    variance <- eig*100/sum(eig)
    cumvar <- paste(round(cumsum(variance),1), "%")
    eig_df <- data.frame(eig = eig,
                         PCs = colnames(pca_output$x),
                         cumvar =  cumvar)
    ggplot(eig_df, aes(reorder(PCs, -eig), eig)) +
      geom_bar(stat = "identity", fill = "lightblue", colour = "blue") + 
      
      geom_text(label = cumvar, size = 4,
                vjust=-0.4) +
      theme_bw(base_size = 14) +
      xlab("PC") +
      ylab("Variances") +
      ylim(0,(max(eig_df$eig) * 1.1))
  })
  
  
  # PC plot
  pca_biplot <- reactive({
    pcs_df <- pca_objects()$pcs_df
    pca_output <-  pca_objects()$pca_output
    
    var_expl_x <- round(100 * pca_output$sdev[as.numeric(gsub("[^0-9]", "", input$the_pcs_to_plot_x))]^2/sum(pca_output$sdev^2), 1)
    var_expl_y <- round(100 * pca_output$sdev[as.numeric(gsub("[^0-9]", "", input$the_pcs_to_plot_y))]^2/sum(pca_output$sdev^2), 1)
    labels <- rownames(pca_output$x)
    grouping <- input$the_grouping_variable
    
    if(grouping == 'None'){
      # plot without grouping variable
      pc_plot_no_groups  <- ggplot(pcs_df, 
                                   aes_string(input$the_pcs_to_plot_x, 
                                              input$the_pcs_to_plot_y
                                   )) +
        
        
        geom_text(aes(label = labels),  size = 5) +
        theme_bw(base_size = 14) +
        coord_equal() +
        xlab(paste0(input$the_pcs_to_plot_x, " (", var_expl_x, "% explained variance)")) +
        ylab(paste0(input$the_pcs_to_plot_y, " (", var_expl_y, "% explained variance)")) 
      # the plot
      pc_plot_no_groups
      
      
    } else {
      # plot with grouping variable
      
      pcs_df$fill_ <-  as.character(pcs_df[, grouping, drop = TRUE])
      pc_plot_groups  <- ggplot(pcs_df, aes_string(input$the_pcs_to_plot_x, 
                                                   input$the_pcs_to_plot_y, 
                                                   fill = 'fill_', 
                                                   colour = 'fill_'
      )) +
        stat_ellipse(geom = "polygon", alpha = 0.1) +
        
        geom_text(aes(label = labels),  size = 5) +
        theme_bw(base_size = 14) +
        scale_colour_discrete(guide = FALSE) +
        guides(fill = guide_legend(title = "groups")) +
        theme(legend.position="top") +
        coord_equal() +
        xlab(paste0(input$the_pcs_to_plot_x, " (", var_expl_x, "% explained variance)")) +
        ylab(paste0(input$the_pcs_to_plot_y, " (", var_expl_y, "% explained variance)")) 
      # the plot
      pc_plot_groups
    }
    
    
  })
  
  output$brush_info <- renderTable({
    # the brushing function
    brushedPoints(pca_objects()$pcs_df, input$plot_brush)
  })
  
  
  # for zooming
  output$z_plot1 <- renderPlotly({
    
    pca_biplot() 
    
  })
  
  # zoom ranges
  zooming <- reactiveValues(x = NULL, y = NULL)
  
  observe({
    brush <- input$z_plot1Brush
    if (!is.null(brush)) {
      zooming$x <- c(brush$xmin, brush$xmax)
      zooming$y <- c(brush$ymin, brush$ymax)
    }
    else {
      zooming$x <- NULL
      zooming$y <- NULL
    }
  })
  
  
  # for zooming
  output$z_plot2 <- renderPlot({
    
    pca_biplot() + coord_cartesian(xlim = zooming$x, ylim = zooming$y) 
    
    
  })
  
  output$brush_info_after_zoom <- renderTable({
    # the brushing function
    brushedPoints(pca_objects()$pcs_df, input$plot_brush_after_zoom)
  })
  
  output$pca_details <- renderPrint({
    # 
    print(pca_objects()$pca_output$rotation)
    summary(pca_objects()$pca_output)
    
  })
  
  output$pcr_detais1 <- renderPrint({
    boston <- data()
    set.seed(123)
    training.samples <- boston$medv %>%
      createDataPartition(p = 0.8, list = FALSE)
    train.data <- boston[training.samples,]
    test.data <- boston[-training.samples,]
    model <- train( medv~., data = train.data, method = "pcr", scale = TRUE, trControl = trainControl("cv",number = 10),tuneLength = 10)
    summary(model$finalModel)
    
    
  })
  
  output$pcr_detais2 <- renderPrint({
    boston <- data()
    training.samples <- boston$medv %>%
      createDataPartition(p = 0.8, list = FALSE)
    train.data <- boston[training.samples,]
    test.data <- boston[-training.samples,]
    model <- train( medv~., data = train.data, method = "pcr", scale = TRUE, trControl = trainControl("cv",number = 10),tuneLength = 10)
    predictions <- model %>% predict(test.data)
    data.frame(RMSE = caret::RMSE(predictions, test.data$medv),
               Rsquare = caret::R2(predictions, test.data$medv)
               )
    
  })
  
  output$pls_detais1 <- renderPrint({
    boston <- data()
    set.seed(123)
    training.samples <- boston$medv %>%
      createDataPartition(p = 0.8, list = FALSE)
    train.data <- boston[training.samples,]
    test.data <- boston[-training.samples,]
    model <- train( medv~., data = train.data, method = "pls", scale = TRUE, trControl = trainControl("cv",number = 10),tuneLength = 10)
    summary(model$finalModel)
    
  })
  
  output$pls_detais2 <- renderPrint({
    boston <- data()
    training.samples <- boston$medv %>%
      createDataPartition(p = 0.8, list = FALSE)
    train.data <- boston[training.samples,]
    test.data <- boston[-training.samples,]
    model <- train( medv~., data = train.data, method = "pls", scale = TRUE, trControl = trainControl("cv",number = 10),tuneLength = 10)
    predictions <- model %>% predict(test.data)
    data.frame(RMSE = caret::RMSE(predictions, test.data$medv),
               Rsquare = caret::R2(predictions, test.data$medv)
    )
    
  })
  
  output$pls_plot4 <- renderPlot({
    
    boston <- data()
    training.samples <- boston$medv %>%
      createDataPartition(p = 0.8, list = FALSE)
    train.data <- boston[training.samples,]
    test.data <- boston[-training.samples,]
    model <- train( medv~., data = train.data, method = "pls", scale = TRUE, trControl = trainControl("cv",number = 10),tuneLength = 10)
    predictions <- model %>% predict(test.data)
    plot(test.data$medv, predictions,main="Test Dataset", xlab="observed", ylab="PLS Predicted")
    abline(0, 1, col="red")
    
    
    
  })
  
  output$pls_plot1 <- renderPlot({
    boston <- data()
    gas1 <- plsr(crim ~., ncomp = input$comp1, data = boston, validation = "CV")
    plot(RMSEP(gas1), legendpos = "topright")
    
  })
  output$pcr_plot1 <- renderPlot({
    boston <- data()
    gas1 <- pcr(crim ~., ncomp = input$comp, data = boston, validation = "CV")
    plot(RMSEP(gas1), legendpos = "topright")
    
  })
  
  output$pcr_plot2 <- renderPlot({
    boston <- data()
    gas1 <- pcr(crim ~., ncomp = input$comp3, data = boston, validation = "CV")
    plot(gas1, plottype = "scores", comps = 1:input$comp3)
    
  })
  
  output$pls_plot2 <- renderPlot({
    boston <- data()
    gas1 <- plsr(crim ~., ncomp = input$comp2, data = boston, validation = "CV")
    plot(gas1, plottype = "scores", comps = 1:input$comp2)
    
  })
  
  output$pls_plot3 <- renderPlot({
    boston <- data()
    gas1 <- plsr(crim ~., ncomp = input$comp5, data = boston, validation = "CV")
  plot(gas1, "loadings", comps = 1:input$comp5, legendpos = "topleft", xlab = "variables")
  abline(h = 0)
  })
  
  
  output$pcr_plot3 <- renderPlot({
    boston <- data()
    gas1 <- pcr(crim ~., ncomp = input$comp6, data = boston, validation = "CV")
    plot(gas1, "loadings", comps = 1:input$comp6, legendpos = "topleft", xlab = "nm")
    abline(h = 0)
  })
  
  output$pca_bar <- renderPlot({
    
    dat <- data()
    columns <-    input$columns
    the_data <- na.omit(data())
    the_data_subset <- na.omit(the_data[, columns, drop = FALSE])
  
    respca  <- prcomp(na.omit(the_data_subset), 
                         center = (input$center == 'Yes'), 
                         scale. = (input$scale. == 'Yes'))
    
    
    pcloadings <- t(respca$sdev*t(respca$rotation))
    #pcscores<-scale(datpca)%*%respca$rotation*sqrt(nr/(nr-1))
    par(mfrow = c(1, 2))
    barplot(pcloadings[,1], main="PC 1",ylim=c(-1,1), cex.names=0.7 , border = "dark blue",  col = c("lightblue", "mistyrose", "cyan",
                                                                                                     "lavender", "cornsilk"))
    barplot(pcloadings[,2], main="PC 2", ylim=c(-1,1), cex.names=0.7, border = "dark blue", col= c("lightblue", "mistyrose", "cyan",
                                                                                                   "lavender", "cornsilk"))
  })
  
  output$pls_scree<- renderPlot({
    
    bost <- data()
    X = model.matrix( crim ~ ., data=bost )
    pc = prcomp(X)
    # screeplot(pc, type = "lines")
    
    p <- fviz_eig(pc, addlabels=TRUE, hjust = -0.3,
                  barfill="white", barcolor ="darkblue",
                  linecolor ="red") + ylim(0, 85) + 
      theme_minimal()
    print(p)
    
    
  })
  
  output$missing<- renderPlot({
    mt <- data()
    missmap(mt, col = c("mistyrose", "skyblue"),title = " ")
    
  })
 output$eigen <- renderPrint({
   mt <- data()
   mt <- mt[,2:11]
   
   res.pca <- PCA(mt, graph = FALSE)
   eig.val <- get_eigenvalue(res.pca)
   print(eig.val)
   
 }) 
 output$corr_dim <- renderPlot({
  
   
   columns <-    input$columns
   data3 <- data()
   the_data <- na.omit(data())
   the_data_subset <- na.omit(the_data[, columns, drop = FALSE])
   
   # mtcars.pca <- prcomp(na.omit(the_data_subset),center = (input$center == 'Yes'), 
   #                      scale. = (input$scale. == 'Yes'))
   
   
   
   res.pca <- PCA(the_data_subset, graph = FALSE)
   var <- get_pca_var(res.pca)
   corrplot(var$contrib, is.corr=FALSE)    
   
 }) 
  
  
}






