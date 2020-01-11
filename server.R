shinyServer(function(input, output, session) {
  
  models <- reactiveValues()
  
  getData <- reactive({
    read.csv(file = "Ass3Data.csv", row.names = "ID")
  })
  
  ############################################################################## 
  getTrControl <- reactive({
    # shared cross validation specification
    y <- getTrainData()[,"Y"]
    n <- 25
    trainControl(method = "boot", number = n, repeats = NA, allowParallel = TRUE, index = caret::createResample(y = y, times = n))
  })
  
  ############################################################################## 
  output$BoxPlots <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, input$Multiplier, length(numeric) > 0)
    d <- scale(d[,numeric], center = input$Normalise, scale = input$Normalise)
    boxplot(d, outline = TRUE, main = paste("Boxplot using IQR multiplier of", input$Multiplier), range = input$Multiplier)
  })
  
  ############################################################################## 
  output$Missing <- renderPlot({
    d <- getData()
    vis_dat(d)
  })
  
  ############################################################################## 
  output$Corr <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, length(numeric) > 0)
    corrgram::corrgram(d, order = "OLO", main = "Numeric Data Correlation")
  })
  
  ############################################################################## 
  output$DataSummary <- renderPrint({
    str(getData())
  })
  
  ############################################################################## 
  output$Table <- DT::renderDataTable({
    d <- getData()
    numeric <- c(FALSE, sapply(d, is.numeric)) # never round rownames which are the first column (when shown)
    DT::datatable(d) %>%
      formatRound(columns = numeric, digits = 3)
  })
  
  ############################################################################## 
  getSplit <- reactive({
    createDataPartition(y = getData()$Y, p = input$Split, list = FALSE)
  })
  
  ############################################################################## 
  getTrainData <- reactive({
    getData()[getSplit(),]
  })
  
  ############################################################################## 
  getTestData <- reactive({
    getData()[-getSplit(),]
  })
  
  ############################################################################## 
  output$SplitSummary <- renderPrint({
    cat(paste("Training observations:", nrow(getTrainData()), "\n", "Testing observations:", nrow(getTestData())))
  })
  
  ##############################################################################  
  getNullRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$NullPreprocess)
  })
  
  ##############################################################################  
  getNullModel <- reactive({
    req(input$NullGo)
    isolate({
      clus <- startMode(input$Parallel)
      tryCatch({
        models$Null <- NULL
        method <- "null"
        showNotification(id = method, paste("Assessing", method, "model using resampling"), session = session, duration = NULL)
        tic("Time taken by null model")
        models$Null <- caret::train(getNullRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 0)
        toc()
        }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
      models$Null
    })
  })
  
  ############################################################################## 
  output$NullModelSummary2 <- renderPrint({
    print(getNullModel())
  })
  
  
  
  ##############################################################################  
  getGlmnetRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$GlmnetPreprocess)
  })
  
  ##############################################################################  
  getGlmnetModel <- reactive({
    library(glmnet)
    req(input$GlmnetGo)
    isolate({
      clus <- startMode(input$Parallel)
      tryCatch({
        models$Glmnet <- NULL
        method <- "glmnet"
        showNotification(id = method, paste("Assessing", method, "model using resampling"), session = session, duration = NULL)
        
        tic("Time Taken for GLMnet")
        models$Glmnet <- caret::train(getGlmnetRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 6)
        toc()
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
      models$Glmnet
    })
  })
  
  ############################################################################## 
  output$GlmnetModelSummary0 <- renderText({
    description("glmnet")
  })

  ############################################################################## 
  output$GlmnetModelSummary1 <- renderTable({
    mod <- getGlmnetModel()
    req(mod)
    as.data.frame(mod$bestTune)
  })  
  
  ############################################################################## 
  output$GlmnetModelPlots <- renderPlot({
    plot(getGlmnetModel())
  })     
  
  ############################################################################## 
  output$GlmnetModelSummary2 <- renderPrint({
    mod <- getGlmnetModel()
    print(mod)
  })

 
  
  ##############################################################################  
  getPlsRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$PlsPreprocess)
  })
  
  ##############################################################################
  getPlsModel <- reactive({
    library(pls)
    req(input$PlsGo)
    isolate({
      clus <- startMode(input$Parallel)
      tryCatch({
        models$Pls <- NULL
        method <- "pls"
        showNotification(id = method, paste("Assessing", method, "model using resampling"), session = session, duration = NULL)
        tic("Time taken by PLS")
        models$Pls <- caret::train(getPlsRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 6)
        toc()
        }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
      models$Pls
    })
  })
  
  ############################################################################## 
  output$PlsModelSummary0 <- renderText({
    description("pls")
  })

  ############################################################################## 
  output$PlsModelSummary1 <- renderTable({
    mod <- getPlsModel()
    req(mod)
    as.data.frame(mod$bestTune)
  })  
  
  ############################################################################## 
  output$PlsModelPlots <- renderPlot({
    plot(getPlsModel())
  })     
  
  ############################################################################## 
  output$PlsModelSummary2 <- renderPrint({
    mod <- getPlsModel()
    summary(mod$finalModel)
  })
  
  
  
  ##############################################################################  
  getRpartRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$RpartPreprocess)
  })
  
  ##############################################################################
  getRpartModel <- reactive({
    library(rpart)
    req(input$RpartGo)
    isolate({
      clus <- startMode(input$Parallel)
      tryCatch({
        models$Rpart <- NULL
        method <- "rpart"
        showNotification(id = method, paste("Assessing", method, "model using resampling"), session = session, duration = NULL)
        tic("Time taken by rpart")
        models$Rpart <- caret::train(getRpartRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 6)
        toc()
        }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
      models$Rpart
    })
  })
  
  ############################################################################## 
  output$RpartModelSummary0 <- renderText({
    description("rpart")
  })
  
  ############################################################################## 
  output$RpartModelSummary1 <- renderTable({
    mod <- getRpartModel()
    req(mod)
    as.data.frame(mod$bestTune)
  })  
  
  ############################################################################## 
  output$RpartModelPlots <- renderPlot({
    plot(getRpartModel())
  })
  
  ############################################################################## 
  output$RpartModelTree <- renderPlot({
    library(rpart.plot)
    rpart.plot::rpart.plot(getRpartModel()$finalModel)
  })     
  
  ############################################################################## 
  output$RpartModelSummary2 <- renderPrint({
    mod <- getRpartModel()
    print(mod)
  })
  
 ############################################################################## 
  
  output$bayesglmModelSummary0 <- renderText({
    description("bayesglm")
  })
  
  ##############################################################################
  
  getbayesglmModelRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$bayesglmModelPreprocess)
  })
  
  ##############################################################################
  #bayesglm
  getbayesglmModel <- reactive({
    library(arm)
    req(input$bayesglmModelGo)
    isolate({
      clus <- startMode(input$Parallel)
      tryCatch({
        models$bayesglm <- NULL
        method <- "bayesglm"
        showNotification(id = method, paste("Assessing", method, "model using resampling"), session = session, duration = NULL)
       tic("Time Taken bayesglm")
        models$bayesglm <- caret::train(getbayesglmModelRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 6)
       toc()
        }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
      models$bayesglm
    })
  })
  
  #############################################################################
  
  output$bayesglmModelSummary1 <- renderTable({
    mod <- getbayesglmModel()
    req(mod)
    as.data.frame(mod$bestTune)
  })  
  
  ############################################################################## 
  
  output$bayesglmModelPlots <- renderPlot({
    plot(getbayesglmModel())
  })
  
  #############################################################################
  
  output$bayesglmModelSummary2 <- renderPrint({
    mod <- getbayesglmModel()
    print(mod)
  })
  
  ##############################################################################
  
  output$gbmModelSummary0 <- renderText({
    description("gbm")
  })
  
  ##############################################################################
  
  gbmModelRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$gbmModelPreprocess)
  })
  
  ##############################################################################
  
  getgbmModel <- reactive({
    library(gbm)
    req(input$gbmModelGo)
    isolate({
      clus <- startMode(input$Parallel)
      tryCatch({
        models$gbm <- NULL
        method <- "gbm"
        showNotification(id = method, paste("Assessing", method, "model using resampling"), session = session, duration = NULL)
        tic("Time taken for gbm")
         models$gbm <- caret::train(gbmModelRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 6)
        toc()
         }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
      models$gbm
    })
  })
  
  ##############################################################################
  
  output$gbmModelSummary1 <- renderTable({
    mod <- getgbmModel()
    req(mod)
    as.data.frame(mod$bestTune)
  })  
  
  ############################################################################## 
  
  output$gbmModelPlots <- renderPlot({
    plot(getgbmModel())
  })
  
  #############################################################################
  
  output$gbmModelSummary2 <- renderPrint({
    mod <- getgbmModel()
    print(mod$finalModel)
  })
  
  #############################################################################
  ## gaussprPoly
  
  ############################################################################
  output$gaussprPolyModelSummary0 <- renderText({
    description("gaussprPoly")
  })
  
  ##############################################################################
  
  gaussprPolyModelRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$gaussprPolyModelPreprocess)
  })
  
  ##############################################################################
  
  getgaussprPolyModel <- reactive({
    library(kernlab)
    req(input$gaussprPolyModelGo)
    isolate({
      clus <- startMode(input$Parallel)
      tryCatch({
        models$gaussprPoly <- NULL
        method <- "gaussprPoly"
        showNotification(id = method, paste("Assessing", method, "model using resampling"), session = session, duration = NULL)
        tic("Time Taken gaussprPoly model")
        models$gaussprPoly <- train(gaussprPolyModelRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 6)
        toc()
         }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
      models$gaussprPoly
    })
  })
  
  ##############################################################################
  
  output$gaussprPolyModelPlots <- renderPlot({
    plot(getgaussprPolyModel())
  })
  
  #############################################################################
  
  output$gaussprPolyModelSummary1 <- renderTable({
    mod <- getgaussprPolyModel()
    req(mod)
    as.data.frame(mod$bestTune)
  }) 
  
  #############################################################################
  
  output$gaussprPolyModelSummary2 <- renderPrint({
    mod <- getgaussprPolyModel()
    print(mod)
  })
  
  ##############################################################################
  
  ## brnn
  
  ################################
  
  output$brnnModelSummary0 <- renderText({
    description("brnn")
  })
  
  ##############################################################################
  
  brnnModelRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$brnnModelPreprocess)
  })
  
  ##############################################################################
  
  getbrnnModel <- reactive({
    library(brnn)
    req(input$brnnModelGo)
    isolate({
      clus <- startMode(input$Parallel)
      tryCatch({
        models$brnn <- NULL
        method <- "brnn"
        showNotification(id = method, paste("Assessing", method, "model using resampling"), session = session, duration = NULL)
        tic("Time Taken for brnn")
        models$brnn <- caret::train(brnnModelRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 6)
        toc()
        }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
      models$brnn
    })
  })
  
  ##############################################################################
  
  output$brnnModelPlots <- renderPlot({
    plot(getbrnnModel())
  })
  
  #############################################################################
  
  output$brnnModelSummary1 <- renderTable({
    mod <- getbrnnModel()
    req(mod)
    as.data.frame(mod$bestTune)
  }) 
  
  #############################################################################
  
  output$brnnModelSummary2 <- renderPrint({
    mod <- getbrnnModel()
    print(mod)
  })
  

  
  ##############################################################################
  
  ## Random Forest:
  
  #############################################################################
  
  output$RandomForestModelSummary0 <- renderText({
    description("rf")
  })
  
  #############################################################################
  
  output$RandomForestSummary1 <- renderTable({
    mod <- getRandomForestModel()
    req(mod)
    as.data.frame(mod$bestTune)
  }) 
  
  #############################################################################
  
  output$RandomForestModelSummary2 <- renderPrint({
    mod <- getRandomForestModel()
    print(mod)
  })
  
  ##############################################################################
  
  randomForestModelRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$RandomForestModelPreprocess)
  })
  
  ##############################################################################
  
  output$RandomForestModelPlots <- renderPlot({
    plot(getRandomForestModel())
  })
  

  
  ##############################################################################
  
  getRandomForestModel <- reactive({
    library(randomForest)
    req(input$RandomForestModelGo)
    isolate({
      clus <- startMode(input$Parallel)
      tryCatch({
        models$RandomForest <- NULL
        method <- "rf"
        showNotification(id = method, paste("Assessing", method, "model using resampling"), session = session, duration = NULL)
        
        tic("Time taken by randomForest")
        models$RandomForest <- caret::train(randomForestModelRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tunelength = 6)
        toc()
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
      models$RandomForest
    })
  })
  
  ##############################################################################
  
  # rvmPoly
  ##################################################
  output$rvmPolyModelSummary0 <- renderText({
    description("rvmPoly")
  })
  
  ##############################################################################
  
  rvmPolyModelRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$rvmPolyModelPreprocess)
  })
  
  ##############################################################################
  
  getrvmPolyModel <- reactive({
    library(kernlab)
    req(input$rvmPolyModelGo)
    isolate({
      clus <- startMode(input$Parallel)
      tryCatch({
        models$rvmPoly <- NULL
        method <- "rvmPoly"
        showNotification(id = method, paste("Assessing", method, "model using resampling"), session = session, duration = NULL)
        tic("Time Taken for rvmPoly")
        set.seed(123)
        models$rvmPoly <- caret::train(rvmPolyModelRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 6)
        toc()
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
      models$rvmPoly
    })
  })
  
  ##############################################################################
  
  output$rvmPolyModelPlots <- renderPlot({
    plot(getrvmPolyModel())
  })
  
  #############################################################################
  
  output$rvmPolyModelSummary1 <- renderTable({
    mod <- getrvmPolyModel()
    req(mod)
    as.data.frame(mod$bestTune)
  }) 
  
  #############################################################################
  
  output$rvmPolyModelSummary2 <- renderPrint({
    mod <- getrvmPolyModel()
    print(mod)
  })
  
  ############################################################################
  #Cubist
  output$CubistModelSummary0 <- renderText({
    description("cubist")
  })
  
  ##############################################################################
  
  getCubistModelRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$CubistModelPreprocess)
  })
  
  ##############################################################################
  
  getCubistModel <- reactive({
    library(Cubist)
    req(input$CubistModelGo)
    isolate({
      clus <- startMode(input$Parallel)
      tryCatch({
        models$Cubist <- NULL
        method <- "cubist"
        showNotification(id = method, paste("Assessing", method, "model using resampling"), session = session, duration = NULL)
        tic("Time Taken Cubist")
        models$Cubist <- caret::train(getCubistModelRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 6)
        toc()
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
      models$Cubist
    })
  })
  
  #############################################################################
  
  output$CubistSummary1 <- renderTable({
    mod <- getCubistModel()
    req(mod)
    as.data.frame(mod$bestTune)
  })  
  
  ############################################################################## 
  
  output$CubistModelPlots <- renderPlot({
    plot(getCubistModel())
  })
  
  #############################################################################
  
  output$CubistModelSummary2 <- renderPrint({
    mod <- getCubistModel()
    print(mod)
  })
  
  ############################################################################## 
  
  
  output$SelectionSummary <- renderPrint({
    results <- resamples(models)
    summary(results)
  })
  
  
 
  ############################################################################## 
  
  getResamples <- reactive({
    results <- caret::resamples(reactiveValuesToList(models))
    NullModel <- "Null"
    
    #scale metrics using null model. Tough code to follow -sorry
    if (input$NullNormalise & NullModel %in% results$models) {
      actualNames <- colnames(results$values)
      # Normalise the various hyper-metrics except R2 (as this is already normalised)
      for (metric in c("RMSE", "MAE")) {
        col <- paste(sep = "~", NullModel, metric)
        if (col %in% actualNames) {
          nullMetric <- mean(results$values[, col], na.rm = TRUE)
          if (!is.na(nullMetric) & nullMetric != 0) {
            for (model in results$models) {
              mcol <- paste(sep = "~", model, metric)
              if (mcol %in% actualNames) {
                results$values[, mcol] <- results$values[, mcol] / nullMetric
              }
            }
          }
        }
      }
    }
    updateRadioButtons(session = session, inputId = "Choice", choices = results$models, selected = "gaussprPoly")
    results
  })
  
  
  ############################################################################## 
  output$SelectionBoxPlot <- renderPlot({
    mod <- getResamples()
    bwplot(mod, notch = input$Notch)
  })
  
  
  ############################################################################## 
  output$Title <- renderUI({
    tags$h3(paste("Unseen data results for chosen model:", input$Choice))
  })
  
  ############################################################################## 
  getTestResults <- reactive({
    test <- getTestData()
    mod <- models[[input$Choice]]
    predictions <- predict(mod, newdata = test)
    d <- data.frame(test$Y, predictions)
    colnames(d) <- c("obs", "pred")
    d
  })
  
  ############################################################################## 
  output$TestSummary <- renderPrint({
    caret::defaultSummary(getTestResults())
  })
  
  ############################################################################## 
  output$TestPlot <- renderPlot({
    d <- getTestResults()
    req(nrow(d) > 0)
    par(pty = "s")
    range <- range(c(d$obs, d$pred), na.rm = TRUE)
    plot(d, xlim = range, ylim = range, main = "Predicted versus Observed for test data")
    abline(a = 0, b = 1, col = c("blue"), lty = c(2), lwd = c(3))
  }, height = 600)
  


####################################################################################

## Ensemble
  
  output$EnsembleModelSummary0 <- renderText({
    description("gbm")
    description("rf")
  })

getEnsembleRecipe <- reactive({
  recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$EnsembleModelPreprocess)
})

getEnsembleModel <- reactive({
  library(caretEnsemble)
  library(gbm)
  req(input$ensembleGo)
  isolate({
    clus <- startMode(input$Parallel)
    tryCatch({
      models$ensemble <- NULL
      method <- c("rf","gbm")
      showNotification(id = method, paste("Assessing model using resampling"), session = session, duration = NULL)
      tic("Time taken by ensemble model")
     
      models$ensemble <- caretEnsemble::caretList(getEnsembleRecipe(), data = getTrainData(), methodList = method, metric = "RMSE", trControl = getTrControl())
      toc()
    }, 
    finally = {
      removeNotification(id = method)
      stopMode(clus)
    })
    models$ensemble
  })
})

output$EnsembleModelSummary2 <- renderPrint({
  mod <- getEnsembleModel()
  print(mod)
})

output$EnsembleModelSummary3 <- renderPrint({
  
  res <- resamples(getEnsembleModel())
  summary(res)
  
  
})

})