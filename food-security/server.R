library(shiny)
library(shinythemes)
library(ggplot2)
library(summarytools)
library(DT)
library(gmodels)
library(caret)
library(nnet)
library(rpart)
library(rattle)
library(ranger)
library(graphics)

# Function to create horizontal bar plots
createBarPlot <- function(group, data){
  barPlot <- data %>% 
    ggplot(aes(x = foodSecurity, group = eval(parse(text = group)))) +
    geom_bar(aes(y = ..prop.., fill = factor(..x..))) +
    scale_y_continuous(labels=scales::percent) +
    scale_fill_discrete(
      name = "Food Security", 
      labels = c("High", "Marginal", "Low", "Very Low", "No Response")) +
      labs(x = "Food Security", y = "Relative Frequencies", title = "") +
      facet_wrap(vars(eval(parse(text = group))))
    print(barPlot)
}

# Function to create vertical bar plots
createOtherPlot <- function(group, data){
  otherPlot <- data %>% 
    summarize(n = n()) %>% 
    mutate(perc = 100*n/sum(n)) %>% 
    ggplot(aes(x = eval(parse(text = group)), y = perc)) +
    geom_bar(stat = "identity") +
    facet_grid(~ foodSecurity) + 
    coord_flip()
  print(otherPlot)
}

shinyServer(function(input, output, session) {
  
  # Create a title for the frequency table
  output$freqTableTitle <- renderUI({
    h3("Frequency Table of ", input$freqVariable)
  })

  # Create a frequency table for a specified categorical variable
  output$freqTable <- DT::renderDataTable({
    if(input$summaryType == "Numerical"){
      if(input$numericalType == "Frequency Tables"){
        if(input$freqVariable == "Food Security"){
          t <- freq(foodSecurity$foodSecurity)
          # Print only selected rows and columns of table, rounding to two 
          # decimal places.
          round(t[-6, -2:-3], 2)
        } else {
        if(input$freqVariable == "Sex"){
          t <- freq(foodSecurity$sex)
          round(t[-3, -2:-3], 2)
        } else {
        if(input$freqVariable == "Race"){
          t <- freq(foodSecurity$race)
          round(t[-17, -2:-3], 2)
        } else {
        if(input$freqVariable == "Hispanic Origin"){
          t <- freq(foodSecurity$hispanicOrigin)
          round(t[-3, -2:-3], 2)
        } else {
        if(input$freqVariable == "Age"){
          t <- freq(foodSecurity$age)
          round(t[-19, -2:-3], 2)
        } else {
        if(input$freqVariable == "US Citizenship"){
          t <- freq(foodSecurity$citizenship)
          round(t[-6, -2:-3], 2)
        } else {
        if(input$freqVariable == "Type of Household"){
          t <- freq(foodSecurity$typeHH)
          round(t[-11, -2:-3], 2)
        } else {
        if(input$freqVariable == "Number of Household Members"){
          t <- freq(foodSecurity$numHHMembers)
          round(t[-11, -2:-3], 2)
        } else {
        if(input$freqVariable == "Employment Status"){
          t <- freq(foodSecurity$employStatus)
          round(t[-8, -2:-3], 2)
        } else {
        if(input$freqVariable == "Annual Household Income"){
          t <- freq(foodSecurity$annualHHIncome)
          round(t[c(-1,-17), -2:-3], 2)
        } else {
        if(input$freqVariable == "Marital Status"){
          t <- freq(foodSecurity$maritalStatus)
          round(t[-8, -2:-3], 2)
        } else {
        if(input$freqVariable == "Living Quarters"){
          t <- freq(foodSecurity$livingQuarters)
          round(t[c(-1,-6), -2:-3], 2)
        } else {
        if(input$freqVariable == "Education Level"){
          t <- freq(foodSecurity$educationLevel)
          round(t[-16, -2:-3], 2)
        } else {
        if(input$freqVariable == "Household Recieved SNAP Benefits"){
          t <- freq(foodSecurity$receivedSNAP)
          round(t[-6, -2:-3], 2)
        }}}}}}}}}}}}}}  # Close out nested if/else statements
      }
    }
  })
  
  # Create a title for the contingency table
  output$contingencyTableTitle <- renderUI({
    h3("Contingency Table of ", input$contingencyVar1,
       " vs. ", input$contingencyVar2)
  })

  # Using `gmodels` package, create a contingency table  for two specified 
  # categorical variables.
  output$contingencyTable <- renderPrint({
    with(foodSecurity,
         CrossTable(
           eval(parse(text = input$contingencyVar1)),
           eval(parse(text = input$contingencyVar2)),
           digits = 2,  # Round to two decimal places
           dnn = c(input$contingencyVar1,
                   input$contingencyVar2) # Rename variable titles
         )
    )
  })
  
  output$barPlot <- renderPlot({
    if(input$summaryType == "Graphical"){
      if(input$plotType == "Bar Plot - Vertical"){
        if(input$barPlotVariable == "Sex"){
          createBarPlot("sex", foodSecurity)
        }
        if(input$barPlotVariable == "Race"){
          createBarPlot("race", foodSecurity)
        }
        if(input$barPlotVariable == "Hispanic Origin"){
          createBarPlot("hispanicOrigin", foodSecurity)
        }
        if(input$barPlotVariable == "Age"){
          createBarPlot("age", foodSecurity)
        }
        if(input$barPlotVariable == "US Citizenship"){
          createBarPlot("citizenship", foodSecurity)
        }
        if(input$barPlotVariable == "Type of Household"){
          createBarPlot("typeHH", foodSecurity)
        }
        if(input$barPlotVariable == "Number of Household Members"){
          createBarPlot("numHHMembers", foodSecurity)
        }
        if(input$barPlotVariable == "Employment Status"){
          createBarPlot("employStatus", foodSecurity)
        }
        if(input$barPlotVariable == "Annual Household Income"){
          createBarPlot("annualHHIncome", foodSecurity)
        }
        if(input$barPlotVariable == "Marital Status"){
          createBarPlot("maritalStatus", foodSecurity)
        }
        if(input$barPlotVariable == "Living Quarters"){
          createBarPlot("livingQuarters", foodSecurity)
        }
        if(input$barPlotVariable == "Education Level"){
          createBarPlot("educationLevel", foodSecurity)
        }
        if(input$barPlotVariable == "Household Recieved SNAP Benefits"){
          createBarPlot("receivedSNAP", foodSecurity)
        }
      }
    }
  })
  
  output$otherPlot <- renderPlot({
    if(input$summaryType == "Graphical"){
      if(input$plotType == "Bar Plot - Horizontal"){
        if(input$otherPlotVariable == "Sex"){
          # Attempted to use a function to create this plot, but 
          # `eval(parse(text = "..."))` wasn't compatible with 
          # `group_by()`.  More research needed.
          foodSecurity %>%
            group_by(sex, foodSecurity) %>%
            summarize(n = n()) %>%
            mutate(perc = 100*n/sum(n)) %>%
            ggplot(aes(x = sex, y = perc)) +
              geom_bar(stat = "identity") +
              facet_grid(~ foodSecurity) +
              coord_flip()
        } else {
        if(input$otherPlotVariable == "Race"){
          foodSecurity %>%
            group_by(race, foodSecurity) %>%
            summarize(n = n()) %>%
            mutate(perc = 100*n/sum(n)) %>%
            ggplot(aes(x = race, y = perc)) +
              geom_bar(stat = "identity") +
              facet_grid(~ foodSecurity) +
              coord_flip()
        } else {
        if(input$otherPlotVariable == "Hispanic Origin"){
          foodSecurity %>%
            group_by(hispanicOrigin, foodSecurity) %>%
            summarize(n = n()) %>%
            mutate(perc = 100*n/sum(n)) %>%
            ggplot(aes(x = hispanicOrigin, y = perc)) +
              geom_bar(stat = "identity") +
              facet_grid(~ foodSecurity) +
              coord_flip()
        } else {
        if(input$otherPlotVariable == "Age"){
          foodSecurity %>%
            group_by(age, foodSecurity) %>%
            summarize(n = n()) %>%
            mutate(perc = 100*n/sum(n)) %>%
            ggplot(aes(x = age, y = perc)) +
              geom_bar(stat = "identity") +
              facet_grid(~ foodSecurity) +
              coord_flip()
        } else {
        if(input$otherPlotVariable == "US Citizenship"){
          foodSecurity %>%
            group_by(citizenship, foodSecurity) %>%
            summarize(n = n()) %>%
            mutate(perc = 100*n/sum(n)) %>%
            ggplot(aes(x = citizenship, y = perc)) +
              geom_bar(stat = "identity") +
              facet_grid(~ foodSecurity) +
              coord_flip()
        } else {
        if(input$otherPlotVariable == "Type of Household"){
          foodSecurity %>%
            group_by(typeHH, foodSecurity) %>%
            summarize(n = n()) %>%
            mutate(perc = 100*n/sum(n)) %>%
            ggplot(aes(x = typeHH, y = perc)) +
              geom_bar(stat = "identity") +
              facet_grid(~ foodSecurity) +
              coord_flip()
        } else {
        if(input$otherPlotVariable == "Number of Household Members"){
          foodSecurity %>%
            group_by(numHHMembers, foodSecurity) %>%
            summarize(n = n()) %>%
            mutate(perc = 100*n/sum(n)) %>%
            ggplot(aes(x = numHHMembers, y = perc)) +
              geom_bar(stat = "identity") +
              facet_grid(~ foodSecurity) +
              coord_flip()
        } else {
        if(input$otherPlotVariable == "Employment Status"){
          foodSecurity %>%
            group_by(employStatus, foodSecurity) %>%
            summarize(n = n()) %>%
            mutate(perc = 100*n/sum(n)) %>%
            ggplot(aes(x = employStatus, y = perc)) +
              geom_bar(stat = "identity") +
              facet_grid(~ foodSecurity) +
              coord_flip()
        } else {
        if(input$otherPlotVariable == "Annual Household Income"){
          foodSecurity %>%
            group_by(annualHHIncome, foodSecurity) %>%
            summarize(n = n()) %>%
            mutate(perc = 100*n/sum(n)) %>%
            ggplot(aes(x = annualHHIncome, y = perc)) +
              geom_bar(stat = "identity") +
              facet_grid(~ foodSecurity) +
              coord_flip()
        } else {
        if(input$otherPlotVariable == "Marital Status"){
          foodSecurity %>%
            group_by(maritalStatus, foodSecurity) %>%
            summarize(n = n()) %>%
            mutate(perc = 100*n/sum(n)) %>%
            ggplot(aes(x = maritalStatus, y = perc)) +
              geom_bar(stat = "identity") +
              facet_grid(~ foodSecurity) +
              coord_flip()
        } else {
        if(input$otherPlotVariable == "Living Quarters"){
          foodSecurity %>% group_by(livingQuarters, foodSecurity) %>%
            summarize(n = n()) %>%
            mutate(perc = 100*n/sum(n)) %>%
            ggplot(aes(x = livingQuarters, y = perc)) +
              geom_bar(stat = "identity") +
              facet_grid(~ foodSecurity) +
              coord_flip()
                } else {
        if(input$otherPlotVariable == "Education Level"){
          foodSecurity %>%
            group_by(educationLevel, foodSecurity) %>%
            summarize(n = n()) %>%
            mutate(perc = 100*n/sum(n)) %>% 
            ggplot(aes(x = educationLevel, y = perc)) +
              geom_bar(stat = "identity") +
              facet_grid(~ foodSecurity) +
              coord_flip()
        } else {
        if(input$otherPlotVariable == "Household Recieved SNAP Benefits"){
          foodSecurity %>%
            group_by(receivedSNAP, foodSecurity) %>%
            summarize(n = n()) %>%
            mutate(perc = 100*n/sum(n)) %>%
            ggplot(aes(x = receivedSNAP, y = perc)) +
              geom_bar(stat = "identity") +
              facet_grid(~ foodSecurity) +
              coord_flip()
        }}}}}}}}}}}}}  # Close out nested if/else statements
      }
    }
  })
  
  observeEvent(input$createMosaicPlot, {
    output$adjustMosaicPlotHeight <- renderUI({
      sliderInput("height",
                  label = "Adjust plot height:",
                  min = 500, max = 900, value = 700)
    })
    output$adjustMosaicPlotWidth <- renderUI({
      sliderInput("width",
                  label = "Adjust plot width:", 
                  min = 600, max = 1200, value = 900)
    })
      
    output$mosaicPlot <- renderPlot(
      width = function() input$width,
      height = function() input$height,
      res = 96,
      {
      if(input$summaryType == "Graphical"){
        if(input$plotType == "Mosaic Plot"){
          if(input$mosaicPlotVariable == "Sex"){
            mosaicplot(~ foodSecurityNR$sex + foodSecurityNR$foodSecurity,
                       shade = TRUE,  # Add color to plot
                       las = 2,  # Print variable names vertically
                       main = "'Sex' vs. 'Food Security'",
                       xlab = "Sex",
                       ylab = "Food Security")
          } else {
          if(input$mosaicPlotVariable == "Race"){
            mosaicplot(~ foodSecurityNR$race + foodSecurityNR$foodSecurity,
                       shade = TRUE,  # Add color to plot
                       las = 2,  # Print variable names vertically
                       main = "'Race' vs. 'Food Security'",
                       xlab = "Race",
                       ylab = "Food Security")
          } else {
          if(input$mosaicPlotVariable == "Hispanic Origin"){
            mosaicplot(~ foodSecurityNR$hispanicOrigin + foodSecurityNR$foodSecurity,
                       shade = TRUE,  # Add color to plot
                       las = 2,  # Print variable names vertically
                       main = "'Hispanic Origin' vs. 'Food Security'",
                       xlab = "Hispanic Origin",
                       ylab = "Food Security")
          } else {
          if(input$mosaicPlotVariable == "Age"){
            mosaicplot(~ foodSecurityNR$age + foodSecurityNR$foodSecurity,
                       shade = TRUE,  # Add color to plot
                       las = 2,  # Print variable names vertically
                       main = "'Age' vs. 'Food Security'",
                       xlab = "Age",
                       ylab = "Food Security")
          } else {
          if(input$mosaicPlotVariable == "US Citizenship"){
            mosaicplot(~ foodSecurityNR$citizenship + foodSecurityNR$foodSecurity,
                       shade = TRUE,  # Add color to plot
                       las = 2,  # Print variable names vertically
                       main = "'US Citizenship' vs. 'Food Security'",
                       xlab = "US Citizenship",
                       ylab = "Food Security")
          } else {
          if(input$mosaicPlotVariable == "Type of Household"){
            mosaicplot(~ foodSecurityNR$typeHH + foodSecurityNR$foodSecurity,
                       shade = TRUE,  # Add color to plot
                       las = 2,  # Print variable names vertically
                       main = "'Type of Household' vs. 'Food Security'",
                       xlab = "Type of Household",
                       ylab = "Food Security")
          } else {
          if(input$mosaicPlotVariable == "Number of Household Members"){
            mosaicplot(~ foodSecurityNR$numHHMembers + foodSecurityNR$foodSecurity,
                       shade = TRUE,  # Add color to plot
                       las = 2,  # Print variable names vertically
                       main = "'Number of Household Members' vs. 'Food Security'",
                       xlab = "Number of Household Members",
                       ylab = "Food Security")
          } else {
          if(input$mosaicPlotVariable == "Employment Status"){
            mosaicplot(~ foodSecurityNR$employStatus + foodSecurityNR$foodSecurity,
                       shade = TRUE,  # Add color to plot
                       las = 2,  # Print variable names vertically
                       main = "'Employment Status' vs. 'Food Security'",
                       xlab = "Employment Status",
                       ylab = "Food Security")
          } else {
          if(input$mosaicPlotVariable == "Annual Household Income"){
            mosaicplot(~ foodSecurityNR$annualHHIncome + foodSecurityNR$foodSecurity,
                       shade = TRUE,  # Add color to plot
                       las = 2,  # Print variable names vertically
                       main = "'Annual Household Income' vs. 'Food Security'",
                       xlab = "Annual Household Income",
                       ylab = "Food Security")
          } else {
          if(input$mosaicPlotVariable == "Marital Status"){
            mosaicplot(~ foodSecurityNR$maritalStatus + foodSecurityNR$foodSecurity,
                       shade = TRUE,  # Add color to plot
                       las = 2,  # Print variable names vertically
                       main = "'Marital Status' vs. 'Food Security'",
                       xlab = "Marital Status",
                       ylab = "Food Security")
          } else {
          if(input$mosaicPlotVariable == "Living Quarters"){
            mosaicplot(~ foodSecurityNR$livingQuarters + foodSecurityNR$foodSecurity,
                       shade = TRUE,  # Add color to plot
                       las = 2,  # Print variable names vertically
                       main = "'Living Quarters' vs. 'Food Security'",
                       xlab = "Living Quarters",
                       ylab = "Food Security")
          } else {
          if(input$mosaicPlotVariable == "Education Level"){
            mosaicplot(~ foodSecurityNR$educationLevel + foodSecurityNR$foodSecurity,
                       shade = TRUE,  # Add color to plot
                       las = 2,  # Print variable names vertically
                       main = "'Education Level' vs. 'Food Security'",
                       xlab = "Education Level",
                       ylab = "Food Security")
          } else {
          if(input$mosaicPlotVariable == "Household Recieved SNAP Benefits"){
            mosaicplot(~ foodSecurityNR$receivedSNAP + foodSecurityNR$foodSecurity,
                       shade = TRUE,  # Add color to plot
                       las = 2,  # Print variable names vertically
                       main = "'Household Recieved SNAP Benefits' vs. 'Food Security'",
                       xlab = "Household Recieved SNAP Benefits",
                       ylab = "Food Security")
          }}}}}}}}}}}}}  # Close out nested if/else statements
        }
      }
    })
  })
  
    observeEvent(input$runMLM, {
        output$summaryMulti <- renderPrint({
            
            # Create a Progress object
            progress <- Progress$new()
            # Make sure it closes when we exit this reactive, even if there's an error.
            on.exit(progress$close())
            # Set the message to the user while cross-validation is running.
            progress$set(message = "Calculation in progress",
                         detail = "This may take a while...")
            
            # Remove "No Response" from `foodSecurity`
            data <- foodSecurity %>% filter(foodSecurity != "No Response")
            data$foodSecurity <- droplevels(data$foodSecurity)
            
            vars <- unlist(input$multiModelVars)
            trainIndex <- createDataPartition(data$foodSecurity,
                                            p = input$splitPercent/100, 
                                            list = FALSE, 
                                            times = 1)
            trainData <- data[ trainIndex,]
            testData  <- data[-trainIndex,]
            multinomFit <- train(foodSecurity ~ ., 
                                 data = trainData[,c(c("foodSecurity"), vars)],
                                 method = 'multinom',
                                 trControl = trainControl(method = "cv", 
                                                          number = input$numFolds),
                                 # Do not print output from the cross validation
                                 trace = FALSE,
                                 # Exclude any observations with missing data
                                 na.action = na.exclude)
            summary(multinomFit)
        })
    })
    
    observeEvent(input$runClassTree, {
        output$summaryClassTree <- renderPlot({
            
            # Create a Progress object
            progress <- Progress$new()
            # Make sure it closes when we exit this reactive, even if there's an error.
            on.exit(progress$close())
            # Set the message to the user while cross-validation is running.
            progress$set(message = "Calculation in progress",
                         detail = "This may take a while...")
            
            # Remove "No Response" from `foodSecurity`
            data <- foodSecurity %>% filter(foodSecurity != "No Response")
            data$foodSecurity <- droplevels(data$foodSecurity)
            
            vars <- unlist(input$classTreeVars)
            trainIndex <- createDataPartition(data$foodSecurity,
                                              p = input$splitPercent/100, 
                                              list = FALSE, 
                                              times = 1)
            trainData <- data[ trainIndex,]
            testData  <- data[-trainIndex,]
            treeFit <- train(foodSecurity ~ ., 
                             data = trainData[,c(c("foodSecurity"), vars)],
                             method = 'rpart',
                             trControl = trainControl(method = "cv", 
                                                      number = input$numFolds),
                             tuneGrid = expand.grid(cp = seq(0, 0.1, by = 0.0001)),
                             # Exclude any observations with missing data
                             na.action = na.exclude)
            rattle::fancyRpartPlot(treeFit$finalModel, tweak = 2)
        })
    })
    
    observeEvent(input$runForest, {
        output$summaryForest <- renderPlot({
            
            # Create a Progress object
            progress <- Progress$new()
            # Make sure it closes when we exit this reactive, even if there's an error.
            on.exit(progress$close())
            # Set the message to the user while cross-validation is running.
            progress$set(message = "Calculation in progress",
                         detail = "This may take a while...")
            
            # Remove "No Response" from `foodSecurity`
            data <- foodSecurity %>% filter(foodSecurity != "No Response")
            data$foodSecurity <- droplevels(data$foodSecurity)
            
            vars <- unlist(input$forestVars)
            trainIndex <- createDataPartition(data$foodSecurity,
                                              p = input$splitPercent/100, 
                                              list = FALSE, 
                                              times = 1)
            trainData <- data[ trainIndex,]
            testData  <- data[-trainIndex,]
            forestFit <- train(foodSecurity ~ ., 
                             data = trainData[,c(c("foodSecurity"), vars)],
                             method = 'ranger',
                             # Needed to retrieve variable importance 
                             importance = "permutation",
                             trControl = trainControl(method = "cv", 
                                                      number = input$numFolds),
                             #tuneGrid = expand.grid(mtry = c(1:15)),
                             # Do not print output from the cross validation
                             # Exclude any observations with missing data
                             na.action = na.exclude)
            temp <- varImp(forestFit)
            importance <- as_tibble(temp$importance, rownames = "variable")
            importance <- importance %>% arrange(desc(Overall))
            ggplot(importance[1:20,], aes(x = reorder(variable, Overall), 
                                          y = Overall, fill = Overall)) +
                geom_col() + coord_flip() + theme(legend.position = "none") +
                labs(x = "Predictors",  
                     y = "Importance %", 
                     title ="Importance of Top 20 Predictors")
        })
    })
    
    
    # Adapted from https://github.com/RiveraDaniel/Regression/blob/83df18abcb632f60f3c381e77987ff6fbbe031db/server.R
    output$rawData <- DT::renderDataTable(
        DT::datatable(foodSecurity,
            options = list(lengthMenu = list(c(5, 15, 20),c('5', '15', '20')),
                           pageLength = 10,
                           initComplete = JS(
                           "function(settings, json) {",
                           "$(this.api().table().header()).css({",
                           "'background-color': 'moccasin', ",
                           "'color': '1c1b1b'});}"),
                           columnDefs = list(list(className = 'dt-center',
                                                  targets="_all")#,
                                             #list(targets = list(0,2),
                                                  #width = '70', visible = TRUE),
                                             #list(targets = list(1,3,6,7,8),
                                                  #width = '150', visible = TRUE),
                                             #list(targets = list(4,5),
                                                  #width = '90', visible = TRUE)
                                             ),
                           autoWidth = TRUE,
                           scrollX = TRUE
                          ),
            filter = "top",
            selection = 'multiple',
            style = 'bootstrap',
            class = 'cell-border stripe',
            rownames = FALSE,
            colnames = c(response, predictors)
        ))

})
