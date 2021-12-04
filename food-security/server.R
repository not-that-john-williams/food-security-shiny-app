# Required packages
library(caret)
library(DT)
library(ggplot2)
library(gmodels)
library(graphics)
library(nnet)
library(ranger)
library(rattle)
library(rpart)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(sjPlot)
library(summarytools)
library(tidyverse)

foodSecurity <- readRDS("./data/foodSecurity.rds")
foodSecurityNR <- readRDS("./data/foodSecurityNR.rds")

variableNames <- list("foodSecurity" = "Food Security",
                      "sex" = "Sex",
                      "race" = "Race",
                      "hispanicOrigin" = "Hispanic Origin",
                      "age" = "Age",
                      "citizenship" = "US Citizenship",
                      "typeHH" = "Type of Household",
                      "numHHMembers" = "Number of Household Members",
                      "employStatus" = "Employment Status",
                      "annualHHIncome" = "Annual Household Income",
                      "maritalStatus" = "Marital Status",
                      "livingQuarters" = "Living Quarters",
                      "educationLevel" = "Education Level",
                      "receivedSNAP" = "Household Received SNAP Benefits")

# Function to create horizontal bar plots
createBarPlot <- function(group, name, data, leg.pos = "none"){
  barPlot <- data %>% 
    ggplot(aes(x = foodSecurity, group = eval(parse(text = group)))) +
    geom_bar(aes(y = ..prop.., fill = factor(..x..))) +
    theme(legend.position = leg.pos) +
    scale_y_continuous(labels=scales::percent) +
    scale_fill_discrete(
      name = "Food Security", 
      labels = c("High", "Marginal", "Low", "Very Low", "No Response")) +
      labs(x = "Food Security",
           y = "Relative Frequencies",
           title = paste0("Relative Frequencies of 'Food Security' by '",
                          name, "'")) +
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
        if(input$freqVariable == "Household Received SNAP Benefits"){
          t <- freq(foodSecurity$receivedSNAP)
          round(t[-6, -2:-3], 2)
        }}}}}}}}}}}}}}  # Close out nested if/else statements
      }
    }
  })
  
  # Using `sjPlot` package, create a contingency table  for two specified 
  # categorical variables.
  output$simpleTable <- renderUI({
    df = "foodSecurity$"
    row <- paste0(df, input$simpleVar)
    col <- paste0(df, "foodSecurity")
    simpleCT <- tab_xtab(var.row = eval(parse(text = row)),
                         var.col = eval(parse(text = col)),
                         var.labels = c(input$simpleVar, "foodSecurity"),
                         title = paste0("Cross Tabulation of foodSecurity vs. ",
                                        input$simpleVar),
                         show.row.prc = TRUE,
                         show.summary = FALSE,
                         emph.total = TRUE)
    HTML(simpleCT$page.complete)
  })
  
  # Using `sjPlot` package, create a contingency table  for two specified 
  # categorical variables.
  output$simpleTableExclude <- renderUI({
    df = "foodSecurityNR$"
    row <- paste0(df, input$simpleVar)
    col <- paste0(df, "foodSecurity")
    simpleCT <- tab_xtab(var.row = eval(parse(text = row)),
                         var.col = eval(parse(text = col)),
                         var.labels = c(input$simpleVar, "foodSecurity"),
                         title = paste0("Cross Tabulation of foodSecurity vs. ",
                                        input$simpleVar),
                         show.row.prc = TRUE,
                         show.summary = FALSE,
                         emph.total = TRUE)
    HTML(simpleCT$page.complete)
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
          createBarPlot("sex", name = "Sex", data = foodSecurity,
                        leg.pos = "right")
        }
        if(input$barPlotVariable == "Race"){
          createBarPlot("race", name = "Race", data = foodSecurity,
                        leg.pos = "right")
        }
        if(input$barPlotVariable == "Hispanic Origin"){
          createBarPlot("hispanicOrigin", name = "Hispanic Origin",
                        data = foodSecurity, leg.pos = "right")
        }
        if(input$barPlotVariable == "Age"){
          createBarPlot("age", name = "Age",
                        data = foodSecurity, leg.pos = "right")
        }
        if(input$barPlotVariable == "US Citizenship"){
          createBarPlot("citizenship", name = "US Citizenship",
                        data = foodSecurity, leg.pos = "right")
        }
        if(input$barPlotVariable == "Type of Household"){
          createBarPlot("typeHH", name = "Type of Household",
                        data = foodSecurity, leg.pos = "right")
        }
        if(input$barPlotVariable == "Number of Household Members"){
          createBarPlot("numHHMembers", name = "Number of Household Members",
                        data = foodSecurity, leg.pos = "right")
        }
        if(input$barPlotVariable == "Employment Status"){
          createBarPlot("employStatus", name = "Employment Status",
                        data = foodSecurity, leg.pos = "right")
        }
        if(input$barPlotVariable == "Annual Household Income"){
          createBarPlot("annualHHIncome", name = "Annual Household Income",
                        data = foodSecurity, leg.pos = "right")
        }
        if(input$barPlotVariable == "Marital Status"){
          createBarPlot("maritalStatus", name = "Marital Status",
                        data = foodSecurity, leg.pos = "right")
        }
        if(input$barPlotVariable == "Living Quarters"){
          createBarPlot("livingQuarters", name = "Living Quarters",
                        data = foodSecurity, leg.pos = "right")
        }
        if(input$barPlotVariable == "Education Level"){
          createBarPlot("educationLevel", name = "Education Level",
                        data = foodSecurity, leg.pos = "right")
        }
        if(input$barPlotVariable == "Household Received SNAP Benefits"){
          createBarPlot("receivedSNAP",
                        name = "Household Received SNAP Benefits",
                        data = foodSecurity, leg.pos = "right")
        }
      }
    }
  })
  
  output$barPlotExclude <- renderPlot({
    if(input$summaryType == "Graphical"){
      if(input$plotType == "Bar Plot - Vertical"){
        if(input$barPlotVariable == "Sex"){
          createBarPlot("sex", name = "Sex", data = foodSecurityNR)
        }
        if(input$barPlotVariable == "Race"){
          createBarPlot("race", name = "Race", data = foodSecurityNR)
        }
        if(input$barPlotVariable == "Hispanic Origin"){
          createBarPlot("hispanicOrigin", name = "Hispanic Origin",
                        data = foodSecurityNR)
        }
        if(input$barPlotVariable == "Age"){
          createBarPlot("age", name = "Age", data = foodSecurityNR)
        }
        if(input$barPlotVariable == "US Citizenship"){
          createBarPlot("citizenship", name = "US Citizenship",
                        data = foodSecurityNR)
        }
        if(input$barPlotVariable == "Type of Household"){
          createBarPlot("typeHH", name = "Type of Household",
                        data = foodSecurityNR)
        }
        if(input$barPlotVariable == "Number of Household Members"){
          createBarPlot("numHHMembers", name = "Number of Household Members",
                        data = foodSecurityNR)
        }
        if(input$barPlotVariable == "Employment Status"){
          createBarPlot("employStatus", name = "Employment Status",
                        data = foodSecurityNR)
        }
        if(input$barPlotVariable == "Annual Household Income"){
          createBarPlot("annualHHIncome", name = "Annual Household Income",
                        data = foodSecurityNR)
        }
        if(input$barPlotVariable == "Marital Status"){
          createBarPlot("maritalStatus", name = "Marital Status",
                        data = foodSecurityNR)
        }
        if(input$barPlotVariable == "Living Quarters"){
          createBarPlot("livingQuarters", name = "Living Quarters",
                        data = foodSecurityNR)
        }
        if(input$barPlotVariable == "Education Level"){
          createBarPlot("educationLevel", name = "Education Level",
                        data = foodSecurityNR)
        }
        if(input$barPlotVariable == "Household Received SNAP Benefits"){
          createBarPlot("receivedSNAP",
                        name = "Household Received SNAP Benefits",
                        data = foodSecurityNR)
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
              labs(x = "Sex", 
                   y = "Percent", 
                   title = "Relative Frequencies of 'Food Security' by 'Sex'") +
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
              labs(x = "Race", 
                   y = "Percent", 
                   title = paste0("Relative Frequencies of 'Food Security' by ",
                                  "'Race'")) +
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
              labs(x = "Hispanic Origin", 
                   y = "Percent", 
                   title = paste0("Relative Frequencies of 'Food Security' by ",
                                  "'Hispanic Origin'")) +
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
              labs(x = "Age", 
                   y = "Percent", 
                   title = "Relative Frequencies of 'Food Security' by 'Age'") +
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
              labs(x = "US Citizenship", 
                   y = "Percent", 
                   title = paste0("Relative Frequencies of 'Food Security' by ",
                                  "'US Citizenship'")) +
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
              labs(x = "Type of Household", 
                   y = "Percent", 
                   title = paste0("Relative Frequencies of 'Food Security' by ",
                                  "'Type of Household'")) +
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
              labs(x = "Number of Household Members", 
                   y = "Percent", 
                   title = paste0("Relative Frequencies of 'Food Security' by ",
                                  "'Number of Household Members'")) +
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
              labs(x = "Employment Status", 
                   y = "Percent", 
                   title = paste0("Relative Frequencies of 'Food Security' by ",
                                  "'Employment Status'")) +
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
              labs(x = "Annual Household Income", 
                   y = "Percent", 
                   title = paste0("Relative Frequencies of 'Food Security' by ",
                                  "'Annual Household Income'")) +
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
              labs(x = "Annual Household Income", 
                   y = "Percent", 
                   title = paste0("Relative Frequencies of 'Food Security' by ",
                                  "'Marital Status'")) +
              coord_flip()
        } else {
        if(input$otherPlotVariable == "Living Quarters"){
          foodSecurity %>% group_by(livingQuarters, foodSecurity) %>%
            summarize(n = n()) %>%
            mutate(perc = 100*n/sum(n)) %>%
            ggplot(aes(x = livingQuarters, y = perc)) +
              geom_bar(stat = "identity") +
              facet_grid(~ foodSecurity) +
              labs(x = "Living Quarters", 
                   y = "Percent", 
                   title = paste0("Relative Frequencies of 'Food Security' by ",
                                  "'Living Quarters'")) +
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
              labs(x = "Education Level", 
                   y = "Percent", 
                   title = paste0("Relative Frequencies of 'Food Security' by ",
                                  "'Education Level'")) +
              coord_flip()
        } else {
        if(input$otherPlotVariable == "Household Received SNAP Benefits"){
          foodSecurity %>%
            group_by(receivedSNAP, foodSecurity) %>%
            summarize(n = n()) %>%
            mutate(perc = 100*n/sum(n)) %>%
            ggplot(aes(x = receivedSNAP, y = perc)) +
              geom_bar(stat = "identity") +
              facet_grid(~ foodSecurity) +
              labs(x = "Household Received SNAP Benefits", 
                   y = "Percent", 
                   title = paste0("Relative Frequencies of 'Food Security' by ",
                                  "'Household Received SNAP Benefits'")) +
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
          if(input$mosaicPlotVariable == "Household Received SNAP Benefits"){
            mosaicplot(~ foodSecurityNR$receivedSNAP + foodSecurityNR$foodSecurity,
                       shade = TRUE,  # Add color to plot
                       las = 2,  # Print variable names vertically
                       main = "'Household Received SNAP Benefits' vs. 'Food Security'",
                       xlab = "Household Received SNAP Benefits",
                       ylab = "Food Security")
          }}}}}}}}}}}}}  # Close out nested if/else statements
        }
      }
    })
  })
  
  trainLogisticModel <- eventReactive(input$runMLM, {
    
    # Create a Progress object
    progress <- Progress$new()
    # Ensure the Progress object closes upon exiting this reactive, even if
    # there is an error.
    on.exit(progress$close())
    # Set the message to the user while cross-validation is running.
    progress$set(message = "Calculation in progress",
                 detail = "This may take a while...")
    
    # Grab the predictor variables to be used in the model from the user input
    vars <- unlist(input$multiModelVars)
    
    # Partition the data into a training set and test set
    trainIndex <- createDataPartition(foodSecurityNR$foodSecurity,
                                      p = input$splitPercent/100, 
                                      list = FALSE, 
                                      times = 1)
    trainData.logistic <- foodSecurityNR[ trainIndex,]
    testData.logistic  <- foodSecurityNR[-trainIndex,]
    
    # Fit a Binomial Logistic Regression Model
    glmFit <- train(foodSecurity ~ ., 
                    data = trainData.logistic[,c(c("foodSecurity"), vars)],
                    method = 'glm',
                    family = 'binomial',
                    trControl = trainControl(method = "cv", 
                                             number = input$numFolds),
                    # Do not print output from the cross validation
                    trace = FALSE,
                    # Exclude any observations with missing data
                    na.action = na.exclude)
    
    # Save the fitted model in a folder.
    saveRDS(glmFit, "./Fitted Models/logistic-regression-model.rds")
    
    #  Print a summary of the Binomial Logistic Regression Model
    logisticSummary <- summary(glmFit)
    
    logisticModelPredict <- predict(glmFit, newdata = testData.logistic)
    logisticFitStats <- confusionMatrix(logisticModelPredict, 
                                        testData.logistic$foodSecurity)
    
    # Return all objects as a list
    list(summary = logisticSummary, fitStats = logisticFitStats)
  })
  
  output$logisticTitle <- renderUI({
    trainLogisticModel()
    h5(strong("Model training is complete."))
  })
  
  output$summaryMulti <- renderPrint({
    trainLogisticModel()$summary
  })
  
  output$logisticFitStats <- renderPrint({
    trainLogisticModel()$fitStats
  })
  
  trainTreeModel <- eventReactive(input$runClassTree, {
    
    # Create a Progress object
    progress <- Progress$new()
    # Ensure the Progress object closes upon exiting this reactive, even if
    # there is an error.
    on.exit(progress$close())
    # Set the message to the user while cross-validation is running.
    progress$set(message = "Calculation in progress",
                 detail = "This may take a while...")
    
    # Grab the predictor variables to be used in the model from the user input
    vars <- unlist(input$classTreeVars)
    
    # Partition the data into a training set and test set
    trainIndex <- createDataPartition(foodSecurityNR$foodSecurity,
                                      p = input$splitPercent/100, 
                                      list = FALSE, 
                                      times = 1)
    trainData.tree <- foodSecurityNR[ trainIndex,]
    testData.tree  <- foodSecurityNR[-trainIndex,]
      
    # Fit a Classification Tree Model using cross validation
    treeFit <- train(foodSecurity ~ ., 
                     data = trainData.tree[,c(c("foodSecurity"), vars)],
                     method = 'rpart',
                     trControl = trainControl(method = "cv", 
                                              number = input$numFolds),
                     tuneGrid = expand.grid(cp = seq(0, 0.1, by = 0.0001)),
                     # Exclude any observations with missing data
                     na.action = na.exclude)
    
    # Save the fitted model in a folder.
    saveRDS(treeFit, "./Fitted Models/classification-tree-model.rds")
    
    # Output a plot of the Classification Tree
    treeSummary <- rattle::fancyRpartPlot(treeFit$finalModel, tweak = 2)
    
    treeModelPredict <- predict(treeFit, newdata = testData.tree)
    treeFitStats <- confusionMatrix(treeModelPredict, 
                                    testData.tree$foodSecurity)
    
    # Return all objects as a list
    list(summary = treeSummary, fitStats = treeFitStats)
  })
  
  output$treeTitle <- renderUI({
    trainTreeModel()
    h5(strong("Model training is complete."))
  })
  
  output$summaryTree <- renderPlot({
    trainTreeModel()$summary
  })
  
  output$treeFitStats <- renderPrint({
    trainTreeModel()$fitStats
  })
  
  trainForestModel <- eventReactive(input$runForest, {
    
    # Create a Progress object
    progress <- Progress$new()
    # Ensure the Progress object closes upon exiting this reactive, even if
    # there is an error.
    on.exit(progress$close())
    # Set the message to the user while cross-validation is running.
    progress$set(message = "Calculation in progress",
                 detail = "This may take a while...")
    
    # Grab the predictor variables to be used in the model from the user input
    vars <- unlist(input$forestVars)
    
    # Partition the data into a training set and test set
    trainIndex <- createDataPartition(foodSecurityNR$foodSecurity,
                                      p = input$splitPercent/100, 
                                      list = FALSE, 
                                      times = 1)
    trainData.forest <- foodSecurityNR[ trainIndex,]
    testData.forest  <- foodSecurityNR[-trainIndex,]
      
    # Fit a Random Forest Model using cross validation
    forestFit <- train(foodSecurity ~ ., 
                       data = trainData.forest[,c(c("foodSecurity"), vars)],
                       method = 'ranger',
                       # Needed to retrieve variable importance 
                       importance = "permutation",
                       trControl = trainControl(method = "cv", 
                                                number = input$numFolds),
                       #tuneGrid = expand.grid(mtry = c(1:15)),
                       # Exclude any observations with missing data
                       na.action = na.exclude)
    
    # Save the fitted model in a folder.
    saveRDS(forestFit, "./Fitted Models/random-forest-model.rds")
    
    temp <- varImp(forestFit)
    importance <- as_tibble(temp$importance, rownames = "variable")
    importance <- importance %>% arrange(desc(Overall))
    
    # Output a plot of the variable importance from the Random Forest Model
    # (Top 20 predictor variables only)
    forestSummary <- ggplot(importance[1:20,],
                            aes(x = reorder(variable, Overall), 
                                y = Overall, fill = Overall)) +
                       geom_col() +
                       coord_flip() +
                       theme(legend.position = "none") +
                       labs(x = "Predictors",  
                            y = "Importance %", 
                            title ="Importance of Top 20 Predictors")
    
    forestModelPredict <- predict(forestFit, newdata = testData.forest)
    forestFitStats <- confusionMatrix(forestModelPredict, 
                                      testData.forest$foodSecurity)
    
    # Return all objects as a list
    list(summary = forestSummary, fitStats = forestFitStats)
  })
  
  output$forestTitle <- renderUI({
    trainForestModel()
    h5(strong("Model training is complete."))
  })
  
  output$summaryForest <- renderPlot({
    trainForestModel()$summary
  })
  
  output$forestFitStats <- renderPrint({
    trainForestModel()$fitStats
  })
  
  output$logisticPredcitonVariables <- renderUI({
    tags$ul(
      tagList(
        lapply(input$multiModelVars, function(variable) {
          tags$li(h5(strong(variableNames[[variable]])))
        })
      )
    )
  })
  
  output$treePredcitonVariables <- renderUI({
    tags$ul(
      tagList(
        lapply(input$classTreeVars, function(variable) {
          tags$li(h5(strong(variableNames[[variable]])))
        })
      )
    )
  })
  
  output$forestPredcitonVariables <- renderUI({
    tags$ul(
      tagList(
        lapply(input$forestVars, function(variable) {
          tags$li(h5(strong(variableNames[[variable]])))
        })
      )
    )
  })
  
  observeEvent(input$getLogisticPrediction, {
    output$logisticPrediction <- renderUI({
      
      # Load in the logistic regression model.
      model <- readRDS("./Fitted Models/logistic-regression-model.rds")
      
      predictorValues <- data.frame(sex = input$sexPred,
                                    race = input$racePred,
                                    hispanicOrigin = input$hispanicOriginPred,
                                    age = input$agePred,
                                    citizenship = input$citizenshipPred,
                                    typeHH = input$typeHHPred,
                                    numHHMembers = input$numHHMembersPred,
                                    emplyStatus = input$employStatusPred,
                                    annualHHIncome = input$annualHHIncomePred,
                                    maritalStatus = input$maritalStatusPred,
                                    livingQuarters = input$livingQuartersPred,
                                    educationLevel = input$educationLevelPred,
                                    receivedSNAP = input$receivedSNAPPred)
      
      thePrediction <- predict(model, predictorValues)
      
      # observe({print(thePrediction)})
      # observe({print(class(thePrediction))})
      
      h3("This individual is likely to be food ", 
         strong(toupper(thePrediction)), ".")
    })
  })
  
  observeEvent(input$getTreePrediction, {
    output$treePrediction <- renderUI({
      
      # Load in the logistic regression model.
      model <- readRDS("./Fitted Models/classification-tree-model.rds")
      
      predictorValues <- data.frame(sex = input$sexPred,
                                    race = input$racePred,
                                    hispanicOrigin = input$hispanicOriginPred,
                                    age = input$agePred,
                                    citizenship = input$citizenshipPred,
                                    typeHH = input$typeHHPred,
                                    numHHMembers = input$numHHMembersPred,
                                    emplyStatus = input$employStatusPred,
                                    annualHHIncome = input$annualHHIncomePred,
                                    maritalStatus = input$maritalStatusPred,
                                    livingQuarters = input$livingQuartersPred,
                                    educationLevel = input$educationLevelPred,
                                    receivedSNAP = input$receivedSNAPPred)
      
      thePrediction <- predict(model, predictorValues)
      
      h3("This individual is likely to be food ", 
         strong(toupper(thePrediction)), ".")
    })
  })
  
  observeEvent(input$getForestPrediction, {
    output$forestPrediction <- renderUI({
      
      # Load in the logistic regression model.
      model <- readRDS("./Fitted Models/random-forest-model.rds")
      
      predictorValues <- data.frame(sex = input$sexPred,
                                    race = input$racePred,
                                    hispanicOrigin = input$hispanicOriginPred,
                                    age = input$agePred,
                                    citizenship = input$citizenshipPred,
                                    typeHH = input$typeHHPred,
                                    numHHMembers = input$numHHMembersPred,
                                    emplyStatus = input$employStatusPred,
                                    annualHHIncome = input$annualHHIncomePred,
                                    maritalStatus = input$maritalStatusPred,
                                    livingQuarters = input$livingQuartersPred,
                                    educationLevel = input$educationLevelPred,
                                    receivedSNAP = input$receivedSNAPPred)
      
      thePrediction <- predict(model, predictorValues)
      
      h3("This individual is likely to be food ", 
         strong(toupper(thePrediction)), ".")
    })
  })
  
  observeEvent(input$viewButton, {
    output$saveButtonTitle <- renderUI({
      h5(strong("After filtering, click the button below to save the data set ",
                "to a .csv file:"))
    })
  })
  
  # Adapted from the 'server.R' file located at
  # https://github.com/RiveraDaniel/Regression/
  
  selectedColumns <- eventReactive(input$viewButton, {
    input$variablesPicked
  })
  
  observeEvent(input$viewButton, {
    colNames <- unlist(variableNames[selectedColumns()])
    names(colNames) <- NULL
    output$rawData <- DT::renderDataTable(
      DT::datatable(foodSecurity[,selectedColumns()],
        options = list(lengthMenu = list(c(5, 15, 20),c('5', '15', '20')),
                       pageLength = 10,
                       initComplete = JS(
                       "function(settings, json) {$(this.api().table()",
                       ".header()).css({'background-color': 'moccasin', ",
                       "'color': '1c1b1b'});}"),
                       columnDefs = list(list(className = 'dt-center',
                                              targets="_all")),
                       autoWidth = TRUE,
                       scrollX = TRUE,
                       dom = 'Bfrtip',
                       buttons = 'csv'),
        extensions = "Buttons",
        filter = "top",
        selection = 'multiple',
        style = 'bootstrap',
        class = 'cell-border stripe',
        rownames = FALSE,
        colnames = colNames
      )
    )
  })

})
