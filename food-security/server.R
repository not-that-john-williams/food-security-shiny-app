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

createBarPlot <- function(group, data){
    barPlot <- data %>% 
               ggplot(aes(x = foodSecurity, group = eval(parse(text = group)))) +
               geom_bar(aes(y = ..prop.., fill = factor(..x..))) +
               scale_y_continuous(labels=scales::percent) +
               scale_fill_discrete(name = "Food Security", 
                                   labels = c("High", "Marginal", "Low", 
                                              "Very Low", "No Response")) +
               labs(x = "Food Security", y = "Relative Frequencies",
                    title = "") +
               facet_wrap(vars(eval(parse(text = group))))
    print(barPlot)
}

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

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    output$dataTable <- DT::renderDataTable({
        if(input$summaryType == "Numerical"){
            if(input$numericalType == "Frequency Tables"){
                if(input$freqVariable == "Food Security"){
                    t <- freq(foodSecurity$foodSecurity)
                    # Print only selected rows and columns of table,
                    # Rounding to two decimal places
                    round(t[-6, -2:-3], 2)
                } else {
                if(input$freqVariable == "Sex"){
                    t <- freq(foodSecurity$sex)
                    round(t[-3, -2:-3], 2)
                } else {
                if(input$freqVariable == "Race"){
                    t <- freq(foodSecurity$race)
                    round(t[-27, -2:-3], 2)
                } else {
                if(input$freqVariable == "Type of Household"){
                    t <- freq(foodSecurity$typeHH)
                    round(t[-13, -2:-3], 2)
                } else {
                if(input$freqVariable == "Annual Household Income"){
                    t <- freq(foodSecurity$annualHHIncome)
                    round(t[c(-1, -18), -2:-3], 2)
                } else {
                if(input$freqVariable == "Marital Status"){
                    t <- freq(foodSecurity$maritalStatus)
                    round(t[-9, -2:-3], 2)
                } else {
                if(input$freqVariable == "Living Quarters"){
                    t <- freq(foodSecurity$livingQuarters)
                    round(t[c(-1, -14), -2:-3], 2)
                } else {
                if(input$freqVariable == "Education Level"){
                    t <- freq(foodSecurity$educationLevel)
                    round(t[-18, -2:-3], 2)
                } else {
                if(input$freqVariable == "Household Recieved SNAP Benefits"){
                    t <- freq(foodSecurity$receivedSNAP)
                    round(t[-6, -2:-3], 2)
                }}}}}}}}}
            }
        }
    })
    
    output$contingencyTable <- renderPrint({
        # Create contingency table using `gmodels` package
        with(foodSecurity, CrossTable(eval(parse(text = input$contingencyVar1)), 
                                      eval(parse(text = input$contingencyVar2))))
    })
    
    output$descripStat <- renderPrint({
        if(input$descripStatVariable == "Age")
            summary(foodSecurity$age)
        else if(input$descripStatVariable == "Number of Household Members")
            summary(foodSecurity$numHHMembers)
    })
    
    output$barPlot <- renderPlot({
        if(input$summaryType == "Graphical"){
            if(input$plotType == "Bar Plot"){
                if(input$barPlotVariable == "Sex"){
                    createBarPlot("sex", foodSecurity)
                }
                if(input$barPlotVariable == "Race"){
                    createBarPlot("race", foodSecurity)
                }
                if(input$barPlotVariable == "Type of Household"){
                    createBarPlot("typeHH", foodSecurity)
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
            if(input$plotType == "Other Plot"){
                if(input$otherPlotVariable == "Sex"){
                    # Attempted to use a function to create this plot, but 
                    # `eval(parse(text = "..."))` wasn't compatible with 
                    # `group_by()`.  More research needed.
                    foodSecurity %>% group_by(sex, foodSecurity) %>%
                        summarize(n = n()) %>% 
                        mutate(perc = 100*n/sum(n)) %>% 
                        ggplot(aes(x = sex, y = perc)) +
                        geom_bar(stat = "identity") +
                        facet_grid(~ foodSecurity) + 
                        coord_flip()
                } else {
                if(input$otherPlotVariable == "Race"){
                    foodSecurity %>% group_by(race, foodSecurity) %>%
                        summarize(n = n()) %>% 
                        mutate(perc = 100*n/sum(n)) %>% 
                        ggplot(aes(x = race, y = perc)) +
                        geom_bar(stat = "identity") +
                        facet_grid(~ foodSecurity) + 
                        coord_flip()
                } else {
                if(input$otherPlotVariable == "Type of Household"){
                    foodSecurity %>% group_by(typeHH, foodSecurity) %>%
                        summarize(n = n()) %>% 
                        mutate(perc = 100*n/sum(n)) %>% 
                        ggplot(aes(x = typeHH, y = perc)) +
                        geom_bar(stat = "identity") +
                        facet_grid(~ foodSecurity) + 
                        coord_flip()
                } else {
                if(input$otherPlotVariable == "Annual Household Income"){
                    foodSecurity %>% group_by(annualHHIncome, foodSecurity) %>%
                        summarize(n = n()) %>% 
                        mutate(perc = 100*n/sum(n)) %>% 
                        ggplot(aes(x = annualHHIncome, y = perc)) +
                        geom_bar(stat = "identity") +
                        facet_grid(~ foodSecurity) + 
                        coord_flip()
                } else {
                if(input$otherPlotVariable == "Marital Status"){
                    foodSecurity %>% group_by(maritalStatus, foodSecurity) %>%
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
                    foodSecurity %>% group_by(educationLevel, foodSecurity) %>%
                        summarize(n = n()) %>% 
                        mutate(perc = 100*n/sum(n)) %>% 
                        ggplot(aes(x = educationLevel, y = perc)) +
                        geom_bar(stat = "identity") +
                        facet_grid(~ foodSecurity) + 
                        coord_flip()
                } else {
                if(input$otherPlotVariable == "Household Recieved SNAP Benefits"){
                    foodSecurity %>% group_by(receivedSNAP, foodSecurity) %>%
                        summarize(n = n()) %>% 
                        mutate(perc = 100*n/sum(n)) %>% 
                        ggplot(aes(x = receivedSNAP, y = perc)) +
                        geom_bar(stat = "identity") +
                        facet_grid(~ foodSecurity) + 
                        coord_flip()
                }}}}}}}}
            }
        }
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
            colnames = c(predictors, response)
        ))

})
