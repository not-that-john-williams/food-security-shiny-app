###############################################################################
#
# This R script is the UI for the 2020 Food Security Shiny App.
#
# Author:  John Williams
# Email:  jwili32@ncsu.edu
#
###############################################################################

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

###############################################################################
#
# Download data objects into global environment.
#
###############################################################################

foodSecurity <- readRDS("./data/foodSecurity.rds")
foodSecurityNR <- readRDS("./data/foodSecurityNR.rds")

dataLink = paste0("https://www.census.gov/data/datasets/",
                  "2020/demo/cps/cps-food-security.html")
USDALink = paste0("https://www.ers.usda.gov/publications/pub-details/",
                  "?pubid=102075")
mapMealGapLink = "https://map.feedingamerica.org/"

factorVariables <- c("Food Security",
                     "Sex",
                     "Race",
                     "Hispanic Origin",
                     "Age",
                     "US Citizenship",
                     "Type of Household",
                     "Number of Household Members",
                     "Employment Status",
                     "Annual Household Income",
                     "Marital Status",
                     "Living Quarters",
                     "Education Level",
                     "Household Received SNAP Benefits")

variableList <- list("Food Security" = "foodSecurity",
                     "Sex" = "sex",
                     "Race" = "race",
                     "Hispanic Origin" = "hispanicOrigin",
                     "Age" = "age",
                     "US Citizenship" = "citizenship",
                     "Type of Household" = "typeHH",
                     "Number of Household Members" = "numHHMembers",
                     "Employment Status" = "employStatus",
                     "Annual Household Income" = "annualHHIncome",
                     "Marital Status" = "maritalStatus",
                     "Living Quarters" = "livingQuarters",
                     "Education Level" = "educationLevel",
                     "Household Received SNAP Benefits" = "receivedSNAP")

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

###############################################################################
#
# Start of Shiny App UI
#
###############################################################################

shinyUI(navbarPage(
    
  # Add a theme.
  theme = shinytheme("united"),
  
  # Add a title.
  title = "2020 Food Security App",
  
  # Create tabs.
  tabsetPanel(
    
###############################################################################
#
# About Tab
#
###############################################################################

    tabPanel(
    
      # Add a title.
      title = "About",
      
      mainPanel(
      
        # Image sourced from "www.mtm-inc.net"
        img(src = "food-security-pyramid.png",
            # Center the image
            style="display: block; margin-left: auto; margin-right: auto;"
        ),
        
        #First Paragraph - Purpose of the app
        "Would you believe over ", a(href=USDALink, "38 million Americans"),
        ", including almost 12 million children, were food insecure in 2020?  ",
        "Someone who faces food insecurity is regularly unable to secure ",
        "financial resources for food at the household level.  Food ",
        "insecurity can cause chronic hunger, higher levels of anxiety, and ",
        "poorer mental and physical health.  When many people do not have the ",
        "resources to meet their basic needs, society as a whole can suffer.  ",
        "Though food insecurity is closely related to poverty, not all people ",
        "living below the poverty line experience food insecurity and people ",
        "living above the poverty line can experience food insecurity.  ",
        strong("The purpose of this app is to explore the demographics of ",
        "food insecurity in America.  "), "Here, we will focus strictly on ",
        "the United States as a whole. A similar app, ", a(href=mapMealGapLink,
        "Map the Meal Gap"), ", highlights food security by state and county ",
        "levels.",
        br(),
        br(),
        
        #Second Paragraph - About the Data
        strong("This app souces data from the \"December 2020 Food ",
        "Security Supplement\" from the United States Census.  "), "Census ",
        "Bureau staff conducted this survey as a supplement to the Current ",
        "Population Survey (CPS). The CPS is a monthly labor force survey in ",
        "which interviews are conducted in approximately 54,000 households ",
        "across the nation.  More information can be found ", a(href=dataLink, 
        "here"), ".  While the CPS has hundreds of varaiables (demographic, ",
        "labor force, economic, etc.) about households and individuals, this ",
        "app utilizes only a select few overarching characteristics that ",
        "impact food security.",
        br(),
        br(),
        
        #Third Paragraph - About each Tab
        "Each tab allows the user to interface with the data:",
        br(),
        tags$ul(
          tags$li(strong("Data Exploration "),
                         "-- Explore numerical and graphical summaries of the ",
                         "data."), 
          tags$li(strong("Modeling "),
                         "-- Fit a model to the data and predict food ",
                         "security."), 
          tags$li(strong("Data "),
                         "-- Subset and view/download the formatted data used ",
                         "by this app.")
        )
      )
    ),
      
###############################################################################
#
# Data Exploration Tab
#
###############################################################################

    tabPanel(
      
      # Add a title.
      title = "Data Exploration",
      
      sidebarPanel(
        h3("Summary Type"
        ),
        radioButtons("summaryType",
          label = "Select one:",
          choices = c("Numerical", "Graphical")
        ),
        conditionalPanel(condition = "input.summaryType == 'Numerical'",
          radioButtons("numericalType",
            label = "Select one:",
            choices = c("Frequency Tables", 
                        "Contingency Tables - Simple",
                        "Contingency Tables - Detailed")
          ),
          conditionalPanel(condition = "input.numericalType == 'Frequency Tables'",
            selectInput("freqVariable",
              label = "Select a group variable:",
              choices = factorVariables
            )
          ),
          conditionalPanel(condition = "input.numericalType == 'Contingency Tables - Simple'",
            selectInput("simpleVar",
              label = "Select first variable:",
              choices = names(foodSecurity)[-1]  # Excluding 'Food Security'
            ),
            checkboxInput("excludeNoResponseFromSimpleTable",
                          label = "Change 'Food Security' to a binary factor; exclude 'No Response'"
            )
          ),
          conditionalPanel(condition = "input.numericalType == 'Contingency Tables - Detailed'",
            selectInput("contingencyVar1",
              label = "Select first variable:",
              choices = names(foodSecurity)
            ),
            selectInput("contingencyVar2",
              label = "Select second variable:",
              choices = names(foodSecurity)[-1]  # Excluding 'Food Security'
            )
          )
        ),
        
        conditionalPanel(condition = "input.summaryType == 'Graphical'",
          radioButtons("plotType",
            label = "Select:",
            choices = c("Bar Plot - Vertical", 
                        "Bar Plot - Horizontal",
                        "Mosaic Plot")
          ),
          conditionalPanel(condition = "input.plotType == 'Bar Plot - Vertical'",
            selectInput("barPlotVariable",
              label = "Select a group variable:",
              choices = factorVariables[-1]  # Excluding 'Food Security'
            ),
            checkboxInput("excludeNoResponseFromBarPlot",
              label = "Change 'Food Security' to a binary factor; exclude 'No Response'"
            )
          ),
          conditionalPanel(condition = "input.plotType == 'Bar Plot - Horizontal'",
            selectInput("otherPlotVariable",
              label = "Select a group variable:",
              choices = factorVariables[-1]  # Excluding 'Food Security'
            )
          ),
          conditionalPanel(condition = "input.plotType == 'Mosaic Plot'",
            selectInput("mosaicPlotVariable",
              label = "Select a group variable:",
              choices = factorVariables[-1]  # Excluding 'Food Security'
            ),
            actionButton("createMosaicPlot", label = "Create Mosaic Plot"),
            br(), br(), br(), # Add vertical space between elements
            uiOutput("adjustMosaicPlotHeight"),
            uiOutput("adjustMosaicPlotWidth")
          )
        )
      ),
      
      mainPanel(
        conditionalPanel(condition = "input.summaryType == 'Numerical'",
          conditionalPanel(condition = "input.numericalType == 'Frequency Tables'",
            htmlOutput("freqTableTitle"),
            DT::dataTableOutput("freqTable")
          ),
          conditionalPanel(condition = "input.numericalType == 'Contingency Tables - Simple'",
            conditionalPanel(condition = "input.excludeNoResponseFromSimpleTable == 0",
              htmlOutput("simpleTable")
            ),
            conditionalPanel(condition = "input.excludeNoResponseFromSimpleTable == 1",
              htmlOutput("simpleTableExclude")
            )
          ),
          conditionalPanel(condition = "input.numericalType == 'Contingency Tables - Detailed'",
            htmlOutput("contingencyTableTitle"),
            verbatimTextOutput("contingencyTable")
          ),
          conditionalPanel(condition = "input.numericalType == 'Descriptive Statistics'",
            verbatimTextOutput("descripStat")
          )
        ),
        conditionalPanel(condition = "input.summaryType == 'Graphical'",
          conditionalPanel(condition = "input.plotType == 'Bar Plot - Vertical'",
            conditionalPanel(condition = "input.excludeNoResponseFromBarPlot == 1",
              plotOutput("barPlotExclude")
            ),
            conditionalPanel(condition = "input.excludeNoResponseFromBarPlot == 0",
              plotOutput("barPlot")
            )
          ),
          conditionalPanel(condition = "input.plotType == 'Bar Plot - Horizontal'",
            plotOutput("otherPlot")
          ),
          conditionalPanel(condition = "input.plotType == 'Mosaic Plot'",
            plotOutput("mosaicPlot"),
          )
        )
      )
    ),
    
###############################################################################
#
# Modeling Tab
#
###############################################################################

    navbarMenu(
      
      title = "Modeling",
      
###############################################################################
#
# Modeling Info Section
#
###############################################################################

      tabPanel(
        title = "Modeling Info",
        
        wellPanel(
          h4(strong("Select a modeling approach to learn more about it:")
          ),
          radioButtons("infoType", 
                       label = "", 
                       choices = c("Binomial Logistic Regression",
                                   "Classification Tree",
                                   "Random Forest"), 
                       selected = character(0)
          )
        ),
        
        mainPanel(fluidPage(
          conditionalPanel(condition = "input.infoType == 'Binomial Logistic Regression'",
            withMathJax(),
            h3(strong(tags$u("Binomial Logistic Regression"))
            ),
            "Logistic Regression is a type of regression analysis, a ",
            "predictive modeling technique which is used to find the ",
            "relationship between a binary response and one or more ",
            "predictors.  When the response is binary, the analysis is called ",
            "Binomial Logistic Regression.  It's called Multinomial Logistic ",
            "Regression when the response has more than two categories.",
            br(),
            h4("Nuts and Bolts of the Model:"),
            helpText("Consider a binary response variable \\(Y\\) with ",
            "predictors \\(X_i\\) for \\(i = 1, ..., n\\).  Logistic ",
            "Regression is based on the assumption that the probability of ",
            "\\(Y\\) given \\(X_i = x_i\\) for all \\(i = 1, ..., n\\) can be ",
            "expressed as"),
            helpText("$$P(Y) = \\frac{e^{g(x)}}{1+e^{g(x)}}$$"),
            helpText("where \\(g(x)\\) is a function of one or more of the ",
            "\\(x_i\\)'s.  One of the most frequently used \\(g(x)\\) is"),
            helpText("$$g(x) = \\beta_0 + \\beta_1x_1 + ... + \\beta_nx_n$$"),
            helpText("where \\(\\beta_0\\) through \\(\\beta_n\\) are unknown ",
            "parameters.  The resulting model for \\(P(Y)\\) takes the form"),
            helpText("$$P(Y) = \\frac{e^{\\beta_0 + \\beta_1x_1 +...+ ",
            "\\beta_nx_n}}{1+e^{\\beta_0 + \\beta_1x_1 +...+ \\beta_nx_n}}$$"),
            br(),
            h4("Example:"),
            "Let's train a Logistic Model on 70% of the 'Food Security' data ",
            "where the response variable 'Food Security' is measured as ",
            "'Secure' or 'Insecure'.  'Secure' will be the reference class ",
            "for the response.  'Hispanic Origin' will be the only predictor, ",
            "\\(x\\).  With 'Hispanic' as the reference class, \\(x = 0\\) if ",
            "an individual is of Hispanic origin, and \\(x = 1\\) otherwise.  ",
            "After fitting, we get the following system of equations as a ",
            "possible model:",
            helpText("$$P(\\text{Food Security} = \\text{Insecure}) = ",
            "\\frac{e^{-1.448-0.803x}}{1+e^{-1.448-0.803x}} = \\cases{0.190  ",
            "& \\text{if Hispanic} \\cr 0.095  & \\text{if non-Hispanic}}$$"),
            helpText("$$P(\\text{Food Security} = \\text{Secure}) = 1 - ",
            "P(\\text{Food Security} = \\text{Insecure}) = \\cases{0.836  & ",
            "\\text{if Hispanic} \\cr 0.905  & \\text{if non-Hispanic}}$$"),
            br(),
            h4("Prediction:"),
            "Logistic Regression models can be useful for making ",
            "predictions.  For given predictor values, the predicted response ",
            "is the class with probability greater than 50% (or with greatest ",
            "probability in Multinomial Lopgistic Regression).  The example ",
            "above, however, isn't great at predicting food security.  ",
            "Whether an individual is Hispanic or non-Hispanic, the model ",
            "gives a greater than 50% probability that the individual is food ",
            "secure.  But all is not lost, Logistic Regression allows quick ",
            "insights into the relationship of predictors to the response.  ",
            "Under this model, we can conclude that the probability that ",
            "Hispanics are food insecure is 2 times higher than non-Hispanics.",
            h4("Advantages:"),
            tags$ul(
              tags$li("Easy to interpret."),
              tags$li("Efficient to train."),
              tags$li("Useful for probability estimation."),
              tags$li("Provides the direction of the association between a",
                      "predictor and the response.")
            ),
            h4("Drawbacks:"),
            tags$ul(
              tags$li("Assumes linearity between the response and predictors."),
              tags$li("Difficult to capture complex relationships and to ",
                      "discover interaction effects among predictors."),
              tags$li("There should be no high correlations among the ",
                      "predictors."),
              tags$li("Sensitive to outliers.")
            )
          ),
          
          conditionalPanel(condition = "input.infoType == 'Classification Tree'",
            h3(strong(tags$u("Classification Tree"))
            ),
            "A decision tree is a non-parametric supervised learning method ",
            "that predicts the value of a response variable by learning ",
            "simple descion rules inferred from the data features.  There are ",
            "two kinds of decision trees, classification and regression.  ",
            "Classification trees are used to model categorical responses; ",
            "regression trees model continuous responses.  Both types of ",
            "trees can use categorical and/or continuous predictor variables ",
            "to fit the model.  Since our target variable 'Food Security' is ",
            "categorical, we could fit a classification tree to make ",
            "predictions about the food security of a particular individual.",
            br(),
            h4("Nuts and Bolts of the Model:"),
            "To build a classification tree, the data is partitioned using ",
            "recursive binary splitting.  During each iteration, a parent ",
            "node is split along a particular predictor, the splitting ",
            "variable, at a given location, the split point.  Many different ",
            "split locations are considered before one is chosen.  The split ",
            "location chosen is the one that minimizes the Gini index or the ",
            "deviance.  For a binary response , we have",
            helpText("$$\\text{Gini Index:} \\ \\ 2p(1-p)$$"),
            helpText("$$\\text{Deviance:} \\ \\ -2p\\log(p)-2(1-p)",
            "\\log(1-p)$$"), " where ", em("p"), " is the probability of the ",
            "response.  Note both of these values will be small if ", em("p"),
            " is near 0 or 1.  When the split location is finalized, two ",
            "branches of the tree are formed, a left child node and a right ",
            "child node.  The recursive binary splitting continues on each of ",
            "these new branches until a child node contains a small subset of ",
            "observations.  These 'terminal' nodes are often called leaf ",
            "nodes.  Finally, the tree is pruned by removing sections of the ",
            "tree that are non-critical and redundant.",
            br(),
            h4("Example:"),
            "Here, we created a classification tree for the subset of ",
            "individuals that received SNAP benefits.  The predictors used ",
            "are 'Race' and 'Hispanic Origin'.",
            img(src = "treePlot.jpg",
                # Center the image
                style="display: block; margin-left: auto; margin-right: auto;"
            ),
            "After following the decision tree down its branches, we see that ",
            "a non-Hispanic individual that identifies as American Indian and ",
            "Alaskan Native, or as the two race combination White/American ",
            "Indian and Alaskan Native, or as Three or More Race Combinations ",
            "is predicted as being food insecure.",
            h4("Advantages:"),
            tags$ul(
              tags$li("Simple to understand and easy to visualize."),
              tags$li("Requires minimal data cleaning."),
              tags$li("Does not require normality assumptions."),
              tags$li("Can handle both numerical and catergorical data."),
              tags$li("Works well with dichotomous and non-dichotomous ",
                      "response variables.")
            ),
            h4("Drawbacks:"),
            tags$ul(
              tags$li("Can be overly complex leading to overfitting."),
              tags$li("Heavily influenced by small variations in data."),
              tags$li("Not good for extrapolation."),
              tags$li("No optimal algorithm; computationaly intensive.")
            )
          ),
          conditionalPanel(condition = "input.infoType == 'Random Forest'",
            h3(strong(tags$u("Random Forest"))
            ),
            "A random forest utilizes an ensemble of decision trees to create ",
            "a prediction model.  The algorithm establishes the predeicted ",
            "response based on the average output of the decision trees.  ",
            "Increasing the number of decision trees in the forest can ",
            "increase predictive power.",
            br(),
            h4("Inside the Algorithm:"),
            "Random forests make great predictive models, not only because ",
            "they are an amalgam of many different decision trees, but ",
            "because random forests also add an additional random component ",
            "while training the model.  Instead of searching for the most ",
            "important predictor overall to split a decision tree node, the ",
            "algorithm searches for the best predictor among a random subset ",
            "of predictors. This results in a wide diversity of decision ",
            "trees, contributing to a hihgly accurate prediction model.",
            br(),
            h4("Variable Importance:"),
            "Another great quality of the random forest algorithm is that it ",
            "is very easy to measure the relative importance of each ",
            "predictor (and factors of predictors).  Consider a random forest ",
            "model for 'Food Security' with predictor variables 'Race', ",
            "'Hispanic Origin', 'US Citizenship', 'Number of Household ",
            "Members', and 'Annual Household Income'.  The graph below ",
            "outlines the imporatnce level of the top 20 preditive factors.  ",
            "We see that several factors of 'Annual Household Income' and ",
            "'Number of Household Members' play a very important role in ",
            "predciting 'Food Security' in this particular model.",
            br(),
            img(src = "variable-importance.png",
                # Center the image
                style="display: block; margin-left: auto; margin-right: auto;"
            ),
            h4("Advantages:"),
            tags$ul(
              tags$li("Easily handles a large data set with a multitude of",
                      "variables."),
              tags$li("can automatically handle missing values."),
              tags$li("Reduced risk of overfitting."),
              tags$li("Robust to outliers."),
              tags$li("Easy to evaluate variable importance, or contribution, ",
                      "to the model.")
            ),
            h4("Drawbacks:"),
            tags$ul(
              tags$li("Not easy to interpret."),
              tags$li("Time-consuming algorithm; parallel processing can ",
                      "speed up computation times."),
              tags$li("Does not work well when data is sparse."),
              tags$li("Not good for extrapolation.")
            )
          )
        ))
      ),
      
###############################################################################
#
# Model Fitting Section
#
###############################################################################

      tabPanel(
        title = "Model Fitting",
        
        sidebarPanel(
          h3("Step 1: Select an Approach"
          ),
          radioButtons("modelType", 
            label = "Choose one model type:", 
            choices = c("Binomial Logistic Regression",
                        "Classification Tree",
                        "Random Forest"), 
            selected = character(0)
          ),
          conditionalPanel(condition = "input.modelType == 'Binomial Logistic Regression'",
            h3("Step 2: Choose Predictors"
            ),
            checkboxGroupInput("multiModelVars", 
              label = "Select one or more variables to include as predictors in the model",
              choices = names(foodSecurity)[-1]
            ),
            h3("Step 3: Select Fit Options"
            ),
            sliderInput("splitPercent", 
              label = "Choose the percent of data used to train your models:",
              min = 50, max = 90, value = 70, post = "%"
            ),
            sliderInput("numFolds", 
              label = "Choose the number of folds to use in cross validation:",
              min = 2, max = 10, value = 5
            ),
            actionButton("runMLM", label = "Create Model")
          ),
          conditionalPanel(condition = "input.modelType == 'Classification Tree'",
            h3("Step 2: Choose Predictors"
            ),
            checkboxGroupInput("classTreeVars", 
              label = "Select one or more variables to include as predictors in the model",
                choices = names(foodSecurity)[-1]
            ),
            h3("Step 3: Select Fit Options"
            ),
            sliderInput("splitPercent", 
              label = "Choose the percent of data used to train your models:",
              min = 50, max = 90, value = 70, post = "%"
            ),
            sliderInput("numFolds", 
              label = "Choose the number of folds to use in cross validation:",
              min = 2, max = 10, value = 5
            ),
            actionButton("runClassTree", label = "Create Model")
          ),
          conditionalPanel(condition = "input.modelType == 'Random Forest'",
            h3("Step 2: Choose Predictors"
            ),
            checkboxGroupInput("forestVars", 
              label = "Select one or more variables to include as predictors in the model",
              choices = names(foodSecurity)[-1]
            ),
            h3("Step 3: Select Fit Options"
            ),
            sliderInput("splitPercent", 
              label = "Choose the percent of data used to train your models:",
              min = 50, max = 90, value = 70, post = "%"
            ),
            sliderInput("numFolds", 
              label = "Choose the number of folds to use in cross validation:",
              min = 2, max = 10, value = 5
            ),
            actionButton("runForest", label = "Create Model")
          )
        ),
        
        mainPanel(fluidPage(
          conditionalPanel(condition = "input.modelType == 'Binomial Logistic Regression'",
            htmlOutput("logisticTitle"),
            h4("Model Fit Summary:"),
            verbatimTextOutput("summaryMulti"),
            br(),
            h4("Model Fit Statistics (on test data):"),
            verbatimTextOutput("logisticFitStats")
          ),
          conditionalPanel(condition = "input.modelType == 'Classification Tree'",
            htmlOutput("treeTitle"),
            h4("Model Fit Summary:"),
            plotOutput("summaryTree"),
            br(),
            h4("Model Fit Statistics (on test data):"),
            verbatimTextOutput("treeFitStats")
          ),
          conditionalPanel(condition = "input.modelType == 'Random Forest'",
            htmlOutput("forestTitle"),
            h4("Model Fit Summary:"),
            plotOutput("summaryForest"),
            br(),
            h4("Model Fit Statistics (on test data):"),
            verbatimTextOutput("forestFitStats")
          )
        ))
      ),

###############################################################################
#
# Prediction Section
#
###############################################################################

      tabPanel(
        
        title = "Prediction",
        
        sidebarPanel(
          radioButtons("predictionModel", 
                       label = "Choose one model type:", 
                       choices = c("Binomial Logistic Regression",
                                   "Classification Tree",
                                   "Random Forest"), 
                       selected = character(0)
          ),
          conditionalPanel(
            condition = (
              "input.predictionModel == 'Binomial Logistic Regression' || 
              input.predictionModel == 'Classification Tree' || 
              input.predictionModel == 'Random Forest'"
            ),
            selectizeInput("sexPred", 
              label = "Sex",
              choices = levels(foodSecurityNR$sex),
              multiple = FALSE,
              options = list(
                placeholder = "",
                onInitialize = I('function() { this.setValue(""); }')
              )
            ),
            selectizeInput("racePred", 
              label = "Race",
              choices = levels(foodSecurityNR$race),
              multiple = FALSE,
              options = list(
                placeholder = "",
                onInitialize = I('function() { this.setValue(""); }')
              )
            ),
            selectizeInput("hispanicOriginPred", 
              label = "Hispanic Origin",
              choices = levels(foodSecurityNR$hispanicOrigin),
              multiple = FALSE,
              options = list(
                placeholder = "",
                onInitialize = I('function() { this.setValue(""); }')
              )
            ),
            selectizeInput("agePred", 
              label = "Age",
              choices = levels(foodSecurityNR$age),
              multiple = FALSE,
              options = list(
                placeholder = "",
                onInitialize = I('function() { this.setValue(""); }')
              )
            ),
            selectizeInput("citizenshipPred", 
              label = "US Citizenship",
              choices = levels(foodSecurityNR$citizenship),
              multiple = FALSE,
              options = list(
                placeholder = "",
                onInitialize = I('function() { this.setValue(""); }')
              )
            ),
            selectizeInput("typeHHPred", 
              label = "Type of Household",
              choices = levels(foodSecurityNR$typeHH),
              multiple = FALSE,
              options = list(
                placeholder = "",
                onInitialize = I('function() { this.setValue(""); }')
              )
            ),
            selectizeInput("numHHMembersPred", 
              label = "Number of Household Members",
              choices = levels(foodSecurityNR$numHHMembers),
              multiple = FALSE,
              options = list(
                placeholder = "",
                onInitialize = I('function() { this.setValue(""); }')
              )
            ),
            selectizeInput("employStatusPred", 
              label = "Employment Status",
              choices = levels(foodSecurityNR$employStatus),
              multiple = FALSE,
              options = list(
                placeholder = "",
                onInitialize = I('function() { this.setValue(""); }')
              )
            ),
            selectizeInput("annualHHIncomePred", 
              label = "Annual Household Income",
              choices = levels(foodSecurityNR$annualHHIncome),
              multiple = FALSE,
              options = list(
                placeholder = "",
                onInitialize = I('function() { this.setValue(""); }')
              )
            ),
            selectizeInput("maritalStatusPred", 
              label = "Marital Status",
              choices = levels(foodSecurityNR$maritalStatus),
              multiple = FALSE,
              options = list(
                placeholder = "",
                onInitialize = I('function() { this.setValue(""); }')
              )
            ),
            selectizeInput("livingQuartersPred", 
              label = "Living Quarters",
              choices = levels(foodSecurityNR$livingQuarters),
              multiple = FALSE,
              options = list(
                placeholder = "",
                onInitialize = I('function() { this.setValue(""); }')
              )
            ),
            selectizeInput("educationLevelPred", 
              label = "Education Level",
              choices = levels(foodSecurityNR$educationLevel),
              multiple = FALSE,
              options = list(
                placeholder = "",
                onInitialize = I('function() { this.setValue(""); }')
              )
            ),
            selectizeInput("receivedSNAPPred", 
              label = "Household Received SNAP Benefits",
              choices = levels(foodSecurityNR$receivedSNAP),
              multiple = FALSE,
              options = list(
                placeholder = "",
                onInitialize = I('function() { this.setValue(""); }')
              )
            )
          )
        ),
        
        mainPanel(fluidPage(
          conditionalPanel(condition = "input.predictionModel == 'Binomial Logistic Regression'",
            h4(strong("The Binomial Logistic Regression Model uses the ",
                      "following varables to predict food security status as ",
                      "either SECURE or INSECURE:")),
            htmlOutput("logisticPredcitonVariables"),
            br(),
            h4(strong("Enter your values of these predictors on the left, ",
                      "then click the button below to get your prediction.")),
            actionButton("getLogisticPrediction", label = "Get Prediction"),
            htmlOutput("logisticPrediction")
          ),
          conditionalPanel(condition = "input.predictionModel == 'Classification Tree'",
            h4(strong("The Classification Tree Model uses the following ",
                      "varables to predict food security status as either ",
                      "SECURE or INSECURE:")),
            htmlOutput("treePredcitonVariables"),
            br(),
            h4(strong("Enter your values of these predictors on the left, ",
                      "then click the button below to get your prediction.")),
            actionButton("getTreePrediction", label = "Get Prediction"),
            htmlOutput("treePrediction")
          ),
          conditionalPanel(condition = "input.predictionModel == 'Random Forest'",
            h4(strong("The Random FOrest Model uses the following varables to ",
                      "predict food security status as either SECURE or ",
                      "INSECURE:")),
            htmlOutput("forestPredcitonVariables"),
            br(),
            h4(strong("Enter your values of these predictors on the left, ",
                      "then click the button below to get your prediction.")),
            actionButton("getForestPrediction", label = "Get Prediction"),
            htmlOutput("forestPrediction")
          )
        ))
      )
    ),
    
###############################################################################
#
# Data Tab
#
###############################################################################

    tabPanel(
      
      # Add a title.
      title = "Data",
      
      fluidPage(
        wellPanel(
          h4(strong("Select variables to include in the data table:")),
          pickerInput("variablesPicked",
            label = NULL,
            choices = variableList,
            options = list(`actions-box` = TRUE,
                           `selected-text-format` = "count > 5",
                           `count-selected-text` = "{0}/{1} variables selected"),
            multiple = TRUE  # Selection of multiple items is allowed
          ),
          actionButton("viewButton", "View"),
          br(),
        ),
        htmlOutput("saveButtonTitle"),
        DT::dataTableOutput("rawData", width = '1800px')
      )
    )
  )
))

