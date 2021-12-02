library(shiny)
library(shinythemes)
library(shinyWidgets)
library(ggplot2)
library(summarytools)
library(DT)
library(caret)
library(rpart)
library(rattle)

#source('../finalProject.R')

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

shinyUI(navbarPage(
    
  # Add a theme.
  theme = shinytheme("united"),
  
  # Add a title.
  title = "2020 Food Security App",
  
  # Create tabs.
  tabsetPanel(
    
    # About section.
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
        "Would you believe over ,", a(href=USDALink, "38 million Americans"),
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
      
    # Data Exploration section.
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
    
    # Modeling Section
    navbarMenu(
      
      # Add a title.
      title = "Modeling",
      
      # Add the Modeling Info tab.
      tabPanel(
        title = "Modeling Info",
        
        wellPanel(
          h4(strong("Select a modeling approach to learn more about it:")
          ),
          radioButtons("infoType", 
                       label = "", 
                       choices = c("Multinomial Logistic Regression",
                                   "Classification Tree",
                                   "Random Forest"), 
                       selected = character(0)
          )
        ),
        
        mainPanel(fluidPage(
          conditionalPanel(condition = "input.infoType == 'Multinomial Logistic Regression'",
            h4(strong(tags$u("Multinomial Logistic Regression"))
            ),
            fluidRow(
              column(width = 5, offset = 1, {
                h4("Advantages")
              }),
              column(width = 4, offset = 1, {
                h4("Drawbacks")
              })
            )
          ),
          conditionalPanel(condition = "input.infoType == 'Classification Tree'",
            h4(strong(tags$u("Classification Tree"))
            ),
            fluidRow(
              column(width = 5, offset = 1, {
                h4("Advantages")
              }),
              column(width = 4, offset = 1, {
                h4("Drawbacks")
              })
            )
          ),
          conditionalPanel(condition = "input.infoType == 'Random Forest'",
            h4(strong(tags$u("Random Forest"))
            ),
            fluidRow(
              column(width = 5, offset = 1, {
                h4("Advantages")
              }),
              column(width = 4, offset = 1, {
                h4("Drawbacks")
              })
            )
          )
        ))
          # Explain the 3 modeling approaches
          # Benefits of each
          # Drawbacks of each
          # Include equations using `mathJax`
      ),
      
      # Add the Model Fitting tab.
      tabPanel(
        title = "Model Fitting",
        
        sidebarPanel(
          h3("Step 1: Select an Approach"
          ),
          radioButtons("modelType", 
            label = "Choose one model type:", 
            choices = c("Multinomial Logistic Regression",
                        "Classification Tree",
                        "Random Forest"), 
            selected = character(0)
          ),
          conditionalPanel(condition = "input.modelType == 'Multinomial Logistic Regression'",
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
          conditionalPanel(condition = "input.modelType == 'Multinomial Logistic Regression'",
            verbatimTextOutput("summaryMulti")
          ),
          conditionalPanel(condition = "input.modelType == 'Classification Tree'",
            plotOutput("summaryClassTree")
          ),
          conditionalPanel(condition = "input.modelType == 'Random Forest'",
            plotOutput("summaryForest")
          )
          
          # Split data into a training and test set, giving the user the 
          # ability to choose the proportion of data used in each.
          # User should have functionality for choosing model settings
          # for each model. For all models, the user should select the
          # variables used in the model. Cross validation should be 
          # used for selecting models where appropriate.
          # When the user is ready, they should be able to press a 
          # button and fit all three models on the training data.
          # Fit statistics (RMSE) should be reported for each model
          # along with the appropriate summaries about the model (for
          # instance summary() run on the glm() fit, a plot showing
          # the variable importance from the random forest model,...)
          # The models should be compared on the test set and 
          # appropriate statistics reported.
        ))
      ),
      tabPanel(
        title = "Prediction",
        mainPanel(fluidPage(
          # Give the user a way to use one of the models for 
          # prediction. That is they should be able to select the 
          # values of the predictors and obtain a prediction for the 
          # response.
        ))
      )
    ),
    
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
          #checkboxGroupInput("columns", 
                             #label = "", 
                             #choices = c(factorVariables),
                             # Default with all variables selected
                             #selected = c(factorVariables),
                             # Render horizontally
                             #inline = TRUE),
          actionButton("viewButton", "View"),
          br(),
        ),
        htmlOutput("saveButtonTitle"),
        DT::dataTableOutput("rawData", width = '1800px')
      )
      
      #sidebarPanel(),
      
      #mainPanel(
        #DT::dataTableOutput("rawData", width = '1800px')
          # User should be able to...
          # Scroll through the data set
          # Subset this data set (rows and columns)
          # Save the possibly subsetted data as a file (.csv is fine)
      #)
    )
  )
))

