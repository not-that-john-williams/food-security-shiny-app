library(shiny)
library(shinythemes)
library(ggplot2)
library(summarytools)
library(DT)

shinyUI(navbarPage(
    
    # Add a theme.
    theme = shinytheme("united"),
    
    # Add a title.
    title = "2021 Food Security Survey from census.gov",
    
    # Create tabs.
    tabsetPanel(
        
        # About section.
        tabPanel(
            
            # Add a title.
            title = "About",
            
            mainPanel(
                # Describe the purpose of the app
                # Briefly discuss the data and its source - providing a link
                # to more information about the data
                # Tell the user the purpose of each tab (page) of the app
                # Include a picture related to the data 
            )
        ),
        
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
                                    "Contingency Tables", 
                                    "Descriptive Statistics")
                    ),
                    conditionalPanel(condition = "input.numericalType == 'Frequency Tables'",
                        selectInput("freqVariable",
                            label = "Select a group variable:",
                            choices = c("Food Security", "Sex", "Race",
                                        "Type of Household",
                                        "Annual Household Income",
                                        "Marital Status",
                                        "Living Quarters",
                                        "Education Level",
                                        "Household Recieved SNAP Benefits")
                        )
                    ),
                    conditionalPanel(condition = "input.numericalType == 'Contingency Tables'",
                        selectInput("contingencyVar1",
                            label = "Select first variable:",
                            choices = c("foodSecurity", "sex", "race",
                                        "typeHH", "annualHHIncome",
                                        "maritalStatus", "livingQuarters",
                                        "educationLevel", "receivedSNAP")
                        ),
                        selectInput("contingencyVar2",
                                    label = "Select second variable:",
                                    choices = c("foodSecurity", "sex", "race",
                                                "typeHH", "annualHHIncome",
                                                "maritalStatus", "livingQuarters",
                                                "educationLevel", "receivedSNAP")
                        )
                    ),
                    conditionalPanel(condition = "input.numericalType == 'Descriptive Statistics'",
                        selectInput("descripStatVariable",
                            label = "Select a group variable:",
                            choices = c("Age", "Number of Household Members")
                        )
                    )
                ),
                conditionalPanel(condition = "input.summaryType == 'Graphical'",
                    radioButtons("plotType",
                        label = "Select:",
                        choices = c("Bar Plot", 
                                    "Other Plot")
                    ),
                    conditionalPanel(condition = "input.plotType == 'Bar Plot'",
                        selectInput("barPlotVariable",
                                    label = "Select a group variable:",
                                    choices = c("Sex", "Race",
                                                "Type of Household",
                                                "Annual Household Income",
                                                "Marital Status",
                                                "Living Quarters",
                                                "Education Level",
                                                "Household Recieved SNAP Benefits")
                        )
                    ),
                    conditionalPanel(condition = "input.plotType == 'Other Plot'",
                        selectInput("otherPlotVariable",
                            label = "Select a group variable:",
                            choices = c("Sex", "Race",
                                        "Type of Household",
                                        "Annual Household Income",
                                        "Marital Status",
                                        "Living Quarters",
                                        "Education Level",
                                        "Household Recieved SNAP Benefits")
                        )
                    )
                )
            ),
            
            mainPanel(
                conditionalPanel(condition = "input.summaryType == 'Numerical'",
                    conditionalPanel(condition = "input.numericalType == 'Frequency Tables'",
                        DT::dataTableOutput("dataTable")
                    ),
                    conditionalPanel(condition = "input.numericalType == 'Contingency Tables'",
                        verbatimTextOutput("contingencyTable")
                    ),
                    conditionalPanel(condition = "input.numericalType == 'Descriptive Statistics'",
                        verbatimTextOutput("descripStat")
                    )
                ),
                conditionalPanel(condition = "input.summaryType == 'Graphical'",
                    conditionalPanel(condition = "input.plotType == 'Bar Plot'",
                        plotOutput("barPlot")
                    ),
                    conditionalPanel(condition = "input.plotType == 'Other Plot'",
                        plotOutput("otherPlot")
                    )
                )
            )
        ),
        
        navbarMenu(
            
            # Add a title.
            title="Modeling",
            
            # Fit 3 supervised learning models:
            # Generalized Linear Regression Model
            # Classification Tree
            # Random Forest Model
            
            # Add the Modeling Info tab.
            tabPanel(
                title = "Modeling Info",
                mainPanel(fluidPage(
                    # Explain the 3 modeling approaches
                    # Benefits of each
                    # Drawbacks of each
                    # Include equations using `mathJax`
                ))
            ),
            tabPanel(
                title = "Model Fitting",
                mainPanel(fluidPage(
                    # Split data into a taining a test set, giving the user the 
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
            
            mainPanel(
                # User should be able to...
                # Scroll through the data set
                # Subset this data set (rows and columns)
                # Save the possibly subsetted data as a file (.csv is fine)
            )
        )
    )
)
)

