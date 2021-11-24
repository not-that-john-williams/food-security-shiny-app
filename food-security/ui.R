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
        
        tabPanel(
            
            # Add a title.
            title = "Modeling",
            
            mainPanel(
            )
        ),
        
        tabPanel(
            
            # Add a title.
            title = "Data",
            
            mainPanel(
            )
        )
    )
)
)

