library(shiny)
library(shinythemes)
library(ggplot2)
library(summarytools)
library(DT)
library(gmodels)

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

})
