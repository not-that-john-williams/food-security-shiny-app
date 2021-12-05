# food-security-shiny-app
Shiny App exploring the 2020 "Current Population Survey: Food Security Supplement" from census.gov

## Required Packages

This app uses the following packages:

- `caret`: Various functions for training and plotting classification and regression models.
- `DT`: Provides an R interface to the JavaScript library DataTables.
- `ggplot2`: A system for declaratively creating graphics, based on "The Grammar of Graphics".
- `gmodels`: Various R programming tools for model fitting.
- `graphics`: R functions for base graphics.
- `nnet`: Software for feed-forward neural networks with a single hidden layer, and for multinomial log-linear models.
- `ranger`: A fast implementation of Random Forests.
- `rattle`: Provides a collection of utility functions for data science.
- `rpart`: Recursive partitioning for classification and regression trees.
- `shiny`: Easily build rich and productive interactive web apps in R.
- `shinythemes`: Provides some Bootstrap themes for use with shiny. 
- `shinyWidgets`: Offers custom widgets and other components to enhance shiny applications.
- `sjPlot`: Collection of plotting and table output functions for data visualization.
- `summarytools`: Provides a coherent set of functions centered on data exploration and simple reporting.
- `tidyverse`: Set of packages that work in harmony because they share common data representations and API design.

To install them all, run this code chunk:

```
install.packages("caret")
install.packages("DT")
install.packages("ggplot2")
install.packages("gmodels")
install.packages("graphics")
install.packages("nnet")
install.packages("ranger")
install.packages("rattle")
install.packages("rpart")
install.packages("shiny")
install.packages("shinythemes")
install.packages("shinyWidgets")
install.packages("sjPlot")
install.packages("summarytools")
install.packages("tidyverse")
```

## Code to run the app:

```
shiny::runGitHub("not-that-john-williams/food-security-shiny-app", ref = "main")
```
