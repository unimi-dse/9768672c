---
title: "README"
author: "Francesco Lazzara"
date: "02/03/2020"
output: html_document
---


Mid-term Project: Coding for Data Science academic Year 2019/2020

   Teslasales Package
-----------------------------------------------------------------

### Introduction
The package contains a dataset of the Sales of Tesla Motorcars in five European Countries (Norw, Ned, Swi, Ger and UK).
The dataset is used for statistical purposes (data analysis, visualization, ecc.) and contains built-in functions for deeper examination of each task.

### Installation

    #Install the package by running the command:
    
    > devtools::install_github("unimi-dse/9768672c")
    
### Dataset

The packages contains a dataset from "Tesla Europe Registration Stats" (https://teslamotorsclub.com/tmc/threads/tesla-europe-registration-stats.61651/) accuratley modified in order to highlight the most relevant data.

      #ADD DATASET DOCUMENTATION (NOT COMPLETE!!!)
      > ?teslasales::repo  
   
### Usage

      #load the package and run the function to recall the dataset
      > require('teslasales')
         import_data()

The function import_data() allows to have the dataset in the global environment as a dataframe.

The function add_listfunc() takes as arguments a df, a function which will be applied to the columns of the dataframe and a character string as the name of the operation that you performed. It returns a list with the original dataset as first element and a dataframe of the function applied as the second one.

The function data_barplotly() takes as arguments a df, a character string for the name of the plot and two character strings for the name of both the x and y axis respectively. It gives as output the barplot itself generated using the package plotly.

The function ts_data() creates a time series, using the R-package plotly, of an specified element (second argument) of a dataframe. It returns a class zoo element.

The function data_tsplotly() takes as arguments a df, a specified number which refers to the time series of the original dataset, a character string for the name of the plot and two others character strings for the name of both the x and y axis respectively. It returns a linechart of the time series generated by the R-package plotly.

The function data_tracesl() takes as arguments a df, two specified numbers each referring to the time series of the original dataset, two character strings for the names of both the time series, another character string for the name of the plot and two others character strings for the name of both the x and y axis respectively.

The function stat_ts() takes as arguments a zoo class object (time series) and a character string or a vector of character string which refers to the statistic that will be applied to the object (ex. "correlation", "covariance", "partial"). It returns a basic plot of the statistic selected.

The function data_statplotly() takes as arguments a variable name assigned to "stat_ts() function", a character string for the name of the plot and two others character strings for the name of both the x and y axis respectively.


### Dependency with other R-packages

-   zoo: package for creating time series objects
-   na.tools: package useful to work with missing datavisualization (plotting)
-   RColorBrewer: package for customizing the colours of the plot with
    plotly
-   lubridate: package for convert date into quarterly
-   plotly: package used for data visualization (plotting)
