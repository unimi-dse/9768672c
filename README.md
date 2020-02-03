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


### Dependency with other R-packages

-   zoo: package for creating time series objects
-   na.tools: package useful to work with missing data
-   RColorBrewer: package for customizing the colours of the plot with
    plotly
-   lubridate: package for convert date into quarterly
-   plotly: package used for data visualization (plotting)
