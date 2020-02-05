#' Import data and filter the original dataset from "Tesla Europe Registration Stats"
#'
#' @description This function grabs the dataset of the sales of Tesla in Europe form 2013 to 2019 and store the data frame to Global Environment.
#' Data taken from: https://teslamotorsclub.com/tmc/threads/tesla-europe-registration-stats.61651/.
#' Run this first function to recall the dataset.
#'
#' @return dataframe
#'
#' @examples
#' import_data()
#' @export
#' @import readr

import_data <- function(){
  dataset_df<-read.csv(system.file("extdata", "Tesla top 5.csv", package="teslasales"),sep=",")
  rownames(dataset_df)<-dataset_df[[1]]
  dataset_df[1]<-NULL
  return(dataset_df)
}


#' Apply a function to a dataset creating another element in a list
#'
#' @description This function allows to create a list with the original dataset and a dataframe
#' containing the values originated by the application of the second argument to the dataset.
#'
#' @param df A dataframe.
#' @param func A character string with the function to apply.
#' @param character A character string for the name of the element added to the list
#'
#' @return list
#'
#' @examples
#' dataset<-data.frame()
#' dataset_lsum <- add_listfunc(dataset, sum, 'TOT')
#' @export

add_listfunc <- function(df,func,character='Function'){
  mat_df<-as.matrix(df)
  mat_func <-apply(mat_df,MARGIN=2,FUN=func)
  tmat_func<-t(mat_func)
  df_tmat<-as.data.frame(tmat_func)
  colnames(df_tmat)<-c(colnames(df))
  df_list<-list('dataset'=df,'dataset_function'=df_tmat)
  return(df_list)
}


#' Create a barplot of the dataframe
#'
#' @description This function allows to crate a coloured barplot using the R-package plotly
#' of the second element of the list deriving by the add_listfunc().
#'
#' @param dataset A dataframe resulting by the function "add_listfunc".
#' @param main A character string for the name of the plot (default = 'Barplot').
#' @param xname A character string for the name of the x-axe (default = 'Countries')
#' @param yname A character string for the name of the y-axe (default = 'N° of sales')
#'
#' @return A barplot for the given arguments.
#' @export
#'
#' @examples
#' dataset<-add_listfunction(df,func,character)
#' data_barplotly(dataset, 'Barplot of dataset', 'X-axe', 'Y-axe')


data_barplotly <- function(dataset,main='Barplot',xname='Countries',yname='Number of sales'){
  data_ly<-as.matrix(dataset[[2]])
  plot_dataset<- plot_ly(x =colnames(data_ly),
                         y = data_ly[1,],
                         name = main,
                         type = "bar", marker = list(color = c('rgb(250,0,0)','rgb(0,250,0)','rgb(39, 129, 241)','rgb(252,215,4)','rgb(39, 211, 242)'),
                                                     line = list(color = 'rgb(0,0,0)',width = 1.5))) %>%
    layout(title = main, xaxis = list(title = xname),yaxis = list(title = yname))
  return(plot_dataset)
}


#' Create a time series of a dataframe
#'
#' @description This function creates a time series of the dataset with teh R-package 'zoo'
#' with the rownames used as Dates (quarterly).
#'
#' @param dataset A dataframe.
#' @param n_ts Number of the column of the dataframe which will generate a time series.
#'
#' @return A class zoo object.
#' @export
#'
#' @examples
#' dataset<-data.frame()
#' ts_data(dataset,1)


ts_data <- function(dataset,n_ts){
  df_data<-as.data.frame(dataset)
  data_qtr<-ymd(rownames(dataset))
  data_qtryr<-quarter(data_qtr,with_year=TRUE)
  ts_df<-zoo(x = df_data[[n_ts]], order.by = data_qtryr)
  return(ts_df)
}


#' Create a lineplot of a time series
#'
#' @description This function creates a lines+scatter plot of a time series using
#' the R-package 'plotly'.
#'
#' @param dataset A dataframe.
#' @param n_ts A number referring to the colum of the dataset to plot as a time series.
#' @param main A character string for the title of the plot (default = 'Time series plot')
#' @param yname A character string for the name of the x-axe (default = 'Dates')
#' @param xname A character string for the name of the y-axe (default = 'Number of sales')
#'
#' @return The plot for the selected arguments.
#' @export
#'
#' @examples
#' dataset<-data.frame()
#' data_tsplotly(dataset, 1, 'Graph', 'Values', 'Dates')


data_tsplotly<-function(dataset,n_ts,main='Time series plot',yname='Number of sales',xname='Dates'){
  data_qtr<-as.Date.character(rownames(dataset))
  data_mat<-as.matrix(dataset)
  plot_ts<-plot_ly(x=data_qtr,y=data_mat[,n_ts], type='scatter',mode='lines+markers',
                   marker = list(color = 'rgba(255, 182, 193)',
                                 line = list(color = 'rgba(152, 0, 0)',width = 2))) %>%
    layout(title = main, yaxis = list(title=yname), xaxis = list(title=xname))
  return(plot_ts)
}


#to plot two time series in the same graph for comparison in the trends

#' Title
#'
#' @param dataset
#' @param n_ts1
#' @param n_ts2
#' @param main
#' @param second
#' @param title
#' @param yname
#' @param xname
#'
#' @return
#' @export
#'
#' @examples


data_tracesl<-function(dataset,n_ts1,n_ts2,main,second,title='Comparison Time series plot',yname='N° of sales',xname='Dates'){
  data_qtr<-as.Date.character(rownames(dataset))
  data_mat<-as.matrix(dataset)
  df<-as.data.frame(data_mat[,c(n_ts1,n_ts2)])
  l <- list(
    font = list(
      family = "sans-serif",
      size = 12,
      color = 'rgb(0,0,0)'),
    bgcolor = "rgb(200, 200, 200)",
    bordercolor = "#FFFFFF",
    borderwidth = 2,
    xanchor='center',
    x=1.2, y=0.5)
  plot_ts<-plot_ly(df,x=data_qtr,y=data_mat[,n_ts1],name=main,type='scatter',mode='lines+markers',
                   marker = list(color = 'rgba(255, 182, 193)', line = list(color = 'rgba(152, 0, 0)',width = 2))) %>%
    add_trace(y =data_mat[,n_ts2], name = second, mode = 'lines+markers') %>%
    layout(title = title, yaxis = list(title=yname), xaxis = list(title=xname),legend=l)
  plot_tslider<-rangeslider(plot_ts,start=start(data_qtr),end=end(data_qtr))
  return(plot_tslider)
}


#to do some statistics with the time series (correlation,covariance, partial autocorr)

#' Title
#'
#' @param zoo_ts
#' @param stat
#'
#' @return
#' @export
#'
#' @examples


stat_ts<-function(zoo_ts,stat){
  statistics<-acf(zoo_ts, lag.max = NULL,type =stat,
                  plot = TRUE, na.action = na.pass,demean = TRUE)
  return(statistics)
}


#to plot a time series statistic with plotly

#' Title
#'
#' @param autoc_dataset
#' @param main
#' @param yname
#' @param xname
#'
#' @return
#' @export
#'
#' @examples


data_statplotly<-function(autoc_dataset,main='Moments of a Time series',yname='Values',xname='lags'){
  acd <- data.frame(lag=autoc_dataset$lag, acf=autoc_dataset$acf)
  plot_autoc<-plot_ly(x=acd$lag,y=acd$acf,mode='markers',type='scatter') %>%
    layout(title = main, yaxis = list(title=yname), xaxis = list(title=xname))
  return(plot_autoc)
}
