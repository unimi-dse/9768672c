#' Import data and filter the original dataset from "Tesla Europe Registration Stats"
#'
#' This function grabs the dataset of the sales of Tesla in Europe form 2013 to 2019 and store the data frame to Global Environment.
#' Data taken from: https://teslamotorsclub.com/tmc/threads/tesla-europe-registration-stats.61651/.
#' Run this first function to recall the dataset.
#'
#' @return data.frame
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

import_data <- function() {
  df <- read_csv(system.file("extdata", "train.csv", package="titanicanalysis"))
  return(df)
}

#' Apply a function (sum, mean, var,...) to a dataset creating another element in a list
#'
#' @param df A dataframe.
#' @param func A character string with the function to apply.
#' @param character A character string for the name of the element added to the list
#'
#' @return list with two elements (the second on with the function applied)
#'
#' @examples
#' dataset<-data.frame()
#' dataset_lsum <- add_listfunc(dataset, sum, 'TOT')
#' @export
#' @import readr

add_listfunc <- function(df,func,character){
  mat_df<-as.matrix(df)
  mat_func <-apply(mat_df,MARGIN=2,FUN=func)
  tmat_func<-t(mat_func)
  df_tmat<-as.data.frame(tmat_func)
  colnames(df_tmat)<-c(colnames(df))
  df_list<-list('dataset'=df,'dataset_function'=df_tmat)
  return(df_list)
}


#to create a barplot with plotly

data_barplotly <- function(dataset,main,xname,yname){
  data_ly<-as.matrix(dataset[[2]])
  plot_dataset<- plot_ly(x =colnames(data_ly),
                         y = data_ly[1,],
                         name = main,
                         type = "bar", marker = list(color = c('rgb(250,0,0)','rgb(0,250,0)','rgb(39, 129, 241)','rgb(252,215,4)','rgb(39, 211, 242)'),
                                                     line = list(color = 'rgb(0,0,0)',width = 1.5))) %>%
    layout(title = main, xaxis = list(title = xname),yaxis = list(title = yname))
  return(plot_dataset)
}


#to create a time series with zoo of the dataset

ts_data <- function(dataset,n_ts){
  df_data<-as.data.frame(dataset)
  data_qtr<-ymd(rownames(dataset))
  data_qtryr<-quarter(data_qtr,with_year=TRUE)
  ts_df<-zoo(x = df_data[[n_ts]], order.by = data_qtryr)
  return(ts_df)
}


#to plot the time series with a scatter and lines plot

data_tsplotly<-function(dataset,n_ts,main,yname,xname){
  data_qtr<-as.Date.character(rownames(dataset))
  data_mat<-as.matrix(dataset)
  plot_ts<-plot_ly(x=data_qtr,y=data_mat[,n_ts], type='scatter',mode='lines+markers',
                   marker = list(color = 'rgba(255, 182, 193)',
                                 line = list(color = 'rgba(152, 0, 0)',width = 2))) %>%
    layout(title = main, yaxis = list(title=yname), xaxis = list(title=xname))
  return(plot_ts)
}


#to plot two time series in the same graph for comparison in the trends

data_tracesl<-function(dataset,n_ts1,n_ts2,main,second,title,yname,xname){
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

stat_ts<-function(zoo_ts,stat){
  statistics<-acf(zoo_ts, lag.max = NULL,type =stat,
                  plot = TRUE, na.action = na.pass,demean = TRUE)
  return(statistics)
}


#to plot a time series statistic with plotly

data_statplotly<-function(autoc_dataset,main,yname,xname){
  acd <- data.frame(lag=autoc_dataset$lag, acf=autoc_dataset$acf)
  plot_autoc<-plot_ly(x=acd$lag,y=acd$acf,mode='markers',type='scatter') %>%
    layout(title = main, yaxis = list(title=yname), xaxis = list(title=xname))
  return(plot_autoc)
}
