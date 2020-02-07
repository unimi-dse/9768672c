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
#' @import na.tools

import_data <- function(){
  dataset_df<-read.csv(system.file("extdata", "Tesla top 5.csv", package="teslasales"),sep=",")
  rownames(dataset_df)<-dataset_df[[1]]
  dataset_df[1]<-NULL
  return(dataset_df)
}


#' Create a barplot of a function applied to a dataframe
#'
#' @description This function allows to crate a coloured barplot using the R-package plotly
#' of a function applied to to a specific dataframe.
#' @param dataset A dataframe.
#' @param main A character string for the name of the plot (default = 'Barplot').
#' @param xname A character string for the name of the x-axe (default = 'Countries').
#' @param yname A character string for the name of the y-axe (default = 'Number of sales').
#' @param func A function to be applied to the columns of the dataset.
#'
#' @return The plot for the selected arguments.
#' @export
#'
#' @examples
#' dataset<-add_listfunction(df,func,character)
#' data_barplotly(dataset, 'Barplot of dataset', 'X-axe', 'Y-axe')
#' @import magrittr dplyr plotly RColorBrewer


barplotly_func <- function(dataset,func,main,xname,yname){
  mat_df<-as.matrix(dataset)
  mat_func <-apply(mat_df,MARGIN=2,FUN=func)
  tmat_func<-t(mat_func)
  df_tmat<-as.data.frame(tmat_func)
  data_ly<-as.matrix(df_tmat)
  plot_dataset<- plot_ly(x =colnames(data_ly),
                         y = data_ly,
                         name = main,
                         type = "bar", marker = list(color = c('rgb(250,0,0)','rgb(0,250,0)','rgb(39, 129, 241)','rgb(252,215,4)','rgb(39, 211, 242)'),
                                                     line = list(color = 'rgb(0,0,0)',width = 1.5))) %>% layout(title = main, xaxis = list(title = xname),yaxis = list(title = yname))
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
#' @return A class 'zoo' object.
#' @export
#'
#' @examples
#' dataset<-data.frame()
#' ts_data(dataset,1)
#' @import lubridate


ts_datazoo <- function(dataset,n_ts){
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
#' @param yname A character string for the name of the x-axe (default = 'Number of sales')
#' @param xname A character string for the name of the y-axe (default = 'Dates')
#'
#' @return The plot for the selected arguments.
#' @export
#'
#' @examples
#' dataset<-data.frame()
#' data_tsplotly(dataset, 1, 'Graph', 'Values')
#' @import zoo


data_tsplotly<-function(dataset,n_ts,main='Time series plot',yname='Number of sales',xname='Dates'){
  data_qtr<-as.Date.character(rownames(dataset))
  data_mat<-as.matrix(dataset)
  plot_ts<-plot_ly(x=data_qtr,y=data_mat[,n_ts], type='scatter',mode='lines+markers',
                   marker = list(color = 'rgba(255, 182, 193)',
                                 line = list(color = 'rgba(152, 0, 0)',width = 2))) %>%
    layout(title = main, yaxis = list(title=yname), xaxis = list(title=xname))
  return(plot_ts)
}


#' Plot two time series from a dataset in the same linegraph
#'
#' @description This function allows comparisons in trends between two timeseries selected
#' from a dataset and includes a slider to better select the periods of the analysis.
#'
#' @param dataset A dataframe.
#' @param n_ts1 A number referring to the colum of the dataset to plot as a time series.
#' @param n_ts2 A number referring to the colum of the dataset to plot as a time series.
#' @param main A character string for the name of the first time series.
#' @param second A character string for the name of the second time series.
#' @param title A character string for the title of the plot (default = 'Comparison Time series plot').
#' @param yname A character string for the name of the y-axe (default = 'Number of sales').
#' @param xname A character string for the name of the x-axe (default = 'Dates').
#'
#' @return The plot for the selected arguments.
#' @export
#'
#' @examples
#' dataset<-data.frame()
#' data_tracesl(dataset,1,2,'High school', 'University', 'Grades in school','Grades')


data_tracesl<-function(dataset,n_ts1,n_ts2,main,second,title='Comparison Time series plot',yname='Number of sales',xname='Dates'){
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


#' To inspect the moments of a time series
#'
#' @description This function allows you to inspect the values of the correlation, covariance
#' and partial autocorrelation starting by an object of class zoo (time series).
#'
#' @param zoo_ts A class 'zoo' object.
#' @param stat A character string chosen between: "correlation", "covariance", "partial"
#' or a character vector containing two or more of them.
#'
#' @return A object of class 'acf'.
#' @export
#'
#' @examples
#' obj_zoo<-zoo(x = data.frame(), order.by = as.Date(c()))
#' stat_ts(obj_zoo, 'correlation')


stat_ts<-function(zoo_ts,stat){
  statistics<-acf(zoo_ts, lag.max = NULL,type =stat,
                  plot = TRUE, na.action = na.pass,demean = TRUE)
  return(statistics)
}


#' To create a time series of a dataframe
#'
#' @description This function allows to create a time series object from a dataframe using
#' the R-package 'stats'.
#' @param dataset A dataframe.
#' @param start_date A numeric vector of two elemets, year and quarter, respectively.
#' @param end_date A numeric vector of two elemets, year and quarter, respectively.
#'
#' @return A class 'ts' object.
#' @export
#'
#' @examples
#' INSERT EXAMPLE
#' @import stats


ts_datastats <- function(dataset,start_date,end_date){
  data_ts<-ts(dataset,start=start_date,end=end_date,deltat=3/12)
  return(data_ts)
}


#' To verify the order of integration of a time series and its statistics
#'
#' @description This function allows to verify if the time series is stationary or it is
#' integrated of order 1 or higher than 1.
#' @param ts A class 'ts' object.
#' @param n_ts A number referring to the colum of the time series.
#'
#' @return A Charachter string and a class 'htest' object.
#' @export
#'
#' @examples
#' INSERT EXAMPLE
#' @import forecast tseries


stat_data<-function(ts, n_ts){
  univar_ts<-ts[,n_ts]
  stat_test<-adf.test(univar_ts, alternative="stationary")
  if(stat_test$p.value<0.05){
    print("THE TIME SERIES IS STATIONARY")
  }
  else if(stat_test$p.value>0.05){
    data_diff<-diff(univar_ts,1)
    stat_testdiff<-adf.test(data_diff, alternative='stationary')
    if (stat_testdiff$p.value < 0.05){
      print("The order of integration is 1")
      return(stat_testdiff)}
    else if (stat_testdiff$p.value > 0.05){
      print("The order of integration is higher than 1")
      return(stat_testdiff)
    }
  }
}


#' To select automatically the best ARIMA model for a specified time series
#'
#' @description This function allows to look for the best parameters for the
#' tailored ARIMA model for a specific time series.
#' @param ts A class 'ts' object.
#' @param n_ts A number referring to the colum of the time series.
#'
#' @return A class 'forecast_ARIMA' , "ARIMA" and 'Arima' object.
#' @export
#'
#' @examples
#' INSERT EXAMPLE


autoparam_tsarima<-function(ts, n_ts){
  univar_ts<-ts[,n_ts]
  autom_param<-auto.arima(univar_ts, trace=TRUE)
  return(autom_param)
}


#' To select manually the best ARIMA model for a specified time series.
#'
#' @param ts A class 'ts' object.
#' @param n_ts A number referring to the colum of the time series.
#' @param vct_param A numeric vector with three integer components (p, d, q), which refers to
#' the AR order, the degree of differencing, and the MA order.
#' @return A class 'Arima' object.
#' @export
#'
#' @examples


manualparam_tsarima<-function(ts,n_ts,vct_param){
  univar_ts<-ts[,n_ts]
  manua_param<-arima(univar_ts,order=vct_param)
  return(manua_param)
}


#' To plot the residuals of a ARIMA model of a Time Series
#'
#' @description This function allows to create a plot of the residuals of an ARIMA model
#' of a time series with both two separate plots of the Autocorrelation (ACF) and Partial Autocorrelation function (PACF)
#' @param ts_arima A class 'Arima' object.
#' @param title A character string for the title of the plot.
#'
#' @return Three plots (Residuals, ACF and PACF).
#' @export
#'
#' @examples
#' INSERT EXAMPLE
#' @import ggplot2


plot_tsres<-function(ts_arima,title){
  plotres<-tsdisplay(residuals(ts_arima), lag.max=10, main=title)
  return(plotres)
}

#' To plot the linear forecast of an ARIMA model of a time series
#'
#' @description This function allows to create a plot which contains a linear forecast,
#' based on pre-selected ARIMA model.
#' @param ts_arima A class 'Arima' object.
#' @param n_period A number refferred to the period of the forecast expressed in quarters.
#' @param vctconf_interv A numeric vector with two elements which refer respectively to the lower and upper
#' limits of the confidence interval of the forecast.
#' @param title A character string for the title of the plot.
#' @param xname A character string for the label of the x-axe.
#' @param yname A character string for the label of the y-axe.
#'
#' @return The plot of the forecast.
#' @export
#'
#' @examples
#' INSERT EXAMPLES


tsforc_data<-function(ts_arima, n_period,vctconf_interv,title,xname,yname){
  ts_fcast<-forecast(ts_arima,h=n_period,level=vctconf_interv)
  forc_plot<-plot(ts_fcast,PI=TRUE, showgap=TRUE, shaded=TRUE, col="blue",fcol="red",main=title,xlab=xname,ylab=yname)
  return(forc_plot)
}
