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
#' @param dataset A dataframe (default= 'NULL' which will recall the original dataset).
#' @param main A character string for the name of the plot (default = 'Barplot').
#' @param xname A character string for the name of the x-axe (default = 'Countries').
#' @param yname A character string for the name of the y-axe (default = 'Number of sales').
#' @param func A function to be applied to the columns of the dataset.
#'
#' @return The plot for the selected arguments.
#' @export
#'
#' @examples
#' data_barplotly(func=sum, title='Barplot of dataset', xname='X-axe', yname='Y-axe')
#' @import magrittr dplyr ggplot2 plotly


barplotly_func <- function(dataset=NULL,func,main='Barplot', xname='Countries',yname='Number of sales'){
  if (is.null(dataset)==TRUE){
    dataset=teslasales::dataset
  }
  else {
    dataset<-dataset}
  listx<-list(title=xname)
  listy<-list(title=yname)
  mat_df<-as.matrix(dataset)
  mat_func <-apply(mat_df,MARGIN=2,FUN=func)
  tmat_func<-t(mat_func)
  df_tmat<-as.data.frame(tmat_func)
  data_ly<-as.matrix(df_tmat)
  plot_dataset<- plotly::plot_ly(x =colnames(data_ly),y = data_ly,name = main,
                         type = "bar", marker = list(color = c('rgb(250,0,0)','rgb(0,250,0)','rgb(39, 129, 241)','rgb(252,215,4)','rgb(39, 211, 242)'),
                                                     line = list(color = 'rgb(0,0,0)',width = 1.5))) %>%
    layout(title = main, yaxis = listx[1], xaxis = listy[1])
  return(plot_dataset)
}

#' Create a time series of a dataframe
#'
#' @description This function creates a time series of the dataset with the R-package 'zoo'
#' with the rownames used as dates (quarterly).
#'
#' @param dataset A dataframe (default= 'NULL' which will recall the original dataset).
#'
#' @return A class 'zoo' object.
#' @export
#'
#' @examples
#' dataset<-data.frame()
#' ts_data(dataset)


ts_datazoo <- function(dataset=NULL){
  if (is.null(dataset)==TRUE){
    dataset=teslasales::dataset
  }
  else {
    dataset<-dataset}
  df_data<-as.data.frame(dataset)
  data_qtr<-lubridate::ymd(rownames(dataset))
  data_qtryr<-lubridate::quarter(data_qtr,with_year=TRUE)
  ts_df<-zoo::zoo(x = df_data, order.by = data_qtryr)
  return(ts_df)
}


#' Create a lineplot of a time series
#'
#' @description This function creates a lines+scatter plot of a time series using
#' the R-package 'plotly'.
#'
#' @param dataset A subsetted dataframe which contains one precise time series of interest
#' (to allow for univariate ts).
#' @param main A character string for the title of the plot (default = 'Time series plot').
#' @param yname A character string for the name of the x-axe (default = 'Number of sales').
#' @param xname A character string for the name of the y-axe (default = 'Dates').
#' @param start_date A character string which refers to the date of the first observation, with format "%Y-%m-%d" (default = "2013-09-30").
#' @param n_obs A number equal to the observations inside the time series (default = 26).
#' @param interval A character string which refers to the time interval between each observation (default = "quarter").
#'
#' @return The plot for the selected arguments.
#' @export
#'
#' @examples
#' dataset<-data.frame(..., 'something'=c(...))
#' data_tsplotly(dataset$something, main='Graph', yname='Values')
#' @import magrittr dplyr


data_tsplotly<-function(dataset,start_date="2013-09-30",n_obs=26,interval="quarter",main='Time series plot',yname='Number of sales',xname='Dates'){
  dates <- seq(as.Date(start_date), length=n_obs, by=interval)
  plot_ts<-plotly::plot_ly(x=dates,y=dataset, type='scatter',mode='lines+markers',
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
#' @param main A character string for the name of the first time series.
#' @param second A character string for the name of the second time series.
#' @param title A character string for the title of the plot (default = 'Comparison Time series plot').
#' @param yname A character string for the name of the y-axe (default = 'Number of sales').
#' @param xname A character string for the name of the x-axe (default = 'Dates').
#' @param dataset1 A subsetted dataframe which contains one precise time series of interest
#' (to allow for univariate ts).
#' @param dataset2 A subsetted dataframe which contains one precise time series of interest
#' (to allow for univariate ts).
#' @param start_date A character string which refers to the date of the first observation, with format "%Y-%m-%d" (default = "2013-09-30").
#' @param n_obs A number equal to the observations inside the time series (default = 26).
#' @param interval A character string which refers to the time interval between each observation (default = "quarter").
#'
#' @return The plot for the selected arguments.
#' @export
#'
#' @examples
#' dataset<-data.frame(..., 'something'=c(...), 'somethingelse'=c(...))
#' data_tracesl(dataset$something,dataset$somethingelse,main='High school', main='University', second='High school',title='Grades', yname='Numbers')
#' @import magrittr dplyr


data_tracesl<-function(dataset1,dataset2,start_date="2013-09-30",n_obs=26,interval="quarter",main,second,title='Comparison Time series plot',yname='Number of sales',xname='Dates'){
  dates <- seq(as.Date(start_date), length=n_obs, by=interval)
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
  plot_ts<-plotly::plot_ly(dataset,x=dates,y=dataset1,name=main,type='scatter',mode='lines+markers',
                   marker = list(color = 'rgba(255, 182, 193)', line = list(color = 'rgba(152, 0, 0)',width = 2))) %>%
    add_trace(y =dataset2, name = second, mode = 'lines+markers') %>%
    layout(title = title, yaxis = list(title=yname), xaxis = list(title=xname),legend=l)
  plot_tslider<-plotly::rangeslider(plot_ts,start=start_date,end=end(dates))
  return(plot_tslider)
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
#' dataset<-data.frame(...)
#' data_ts(dataset,start_date=c(2010,2),end_data=c(2020,4))


ts_datastats <- function(dataset=NULL,start_date=c(2013,3),end_date=c(2019,4)){
  if (is.null(dataset)==TRUE){
    dataset=teslasales::dataset
  }
  else {
    dataset<-dataset}
  data_ts<-stats::ts(dataset,start=start_date,end=end_date,deltat=3/12)
  return(data_ts)
}


#' To verify the order of integration of a time series
#'
#' @description This function allows to verify if the time series is stationary or it is
#' integrated of order 1 or higher than 1.
#'
#' @param ts A univariate time series of class 'ts' object.
#'
#' @return Print a character string.
#' @export
#'
#' @examples
#' dataset<-data.frame(...)
#' dataset_ts<-ts(data.frame,...)
#' stationary_data(dataset[,1])


stationary_data<-function(ts){
  c=0
  stat_test<-tseries::adf.test(ts, alternative="stationary")
  if(stat_test$p.value<0.05){
    print("THE TIME SERIES IS STATIONARY")
  }
  else if(stat_test$p.value>0.05){
    while (stat_test$p.value>0.05){
      c=c+1
      data_diff<-diff(ts,1)
      stat_test<-tseries::adf.test(data_diff, alternative='stationary')
    }
    sprintf("THE ORDER OF INTEGRATION IS %d",c)
  }
}


#' To select automatically the best ARIMA model for a specified time series
#'
#' @description This function allows to look for the best parameters for the
#' tailored ARIMA model for a specific time series.
#'
#' @param ts A univariate time series of class 'ts' object.
#'
#' @return A class 'forecast_ARIMA' , "ARIMA" and 'Arima' object.
#' @export
#'
#' @examples
#' dataset<-data.frame(...)
#' dataset_ts<-ts(data.frame,...)
#' autoparam_tsarima(dataset_ts[,1])


autoparam_tsarima<-function(ts){
  autom_param<-forecast::auto.arima(ts, trace=TRUE)
  return(autom_param)
}


#' To select manually the best ARIMA model for a specified time series.
#'
#' @param ts A univariate time series of class 'ts' object.
#' @param vct_param A numeric vector with three integer components (p, d, q), which refers to
#' the AR order, the degree of differencing, and the MA order.
#' @return A class 'Arima' object.
#' @export
#'
#' @examples
#' dataset<-data.frame(...)
#' dataset_ts<-ts(data.frame,...)
#' manualparam_tsarima(dataset_ts[,1])


manualparam_tsarima<-function(ts,vct_param){
  manua_param<-forecast::Arima(ts,order=vct_param)
  return(manua_param)
}


#' To plot the residuals of a ARIMA model of a Time Series
#'
#' @description This function allows to create a plot of the residuals of an ARIMA model
#' of a time series with both two separate plots of the Autocorrelation (ACF) and Partial Autocorrelation function (PACF)
#' @param ts_arima A univariate time series with class 'Arima' object.
#' @param title A character string for the title of the plot.
#'
#' @return Three plots (Residuals, ACF and PACF).
#' @export
#'
#' @examples
#' dataset<-data.frame(...)
#' dataset_ts<-ts(data.frame,...)
#' arima_ts<-autoparam_tsarima(dataset_ts[,1])
#' plot_tsres(arima_ts, "ARIMA")


plot_tsres<-function(ts_arima,title='PLOT ARIMA MODEL'){
  plotres<-forecast::tsdisplay(residuals(ts_arima), lag.max=10, main=title)
  return(plotres)
}

#' To plot the linear forecast of an ARIMA model of a time series
#'
#' @description This function allows to create a plot which contains a linear forecast,
#' based on pre-selected ARIMA model.
#' @param ts_arima A univariate time series with class 'Arima' object.
#' @param n_period A number refferred to the period of the forecast expressed in quarters (default= 4)
#' @param vctconf_interv A numeric vector with two elements which refer to the lower and upper
#' limits of the confidence interval of the forecast, respectively (default= c(80,95)).
#' @param title A character string for the title of the plot (default="FORECAST PLOT").
#' @param xname A character string for the label of the x-axe (default="Dates").
#' @param yname A character string for the label of the y-axe (default="Values").
#'
#' @return The plot of the forecast.
#' @export
#'
#' @examples
#' dataset<-data.frame(...)
#' dataset_ts<-ts(data.frame,...)
#' arimamodel_ts<-autoparam_tsarima(dataset_ts[,1])
#' tsforc_data(arimamodel_ts, 8)


tsforc_data<-function(ts_arima, n_period=4,vctconf_interv=c(80,95),title="FORECAST PLOT",xname='Dates',yname='Values'){
  ts_fcast<-forecast::forecast(ts_arima,h=n_period,level=vctconf_interv)
  forc_plot<-graphics::plot(ts_fcast,PI=TRUE, showgap=FALSE, shaded=TRUE, col="blue",fcol="red",main=title,xlab=xname,ylab=yname)
  return(forc_plot)
}
