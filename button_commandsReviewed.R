#install.packages("corrgram")
#install.packages("zoo")
#install.packages("forecast")
#install.packages("lubridate")
library("lubridate", lib.loc="~/R/win-library/3.1")
#Source of data
#http://tcial.org/the-button/button.csv
button <- read.csv("~/Button Data/button5_20.csv")
button$time<-as.POSIXct(button$now_timestamp, origin="1970-01-01") #taken from http://stackoverflow.com/questions/13456241/convert-unix-epoch-to-date-object-in-r

#I must manually imput a minimum for the button, it never was below 8.
button$seconds_left[button$seconds_left<1]<-99

#The following line was my first instinct. Will cause a crash if I try to run it. Lots of data.
# plot(button$time,button$seconds_left)
# 
# #this should be operational
# x<-dim(button)
# x[1]
# subsample<-sort(sample(1:x[1],5000))
# plot(button$time[subsample],60-button$seconds_left[subsample],type="l",main="Sample of Time Elapsed Since Last Press (Size=5000)", xlab="Time",ylab="Time Elapsed")
# 
# #the following represents a typical, naieve biased estimate.It is biased because the time for t+1 depends on time 1. The button is an AUTO-regressive sequence.
# biased<-lm((60-button$seconds_left)~button$time) #use full sample to generate this.  Note the high significance is a result of autoregressive bias!
# abline(biased,col="red")

#First there is the missing data. There is the periods between clicks where the timer clicks down by 1 second, and actually missing data.
  #http://bocoup.com/weblog/padding-time-series-with-r/
#Get opening and closing time to sequence data.
  time.min<-button$time[1]
  time.max<-button$time[length(button$time)]
    all.dates<-seq(time.min, time.max, by="sec")
    all.dates.frame<-data.frame(list(time=all.dates))
#merge data into single data frame with all data
  merged.data<-merge(all.dates.frame, button,all=FALSE)
  
   list_na<-is.na(merged.data$seconds_left) 
# #na values have been established, but we cannot slavishly make them 0, and we need to mark periods that are LONG (>30sec, selected arbitrarily) as imputed.
#   #http://www.cookbook-r.com/Manipulating_data/Finding_sequences_of_identical_values/
#   #  table_na<-rle(list_na)
#   #  cumsum(table_na$lengths)
#   #  table_na$lengths[table_na$values==TRUE]  
# # Mark all of them. It's easier- http://r.789695.n4.nabble.com/find-data-date-gaps-in-time-series-td908012.html
# merged.data$missing_data[!merged.data$time %in% button$time<4]<-1 #if it's not in merged data, it's missing. If it's 0, it's missing.
# merged.data$missing_data[is.na(merged.data$missing_data)]<-0 #the rest were present.
  #table(merged.data$missing_data)


#We must fill in these NA's with the last value -1. http://stackoverflow.com/questions/19838735/how-to-na-locf-in-r-without-using-additional-packages
library("zoo", lib.loc="~/R/win-library/3.1")
library("xts", lib.loc="~/R/win-library/3.1")

#I trust that I did this correctly. Let us replace the button data frame now, officially.
# merged.data$imputed_sec_left<-imputed_data
button<-merged.data


#let us collapse this http://stackoverflow.com/questions/17389533/aggregate-values-of-15-minute-steps-to-values-of-hourly-steps
  #Need things as xts: http://stackoverflow.com/questions/4297231/r-converting-a-data-frame-to-xts
  #https://stat.ethz.ch/pipermail/r-help/2011-February/267752.html
     button_xts<-as.xts(button[,-1],order.by=button[,1])
     button_xts<-button_xts['2015/'] #2015 to end of data set. Fixes odd error timings.
#     button_xts<-button_xts[button_xts$missing_data==0]
 t<-10 #how many minutes each period is 10 minutes will allow for NO inf to show up. No shortage>15 min.
   end<-endpoints(button_xts,on="seconds",t*60) # t minute periods
col1<-period.apply(button_xts$seconds_left,INDEX=end,FUN=function(x) {min(x,na.rm=TRUE)}) #generates some empty sets
col2<-period.apply(button_xts$participants,INDEX=end,FUN=function(x) {min(x,na.rm=TRUE)})

  button_xts<-merge(col1,col2)
# we will add a lowest observed badge function.
  min_badge<-c(1:length(button_xts$seconds_left))
  for(i in 1:length(button_xts$seconds_left)){
    min_badge[i]<-floor(min(button_xts$seconds_left[1:(max(c(i-60/t,1)))])/10) #lowest badge seen yesterday is important.
  }

badge_class<-model.matrix(~~as.factor(min_badge))  #let's get these factors as dummy variables. http://stackoverflow.com/questions/5048638/automatically-expanding-an-r-factor-into-a-collection-of-1-0-indicator-variables
          

#Seasons matter: http://robjhyndman.com/hyndsight/longseasonality/
fourier <- function(t,terms,period)
{
  n <- length(t)
  X <- matrix(,nrow=n,ncol=2*terms)
  for(i in 1:terms)
  {
    X[,2*i-1] <- sin(2*pi*i*t/period)
    X[,2*i] <- cos(2*pi*i*t/period)
  }
  colnames(X) <- paste(c("S","C"),rep(1:terms,rep(2,terms)),sep="")
  return(X)
}
  hours<-fourier(1:length(index(button_xts)),1,60/t)
  days<-fourier(1:length(index(button_xts)),1,24*60/t)
  weeks<-fourier(1:length(index(button_xts)),1,7*24*60/t)
regressors<-data.frame(hours,days,weeks,badge_class[,2:dim(badge_class)[2]]) #badge_class[,2:dim(badge_class)[2]] #tried to use particpants. They are not significant.

library("forecast", lib.loc="~/R/win-library/3.1")

#reg_auto<-auto.arima(button_xts$seconds_left,xreg=regressors) #automatically chose from early ARIMA sequences, seasonal days, weeks, individual badge numbers are accounted for as a DRIFT term in the ARIMA sequence.
reg<-Arima(button_xts$seconds_left,order=c(1,1,1),xreg=regressors,include.constant=TRUE)
res<-residuals(reg)
png(filename="~/Button Data/5_20_acf.png")
  acf(res,na.action=na.omit)
    dev.off()
png(filename="~/Button Data/5_20_pacf.png")
  pacf(res,na.action=na.omit)
    dev.off()

#Let's see how good this plot is of the hourly trend?
t.o.forecast<-paste("Prediction starts at: ", date(),sep="")
png(filename="~/Button Data/5_20_historical.png")
  plot(fitted(reg), main="Past Values of Button", xlab="Time (in 10 minute increments)", ylab="Lowest Button Time in 10 minute Interval)", ylim=c(0,60))
    mtext(paste(t.o.forecast),side=1,line=4)
      dev.off()
png(filename="~/Button Data/5_20_error.png")
  plot(res, main="Error of Forecast",,xlab="Time (in 10 minute increments)", ylab="Error of Forecast Technique on Past Data")
    mtext(paste(t.o.forecast),side=1,line=4)
      dev.off()
png(filename="~/Button Data/5_20_overlay.png")
  plot(fitted(reg), main="Past Values of Button overlayed with Forecast",xlab="Time (in 10 minute increments)", ylab="Lowest Button Time in 10 minute Interval", ylim=c(0,60))
    mtext(paste(t.o.forecast),side=1,line=4)
      lines(as.vector(button_xts),col="red")
        dev.off()

#forecast value of button:
  #size of forecast
  w<-2 #weeks of repetition of our last week.
    n<-7*24*60/t
    viable<-(dim(regressors)[1]-n):dim(regressors)[1] #gets the last week.
    #regressors$missing_data<-median(regressors$missing_data) #but we don't want to assume a bunch of unneeded missing data. 
  forecast_values<-forecast(reg,xreg=regressors[rep(viable,w),],level=75)
  start<-index(button_xts)[1]

  f_cast<-append(forecast_values$x,forecast_values$mean)
  a=as.Date(seq(start, by="15 min",length.out=length(f_cast)))

png(filename="~/Button Data/5_20_forecast.png")
  plot(forecast_values,ylim=c(0,60), main="Lowest Button Time In Every 10 minute Period", ylab="10 minute Minimum of Button", xlab="Number of 10 minute Periods Since Button Creation")
    footnotes<-paste("Timer Death in about 4 weeks. Prediction starts at ", date(),". 75% CI in Grey.",sep="")
    mtext(paste(footnotes),side=1,line=4)
  dev.off()

