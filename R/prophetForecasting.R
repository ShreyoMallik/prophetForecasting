## cheking if necesary packages are installed (and installing if not)
list.of.packages <- c("prophet")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("prophet")
#library(dplyr)



prophetForecasting <- function(input){
  #input can either be csv file or data
  df <- if(is.character(input) && file.exists(input)){
    read.csv(input)
  } else {
    as.data.frame(input)
  }

df = aggregate(df$y, by = list(df$ds, df$dow, df$holiday), mean)

names(df) = c("ds","dow","holiday","y")
df$ds = as.Date(df$ds)
df = df[order(df$ds),]

holidaysdf = read.table("data/holidays.csv", head=T, sep="\t")
names(holidaysdf) = c("ds", "holiday")
holidaysdf$ds = as.Date(holidaysdf$ds)

df2 = df[df$dow!=1 & df$dow!=7,]


m <- prophet(df2,yearly.seasonality=TRUE, holidays =holidaysdf)

future <- make_future_dataframe(m, periods = 7)
tail(future)

forecast <- predict(m, future)
#tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
#plot(m, forecast)
fc = forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')]


pred = fc[(nrow(fc)-6):nrow(fc),]

pred$dow = lubridate::wday(pred$ds)


df3 = df[df$dow==7 | df$dow==1,]

weekendsToPred = pred$ds[which(pred$dow %in% c(1,7))]

f.weekendPred = function(df3,x){
  dfReturn = data.frame(ds=NA,yhat=NA, yhat_lower=NA, yhat_upper=NA)
  for(i in 1:length(x)){
    prev = c(as.Date(x[i])-7,as.Date(x[i])-14)
    val = c(df3$y[df3$ds %in% prev])
    meaN = mean(val)
    sD <- sd(val)
    n <- 2
    error <- qnorm(0.975)*sD/sqrt(n)
    left <- meaN-error
    right <- meaN+error
    dfReturn[i,] = c(as.Date(x[i]),meaN, left, right)

  }
  return(dfReturn)
}

predW= f.weekendPred(df3,weekendsToPred)

pred[which(pred$dow %in% c(1,7)),2:4] = predW[2:4]

return(pred)
}


# input = read.table("input_data.csv", sep=";", head=T, dec=".")
# prophetForecasting(input)
