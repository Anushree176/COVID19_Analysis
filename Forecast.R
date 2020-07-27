covid <- as.data.frame(read.csv("covid_19_india.csv"))
covid <- subset(covid, select = -c(1,3,5,6))
covid = covid[!covid$State.UnionTerritory == "Cases being reassigned to states", ]
covid = covid[!covid$State.UnionTerritory == "Unassigned", ]
covid <- covid[complete.cases(covid), ]
covid$Date <- as.Date(covid$Date, format = "%d/%m/%y")
covid <- covid[with(covid, order(covid$Date)),]
covid <- covid[,-2]

sum_covid <- plyr::ddply(covid, "covid$Date", numcolwise(sum))
sum_covid <- as.data.frame(sum_covid)
sum_covid <- cbind(sum_covid, sum_covid$Confirmed-(sum_covid$Deaths+sum_covid$Cured))
colnames(sum_covid) <- c("Dates","Cured", "Deaths", "Confirmed", "Active")
sum_covid <- sum_covid[,c(1,4,2,5,3)]
all_plot <- plot_ly(sum_covid, x = sum_covid$Dates, mode = "lines")
all_plot <- all_plot %>% add_trace(y = sum_covid$Confirmed, name = "Confirmed")
all_plot <- all_plot %>% add_trace(y = sum_covid$Cured, name = "Cured")
all_plot <- all_plot %>% add_trace(y = sum_covid$Active, name = "Active")
all_plot <- all_plot %>% add_trace(y = sum_covid$Deaths, name = "Deaths")
all_plot

#############################################################################################

covid_recent <- as.data.frame(read.csv("covid_19_india_new.csv"))
covid_recent <- subset(covid_recent, select = -c(1,3,5,6))
covid_recent = covid_recent[!covid_recent$State.UnionTerritory == "Cases being reassigned to states", ]
covid_recent = covid_recent[!covid_recent$State.UnionTerritory == "Unassigned", ]
covid_recent <- covid_recent[complete.cases(covid_recent), ]
covid_recent$Date <- as.Date(covid_recent$Date, format = "%d/%m/%y")
covid_recent <- covid_recent[with(covid_recent, order(covid_recent$Date)),]
covid_recent <- covid_recent[,-2]

sum_covid_recent <- plyr::ddply(covid_recent, "covid_recent$Date", numcolwise(sum))
sum_covid_recent <- as.data.frame(sum_covid_recent)
sum_covid_recent <- cbind(sum_covid_recent, sum_covid_recent$Confirmed-(sum_covid_recent$Deaths+sum_covid_recent$Cured))
colnames(sum_covid_recent) <- c("Dates","Cured", "Deaths", "Confirmed", "Active")
sum_covid_recent <- sum_covid_recent[,c(1,4,2,5,3)]
all_plot_recent <- plot_ly(sum_covid_recent, x = sum_covid_recent$Dates, mode = "lines")
all_plot_recent <- all_plot_recent %>% add_trace(y = sum_covid_recent$Confirmed, name = "Confirmed")
all_plot_recent <- all_plot_recent %>% add_trace(y = sum_covid_recent$Cured, name = "Cured")
all_plot_recent <- all_plot_recent %>% add_trace(y = sum_covid_recent$Active, name = "Active")
all_plot_recent <- all_plot_recent %>% add_trace(y = sum_covid_recent$Deaths, name = "Deaths")
all_plot_recent

##############################################################################################

library(TTR)
library(forecast)
library(tseries)
#Converting to time series
ts_conf <- cbind(ts(sum_covid[,2], frequency=2))

#ts_conf <- cbind(ts(sum_covid[,2]))
?mstl
mstl(ts_conf)

######plot(mstl(ts_conf))

plot(ts_conf)
ts_conf
dd <- decompose(ts_conf)
plot(dd)
ts_conf1 <- tsclean(ts_conf)

#Augmented Dickey Fuller Test
adf.test(ts_conf)

#Simple Moving Avg
sma.model <- SMA(ts_conf, n=50)
sma.model
sma.forecast <- forecast(sma.model,14)
sma.forecast
plot(sma.forecast)

#Weighted Moving Avg  
wma.model <- WMA(ts_conf, n=30, wts = 1:30)
wma.froecast <- predict(wma.model,14)
plot(wma.froecast)
wma.froecast

#Exponential Moving Avg
ema.model <- EMA(ts_conf, n = 30, wts = 1:30)
ema.model
ema.forecast <- forecast(ema.model, 14)
plot(ema.forecast)
ema.forecast

#ARIMA MODEL
?auto.arima
arima.model <- auto.arima(ts_conf)
arima.model$arma
summary(arima.model)
##RMSE
qqnorm(arima.model$residuals)
qqline(arima.model$residuals)
confirmed.forecast <- forecast(arima.model, 14)
plot(confirmed.forecast)
confirmed.forecast

