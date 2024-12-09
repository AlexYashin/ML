library(fpp3)
library(tibble)
library(ggplot2)
library(TTR)
library(urca)
library(tseries)
library(forecast)
library(dplyr)
library(zoo)

#преобразуем данные
data=read.csv("CourseProject/rice_beef_coffee_price_changes.csv")
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
data$MonthNum <- match(data$Month, months)
data$Date <- yearmonth(paste(data$Year, data$MonthNum, sep = "-"))
data <- as_tsibble(data, index = Date)

#избавимся от пропусков
#так как у нас есть цены фактические
#и можно посмотреть коэфициент инфляции на тот временной интервал с сайта  https://smartasset.com/investing/inflation-calculator
#то можно найти цены с поправкой на инфляцию, на 2006 год коэфициент инфляции составил 31.96

data[data$Year==2006,]$Inflation_rate=31.96
data$Price_rice_infl[168:179]=data$Price_rice_kilo[168:179]*(1+31.96/100)
data$Price_beef_infl[168:179]=data$Price_beef_kilo[168:179]*(1+31.96/100)
data$Price_coffee_infl[168:179]=data$Price_coffee_kilo[168:179]*(1+31.96/100)

data[data$Year==2022,]$Inflation_rate=0

data$Price_rice_infl[360]=data$Price_rice_kilo[360]
data$Price_beef_infl[360]=data$Price_beef_kilo[360]
data$Price_coffee_infl[360]=data$Price_coffee_kilo[360]

#визуализируем данные
gg_tsdisplay(data,y=Price_beef_infl)
gg_tsdisplay(data,y=Price_rice_infl)
gg_tsdisplay(data,y=Price_coffee_infl)
#на графиках acf корреляция на лагах 12 и 24 зависимость выходит
#за коридор значимости, это означает наличие сезонности
#и тренда в данных и объясняет почему это не просто шум
#что видно и на каждом из графиков
ggplot(data,aes(x=Date))+
  geom_line(aes(y=Price_rice_infl,color="Rice"))+
  geom_line(aes(y=Price_beef_infl,color="Beef"))+
  geom_line(aes(y=Price_coffee_infl,color="Coffe"))


#обычная разность - более лучший результат приведения к стационарности
# чем сезонное дифференцирование
data$diff_coffe=c(0,diff(data$Price_coffee_infl))
data$diff_beef=c(0,diff(data$Price_beef_infl))
data$diff_rice=c(0,diff(data$Price_rice_infl))


gg_tsdisplay(data,y=diff_beef)
gg_tsdisplay(data,y=diff_rice)
gg_tsdisplay(data,y=diff_coffe)

ggplot(data,aes(x=Date))+
  geom_line(aes(y=diff_rice,color="Rice"))+
  geom_line(aes(y=diff_beef,color="Beef"))+
  geom_line(aes(y=diff_coffe,color="Coffe"))

#сезонная разность - худшее приведение к стационарности
# чем простое сезонное дифференцирование
zero=c(0,0,0,0,0,0,0,0,0,0,0,0)

data$diff_coffe_s=c(zero,diff(data$Price_coffee_infl,12))
data$diff_beef_s=c(zero,diff(data$Price_beef_infl,12))
data$diff_rice_s=c(zero,diff(data$Price_rice_infl,12))

gg_tsdisplay(data,y=diff_beef_s)
gg_tsdisplay(data,y=diff_rice_s)
gg_tsdisplay(data,y=diff_coffe_s)

ggplot(data,aes(x=Date))+
  geom_line(aes(y=diff_rice_s,color="Rice"))+
  geom_line(aes(y=diff_beef_s,color="Beef"))+
  geom_line(aes(y=diff_coffe_s,color="Coffe"))



#комбинированное дифференцирование
data$comb_diff_coffee=c(zero,diff(data$diff_coffe,12))
data$comb_diff_beef=c(zero,diff(data$diff_beef,12))
data$comb_diff_rice=c(zero,diff(data$diff_rice,12))

gg_tsdisplay(data,y=comb_diff_beef)
gg_tsdisplay(data,y=comb_diff_rice)
gg_tsdisplay(data,y=comb_diff_coffee)

ggplot(data,aes(x=Date))+
  geom_line(aes(y=comb_diff_rice,color="Rice"))+
  geom_line(aes(y=comb_diff_beef,color="Beef"))+
  geom_line(aes(y=comb_diff_coffee,color="Coffe"))
#комбинированное дифференцирование делает ряды больее стационарными,
#чем предыдущие


#прогнозирование
data_train=data[1:357,]
data_test=data[358:360,]
#Наивная модель
models=model(data_train,
             naive_m=NAIVE(Price_beef_infl),
             ets_m=ETS(Price_beef_infl),
             arima_m=ARIMA(Price_beef_infl),
             theta_m=THETA(Price_beef_infl)
        )

forecast_model=forecast(models,h=3)

accuracy(forecast_model,data_test)
mean(data$Price_beef_infl)
autoplot(forecast_model,data_test,level=NULL)

#ETS




#ARIMA



#случайный лес





#тетта



#усреднение моделей лидеров

