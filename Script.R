library(quantmod)
library(ggplot2)
library(forecast)
library(tseries)
library(tsfknn)
setwd("E:/679final")

fossil_con<-read.csv("Fossil energy consumption.csv")
fossil_con<-fossil_con[-c(1,2),]
renewable_con<-read.csv("Renewable energy consumption.csv")
renewable_con<-renewable_con[-c(1,2),]

# Convert data to time series
ts_data1 <- ts(fossil_con$coal, frequency = 1, start = 1970,end = 2020)
ts_data2 <- ts(fossil_con$Natural.Gas, frequency = 1, start = 1970,end = 2020)
ts_data3 <- ts(fossil_con$HGL, frequency = 1, start = 1970,end = 2020)
ts_data4 <- ts(fossil_con$Jet.Fuel, frequency = 1, start = 1970,end = 2020)
ts_data5 <- ts(fossil_con$Motor.Gasoline, frequency = 1, start = 1970,end = 2020)
ts_data6 <- ts(fossil_con$Residual.Fuel.Oil, frequency = 1, start = 1970,end = 2020)
ts_data7 <- ts(fossil_con$Other, frequency = 1, start = 1970,end = 2020)
ts_data8 <- ts(fossil_con$Total, frequency = 1, start = 1970,end = 2020)

# Combine time series into a data frame
fossil_con <- data.frame(Date = time(ts_data1),
                         coal = ts_data1,
                         Natural.Gas = ts_data2,
                         HGL = ts_data3,
                         Jet.Fuel = ts_data4,
                         Motor.Gasoline = ts_data5,
                         Residual.Fuel.Oil = ts_data6,
                         Other = ts_data7,
                         Total = ts_data8)

# Plot the data using ggplot2
ggplot(fossil_con, aes(x = Date)) +
  geom_line(aes(y = coal, color = "coal")) +
  geom_line(aes(y = Natural.Gas, color = "Natural.Gas")) +
  geom_line(aes(y = HGL, color = "HGL")) +
  geom_line(aes(y = Jet.Fuel, color = "Jet.Fuel")) +
  geom_line(aes(y = Motor.Gasoline, color = "Motor.Gasoline")) +
  geom_line(aes(y = Residual.Fuel.Oil, color = "Residual.Fuel.Oil")) +
  geom_line(aes(y = Other, color = "Other")) +
  geom_line(aes(y = Total, color = "Total")) +
  labs(x = "Date", y = "Consumption (Trillion Btu)", color = "Series", title = "Fossil Energy") +
  scale_color_manual(values = c("coal" = "#3399FF", 
                                "Natural.Gas" = "#99FF00", 
                                "HGL" = "#99CCFF",
                                "Jet.Fuel" = "#FFCCFF",
                                "Motor.Gasoline" = "#FFCC00",
                                "Residual.Fuel.Oil" = "#CC0000",
                                "Other" = "#9900FF",
                                "Total" = "#FF0000"))

print(adf.test(fossil_con$Total))

print(adf.test(diff(fossil_con$Total)))

par(mfrow = c(1, 2))
acf(fossil_con$Total)
pacf(fossil_con$Total)

modelfit_1 <-auto.arima(fossil_con$Total, lambda = "auto")
summary(modelfit_1)
par(mfrow = c(1, 1))
plot(resid(modelfit_1),type = "p",ylab="Residuals",main="Residuals(Arima(0,1,0)) vs. Time")

tsdiag(modelfit_1)

Box.test(modelfit_1$residuals, type="Ljung-Box")

par(mfrow = c(1, 1))
plot(forecast(modelfit_1,h=30))

#repeat for renewable energy

# Convert data to time series
ts_data1 <- ts(renewable_con$Hydro.electric.Power, frequency = 1, start = 1970)
ts_data2 <- ts(renewable_con$Biomass, frequency = 1, start = 1970)
ts_data3 <- ts(renewable_con$Geo.thermal, frequency = 1, start = 1970)
ts_data4 <- ts(renewable_con$Solar, frequency = 1, start = 1970)
ts_data5 <- ts(renewable_con$Wind, frequency = 1, start = 1970)
ts_data6 <- ts(renewable_con$Total, frequency = 1, start = 1970)

# Combine time series into a data frame
renewable_con <- data.frame(Date = time(ts_data1),
                         Hydro.electric.Power = ts_data1,
                         Biomass = ts_data2,
                         Geo.thermal = ts_data3,
                         Solar = ts_data4,
                         Wind = ts_data5,
                         Total = ts_data6)

ggplot(renewable_con, aes(x = Date)) +
  geom_line(aes(y = Hydro.electric.Power, color = "Hydro.electric.Power")) +
  geom_line(aes(y = Biomass, color = "Biomass")) +
  geom_line(aes(y = Geo.thermal, color = "Geo.thermal")) +
  geom_line(aes(y = Solar, color = "Solar")) +
  geom_line(aes(y = Wind, color = "Wind")) +
  geom_line(aes(y = Total, color = "Total")) +
  labs(x = "Date", y = "Consumption (Trillion Btu)", color = "Series", title = "Renewable Energy") +
  scale_color_manual(values = c("Hydro.electric.Power" = "#3399FF", 
                                "Biomass" = "#99FF00", 
                                "Geo.thermal" = "#99CCFF",
                                "Solar" = "green",
                                "Wind" = "#FFCC00",
                                "Total" = "#FF0000"))
print(adf.test(renewable_con$Total))

print(adf.test(diff(renewable_con$Total)))



par(mfrow = c(1, 2))
acf(renewable_con$Total)
pacf(renewable_con$Total)


modelfit_2 <-auto.arima(renewable_con$Total, lambda = "auto")
summary(modelfit_2)
par(mfrow = c(1, 1))
plot(resid(modelfit_2),type="p",ylab="Residuals",main="Residuals(Arima(0,1,0)) vs. Time")

tsdiag(modelfit_2)

Box.test(modelfit_2$residuals, type="Ljung-Box")

par(mfrow = c(1, 2))

plot(forecast(modelfit_1,h=20))
plot(forecast(modelfit_2,h=20))

lambda_1 = BoxCox.lambda(renewable_con$Total)
dnn_fit_1 = nnetar(renewable_con$Total,lambda=lambda_1)
dnn_fit_1
dnn_forecast_1 = forecast(dnn_fit_1,PI=T,h=20)
autoplot(dnn_forecast_1)

ggplot()+
  autolayer(fossil_con$Total,series = "fossil consumption")+
  autolayer(renewable_con$Total,series = "renewable consumption")+
  autolayer(forecast(modelfit_1,h=20),alpha=0.5)+
  autolayer(dnn_forecast_1,alpha=0.5)+
  labs(x = "Date", y = "Consumption (Trillion Btu)", title = "Trend and prediction") 

population<-read.csv("Population.csv")
population<-ts(population$Population,frequency = 1,start = 1990)
ave_fossil_con<-fossil_con$Total[21:51]/population
par(mfrow = c(1, 1))
plot(ave_fossil_con,type = "l",xlab="Year",main="Fossil Fuels Consumption (Trillion Btu per year per capita)")

ave_renewable_con<-renewable_con$Total[21:51]/population
plot(ave_renewable_con,type = "l",xlab="Year",main="Renewable Energy Consumption (Trillion Btu per year per capita)")

ave_total_con<-(renewable_con$Total[21:51]+fossil_con$Total[21:51])/population
plot(ave_total_con,type = "l",xlab="Year",main="Total Energy Consumption (Trillion Btu per year per capita)")

ggplot()+
  autolayer(ts(ave_fossil_con,frequency = 1,start = 1990),series = "Fossil Fuels Consumption")+
  autolayer(ts(ave_renewable_con,frequency = 1,start = 1990),series = "Renewable Energy Consumption")+
  autolayer(ts(ave_total_con,frequency = 1,start = 1990),series = "Total Energy Consumption")+
  
  labs(x = "Date", y = "Consumption", title = "Energy consumption (Unit:Trillion Btu per year per capita)")

#-------------------------------------#

production<-read.csv("production.csv")
production<-production[-c(1:6),]
ts_data1 <- ts(production$coal, frequency = 1, start = 1970)
ts_data2 <- ts(production$Ntural.gas, frequency = 1, start = 1970)
ts_data3 <- ts(production$crude.oil, frequency = 1, start = 1970)
ts_data4 <- ts(production$fossil.total, frequency = 1, start = 1970)
ts_data5 <- ts(production$biofuels, frequency = 1, start = 1970)
ts_data6 <- ts(production$wood.and.waste, frequency = 1, start = 1970)
ts_data7 <- ts(production$other, frequency = 1, start = 1970)
ts_data8 <- ts(production$renewable.total, frequency = 1, start = 1970)

production <- data.frame(Date = time(ts_data1),
                         coal = ts_data1,
                         Ntural.gas = ts_data2,
                         crude.oil = ts_data3,
                         fossil.total = ts_data4,
                         biofuels = ts_data5,
                         wood.and.waste = ts_data6,
                         other = ts_data7,
                         renewable.total = ts_data8)



ggplot(production, aes(x = Date)) +
  geom_line(aes(y = coal, color = "coal")) +
  geom_line(aes(y = Ntural.gas, color = "Ntural.gas")) +
  geom_line(aes(y = crude.oil, color = "crude.oil")) +
  geom_line(aes(y = fossil.total, color = "fossil.total")) +
  geom_line(aes(y = biofuels, color = "biofuels")) +
  geom_line(aes(y = wood.and.waste, color = "wood.and.waste")) +
  geom_line(aes(y = other, color = "other")) +
  geom_line(aes(y = renewable.total, color = "renewable.total")) +
  labs(x = "Date", y = "Production (Trillion Btu)", color = "Series", title = "Fossil & Renewable Energy Production") +
  scale_color_manual(values = c("coal" = "#3399FF", 
                                "Ntural.gas" = "#99FF00", 
                                "crude.oil" = "#99CCFF",
                                "fossil.total" = "black",
                                "biofuels" = "#FFCC00",
                                "wood.and.waste" = "#FFA500",
                                "other" = "#9900FF",
                                "renewable.total" = "#FF0000"))


modelfit_3<-auto.arima(production$fossil.total, lambda = "auto")
modelfit_4<-auto.arima(production$renewable.total, lambda = "auto")
plot(forecast(modelfit_3,h=20))
plot(forecast(modelfit_4,h=20))


lambda_2 = BoxCox.lambda(production$renewable.total)
dnn_fit_2 = nnetar(production$renewable.total,lambda=lambda_2)
dnn_fit_2
dnn_forecast_2 = forecast(dnn_fit_2,PI=T,h=20)
autoplot(dnn_forecast_2)


ggplot()+
  autolayer(production$fossil.total,series = "fossil production")+
  autolayer(production$renewable.total,series = "renewable production")+

  autolayer(forecast(modelfit_3,h=20),alpha=0.5)+
  autolayer(dnn_forecast_2,alpha=0.5)+
  labs(x = "Date", y = "Production (Trillion Btu)", title = "Trend and prediction") 


dif_renewable<-production$renewable.total- renewable_con$Total
par(mfrow = c(1, 1))
plot(dif_renewable)

lambda_3 = BoxCox.lambda(dif_renewable)
dnn_fit_3 = nnetar(dif_renewable,lambda=lambda_3)
dnn_fit_3
dnn_forecast_3 = forecast(dnn_fit_3,PI=T,h=20)
autoplot(dnn_forecast_3)




