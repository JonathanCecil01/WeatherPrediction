suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(gmodels))
suppressPackageStartupMessages(library(lattice))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(gridExtra))
library(glue)
library(httr)
library(jsonlite)
suppressPackageStartupMessages(library(ROCR))
suppressPackageStartupMessages(library(corrplot))

res = GET(glue("https://api.weatherbit.io/v2.0/history/daily?city=Coimbatore&start_date=2021-{1}-{1}&end_date=2021-{1}-{2}&key=d45464d6642d42709a35b18503b961a3"))
data = fromJSON(rawToChar(res$content))
weather_mydata<-as.data.frame(data)
View(weather_mydata)
#obtaining data
l<-seq(1,28,7)
for (i in c(1:9))
  for (j in l){
    
    res = GET(glue("https://api.weatherbit.io/v2.0/history/daily?city=Coimbatore&start_date=2021-{i}-{j}&end_date=2021-{i}-{j+7}&key=d45464d6642d42709a35b18503b961a3"))
    data1 <-fromJSON(rawToChar(res$content))
    temp_frame<-as.data.frame(data1)
    weather_mydata<-rbind(weather_mydata, temp_frame)
  }
weather_mydata2<- subset(weather_mydata, select = -c(timezone,state_code, country_code, lat, lon, city_name, station_id,data.precip_gpm, data.datetime, sources, city_id))
(n <- nrow(weather_mydata))
#removing columns with na
(cols_withNa <- apply(weather_mydata2, 2, function(x) sum(is.na(x))))
cols_withNa
weather_mydata3 <- subset(weather_mydata2, select = -c(data.snow_depth, data.snow))
factor_vars <- names(which(sapply(weather_mydata3, class) == "factor"))
numeric_vars <- setdiff(colnames(weather_mydata3), factor_vars)
numeric_vars <- setdiff(numeric_vars, "data.precip")
chisq_test_res <- lapply(numeric_vars, function(x) { 
  chisq.test(weather_mydata3[,x], weather_mydata3[, "data.precip"], simulate.p.value = TRUE)
})
names(chisq_test_res) <- numeric_vars
chisq_test_res
  numeric_vars_mat <- as.matrix(weather_mydata3[, numeric_vars, drop=FALSE])
numeric_vars_cor <- cor(numeric_vars_mat)
#correlation plot
corrplot(numeric_vars_cor)
#Pair Plot
pairs(weather_mydata3[,numeric_vars], col=weather_mydata3$data.precip)

#Modeling 
weather_mydata6 <- subset(weather_mydata, select = -c(timezone,state_code, country_code, lat, lon, city_name, station_id,data.precip_gpm, data.datetime, sources, city_id,data.snow_depth, data.snow))
weather_mydata6$HumidityTomorrow <- c(weather_mydata6$data.rh[2:nrow(weather_mydata6)], NA)
weather_mydata6$WindGustSpeedTomorrow <- c(weather_mydata6$data.wind_gust_spd[2:nrow(weather_mydata6)], NA)
weather_mydata6$SunshineTomorrow <- c(weather_mydata6$data.solar_rad[2:nrow(weather_mydata6)], NA)
weather_mydata6$MinTempTomorrow <- c(weather_mydata6$data.min_temp[2:nrow(weather_mydata6)], NA)
weather_mydata6$MaxTempTomorrow <- c(weather_mydata6$data.max_temp[2:nrow(weather_mydata6)], NA)


today_record<-weather_mydata[258, ]
#Sunshine 
sun_fit <- lm(SunshineTomorrow ~ data.solar_rad*data.rh + data.clouds  + data.wind_gust_spd - 1, data = weather_mydata6)
summary(sun_fit)
#Cloud
cloud_fit <- lm(data.clouds ~ data.solar_rad, data = weather_mydata6)
#Humidity
h_fit <- lm(HumidityTomorrow ~ data.rh + data.solar_rad, data = weather_mydata6)

#MIN TEMPERATURE 
minTemp_fit <- lm(MinTempTomorrow ~ data.min_temp+data.temp+data.dewpt , data = weather_mydata6)
minTemp_pred <- round(predict(minTemp_fit, today_record), 1)
#MAX TEMPERTURE
maxTemp_fit <- lm(MaxTempTomorrow ~ data.max_temp+data.temp+data.dewpt+data.max_wind_spd_ts , data = weather_mydata6)
summary(maxTemp_fit)
#lm_pred <- predict(minTemp_fit, weather_mydata6)
#plot(x = weather_mydata6$Sunshine, y = weather_data7$MinTemp, type='p', xlab = "Sunshine", ylab = "MinTemp")
#legend("topright", c("actual", "fitted"), fill = c("black", "red"))
#points(x = weather_data7$Sunshine, y = lm_pred, col='red')
maxTemp_pred <- round(predict(maxTemp_fit, today_record), 1)


#Cloud Prediction

computeCloudConditions = function(data.clouds) {
  cloud_avg = min(round((data.clouds), 100))
  cc_str = NULL
  if (cloud_avg == 100) {
    cc_str = "Cloudy"
  } else if (cloud_avg >= 60) {
    cc_str = "Mostly Cloudy"
  } else if (cloud_avg >= 30) {
    cc_str = "Partly Cloudy"
  } else if (cloud_avg >= 10) {
    cc_str = "Mostly Sunny"
  } else if (cloud_avg < 10) {
    cc_str = "Sunny"
  }
  cc_str
}
weather_report <- function(today_record) {
  #Sunshine 
  sun_fit <- lm(SunshineTomorrow ~ data.solar_rad*data.rh + data.clouds  + data.wind_gust_spd - 1, data = weather_mydata6)
  #Cloud
  cloud_fit <- lm(data.clouds ~ data.solar_rad, data = weather_mydata6)
  #Humidity
  h_fit <- lm(HumidityTomorrow ~ data.rh + data.solar_rad, data = weather_mydata6)
  
  #MIN TEMPERATURE 
  minTemp_fit <- lm(MinTempTomorrow ~ data.min_temp+data.temp+data.dewpt , data = weather_mydata6)
  minTemp_pred <- round(predict(minTemp_fit, today_record), 1)
  #MAX TEMPERTURE
  maxTemp_fit <- lm(MaxTempTomorrow ~ data.max_temp+data.temp+data.dewpt+data.max_wind_spd_ts , data = weather_mydata6)
  #lm_pred <- predict(minTemp_fit, weather_mydata6)
  #plot(x = weather_mydata6$Sunshine, y = weather_data7$MinTemp, type='p', xlab = "Sunshine", ylab = "MinTemp")
  #legend("topright", c("actual", "fitted"), fill = c("black", "red"))
  #points(x = weather_data7$Sunshine, y = lm_pred, col='red')
  maxTemp_pred <- round(predict(maxTemp_fit, today_record), 1)
  

  # Humidity3pm prediction
  h_pred <- round(predict(h_fit, today_record), 1)
  
  # sunshine prediction is used to fit Cloud9am and Cloud3pm
  sun_pred <- predict(sun_fit, today_record)
  
  cloud_pred <- min(round(predict(cloud_fit, today_record)), 100)
  # a descriptive cloud conditions string is computed
  CloudConditions_pred <- computeCloudConditions(cloud_pred)
  
  # MinTemp prediction
  minTemp_pred <- round(predict(minTemp_fit, today_record), 1)
  
  # MaxTemp prediction
  maxTemp_pred <- round(predict(maxTemp_fit, today_record), 1)
  
  h_pred_str <- paste(h_pred, "%", sep = "")
  minTemp_pred_str <- paste(minTemp_pred, "°C", sep= "")
  maxTemp_pred_str <- paste(maxTemp_pred, "°C", sep= "")
  
  report <- data.frame( 
                       Humidity = h_pred_str,
                       CloudConditions = CloudConditions_pred,
                       MinTemp = minTemp_pred_str,
                       MaxTemp = maxTemp_pred_str
                       )
  report
}
weather_report(today_record)
for(i in 1:6)
{
  today_record<-weather_mydata[200+i, ]
  print(weather_report(today_record))
}

