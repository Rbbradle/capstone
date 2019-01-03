#Data Story

library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(GGally)
library(scatterplot3d)
library(ggpmisc)

bike.share <- read.csv("C:/Users/rbbra/Documents/R/Capstone/DC-Bike-Share-Data/Bike-Sharing-Dataset/day.csv")
hourly <- read.csv("C:/Users/rbbra/Documents/R/Capstone/DC-Bike-Share-Data/Bike-Sharing-Dataset/hour.csv") ## Import dataset
names(bike.share) ##names of the columns

dteday <- as.Date(bike.share$dteday)
weathersit <- factor(bike.share$weathersit)
yr <- factor(bike.share$yr)
mnth <- factor(bike.share$mnth)
season <- factor(bike.share$season)
holiday <- factor(bike.share$holiday)
weekday <- factor(bike.share$weekday)
workday <- factor(bike.share$workday)
cnt <- bike.share$cnt
temp <- bike.share$temp
windspd <- bike.share$windspd
hum <- bike.share$hum


#Wrangling


#Updating Season Columns
as.character(bike.share$season,stringsAsFactors=FALSE)

winter.vec <- bike.share$season =="1"
bike.share$season[winter.vec] <- "Winter"

spring.vec <- bike.share$season =="2"
bike.share$season[spring.vec] <- "Spring"

summer.vec <- bike.share$season =="3"
bike.share$season[summer.vec] <- "Summer"

fall.vec<- bike.share$season =="4"
bike.share$season[fall.vec] <- "Fall"


#Updating temp column to degrees in Fahrenheit
temp.form <- function(x){
  output <- ((x*47)-8)*(9/5)+32
  return(output)
}

bike.share$temp <- temp.form(x = bike.share$temp)



#Updating hum column to percentages
temp.hum <- function(x){
  output <- (x*100)
  return(output)
}

bike.share$hum <- temp.hum(x = bike.share$hum)

#Research questions

#Average number of casual and registered users

avgcas <- mean(bike.share$casual)
avgreg <- mean(bike.share$reg)

avgcas
avgreg



#Visualiations

#Total Riders by humidity.
#shows slight negative trend. High temperature is above the line, cool is below.

ggplot(bike.share, aes(x=hum, y=cnt, col = temp))+
  geom_point()+
  labs(x="Humidity (%)", y = "Total Riders")+
  scale_color_gradientn(colors = brewer.pal(9, "YlOrRd"))+
  #facet_grid(weathersit~.)+
  geom_smooth(method = "lm", formula = y ~ x)

#Total Riders by temperature, following a lm function of the data. 
#Slight trend noticed that very high T is below the trend line
# y = -167 + 78.5x

ggplot(bike.share, aes(x=bike.share$temp, y=cnt, col = hum))+
  geom_point()+
  labs(x="Temp (F)", y = "Total Riders")+
  #facet_grid(weathersit~.)+
  geom_smooth(method = "lm", formula = y~x)+
  stat_poly_eq(parse =T, aes(label= ..eq.label..), formula = y~x)

#Same as above but with a cubic function
# y = 1460 -153x + 6.51x^2 - .0492x^3
formula <- y ~ poly(x,3, raw = TRUE)

ggplot(bike.share, aes(x=temp, y=cnt, col = weathersit))+
  geom_point(aes(color = factor(weathersit)))+
  labs(x="Temperature (F)", y = "Total Riders")+
  #scale_color_gradientn(colors = brewer.pal(9, "YlOrRd"))+
  #facet_grid(weathersit~.)+
  geom_smooth(method = "glm", formula = formula)+
  stat_poly_eq(parse =T, aes(label= ..eq.label..), formula = formula)

#windspeed verse count
ggplot(bike.share, aes(x=bike.share$windspd, y = bike.share$cnt))+
  geom_point()+
  geom_smooth(method = "lm", formula = y~x)+
  #scale_color_gradientn(colors = brewer.pal(9, "YlOrRd"))+
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Linear Model Fitted to Data")


#Total Riders by Humidity colored by temperature
# y = 5510 - 16x

ggplot(bike.share, aes(x=hum , y=cnt, col = temp))+
  geom_point()+
  labs(x="Humidity (%)", y = "Total Riders")+
  scale_color_gradientn(colors = brewer.pal(9, "YlOrRd"))+
  #facet_grid(weathersit~.)+
  geom_smooth(method = "lm", formula = y~x)+
  stat_poly_eq(parse =T, aes(label= ..eq.label..), formula = y~x)+
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Linear Model Fitted to Data")


##Analysis

#split data by days of the month to create test and train sets
train <- bike.share[as.integer(substr(bike.share$dteday, 9,10)) < 24, ]
test <- bike.share[as.integer(substr(bike.share$dteday, 9,10)) > 23,]

nrow(train)/ nrow(bike.share) #75%
nrow(test)/ nrow(bike.share) #25%


#first model will include all variables
fit_1 <- lm(cnt ~ season + yr + mnth + holiday + weekday + 
             workday + weathersit + temp + hum + windspd, data= train)
summary(fit_1)  #month, holiday, workday: insignificant



tst.cnt <- predict(fit_1, test)
test$tst.cnt <- tst.cnt
tst.MSE <- sum((test$cnt - test$tst.cnt)^2)/nrow(test) ##MSE Formula
tst.MSE
tst.result = ggplot(test,aes(cnt,tst.cnt))+geom_point()
tst.result
test$tst.cnt[test$tst.cnt < 0] = 0
tst.MSE <- sum((test$cnt - test$tst.cnt)^2)/nrow(test)
tst.MSE  ##1,043,384
tst.result <- ggplot(test,aes(cnt,tst.cnt))+geom_point()
tst.result ##1,043,262

#reduce model by what was found insignificant in the last model
fit_2 <- lm(cnt ~ season + yr + weathersit + workday + temp + hum + windspd, train)
summary(fit_2)  #workday: insignificant


tst.cnt <- predict(fit_2, test)
test$tst.cnt <- tst.cnt
tst.MSE <- sum((test$cnt - test$tst.cnt)^2)/nrow(test) ##MSE Formula
tst.MSE
tst.result = ggplot(test,aes(cnt,tst.cnt))+geom_point()
tst.result
test$tst.cnt[test$tst.cnt < 0] = 0
tst.MSE <- sum((test$cnt - test$tst.cnt)^2)/nrow(test)
tst.MSE  ##1,013,558
tst.result <- ggplot(test,aes(cnt,tst.cnt))+geom_point()
tst.result ##1,013,558


#reduce model by what was found insignificant in the last model
fit_3 <- lm(cnt ~ season + yr + weathersit + temp + hum + windspd, train)
summary(fit_3)  #all significant


tst.cnt <- predict(fit_3, test)
test$tst.cnt <- tst.cnt
tst.MSE <- sum((test$cnt - test$tst.cnt)^2)/nrow(test) ##MSE Formula
tst.MSE
tst.result = ggplot(test,aes(cnt,tst.cnt))+geom_point()
tst.result
test$tst.cnt[test$tst.cnt < 0] = 0
tst.MSE <- sum((test$cnt - test$tst.cnt)^2)/nrow(test)
tst.MSE  ##1,029,739
tst.result <- ggplot(test,aes(cnt,tst.cnt))+geom_point()
tst.result ##1,029,739

#3rd test has a higher MSE therefor we believe test #2 has the best relationship.