economy <- read.csv("/Users/a.bsurekha/Downloads/economy.csv", header = FALSE)
str(economy)
head(economy)
new_colnames <- c("Country_name","Country_code","Year","Indicator_name","Indicator_code",
                  "Value")
colnames(economy) <- new_colnames
head(economy$Year)
economy$Year <- as.character(economy$Year)
str(economy)
economy$Year <- as.numeric(as.character(economy$Year))

economy$Year[is.na(economy$Year)] <- round(mean(economy$Year, na.rm = TRUE))
str(economy)

head(economy$Value)
economy$Value <- as.character(economy$Value)
economy$Value <- as.numeric(as.character(economy$Value))
str(economy)
summary(economy_df)

#replacing year mean values with NA 
economy$Value[is.na(economy$Value)] <- round(mean(economy$Value, na.rm = TRUE))
str(economy)
economy_Value <- economy$Value
economy_Value
#linear modelling
linear_model <- lm(Year ~ Value, data = economy)
linear_model
summary(linear_model)


scatter.smooth(x = economy$Year,
               y = economy$Value,
               xlab="Year",
               ylab="Value",
               main = "Scatter plot Year ~ Value")
abline(reg = lm(economy$Year ~ time(economy$Value)))


cor(economy$Year, economy$Value)
confint(linear_model)

install.packages("e1071")
library(e1071)
par(mfrow = c(1, 2))

plot(density(economy$Value), main = "Density Plot: Value", 
     ylab = "Frequency", 
     sub = paste("Skewness:", 
                 round(e1071::skewness(economy$Value), 2)))
polygon(density(economy$Value), col = "red")

plot(density(economy$Year), main = "Density Plot: Year", 
     ylab = "Frequency", 
     sub = paste("Skewness:", 
                 round(e1071::skewness(economy$Year), 2)))
polygon(density(economy$Year), col = "red")

#timeseries modelling
t_economy <- ts(economy_Value, start=c(2009), end=c(2014), frequency=1)
t_economy
plot(t_economy)
class(t_economy)

#combining the values
cat("Start : ", start(t_economy), "\n")
cat("End : ", end(t_economy), "\n")
cat("Frequency : ", frequency(t_economy), "\n")
print(summary(t_economy))

frequency(t_economy)
cycle(t_economy)
#scatter plot
plot(t_economy,
     xlab="Year",
     ylab="Value",
     main = "Scatter plot")
abline(reg = lm(t_economy ~ time(t_economy)))
plot(aggregate(t_economy,FUN=mean))

par(mfrow = c(1, 2))

#boxplot
boxplot(economy$Year, main = "Year", sub = paste("Outlier rows: ", boxplot.stats(economy$Year)$out))
boxplot(economy$Value, main = "Value", sub = paste("Outlier rows: ", boxplot.stats(economy$Value)$out))

#install.packages("tseries")
#install.packages("forecast")
library(forecast)
library(tseries)
suggested_k <- trunc((length(t_economy)-1)^(1/3))
suggested_k
nsdiffs(t_economy)
adf.test(t_economy)

acf_results <- Acf(t_economy, main = 'acf')
acf_results
pacf_results <- pacf(t_economy, main = "raw")
pacf_results
plot(t_economy, main = "raw")
#arima test
fit <- arima(t_economy, 
             c(1,1,1), seasonal = list(order = c(1,1,1), 
                                      period = 1))
fit
accuracy(fit)

prediction <- predict(fit, n.ahead = 3 * 1)
prediction
#forecasting
forecast_economy <- forecast(fit, level = c(95), h = 36)
forecast_economy
autoplot(forecast_economy)

plot(forecast(forecast_economy, 3), xlab = "Year", ylab = "Value")

auto_arima_model <- auto.arima(t_economy)
auto_arima_model
accuracy(auto_arima_model)
accuracy(fit)

plot(forecast(auto_arima_model, 5), xlab = "Year", ylab = "Values")
plot(forecast(fit, 3 * 1), xlab = "Year", ylab = "Values")
#Q-Q plot
qqnorm(arima_model$residuals)
qqline(arima_model$residuals)

Box.test(auto_arima_model$residuals, type = "Ljung-Box")
#Q-Q plot
qqnorm(fit$residuals)
qqline(fit$residuals)

Box.test(fit$residuals, type = "Ljung-Box")

#training and testing the data
economy_train <- window(x = t_economy, start=c(2000), end=c(2015, 1))
economy_test <- window(x = t_economy, start=c(2010))
economy_train
economy_test


fit <- arima(economy_train, 
             c(1,1,1), 
             seasonal = list(order = c(1,1,1), 
                             period = 1))
fit

auto_arima_model <- auto.arima(economy_train)
auto_arima_model

#predicting values
predict_auto_ARIMA <- forecast(auto_arima_model, 3 * 1)
predict_auto_ARIMA


predict_manual_ARIMA <- forecast(fit, 3 * 1)
predict_manual_ARIMA
