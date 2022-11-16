#' Univariate Time-Series Analysis
#' Data: AirPassengers (from R corpus): preformatted as time-series data
#' Note: Univariate analysis is used mostly for forecasting, where the goal 
#' is prediction and not explanation. For explanation, we need multivariate
#' regression analysis, which we can combine with trend, seasonality, and lag. 

data(AirPassengers)
ts <- AirPassengers
ts
View(ts)
str(ts)

# Explore data
start(ts)
end(ts)
class(ts)
frequency(ts)
cycle(ts)

# Visualize data
plot(ts, xlab="Month/Year", ylab="Passenger Count (1000's)", main="Air Passenger Counts")
abline(lm(ts ~ time(ts)), col="red")  # Linear trend plot

boxplot(ts ~ cycle(ts))               # Seasonality plot
# Question: What inferences can you draw from this boxplot?

# Identification: Identify d in I(d)
plot(log(ts))                      # Log takes care of unequal variance
plot(diff(log(ts)))                # Differentiating makes the mean zero; hence d=1
plot(diff(diff(log(ts))))          # What happens with d=2

# install.packages("tseries")
library(tseries)
adf.test(diff(log(ts)))            # Augmented Dickey-Fuller Test, 
                                   # H0: Time series is not stationary

# Question: Is the time series stationary? What inferences do we get from the above test?

# Estimation: Estimate p and q for AR(p) and MA(q)

acf(ts)                            # Correlation between Y(t) and lags Y(t-p)
# Dashed blue line indicates 95% C.I. for stationary series; 
# our goal is to bring the ACFs within the blue lines

acf(diff(log(ts)))                 # p = 2 for AR(p)
pacf(diff(log(ts)))                # q = 1 for MA(q)

# ARIMA model
model <- arima(log(ts), c(2,1,1), seasonal=list(order=c(2,1,1), period=12))  # c(p,d,q)
model

forecasted <- predict(model, n.ahead=5*12)
forecasted
forecasted <- exp(forecasted$pred)
forecasted <- round(forecasted,0)
forecasted

ts.plot(ts, forecasted, lty=c(1,3))

# Cross-validation using training data Year 1949-1958, and test data Year 1959-1960
train <- ts(ts, frequency=12, start=c(1949,1), end=c(1958,12))
test  <- tail(ts, 24)
model <- arima(log(train), c(2,1,1), seasonal=list(order=c(2,1,1), period=12))
forecasted <- predict(model, n.ahead=2*12)
forecasted <- exp(forecasted$pred)
forecasted <- round(forecasted,0)
forecasted
test
test - forecasted

n <- length(test - forecasted)
RMSE1 <- sqrt(sum((test - forecasted)^2)/n)
RMSE1                                        # RMSE1=22

# Using auto.arima() function in forecast package
# install.packages("forecast")
library(forecast)
model2 <- auto.arima(ts)
model2
forecasted2 <- forecast(model2, level=c(95), h=5*12)  # c is 95% CI and h is forecast horizon
forecasted2 <- round(forecasted2$mean,0)
forecasted2
ts.plot(ts, forecasted2, lty=c(1,3))

model2 <- auto.arima(train)
forecasted2 <- predict(model2, n.ahead=2*12)
forecasted2 <- round(forecasted2$pred,0)
forecasted2
test
test - forecasted2

RMSE2 <- sqrt(sum((test - forecasted2)^2)/n)
RMSE2                                       # RMSE2=48

# Given its higher RMSE, auto-arima is clearly inferior to the arima function
