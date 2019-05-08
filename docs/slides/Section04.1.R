library(readr)
library(forecast)
path = "https://jaredsmurray.github.io/sta371g_s19/data/"
beer_prod = scan(paste0(path, 'beerprod.csv'))
beer_prod_series = ts(beer_prod)
plot(beer_prod_series)

ohare = scan(paste0(path, 'weather.csv'))
ohare_series = ts(ohare)
plot(ohare_series)

sales = read_csv(paste0(path, 'sales_ts.csv'))
sales_series = ts(sales$Sales)
plot(sales_series)
points(sales_series)

sales_fit = tslm(sales_series~trend)
print(sales_fit)

# Generate forecasts for the next 10 months, along with 
# 80 and 95% prediction intervals
sales_pred = forecast(sales_fit, h=10)
plot(sales_pred)
print(sales_pred)

# Checking the residuals...
plot(resid(sales_fit))

hotel = read_csv(paste0(path, 'hotelocc.csv'))
hotel_fit_1 = lm(Hotel~Chicago, data=hotel)
print(hotel_fit_1)

# Residual vs fitted and vs covariate plots look OK
plot(fitted(hotel_fit_1), rstandard(hotel_fit_1))
plot(hotel$Chicago, rstandard(hotel_fit_1))

# Residual vs time plot does not!
hotel_resid_ts = ts(rstandard(hotel_fit_1))
plot(hotel_resid_ts, ylab="Std Resid")
points(hotel_resid_ts)
abline(tslm(hotel_resid_ts~trend), col='red')

hotel_ts = ts(hotel)
hotel_fit_2 = tslm(Hotel~Chicago + trend, data=hotel_ts)
coef(hotel_fit_2)
# Check the residuals after adding the time trend...
hotel_resid_ts = ts(rstandard(hotel_fit_2))
plot(hotel_resid_ts, ylab="Std Resid")
points(hotel_resid_ts)
abline(tslm(hotel_resid_ts~trend), col='red')

# Ohare example - examining the relationship between y[t-1] and y[t]
# 
# The stats::lag function will create a lagged version of the time series;
# k = -1 lines up observation t-1 with observation t; k=-h lines up
# observation t-h with observation t
ohare_lag1 = stats::lag(ohare_series, -1)
plot(ohare_lag1, ohare_series, 
     ylab = "temp[t]", xlab="temp[t-1]",
     xy.lines=FALSE, xy.labels=FALSE)

# Working with lagged variables in R can be tricky, since not all R functions
# understand how to deal with time series. It can be useful to 
# combine them using ts.intersect, which will avoid indexing mistakes

# For example, this won't work correctly because cor() doesn't know how 
# to handle time series:
cor(ohare_lag1, ohare_series)

# But if we combine them first we're OK
ohare_comb = ts.intersect(y = ohare_series, lag1=ohare_lag1, dframe=TRUE)
# In ohare_comb, y[t] is temp[t] from the lecture slides, and 
# lag1[t] is temp[t-1]

# Check that the combined dataset makes sense by looking at the
# first and last few observations:
head(ohare_comb)
tail(ohare_comb)

# Now we can compute the correlation correctly:
cor(ohare_comb$y, ohare_comb$lag1)

# Plot the autorcorrelation function
acf(ohare_series)

# Fit the AR(1) model to the ohare data; this is another
# case where its important to use a combined dataset
ohare_ar1 = tslm(y~lag1, data=ohare_comb)
summary(ohare_ar1)

acf(resid(ohare_ar1))

plot(rstandard(ohare_ar1))

# To get proper forecasts from AR models, we need can use the Arima
# function from the forecast package. You'll get the same estimates
# as from tslm, but the forecast() function knows how to deal with the
# lagged covariates. The "Ar" in ARIMA stand for "autoregressive"; we
# won't have time in this class to talk about the "IMA".
#
# We can get a higher order AR model (say a model with p lags) by changing 
# c(1,0,0) below to c(p,0,0)
ohare_ar1_arima = Arima(ohare_series, c(1,0,0))

plot(forecast(ohare_ar1_arima, h=1), main="")

plot(forecast(ohare_ar1_arima, h=2), main="")

plot(forecast(ohare_ar1_arima, h=3), main="")

plot(forecast(ohare_ar1_arima, h=30), main="")

# Beer production example
beer = read_csv(paste0(path, 'beerprod.csv'), col_names=FALSE)
# Here I'm being a little more careful in making the time series
# object. I'm specifying the start date and the frequency of measurement
# (monthly, so 12 observations per year). This will let us avoid making
# seasonal dummies by hand.
beer_series = ts(beer$X1, start=c(1990, 1), frequency=12)

plot(beer_series, ylab="Beer Production")

beer_fit = tslm(beer_series~season)
print(beer_fit)

# Let's see what the X's look like here. tslm uses a function called
# seasonaldummy to automatically construct seasonal dummies. Otherwise
# you would have to do this by hand, which is tedious but not difficult.
#
# The cbind function below will "glue" the response (beer_series) to the
# constructed covariates (which we extract from the model fit using the
# model.matrix function)
print(cbind(beer_series, X=model.matrix(beer_fit)[,-1]))
View(cbind(beer_series, X=model.matrix(beer_fit)[,-1]))

# Let's look at the model fit
plot(beer_series, ylab="Beer Production")
points(beer_series)
lines(ts(predict(beer_fit), start=c(1990, 1), frequency=12), 
      col='red', lwd=2)

# ...And the forecasts
plot(forecast(beer_fit, h=24), ylab="Beer Production")

