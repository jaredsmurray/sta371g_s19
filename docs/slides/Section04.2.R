library(readr)
library(forecast)
path = "https://jaredsmurray.github.io/sta371g_f17/data/"
airline = read_csv(paste0(path, 'airline.csv'))
head(airline)
passengers = ts(airline$Passengers, start=c(1949, 1), frequency=12)
plot(passengers)

# Start by fitting a trend...
fit0 = tslm(passengers~trend)
plot(passengers)
lines(ts(predict(fit0), start=c(1949, 1), frequency=12), col='red')

# Big trouble in the residuals!
plot(rstandard(fit0), ylab="Std Residuals")
abline(h=0, lty=2)

# First let's try a log transform to get constant variance
fit1 = tslm(log(passengers)~trend)
plot(log(passengers))
lines(ts(predict(fit1), start=c(1949, 1), frequency=12), col='red')

plot(rstandard(fit1), ylab="Std Residuals")

# Now adding seasonal dummies:
fit2 = tslm(log(passengers)~trend+season, data=passengers)
plot(log(passengers))
lines(ts(predict(fit2), start=c(1949, 1), frequency=12), col='red')

plot(rstandard(fit2), ylab="Std Residuals")

acf(resid(fit2))

# Adding a lagged term to the model:
lagged_y = stats::lag(log(passengers), -1)
lagged_df = ts.intersect(y=log(passengers), lag1=lagged_y)
fit3 = tslm(y~season+trend+lag1, data=lagged_df)
plot(log(passengers))
# Note that below, the first prediction is for Feb since we lose the first observation to
# construct the lagged variable!
lines(ts(predict(fit3), start=c(1949, 2), frequency=12), col='red')

plot(rstandard(fit3), ylab="Std Residuals")

acf(resid(fit3))

# It's a little more work to get forecasts from a model with covariates
# (here the trend and seasonal dummies) in addition to lagged variables; we have to
# construct the covariates ourselves

X = cbind(trend=1:length(passengers), # This is the trend
          season=seasonaldummy(passengers) # These are the seasonal (month) dummies
          )

# Take a look at our constructed covariates for the first 14 observations:
head(X, 14)

# We can use Arima to generate forecasts with lagged variables here too, by
# specifying the other variables (trend + seasonal dummies) using the xreg 
# argument below
airline_final_model = Arima(log(passengers), order=c(1,0,0), xreg = X)

# Let's make a forecast for next year (the next 12 observations)
h = 12
# First we need to build the new covariates:
new_X = cbind(trend = seq(length(passengers)+1, length(passengers)+h),
              season = seasonaldummy(passengers, h=h))

# Then generate the forecast
airline_forecast = forecast(airline_final_model, xreg=new_X)

# Forecasts on the log scale:
plot(airline_forecast, main="")


# On the original scale, with just the 95% pred interval:
plot(passengers, xlim=c(1949, 1962), ylim=c(90, 750))
lines(exp(airline_forecast$mean), col='red', lwd=2)
lines(exp(airline_forecast$lower[,2]), col='red', lty=2)
lines(exp(airline_forecast$upper[,2]), col='red', lty=2)

