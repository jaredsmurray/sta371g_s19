# Load price/sales data from section 3.1
library(readr)
path = "https://jaredsmurray.github.io/sta371g_s19/data/"
price_sales = read_csv(paste0(path, 'price_sales.csv'))

fit = lm(Sales~p1+p2, data=price_sales)
print(fit)

print(sigma(fit)) # sigma(fit) extracts s from an lm fit

new_data = data.frame(p1=8, p2=10)
predict(fit, newdata = new_data, 
        interval="prediction", level=0.95)

# Plot residuals vs the covariates and yhat
resids = resid(fit)
yhat = predict(fit)
par(mfrow=c(1,3)) # This sets up a 1 by 3 grid of plots like in the slides
plot(resids~p1, data=price_sales)
plot(resids~p2, data=price_sales)
plot(resids~yhat, data=price_sales)
par(mfrow=c(1,1)) # This returns to a single plot

# Plot y vs yhat
plot(Sales~yhat, data=price_sales)
abline(0,1) # adds the line with intercept 0 and slope 1 (i.e., y=x) for reference

# Compare predictions from each SLR fit to the MLR fit
fit_p1 = lm(Sales~p1, data=price_sales)
fit_p2 = lm(Sales~p2, data=price_sales)
fit_p1p2 = lm(Sales~p1+p2, data=price_sales)

par(mfrow=c(1,3))
plot(Sales~predict(fit_p1), data=price_sales); abline(0,1)
plot(Sales~predict(fit_p2), data=price_sales); abline(0,1)
plot(Sales~predict(fit_p1p2), data=price_sales); abline(0,1)
par(mfrow=c(1,1))

# Load baseball data
path = "https://jaredsmurray.github.io/sta371g_s19/data/"
baseball = read_csv(paste0(path, 'baseball.csv'))

both_fit = lm(RPG ~ OBP + SLG, data=baseball)
print(both_fit)

summary(both_fit)

both_fit = lm(RPG ~ OBP + SLG, data=baseball); coef(both_fit)

obp_fit = lm(RPG ~ OBP, data=baseball); coef(obp_fit)

slg_fit = lm(RPG ~ SLG, data=baseball); coef(slg_fit)

