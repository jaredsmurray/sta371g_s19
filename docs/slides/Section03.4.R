library(readr)
path = "https://jaredsmurray.github.io/sta371g_s19/data/"

library(datasets)
data(anscombe)

fit1 = lm(y1~x1, data=anscombe)
fit2 = lm(y2~x2, data=anscombe)

par(ask=FALSE, mfrow=c(1,2))
plot(y1~x1, data=anscombe); abline(fit1)
plot(y2~x2, data=anscombe); abline(fit2)


par(ask=FALSE, mfrow=c(1,2))
plot(resid(fit1)~fitted(fit1))
# Add horizontal line at zero
abline(h=0)
plot(resid(fit2)~fitted(fit2))
abline(h=0)

### Mid City Housing Example
housing = read_csv(paste0(path, 'MidCity.csv'))
# Rescale the response to be in $1000 & Size in 1000sqft
housing$Price = housing$Price/1000
housing$Size = housing$SqFt/1000

housing_fit = lm(Price~factor(Nbhd) + Size + Brick + Bedrooms + Bathrooms, data=housing)

par(mfrow=c(1,2))
plot(housing$Price~fitted(housing_fit))
# Add the line y=x; abline(b,a) plots y = ax + b
abline(0,1)
plot(resid(housing_fit)~fitted(housing_fit))
# Add horizontal line at zero
abline(h=0)

# It's useful to plot the residuals against each covariate;
# while cor(e, x) = 0 always, you might see a *nonlinear* pattern.
# This plot looks pretty good.
par(mfrow=c(1,1))
plot(resid(housing_fit)~housing$Size)
abline(h=0)

### Telemarketing Example

tele = read_csv(paste0(path, 'tele.csv'))
plot(calls~months, data=tele)

telefit = lm(calls~months, data=tele)
plot(resid(telefit)~fitted(telefit), data=tele)
abline(h=0)

telefit2 = lm(calls~months + I(months^2), data=tele)
print(telefit2)

library(mosaic)
plotModel(telefit2)

### OJ Elasticity Example

oj = read_csv(paste0(path, 'oj.csv'))
lm(Sales~Price, data=oj)

ojfit = lm(Sales~Price, data=oj)
plotModel(ojfit)
plot(resid(ojfit)~oj$Price)
abline(h=0)
ojfitelas = lm(log(Sales)~log(Price), data=oj)
coef(ojfitelas)

# Get predictions using the transformed model
newdata = data.frame(Price = seq(min(oj$Price), max(oj$Price), length.out=100))
yhat = predict(ojfitelas, newdata=newdata)
plot(Sales~Price, data=oj)
lines(exp(yhat)~newdata$Price, col='red')
plot(resid(ojfitelas)~oj$Price)
abline(h=0)

# Add plug-in predictive intervals
s = sigma(ojfitelas)
plot(log(Sales)~log(Price), data=oj)
abline(ojfitelas, col='red')
lines(log(newdata$Price), yhat+2*s, lty=2, col='red')
lines(log(newdata$Price), yhat-2*s, lty=2, col='red')

plot(Sales~Price, data=oj)
lines(exp(yhat)~newdata$Price, col='red')
lines(newdata$Price, exp(yhat+2*s), lty=2, col='red')
lines(newdata$Price, exp(yhat-2*s), lty=2, col='red')

### Mammals outlier example
mammals = read_csv(paste0(path, 'mammals.csv'))

plot(brain~body, data=mammals)

plot(log(brain)~log(body), data=mammals)
mamfit = lm(log(brain)~log(body), data=mammals)
abline(mamfit)

plot(rstandard(mamfit)~log(body), data=mammals, ylim=c(-4,4))
abline(h=c(-3,3), col='red')

# We can examine outliers by finding points with large residuals
# Remember, there is no single cutoff for what is an outlier! In
# large datasets we expect to see large standardized residuals by chance,
# and there is always a chance we'll see large outliers in small datasets
# by chance too. 

# Here we decided to look at cases with standardized residuals bigger
# than 2. We can find those cases for the model in mamfit using the code below.
possible.outliers = which(abs(rstandard(mamfit))>2)

print(possible.outliers)

# Now we can look at the relevant rows of the dataset
print(mammals[possible.outliers,])
#... and their standardized residuals
print(rstandard(mamfit)[possible.outliers])

