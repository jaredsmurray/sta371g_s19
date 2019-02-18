# Here I'm putting this small dataset right in the R script...
housing = data.frame(
  Size=c(0.8,0.9,1.0,1.1,1.4,1.4,1.5,1.6,1.8,2.0,2.4,2.5,2.7,3.2,3.5),
  Price=c(70,83,74,93,89,58, 85,114, 95,100,138,111,124,161,172)
)

# The "~" notation here says plot Price on the y axis and Size on the x axis,
# where Price and Size are columns in the object called housing. This notation
# (called a formula) shows up over and over in R.
plot(Price~Size, data=housing)
      
# Here we see the same formula notation -- Price~Size -- that we used to 
# make the scatterplot
fit = lm(Price~Size, data=housing)
print(fit)

# There's a lot more information in the fit object. We'll be unpacking what
# these numbers are over the next few classes.
fit = lm(Price~Size, data=housing)
summary(fit)

# the predict command extracts the predictions (yhat) for each
# observation in the sample. Here we plot those predictions against
# the independent variable Size.
plot(predict(fit)~Size, data=housing, 
     ylab="fitted values yhat")

# cor() computes the sample correlation, which we'll talk more about
# soon.
cor(predict(fit), housing$Size)

# resid() extracts the residuals y - yhat for each observation in the sample
plot(resid(fit)~Size, data=housing, ylab="residuals")

mean(resid(fit));  cor(resid(fit), housing$Size)

# Computing least squares estimates "by hand"
y = housing$Price; x = housing$Size
rxy = cor(y, x)
sx = sd(x)
sy = sd(y)
ybar = mean(y)
xbar = mean(x)
b1 = rxy*sy/sx
b0 = ybar - b1*xbar
print(b0); print(b1)

# We get the same result as lm()
fit = lm(Price~Size, data=housing)
print(fit)

# R^2 is on the second to last line of the R output, under
# "Multiple R-squared". We'll talk about "Adjusted R-squared"
# when we get to multiple linear regression.
summary(fit)

# The anova() function will extract sums of squares, and do some hypothesis tests.
# We can use it to "check" how the sums of squares are related
anova(fit)

