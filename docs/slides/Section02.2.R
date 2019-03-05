# Here I'm putting this small dataset right in the R script...
housing = data.frame(
  Size=c(0.8,0.9,1.0,1.1,1.4,1.4,1.5,1.6,1.8,2.0,2.4,2.5,2.7,3.2,3.5),
  Price=c(70,83,74,93,89,58, 85,114, 95,100,138,111,124,161,172)
)
fit = lm(Price~Size, data=housing)


confint(fit, level=0.95)
38.885 - 2*9.094; 38.885 + 2*9.094; 
35.386 - 2*4.494; 35.386 + 2*4.494; 

library(readr)
path = "https://jaredsmurray.github.io/sta371g_f17/data/"
windsor = read_csv(paste0(path, 'windsor.csv'))

plot(Windsor~SP500, data=windsor)
fit = lm(Windsor~SP500, data=windsor)
summary(fit)

confint(fit, level=0.95)

2*pnorm(abs(1.079 - 1)/0.035, lower.tail=FALSE)
fit = lm(Price~Size, data=housing)

# Make a data.frame with some X_f values 
# (X values where we want to forecast)
newdf = data.frame(Size=c(1, 1.85, 3.2, 4.1))
predict(fit, newdata = newdf, 
        interval = "prediction", level = 0.95)

