library(readr)
path = "https://jaredsmurray.github.io/sta371g_s19/data/"
price = read_csv(paste0(path, 'price_sales.csv'))
pricefit = lm(Sales~p1+p2, data=price)

# Fit the three beer ~ height + weight models
beer = read_csv(paste0(path, 'beer.csv'))
beerfit = lm(nbeer~height, data=beer)
summary(beerfit)

beerfit_hw = lm(nbeer~height+weight, data=beer)
summary(beerfit_hw)

beerfit_w = lm(nbeer~weight, data=beer)
summary(beerfit_w)

# Comparing the standard errors on OBP in the SLR and
# OBP + SLG models
baseball = read_csv(paste0(path, 'baseball.csv'))
both_fit = lm(RPG ~ OBP + SLG, data=baseball)
obp_fit = lm(RPG ~ OBP, data=baseball)

# Why is the SE higher in the MLR model? Collinearity:
cor(baseball$OBP, baseball$SLG)

# Supervisor data
super = read_csv(paste0(path, 'supervisor.csv'))
superfit = lm(Y~., data=super)
summary(superfit)

# Make a new set of random covariates unrelated to Y
set.seed(2210)
G = matrix(rnorm((ncol(super)-1)*nrow(super)), nrow(super))
garbage_data = data.frame(Y = super$Y, G=G)
garbagefit = lm(Y~., data=garbage_data)
summary(garbagefit)

