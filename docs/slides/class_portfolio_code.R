
## Parameters from the Ford/Tesla example in class
## mf = Mean returns for Ford (percent)
## sf = SD of returns for Ford (percent)
## mt, st = same for Tesla
## cc = covariance between Ford & tesla returns
mf = 0.12
sf = 2.29
mt = 0.14
st = 3.12
cc = 3.063

## The seq() function generates a grid of values between
## its first and second arguments, where the gap between
## each value is given by the "by" argument.
w = seq(0, 1, by = 0.001)

## In the following snippet, mf, mt, st, sf, and cc are all
## scalars. The addition and multiplication below all take place
## elementwise across the vector of w values. For a simpler example,
## try entering some other functions of w in the console (w^2, w+1, etc)
mp = w*mf + (1-w)*mt
vp = w^2*sf^2 + (1-w)^2*st^2 + 2*w*(1-w)*cc
sp = sqrt(vp)
sharpe = mp/sp

## the sharpe variable defined above is also a vector, with entries that "match"
## the w vector (i.e., sharpe[4] is the sharpe ratio for the portfolio with
## weight w[4]). We can plot them together like so:

plot(w, sharpe)

## The plot function's first two arguments are vectors giving the x-coordinate 
## and the y-coprdinate of the points to be plotted.
## The argument type='l' (short for type = line) connects the dots:

plot(w, sharpe, type='l')

# We can find the (approximate) weight that maximizes the Sharpe ratio
# by staring at the graph, or using the code below

max_sharpe_location = which.max(sharpe)

# max_sharpe_location holds the index corresponding to the
# best portfolio weight, which is 

w[max_sharpe_location]

# We can also find the lowest variance portfolio
min_variance_location = which.min(vp)
w[min_variance_location]

# At the end of class we plotted the sharpe ratio, mean, and variance all
# together.

# set up a panel of three plots in one column:
par(mfrow=c(3,1))

plot(w, mp, type='l')
# add blue dotted line at the best w according to sharpe ratio
abline(v=w[max_sharpe_location], lty=2, col='blue') 
# and red at the min variance portfolio 
abline(v=w[min_variance_location], lty=3, col='red') 
plot(w, vp, type='l')
abline(v=w[max_sharpe_location], lty=2, col='blue') 
abline(v=w[min_variance_location], lty=3, col='red') 
plot(w, sharpe, type='l')
abline(v=w[max_sharpe_location], lty=2, col='blue') 
abline(v=w[min_variance_location], lty=3, col='red') 

# Reset to one panel of plots
par(mfrow=c(1,1))

