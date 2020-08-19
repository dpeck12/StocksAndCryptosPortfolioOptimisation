# libraries 
library(quantmod)
library(tseries)

# data
load(url("https://s3.amazonaws.com/finance-r/2018/ib.crypto.stock.rda"))

# rename the scenario set
# parse a character vector of all ticker symbols
# count the amount of cryptos 
# sorted alphabetically, ZEC is the last one 

scenario.set <- ib.crypto.stock
symbols <- names(scenario.set)
n_cryptos <- which(symbols=="ZEC")

# check which cryptos are in asset universe
#print(symbols[1:n_cryptos])

# computing a basic markowitz portfolio
# static portfolio optimisation
timeframe <- "2017"
portfolio <- round(portfolio.optim(scenario.set[timeframe])$pw, 2)

# check which assets are in our optimised portfolio
# extracting all assets whose weights are larger than 0

# ticker symbols
print("-----TICKER SYMBOLS-----") 
print(symbols[which(portfolio > 0)])

# weights
print("-----Weight in the porfolio-----")
print(portfolio[which(portfolio > 0)])

# solely crypto assets 
print(symbols[which(portfolio[1:n_cryptos] > 0)])
print(portfolio[which(portfolio[1:n_cryptos] > 0)])

# draw a pie chart
eps <- 0.03

# pie(portfolio[which(portfolio>eps)], labels=symbols[which(portfolio>eps)], col=rainbow(length(which(portfolio > eps))))

# have seen that a plain Markowitz optimisation allocates 
# only 1% of the budget into cryptos based on data from 2017
# Now, expand the horizon test over the whole of Q1 2018

# ROLLING HORIZON PORTFOLIO OPTIMIZATION
### Rolling Horizon Portfolio Optimization
n_total <- nrow(scenario.set)
n_2017 <- nrow(scenario.set["2017"])

crypto_sum <- numeric()
portfolios <- matrix(ncol=n_cryptos, nrow=0)
for(current.pos in (n_2017+1):(n_total-1)) {
  portfolio <- round(portfolio.optim(scenario.set[(current.pos-n_2017):(current.pos), ])$pw, 2)
  portfolios <- rbind(portfolios, portfolio[1:n_cryptos])
  crypto_sum <- c(crypto_sum, sum(portfolio[1:n_cryptos]))
}

# from this rolling horizon we store two different data sets
# for closer inspection. 
# First, store cryptocurrency allocation into a matrix portfolios 

df.crypto.portfolio <- data.frame(portfolios)
names(df.crypto.portfolio) <- names(scenario.set[,1:n_cryptos])
head(df.crypto.portfolio)


# store the sum of % allocated to cryptos, into a vector 
# crypto_sum and plot this value for each day of our 
# rolling horizon test

plot(xts(crypto_sum, order.by=index(scenario.set[(n_2017+1):(n_total-1)])),
     ylim=c(0,0.03), main="Crypto Weight in Portfolio")