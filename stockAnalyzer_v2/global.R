require(ggplot2)
require(quantmod)

nasdaq <- read.csv("data/NASDAQ.csv", header = T)
nasdaq <- nasdaq[1:9]
nasdaq$exchange <- "nasdaq"

nyse <- read.csv("data/NYSE.csv", header=T)
nyse <- nyse[1:9]
nyse$exchange <- "nyse"

exchanges <- merge(nasdaq, nyse, all.x=T, all.y=T)

#Truncated to only include symbols
symbols <- unique(exchanges$Symbol)
symbols <- levels(symbols)
rm(nasdaq, nyse, exchanges)