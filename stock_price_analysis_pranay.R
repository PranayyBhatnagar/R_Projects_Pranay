
# Install and load required package
install.packages("quantmod")
library(quantmod)

# Fetch historical stock data for Apple Inc.
getSymbols("AAPL", src = "yahoo", from = "2022-01-01", to = Sys.Date())

# Display first few rows
head(AAPL)

# Calculate moving averages
sma50 <- SMA(Cl(AAPL), n = 50)
sma200 <- SMA(Cl(AAPL), n = 200)

# Visualize the stock price with SMAs
chartSeries(AAPL, theme = chartTheme("white"), TA = NULL)
addSMA(n = 50, col = "blue")
addSMA(n = 200, col = "red")
