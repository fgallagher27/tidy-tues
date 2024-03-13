#### This script contains functions for the stock price dataset

#### 1. Data cleaning ----

#### 2. Modelling ----

#' @description calculates intra day stock volatilty as intra day price range
#' over average price for the day
calc_volatility <- function(max, min, avg) {
    return((max - min) / avg)
}