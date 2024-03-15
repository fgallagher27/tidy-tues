#### This script contains functions for the stock price dataset

#### 1. Data cleaning ----

#' @description This function processes the raw stock price data doing the following:
#' 1. converts date to date type
#' 2. calculates daily average price as mean of open and close price
#' 3. calculates the volatility using `calc_volatility`
#' 4. calculates the proportion of price kept by investor as adjusted close price / close price
process_stock_data <- function(df) {
    df |>
        dplyr::mutate(
            date = as.Date(date),
            avg_price = (open + close) / 2,
            volatility = calc_volatility(high, low, avg_price),
            # calc adjusted proportion as adjusted close as fraction of closing price
            adj_prop = adj_close / close
        )
}

#### 2. Modelling ----

#' @description calculates intra day stock volatilty as the intra day range over
#' the average day price.  A value of 100 means the price variations in a day have a range
#' equal to the average day price, whilst 0 means no price movement at all.
calc_volatility <- function(max, min, avg) {
    return((max - min) / avg * 100)
}

#' @description lags the target variable by p to create input dataset
#'
#' @param df dataframe object
#' @param id_cols cols to group by when creating lags
#' @param target_col name of target variable column
#' @param p order of the AR model
#'
#' @return dataframe object
create_p_lags <- function(df, id_cols, target_col, p) {
    for (lag in 1:p) {
        df <- calc_delta(df, id_cols, target_col, lag = lag)
    }
    return(df)
}

#' @description constructs the framework for an autoregression model of order p.
#'
#' Key stationarity assumption is that the sum of all parameters on lagged values
#' is less than 1
#'
#' @param target_col name of target variable column
#' @param input_cols list of name of input features
#' @param p order of the AR model
#'
#' @return tidymodels workflow
ar_p <- function(target_col, input_cols, p) {
    lm_model <- parsnip::linear_reg() |>
        parsnip::set_engine("lm")

    formula_str <- paste(target_col, "~", paste(input_cols, collapse = " + "))
    ar_model <- workflows::workflow() |>
        workflows::add_model(lm_model) |>
        workflows::add_formula(as.formula(formula_str))

    return(ar_model)
}
