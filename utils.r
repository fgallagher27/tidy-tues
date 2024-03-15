#### This file contains helper functions

#### SECTION 1: SETUP ----

required_packages <- c(
    "arrow",
    "assertthat",
    "broom",
    "dbplyr",
    "dplyr",
    "dtplyr",
    "duckdb",
    "furrr",
    "ggplot2",
    "ggrepel",
    "here",
    "knitr",
    "readr",
    "readxl",
    "stringr",
    "testthat",
    "tictoc",
    "tidyverse",
    "tidymodels",
    "yaml",
    "zoo"
)

# checks for missing packages and installs them
# then loads all required packages
check_packages <- function(packages, install_packages = TRUE) {
    missing <- setdiff(packages, installed.packages()[, "Package"])
    if (length(missing) > 0) {
        cat("The following packages have not been installed:\n")
        cat(paste(missing, collapse = ", ", "\n"))

        if (install_packages) {
            install.packages(missing, dependencies = TRUE)
        } else {
            cat("Please install required packages manually before using functions")
            return(NULL)
        }
    }
}

#' @description loads data from a url depending on the file extension
#'
#' @param path string containing the filepath or url where data file is located
#'
#' @return a tidyverse dataframe
load_data <- function(path, ...) {
    func <- switch(
        tools::file_ext(path),
        csv = readr::read_csv,
        rds = readRDS,
        xlsx = readxl::read_xlsx,
        parquet = arrow::read_parquet
    )

    if (is.null(func)) {
        rlang::abort("Invalid file extension")
    }
    func(path, ...)
}

#### SECTION 2: Data cleaning ----

# functions to count the number of missing, zero, negative, or unique values in a df
na_counter <- function(variable) {
    sum(is.na(variable))
}
zero_counter <- function(variable) {
    sum(ifelse(variable == 0, 1, 0), na.rm = TRUE)
}
negative_counter <- function(variable) {
    sum(ifelse(variable < 0, 1, 0), na.rm = TRUE)
}
unique_counter <- function(variable) {
    length(unique(variable))
}

# checks if a df is distinct
distinct_df <- function(df) {
    nrow(df) == nrow(distinct(df))
}

# returns names of columns with NA values in
check_col_na <- function(df) {
    colnames(df) [apply(df, 2, anyNA)]
}

# Find common variables from two datasets
common_vars <- function(tibble_x, tibble_y) {
    print("Variable names which match exactly:")
    intersect(colnames(tibble_x), colnames(tibble_y))
}

#' @description finds all items in list1 that do not occur in list2
#'
#' @param list1 a list of items
#' @param list2 a list of items
#'
#' @return a list of items in list1 not in list2
missing_items <- function(list1, list2) {
    list1[!list1 %in% list2]
}

#' @description pulls all unique combinations from a column
#'
#' @param df a dataframe object
#' @param col string column name
#'
#' @return a list of unique values
pull_unique_vals <- function(df, col) {
    df |>
        dplyr::select(col) |>
        dplyr::distinct() |>
        dplyr::pull()
}

#' @description Sums across groups
#'
#' @param df a dataframe object
#' @param grouping_vars list of column names to group by
#' @param value_col name of value column
#'
#' @return summarised dataframe
sum_across <- function(df, grouping_vars, value_col = "value") {
    df |>
        dplyr::group_by(across(all_of(grouping_vars))) |>
        dplyr::summarise(
            {{value_col}} := sum(.data[[value_col]], na.rm = TRUE),
            .groups = "drop"
        )
}

#' @description Annualises a dataframe
#'
#' @param df a dataframe object with year column
#' @param nesting_vars list of column names (excluding year)
#' @param start_year integer
#' @param end_year integer
#'
#' @return dataframe object with rows for every year in every nested combination
#' final dataframe will hold NAs where years were not present
#' use `annualise_and_interpolate_data` to interpolate these missing values
annualise_df <- function(df, nesting_years, start_year, end_year) {
    df |>
        tidyr::expand(
            tidyr::nesting(across(nesting_vars)),
            year = seq(start_year, end_year, 1)
        ) |>
        dyplr::left_join(df)
}

#' @description Interpolate missing values
#'
#' @param df a dataframe object with year column
#' @param nesting_vars list of column names (excluding year and value col)
#' @param start_year integer
#' @param end_year integer
#' @param value_col name of value column to interpolate
#'
#' @return dataframe object with interpolated values for every year in range
annualise_and_interpolate_data <- function(
    df,
    grouping_vars,
    value_col = "value",
    ...
) {
    df |>
        annualise_and_interpolate_data(...) |>
        dyplr::group_by(across(all_of(grouping_vars))) |>
        dplyr::mutate(
            {{value_col}} := approx(
                x = year,
                y = .data[[value_col]],
                xout = year,
                rule = 2
            )$y
        ) |>
        dplyr::ungroup()
}

#' @description converts a stock variable to a flow. Uses interpolation for missing years.
#'
#' @param df dataframe object
#' @param grouping_vars list of grouping variables
#' @param value_col str for value column name
#'
#' @return df object with flow values
convert_stock_to_flow <- function(df, grouping_vars, value_col = "value", ...) {
    df |>
        annualise_df(...) |>
        # Approximate annual capacity in each year
        dplyr::group_by(across(all_of(grouping_vars))) |>
        dplyr::mutate(
            {{value_col}} := approx(x = year, y = .data[[value_col]], xout = year, rule = 2)$y,
            {{value_col}} := .data[[value_col]] - lag(.data[[value_col]])) |>
        dplyr::ungroup()
}

#' @description calculates the difference between observations in a long
#' format df
#'
#' @param df dataframe object
#' @param grouping_vars vector of column names to group by
#' @param value_col chr name of column with values
#' @param lag integer value for number of values to lag by
#'
calc_delta <- function(df, grouping_vars, value_col, lag = 1) {
    df |>
        dplyr::group_by(across(all_of(grouping_vars))) |>
        dplyr::mutate(!!paste("lag", lag, value_col, sep = "_") := dplyr::lag(!!sym(value_col), n = lag)) |>
        dplyr::ungroup()
}

#### SECTION 3. MODELLING ----

#' @description splits a dataset into test and train set based on a test proportion
#'
#' @param df dataframe containing full dataset
#' @param test_prop proportion of dataset to have in test set
#' @param strata chr vector of identifying columns to stratify by to maintain training and test
#' set having the same proportions of each group. NULL if no stratifying applied.
#' @param mode string for type of split i.e. random or latest
#'
#' @return a list of the training and testing dataframe
split_train_test <- function(df, test_prop = 0.05, strata = NULL, mode = "random", time_col = "date") {
    if (mode == "random") {
        split <- rsample::initial_split(df, prop = 1 - test_prop, strata = strata)
        train <- rsample::training(split)
        test <- rsample::testing((split))
    } else if (mode == "latest") {
        # create custom time split to allow strata
        train <- df |>
            dplyr::group_by(across(all_of(strata))) |>
            dplyr::arrange(!!sym(time_col), .by_group = TRUE) |>
            dplyr::slice_head(prop = 1 - test_prop) |>
            dplyr::ungroup()
        test <- dplyr::anti_join(df, train)

    } else {
        rlang::abort("`mode` should be either `random` or `latest`")
    }
    return(dplyr::lst(train, test))
}

#' @description Retrieves predictions of fitted model
#'
#' @param model a fitted model
#' @param eval_data evaluation data to generate predictions on
#' @param target_col chr name of observed values col in eval data
#' @param get_metrics boolean toggle to return metrics
#' @param metrics a list of metrics. A function of the same name needs to be defined.
#' This function is applied rowwise where the observed value is first variable, and
#' predicted value is the second. An average is then taken over all observations.
#'
#' @return list of df object with id cols, input cols, and predictions and df with metrics
get_predictions <- function(model, eval_data, target_col, get_metrics = FALSE, metrics = NULL) {

    # 1. Generate predictions
    predictions <- predict(model, eval_data)
    predictions <- dplyr::bind_cols(eval_data, predictions)

    if (get_metrics) {
        metric_df <- predictions
        # 2. apply metric rowwise
        for (metric in metrics) {
            func <- match.fun(metric)
            metric_df <- metric_df |>
                dplyr::mutate(!!metric := func(!!sym(target_col), .pred))
        }
        # 3. take average for each metric
        metric_df <- metric_df |>
            dplyr::summarise(across(all_of(metrics), \(x) mean(x, na.rm = TRUE)))
        return(lst(predictions, metric_df))
    } else {
        return(lst(predictions))
    }
}

# calculates mean squared error of two numbers
mse <- function(a, b) {
    return((a - b) ^ 2)
}

#### SECTION 4. PLOTTING ----

#' @description line plot of two variables by id
#'
#' @param df dataframe
#' @param x_col name of x variable column
#' @param y_col name of y variable column
#' @param id_cols list of id columns
#' @param legend boolean for showing legend
#' @param ... variables to be passed to label function (title, x, y, color)
#'
#' @return ggplot object of geom_line() plot, display by print(p)
quick_line_plot <- function(df, x_col, y_col, id_cols, legend = TRUE, ...) {
    p <- ggplot(df, aes(x = !!sym(x_col), y = !!sym(y_col), color = !!sym(id_cols))) +
        geom_line() +
        labs(...) +
        theme_minimal() +
        theme(legend.position = ifelse(legend, "right", "none"))
    return(p)
}
