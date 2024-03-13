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
set_up_packages <- function(packages, install_packages = TRUE) {
    missing <- setdiff(packages, installed.packages()[, "Package"])
    if (length(missing) > 0) {
        cat("The following packages have not been installed:\n")
        cat(paste(missing, collapse = ", ", "\n"))

        if (install_packages) {
            install.packages(missing, dependencies = TRUE)
        } else {
            cat("Please install required packages manually before using functions")
            break()
        }
    }

    for (package in packages) {
        if (!requireNamespace(package, quietly = TRUE)) {
            library(package, character.only = TRUE)
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
        stop("Invalid file extension")
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
        distinct() |>
        pull()
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
        group_by(across(all_of(grouping_vars))) |>
        summarise(
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
        expand(
            nesting(across(nesting_vars)),
            year = seq(start_year, end_year, 1)
        ) |>
        left_join(df)
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
        group_by(across(all_of(grouping_vars))) |>
        mutate(
            {{value_col}} := approx(
                x = year,
                y = .data[[value_col]],
                xout = year,
                rule = 2
            )$y
        ) |>
        ungroup()
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
        group_by(across(all_of(grouping_vars))) |>
        mutate(
            {{value_col}} := approx(x = year, y = .data[[value_col]], xout = year, rule = 2)$y,
            {{value_col}} := .data[[value_col]] - lag(.data[[value_col]])) |>
        ungroup()
}