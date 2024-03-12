#### This file contains helper functions

# Set up
library(tidyverse)
library(tidymodels)
library(config)

run_set_up <- function() {
    current_dir <- get_dir_info()
    return(current_dir)
}

get_dir_info <- function() {

}

get_data <- function(url) {
    # use logic of switch from BRI here
    return(readr::read_csv(url))
}
