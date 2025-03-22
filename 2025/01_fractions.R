
library(tidyverse) # to do tidyverse things
library(tidylog) # to get a log of what's happening to the data
library(janitor) # tools for data cleaning
library(danstat) # package to get Danish statistics via api
library(ggtext) # enhancements for text in ggplot

# some custom functions
source("~/Data/r/basic functions.R")


# metadata for table variables, click thru nested tables to find variables and ids for filters
table_meta <- danstat::get_table_metadata(table_id = "hfudd11", variables_only = TRUE)

# create variable list using the ID value in the variable
variables_ed <- list(
	list(code = "bopomr", values = c("000", "081", "082", "083", "084", "085")),
	list(code = "hfudd", values = c("H10", "H20", "H30", "H35",
																	"H40", "H50", "H60", "H70", "H80", "H90")),
	list(code = "køn", values = c("TOT")),
	list(code = "alder", values = c("25-29", "30-34", "35-39", "40-44", "45-49",
																	"50-54", "55-59", "60-64", "65-69")),
	list(code = "tid", values = 2023))

# past variable list along with table name.
# note that in package, table name is lower case, though upper case on statbank page.
edattain <- get_data("hfudd11", variables_ed, language = "da") %>%
	as_tibble()

%>%
	select(sex = KØN, age = ALDER, edlevel = HFUDD, n = INDHOLD)
