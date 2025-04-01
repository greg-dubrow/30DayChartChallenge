# median earnings bubbles by sector (x axis) and level of education (y axis)
# based on https://albert-rapp.de/posts/ggplot2-tips/14_scaled_heatmaps/14_scaled_heatmaps.html#bubble-chart

library(tidyverse) # to do tidyverse things
library(tidylog) # to get a log of what's happening to the data
library(janitor) # tools for data cleaning
library(danstat) # package to get Danish statistics via api
library(ggtext) # enhancements for text in ggplot
library(ggrepel)
library(scales)
library(packcircles)
#library(patchwork)

# some custom functions
source("~/Data/r/basic functions.R")

# LONS11
table_meta <- danstat::get_table_metadata(table_id = "lons11", variables_only = TRUE)

# create variable list using the ID value in the variable
variables_ed <- list(
	list(code = "uddannelse", values = c("H10", "H20", "H30", #"H35",
																			 "H40", "H50", "H60", "H70", "H80" )),
#																			 , "H90")),
	list(code = "sektor", values = c(1032, 1016, 1018, 1020, 1025, 1046)),
#	list(code = "afloen", values = c("TIME", "FAST")),
  list(code = "lønmål", values =  "MDRSNIT"),
	list(code = "tid", values = 2023))

sal1 <- get_data("lons11", variables_ed, language = "en") %>%
	as_tibble() %>%
	clean_names()

glimpse(sal2)

sal2 <- sal1 |>
	mutate(income = as.numeric(indhold)) %>%
	mutate(income = round(income, 0)) %>%
	mutate(income_sq = income*income)
# %>%
# 	group_by(sektor, uddannelse) |>
# 	summarise(
# 		n = n(),
# 		med_inc = median(income),
# 		.groups = 'drop'
# 	) |>
# 	complete(sektor, uddannelse)

glimpse(sal2)

sal2 %>%
	ggplot(aes(x = sektor, y = uddannelse)) +
	geom_point(aes(col = income, fill = income, size = income_sq), shape = 21) +
	theme_minimal() +
	theme(legend.position="none")
