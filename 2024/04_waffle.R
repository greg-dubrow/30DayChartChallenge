## code for 30 Day Chart Challenge 2024, day 1 Part to Whole
## using Tidy Tuesday data https://github.com/rfordatascience/tidytuesday/blob/master/data/2024/2024-01-23/readme.md
## redo of prompt 1, now to waffle

library(tidyverse) # to do tidyverse things
library(tidylog) # to get a log of what's happening to the data
library(janitor) # tools for data cleaning
library(tidytuesdayR) #to get data from tidy tuesday repo

# some custom functions
source("~/Data/r/basic functions.R")

## ggplot helpers - load if necessary
library(patchwork) # to stitch together plots
library(ggtext) # helper functions for ggplot text
library(ggrepel) # helper functions for ggplot text

# load tidy tuesday data
ukeduc1 <- tt_load("2024-01-23")

# get variable names
ukeduc_names <- as_tibble(names(ukeduc1$english_education))

# create tibble from csv, clean data
ukeduc <- as_tibble(ukeduc1$english_education) %>%
	mutate(town11nm = ifelse(town11nm == "Outer london BUAs", "Outer London BUAs", town11nm)) %>%
	mutate(
		ttwa_classification = ifelse(town11nm %in% c("Inner London BUAs", "Outer London BUAs"),
																 "Majority conurbation", ttwa_classification)) %>%
	mutate(ttwa11nm = ifelse(town11nm %in% c("Inner London BUAs", "Outer London BUAs"),
													 "London", ttwa11nm)) %>%
	mutate(ttwa11cd = ifelse(town11nm %in% c("Inner London BUAs", "Outer London BUAs"),
													 "E30000234", ttwa11cd)) %>%
	mutate(across(26:29, ~ifelse(is.na(.),0,.))) %>%
	mutate(level_sum = rowSums(.[c(26:29)])) %>%
	mutate(highest_level_qualification_achieved_by_age_22_na = 100 - level_sum) %>%
	mutate(n_lesslev1_age22 =
				 	round(highest_level_qualification_achieved_by_age_22_less_than_level_1 * (ks4_2012_2013_counts/100) ,0)) %>%
	mutate(n_lev1to2_age22 =
				 	round(highest_level_qualification_achieved_by_age_22_level_1_to_level_2 * (ks4_2012_2013_counts/100) ,0)) %>%
	mutate(n_lev3to5_age22 =
				 	round(highest_level_qualification_achieved_by_age_22_level_3_to_level_5 * (ks4_2012_2013_counts/100) ,0)) %>%
	mutate(n_lev6plus_age22 =
				 	round(highest_level_qualification_achieved_by_age_22_level_6_or_above * (ks4_2012_2013_counts/100) ,0)) %>%
	mutate(n_lev_na_age22 =
				 	round(highest_level_qualification_achieved_by_age_22_na * (ks4_2012_2013_counts/100) ,0))

glimpse(ukeduc)

## charts

# cohort population by region
ukeduc2 <-
	ukeduc %>%
	rename(region =rgn11nm ) %>%
	group_by(region) %>%
	summarise(region_cohort = sum(ks4_2012_2013_counts),
						region_levless1_age22 = sum(n_lesslev1_age22),
						region_lev1to2_age22 = sum(n_lev1to2_age22),
						region_lev3to5_age22 = sum(n_lev3to5_age22),
						region_lev6plus_age22 = sum(n_lev6plus_age22)) %>%
	pivot_longer(cols = ends_with("age22"), names_to = "ed_attain_age22", values_to = "ed_attain_n") %>%
	mutate(ed_attain_pct = ed_attain_n / region_cohort) %>%
	mutate(ed_attain_pct2 = round(ed_attain_pct*100)) %>%
	mutate(region = factor(region))%>%
	filter(!is.na(region)) %>%
	select(region, region_cohort, ed_attain_age22, ed_attain_pct2) %>%
	pivot_wider(names_from = ed_attain_age22, values_from = ed_attain_pct2) %>%
	mutate(region_sum = rowSums(.[c(3:6)])) %>%
	mutate(region_lev_na_age22 = ifelse(region_sum < 100, 100 - region_sum, 0)) %>%
	select(-region_sum) %>%
	pivot_longer(cols = ends_with("age22"), names_to = "ed_attain_age22", values_to = "ed_attain_pct") %>%
	mutate(ed_attain_age22 =
			 	factor(ed_attain_age22,
			 				 levels = c("region_lev_na_age22", "region_levless1_age22", "region_lev1to2_age22",
			 				 					 "region_lev3to5_age22", "region_lev6plus_age22"),
			 				 labels = c("No data", "Level <1", "Level = 1-2",
			 				 					 "Level = 3-5", "Level = 6+")))
glimpse(ukeduc2)

ukeduc2	%>%
	ggplot(aes(fill = ed_attain_age22, values = ed_attain_pct)) +
	geom_waffle(na.rm=TRUE, n_rows=10, flip=TRUE, size = 0.33, colour = "white") +
	facet_wrap(~region, nrow=1,strip.position = "bottom") +
	scale_x_discrete() +
	scale_y_continuous(labels = function(x) x * 10, # make this multiplyer the same as n_rows
										 expand = c(0,0)) +
	scale_fill_brewer(palette = "Set2") +
		labs(title = "Students in London most likely to have at least 4-year degree by Age 22",
				 subtitle = "Sixth Year Educational Outcomes for Level 4 2012-13 Cohort by UK Region<br>
				 Each block = 1 %",
				 caption = "*Tidy Tuesday data 01/23/2024, from UK Office of National Statistics*",
				 x = "", y = "") +
	theme_minimal() +
		theme(legend.position = "bottom", legend.spacing.x = unit(0, 'cm'),
					legend.key.width = unit(1.5, 'cm'), legend.margin=margin(-10, 0, 0, 0),
					plot.title = element_text(hjust = 0), plot.subtitle = element_markdown(),
					plot.caption = element_markdown(),
					panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
	guides(fill = guide_legend(label.position = "bottom",
														 title = "Cohort at Age 22", title.position = "top"))
