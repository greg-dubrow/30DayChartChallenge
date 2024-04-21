## code for 30 Day Chart Challenge 2024, day 1 Part to Whole
## using Tidy Tuesday data https://github.com/rfordatascience/tidytuesday/blob/master/data/2024/2024-01-23/readme.md

## original data https://www.ons.gov.uk/peoplepopulationandcommunity/educationandchildcare/articles/whydochildrenandyoungpeopleinsmallertownsdobetteracademicallythanthoseinlargertowns/2023-07-25
  ## includes explanation of education index

## explanation of key stages https://www.gov.uk/national-curriculum
## explanation of levels https://www.gov.uk/what-different-qualification-levels-mean/list-of-qualification-levels

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

# check counts of regions and other categories
ukeduc %>%
	select(town11nm, ks4_2012_2013_counts, highest_level_qualification_achieved_by_age_22_less_than_level_1:popcheck) %>%
	view()

ukeduc %>%
	count(rgn11nm)

ukeduc %>%
	count(ttwa11nm) %>%
	view()

ukeduc %>%
	filter(is.na(ttwa_classification)) %>%
	view()

ukeduc %>%
	filter(town11cd == "E34000815") %>%
	select(highest_level_qualification_achieved_by_age_22_less_than_level_1:highest_level_qualification_achieved_b_age_22_average_score) %>%
	head()

## charts

# cohort population by region
ukeduc %>%
	rename(region =rgn11nm ) %>%
	group_by(region) %>%
	summarise(region_cohort = sum(ks4_2012_2013_counts),
						region_levless1_age22 = sum(n_lesslev1_age22),
						region_lev1to2_age22 = sum(n_lev1to2_age22),
						region_lev3to5_age22 = sum(n_lev3to5_age22),
						region_lev6plus_age22 = sum(n_lev6plus_age22),
						region_lev_na_age22 = sum(n_lev_na_age22)) %>%
	pivot_longer(cols = ends_with("age22"), names_to = "ed_ettain_age22", values_to = "ed_attain_n") %>%
	mutate(ed_ettain_age22 =
				 	factor(ed_ettain_age22,
				 				 levels = c("region_lev_na_age22", "region_levless1_age22", "region_lev1to2_age22",
				 				 					 "region_lev3to5_age22", "region_lev6plus_age22"),
				 				 labels = c("No data", "Level <1", "Level = 1-2",
				 				 					 "Level = 3-5", "Level = 6+"))) %>%
	mutate(ed_attain_pct = ed_attain_n / region_cohort) %>%
	mutate(ed_attain_pct2 = round(ed_attain_pct*100, 1)) %>%
	ungroup() %>%
	mutate(region = factor(region)) %>%
	filter(!is.na(region)) %>%
	{. ->> tmp} %>%
	ggplot(aes(ed_attain_pct, fct_rev(region), fill = fct_rev(ed_ettain_age22))) +
	geom_bar(stat = "identity") +
	scale_x_continuous(expand = c(0,0),
										 breaks = c(0, 0.25, 0.50, 0.75, 1),
										 labels = c("0", "25%", "50%", "75%", "100%")) +
	geom_text(data = subset(tmp, ed_attain_pct >0.025),
		aes(label = scales::percent(round(ed_attain_pct , 2))),
						position = position_stack(vjust = 0.5),
						color= "white", vjust = 1, size = 6) +
	labs(title = "Students in London most likely to have at least 4-year degree by Age 22",
		subtitle = "Sixth Year Educational Outcomes for Level 4 2012-13 Cohort by UK Region",
		caption = "*Tidy Tuesday data 01/23/2024, from UK Office of National Statistics*",
		x = "", y = "Region") +
	scale_fill_brewer(palette = "Set2") +
	theme_minimal() +
	theme(legend.position = "bottom", legend.spacing.x = unit(0, 'cm'),
				legend.key.width = unit(1.5, 'cm'),
				plot.title = element_text(hjust = 0.5), plot.caption = element_markdown(),
				panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
	guides(fill = guide_legend(label.position = "bottom", reverse = TRUE,
														 title = "Cohort at Age 22", title.position = "top"))
rm(tmp)



## UNUSED CODE
	# %>% mutate(level_sum = rowSums(.[c(26:29)]))
# scale_x_continuous(expand = c(0,0))
