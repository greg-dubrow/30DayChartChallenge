library(tidyverse) # to do tidyverse things
library(tidylog) # to get a log of what's happening to the data
library(janitor) # tools for data cleaning
library(tidyverse) # to do tidyverse things
library(danstat) # package to get Danish statistics via api
library(waffle) # to make waffles
library(gt)
library(scales)
library(patchwork)
library(ggtext)


source("~/Data/r/basic functions.R")

# LABY19: Educational attainment (15-69 years) (number) by municipality groups,
# highest education completed and age
table_meta <- danstat::get_table_metadata(table_id = "laby19", variables_only = TRUE)
table_meta_dk <- danstat::get_table_metadata(table_id = "laby19", variables_only = TRUE, language = "da")

# create variable list using the ID value in the variable
variables_ed <- list(
	list(code = "komgrp", values = c(1, 2, 3, 4, 5)),
	list(code = "hfudd2", values = c("TOT", "H10", "H20", "H30", "H35",
		"H40", "H50", "H60", "H70", "H80", "H90")),
	list(code = "alder", values = "*"),
	list(code = "tid", values = 2023))

ed_urban1 <- get_data("laby19", variables_ed, language = "en") %>%
	as_tibble() %>%
	clean_names()

glimpse(ed_urban1)

ed_urban1 %>%
	count(komgrp)

ed_urban_main <- ed_urban1 %>%
	mutate(komgrp = str_remove(komgrp, " municipalities")) %>%
	mutate(alder = ifelse(alder == "Age, total", "Total", alder)) %>%
	mutate(alder =str_remove(alder, " years")) %>%
	mutate(age_group = case_when(
		alder %in% c("30-34", "35-39") ~ "30-39",
		alder %in% c("40-44", "45-49") ~ "40-49",
		alder %in% c("50-54", "55-59") ~ "50-59",
		alder %in% c("60-64", "65-69") ~ "60-69",
		TRUE ~ alder)) %>%
	mutate(hfudd2 = case_when(
		hfudd2 == "H10 Primary education" ~ "Primary",
		hfudd2 == "H20 Upper secondary education" ~
			"HS-Academic",
		hfudd2 == "H30 Vocational Education and Training (VET)" ~
			"HS-Vocational",
		hfudd2 == "H35 Qualifying educational programs" ~
			"Qualifying educ paths",
		hfudd2 == "H40 Short cycle higher education" ~
			"Short-cycle coll",
		hfudd2 == "H50 Vocational bachelors educations" ~
			"Bach-Vocational",
		hfudd2 == "H60 Bachelors programs" ~
			"Bach-Academic",
		hfudd2 == "H70 Masters programs" ~ "Masters",
		hfudd2 == "H80 PhD programs" ~ "PhD",
		hfudd2 == "H90 Not stated" ~ "Not stated",
		TRUE ~ hfudd2)) %>%
	select(muni_grp = komgrp, ed_highest = hfudd2, age = alder, age_group, N = indhold) %>%
	group_by(muni_grp, ed_highest, age_group) %>%
	mutate(N_age_group = sum(N)) %>%
	ungroup()


glimpse(ed_urban_main)

ed_urban_main %>%
	count(age, age_group)


# gt table with percent of all ages by municipality
ed_urban_main %>%
	filter(ed_highest == "Total") %>%
#	filter(age == "Total") %>%
	filter(!age %in% c("Total", "15-19", "20-24")) %>%
	select(muni_grp, N) %>%
	group_by(muni_grp) %>%
	mutate(N_muni_grp = sum(N)) %>%
	ungroup() %>%
	select(-N) %>%
	distinct(muni_grp, .keep_all = TRUE) %>%
	mutate(muni_pct = N_muni_grp / sum(N_muni_grp)) %>%
	gt() %>%
	cols_label(
		muni_grp = "Municipal type",
		N_muni_grp = "Pop: 25-69",
		muni_pct = "Pct of all DK") %>%
	# tab_style(
	# 	style = cell_text(align = "center"),
	# 	locations = cells_column_labels(
	# 		columns = c(dkk_million_2017, dkk_million_2023, pct_chg_2023))) %>%
	fmt_number(columns = N_muni_grp, sep_mark = ",", decimals = 0) %>%
	fmt_percent(columns = muni_pct, decimals = 1) %>%
	tab_footnote(footnote = md("*Data from Danmarks Statistik table LABY19 via danstat package.*")) %>%
	opt_stylize(style = 5)

# bar chart with municipality x axis, by age breakdown stacked bar

# bar chart with municipality x axis, by ed level breakdown stacked bar

# faceted bar charts by municipality, x axis is age group, stacked ed level pct
# filter out 15-24?
