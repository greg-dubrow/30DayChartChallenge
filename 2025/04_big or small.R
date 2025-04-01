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

# lons11
table_meta <- danstat::get_table_metadata(table_id = "lons11", variables_only = TRUE, language = "da")

# create variable list using the ID value in the variable
variables_ed <- list(
	list(code = "uddannelse", values = c("H10", "H20", "H30", "H40", "H50", "H60", "H70", "H80")),
	list(code = "sektor", values = c(1016, 1020, 1025, 1046)),
	list(code = "lønmål", values = "MDRSNIT"), # avg monthly
#	list(code = "køn", values = c("M", "K")),
  list(code = "afloen", values = c("TIME", "FAST")),
	list(code = "tid", values = 2023))


sal1 <- get_data("lons11", variables_ed, language = "en") %>%
	as_tibble() %>%
	clean_names()

glimpse(sal1)

sal1 %>%
	count(uddannelse)

sal2 <- sal1 %>%
	mutate(income = as.numeric(indhold)) %>%
	mutate(income = round(income, 2)) %>%
	mutate(sector = case_when(
		sektor == "Corporations and organizations" ~ "Private sector",
		sektor == "Government including social security funds" ~ "Gov't - National",
		sektor == "Municipal government" ~ "Gov't - Municipal",
		sektor == "Regional government" ~ "Gov't - Regional")) %>%
	mutate(sector =
				 	factor(sector,
				 				 levels = c("Gov't - Municipal", "Gov't - Regional",
				 				 					 "Gov't - National", "Private sector"))) %>%
	mutate(ed_level =
				 	case_when(uddannelse == "H10 Primary education" ~ "Primary",
				 						uddannelse == "H20 Upper secondary education" ~
				 							"HS-Academic",
				 						uddannelse == "H30 Vocational Education and Training (VET)" ~
				 							"HS-Vocational",
				 						uddannelse == "H40 Short cycle higher education" ~
				 							"Short-cycle college",
				 						uddannelse == "H50 Vocational bachelors educations" ~
				 							"Bachelor-Vocational",
				 						uddannelse == "H60 Bachelors programs" ~
				 							"Bachelor-Academic",
				 						uddannelse == "H70 Masters programs" ~ "Masters",
				 						uddannelse == "H80 PhD programs" ~ "PhD")) %>%
	mutate(ed_level =
				 	factor(ed_level,
				 				 levels = c("Primary", "HS-Academic", "HS-Vocational",
				 				 					 "Short-cycle college",
				 				 					 "Bachelor-Vocational", "Bachelor-Academic",
				 				 					 "Masters", "PhD")))

glimpse(sal2)

sal2 %>%
	count(uddannelse, ed_level)

sal2 %>%
#	filter(!sektor == "Municipal and regional government total") %>%
	ggplot(aes(x = sector, y = ed_level)) +
	geom_point(aes(col = income, fill = income, size = income), shape = 21) +
	theme_minimal() +
#	theme(legend.position="none") +
	scale_size_area(max_size = 15) +
	labs(x = "", y = "") +
	facet_wrap(~ afloen) +
	theme(legend.position = 'top',
				legend.justification = c(.95,0),
		text = element_text(color = 'grey40')) +
	guides(
		col = guide_none(),
		size = guide_none(),
		fill = guide_colorbar(
			barheight = unit(0.5, 'cm'),
			barwidth = unit(10, 'cm'),
			title.position = 'top')) +
	scale_fill_continuous(limit = c(25000, 80000),
		breaks = c(30000, 40000, 50000, 60000, 70000, 80000)) +
	geom_text(data = subset(sal2, !is.na(income)),
		aes(label =
					paste0(round(income, 0), " DKK")), nudge_x = 0.35)

#	scale_color_viridis_c(
#		trans = "log",
#		labels = scales::label_dollar(),
#		na.value = 'grey80') +
#	scale_fill_viridis_c(
#		trans = "log",
#		labels = scales::label_dollar(),
#		na.value = 'grey80')


####

#OVGARB10
table_meta <- danstat::get_table_metadata(table_id = "ovgarb10", variables_only = TRUE)

variables_ed <- list(
	list(code = "uddangroup", values = c("H10", "H15", "H21", "H31", "H40", "H50", "H60", "H70")),
	list(code = "uddstat", values = 0),
	list(code = "statusafg", values = c("001", "002")),
	list(code = "statustid", values = c("9M")),
	list(code = "alderlev", values = c("30-34", "35-39", "40-44", "45-49")),
	list(code = "tid", values = 2019))

edwork1 <- get_data("ovgarb10", variables_ed, language = "en") %>%
	as_tibble() %>%
	clean_names()

glimpse(edwork1)

edwork2 <- edwork1 %>%
	group_by(uddangroup, alderlev) %>%
	mutate(age_ed_tot = sum(indhold)) %>%
	mutate(age_ed_pct = indhold/age_ed_tot)

###

# UDDAKT60
table_meta <- danstat::get_table_metadata(table_id = "uddakt60", variables_only = TRUE)

# create variable list using the ID value in the variable
variables_ed <- list(
	list(code = "uddannelse", values = c("H6020", "H6025", "H6030", "H6035",
																			 "H6039", "H6059", "H6075", "H6080", "H6090")),
	list(code = "fstatus", values = c("F")),
	list(code = "køn", values = c("M", "K")),
	#list(code = "alder", values = c("TOT")),
	list(code = "tid", values = 2023))

degs1 <- get_data("uddakt60", variables_ed, language = "en") %>%
	as_tibble() %>%
	mutate(deg_field = case_when(UDDANNELSE == "H6020 Educational, BACH" ~ "Educ.",
															 UDDANNELSE == "H6025 Humanities and theological, BACH" ~ "Humanities",
															 UDDANNELSE == "H6030 Arts, BACH" ~ "Arts",
															 UDDANNELSE == "H6035 Science, BACH" ~ "Science",
															 UDDANNELSE == "H6039 Social Sciences, BACH" ~ "Social Science",
															 UDDANNELSE == "H6059 Technical sciences, BACH" ~ "Tech Science",
															 UDDANNELSE == "H6075 Food, biotechnology and laboratory technology, BACH"
															 ~ "Food/Biotech/LabTech",
															 UDDANNELSE == "H6080 Agriculture, nature and environment, BACH"
															 ~ "Agricultural Science",
															 UDDANNELSE == "H6090 Health science, BACH" ~ "Health Sciences")) %>%
	clean_names()

glimpse(degs1)

degs1 %>%
	ggplot(aes(x = kon, y = deg_field)) +
	geom_point(aes(col = indhold, fill = indhold, size = indhold), shape = 21) +
	theme_minimal() +
	theme(legend.position="none")

###

#LIGEUB5
table_meta <- danstat::get_table_metadata(table_id = "ligeub5", variables_only = TRUE)

variables_ed <- list(
	list(code = "startud", values = c("H21", "H31", "H40", "H50", "H60", "H70", "H80")),
	list(code = "stat", values = 6),
	list(code = "startald", values = c("18", "19", "20", "21", "22", "23", "24",
											"25", "26", "27", "28", "29",
											"30-34", "35-39", "40-44", "45-49", "50-")),
	list(code = "tid", values = 2018))

degs1 <- get_data("ligeub5", variables_ed, language = "en") %>%
	as_tibble() %>%
	clean_names()

glimpse(degs1)

degs1 %>%
	count(startud)

degs2 <- degs1 %>%
	mutate(age_group =
				 	case_when(startald %in% c("18 years", "19 years", "20 years", "21 years",
				 														"22 years", "23 years", "24 years") ~ "18-24",
				 						startald %in% c("25 years", "26 years", "27 years",
				 														"28 years", "29 years") ~ "25-29",
				 						startald == "50- years" ~ "50 +",
				 						TRUE ~ startald)) %>%
	mutate(age_group = str_remove(age_group, " years")) %>%
	mutate(deg_field = case_when(startud == "H21 Upper secondary education" ~ "Secondary - Academic",
															 startud == "H31 Vocational Education and Training (VET)"
															 ~ "Secondary - Vocational",
															 startud == "H40 Short cycle higher education"
															 ~ "Tertiary - Short cycle",
															 startud == "H50 Vocational bachelors educations"
															 ~ "Tertiary - Vocational",
															 startud == "H60 Bachelors programmes" ~ "Tertiary - Bachelor",
															 startud == "H70 Masters programmes" ~ "Masters",
															 startud == "H80 PhD programmes" ~ "Ph.D.")) %>%
	group_by(deg_field, age_group) %>%
	mutate(ed_age_n = sum(indhold)) %>%
	ungroup() %>%
	select(-stat, -tid, -startald, -indhold) %>%
	distinct(startud, age_group, .keep_all = T) %>%
	mutate(ed_age_n3 = ed_age_n*ed_age_n*ed_age_n)

glimpse(degs2)

degs2 %>%
	count(deg_field)

degs2 %>%
	filter(age_group %in% c("18-24", "25-29", "30-34", "35-39")) %>%
	ggplot(aes(x = age_group, y = deg_field)) +
	geom_point(aes(col = ed_age_n, fill = ed_age_n, size = ed_age_n3), shape = 21) +
	theme_minimal() +
	theme(legend.position="none")

####

#LABY52

table_meta <- danstat::get_table_metadata(table_id = "laby52", variables_only = TRUE)

variables_ed <- list(
	list(code = "komgrp", values = c(1, 2, 3, 4, 5)),
	list(code = "uddannelse", values = c("H40", "H50", "H60", "H70", "H80")),
	list(code = "tid", values = 2021))

dist1 <- get_data("laby52", variables_ed, language = "en") %>%
	as_tibble() %>%
	clean_names() %>%
	rename(km = indhold)

glimpse(dist1)

dist1 %>%
	ggplot(aes(x = komgrp, y = uddannelse)) +
	geom_point(aes(col = km, fill = km, size = km), shape = 21) +
	theme_minimal() +
	theme(legend.position="none")
