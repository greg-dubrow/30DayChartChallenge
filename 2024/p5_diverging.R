## code for 30 Day Chart Challenge 2024, day 5 diverging
## educational attainment by sex as percentage by level. denmark ages 15-69 30th september of the year
## data from statistics danmark https://www.dst.dk/
## original table https://www.statbank.dk/statbank5a/SelectVarVal/Define.asp?Maintable=HFUDD11&PLanguage=1
# using danstat package https://github.com/ValeriVoev/danstat

library(tidyverse)
library(tidyverse) # to do tidyverse things
library(tidylog) # to get a log of what's happening to the data
library(janitor) # tools for data cleaning
library(danstat) # package to get Danish statistics via api
library(ggtext) # enhancements for text in ggplot

## Approach
# group by sex, count ed level
# sum of all ed levels is population
# divide level by total population
# diverging bar chart
# see code here https://peder.quarto.pub/blog/posts/apr_30day_p2/#april-day-5---diverging

# metadata for table variables
table_meta <- danstat::get_table_metadata(table_id = "hfudd11", variables_only = TRUE)

# create variable list
variables_ed <- list(
	list(code = "bopomr", values = "000"),
	list(code = "hfudd", values = c("H10", "H20", "H30", "H35",
														 "H40", "H50", "H60", "H70", "H80", "H90")),
	list(code = "køn", values = c("M","K")),
	list(code = "alder", values = c("25-29", "30-34", "35-39", "40-44", "45-49",
																	"50-54", "55-59", "60-64", "65-69")),
	list(code = "tid", values = 2023))

# past variable list along with table name
edattain <- get_data("hfudd11", variables_ed, language = "da") %>%
	as_tibble() %>%
	select(sex = KØN, age = ALDER, edlevel = HFUDD, n = INDHOLD)

glimpse(edattain)

edattain %>%
	count(edlevel)

edattain1 <- edattain %>%
	mutate(edlevel =
				 	factor(edlevel,
				 				 levels = c("H10 Grundskole", "H20 Gymnasiale uddannelser",
				 				 					 "H30 Erhvervsfaglige uddannelser", "H35 Adgangsgivende uddannelsesforløb",
				 				 					 "H40 Korte videregående uddannelser, KVU", "H50 Mellemlange videregående uddannelser, MVU",
				 				 					 "H60 Bacheloruddannelser, BACH", "H70 Lange videregående uddannelser, LVU",
				 				 					 "H80 Ph.d. og forskeruddannelser", "H90 Uoplyst mv."),
				 				 labels = c("Grundskole/Primary", "Gymnasium",
				 				 					 "Erhvervsfaglige/Vocational HS", "Adgangsgivende/Qualifying",
				 				 					 "KVU/2-year college", "MVU/Professional BA",
				 				 					 "Bachelor", "LVU/Masters", "PhD", "Not stated" ))) %>%
	mutate(sex = ifelse(sex == "Kvinder", "Kvinder/Women", "Mænd/Men")) %>%
	arrange(sex, edlevel) %>%
	group_by(sex) %>%
	mutate(tot_sex = sum(n)) %>%
	ungroup() %>%
	group_by(sex, edlevel) %>%
	mutate(tot_sex_edlev = sum(n)) %>%
	ungroup() %>%
	group_by(sex, age) %>%
	mutate(tot_sex_age = sum(n)) %>%
	ungroup() %>%
	mutate(level_pct = round(tot_sex_edlev / tot_sex, 3)) %>%
	mutate(level_pct = ifelse(sex == "Mænd/Men", level_pct *-1, level_pct)) %>%
	mutate(level_pct2 = round(level_pct * 100, 1)) %>%
	mutate(age_level_pct = round(n / tot_sex_age, 3)) %>%
	mutate(age_level_pct = ifelse(sex == "Mænd/Men", age_level_pct *-1, age_level_pct)) %>%
	mutate(age_level_pct2 = round(age_level_pct * 100, 1))

glimpse(edattain1)

edattain1 %>%
	filter(!edlevel == "Not stated") %>%
	distinct(sex, edlevel, .keep_all = TRUE) %>%
	select(sex, edlevel:tot_sex, level_pct, level_pct2 ) %>%
	{. ->> tmp} %>%
	ggplot() +
	geom_col(aes(x = -50, y = edlevel), width = 0.75, fill = "#e0e0e0") +
	geom_col(aes(x = 50, y = edlevel), width = 0.75, fill = "#e0e0e0") +
	geom_col(aes(x = level_pct2, y = edlevel, fill = sex, color = sex), width = 0.75) +
	scale_x_continuous(labels = function(x) abs(x), breaks = seq(-100, 100, 20)) +
	geom_vline(data = vlines_df, aes(xintercept = xintercept), color = "#FFFFFF", size = 0.1, alpha = 0.5) +
	coord_cartesian(clip = "off") +
	scale_fill_manual(values = c("#C8102E", "#FFFFFF")) +
	scale_color_manual(values = c("#C8102E", "#C8102E")) +
	geom_text(data = subset(tmp, sex == "Mænd/Men"),
						aes(x = level_pct2, y = edlevel, label = paste0(abs(level_pct2), "%")),
						size = 5, color = "#C8102E",
						hjust = 1, nudge_x = -.5) +
	geom_text(data = subset(tmp, sex == "Kvinder/Women"),
						aes(x = level_pct2, y = edlevel, label = paste0(abs(level_pct2), "%")),
						size = 5, color = "#C8102E",
						hjust = -.25) +
	labs(x = "", y = "",
			 title = "In Denmark, <span style = 'color: #C8102E;'>women</span> more likely than men for highest education
			 level to be Professional BA (MVU) or Masters.<br>Men more likely to stop at primary level or vocational secondary diploma.",
			 subtitle = "*Highest level of education attained by all people in Denmark aged 25-69 as of Sept 30 2023*",
			 caption = "*Data from Danmarks Statistik via danstat package*") +
	theme_minimal() +
	theme(panel.grid = element_blank(), plot.title = element_markdown(),
				plot.subtitle = element_markdown(), plot.caption = element_markdown(),
				legend.position = "none",
				axis.text.y = element_text(size = 10))
rm(tmp)

