
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
edattain1 <- get_data("hfudd11", variables_ed, language = "da") %>%
	as_tibble() %>%
	select(region = BOPOMR, age = ALDER, edlevel = HFUDD, n = INDHOLD)

edattain1 %>%
	count(edlevel)

# arrange data
# collapse age groups 25-39, 40-49, 50-59, 60-69
edattain <- edattain1 %>%
	mutate(age_group = case_when(age %in% c("25-29 år", "30-34 år", "35-39 år") ~ "25-39",
												age %in% c("40-44 år","45-49 år") ~ "40-49",
												age %in% c("50-54 år","55-59 år") ~ "50-59",
												age %in% c("60-64 år","65-69 år") ~ "60-69")) %>%
	mutate(ed_group = case_when(edlevel == "H10 Grundskole" ~ "Grundskole/Primary",
															edlevel %in% c("H20 Gymnasiale uddannelser",
																						 "H30 Erhvervsfaglige uddannelser",
																						 "H35 Adgangsgivende uddannelsesforløb") ~ "Secondary",
															edlevel == "H40 Korte videregående uddannelser, KVU" ~ "Tertiary - 2yr",
															edlevel %in% c("H50 Mellemlange videregående uddannelser, MVU",
																						 "H60 Bacheloruddannelser, BACH") ~ "Tertiary - Bachelor",
															edlevel == "H70 Lange videregående uddannelser, LVU" ~ "Tertiary - Masters",
															edlevel == "H80 Ph.d. og forskeruddannelser" ~ "Tertiary - PhD",
															edlevel == "H90 Uoplyst mv." ~ "Not stated")) %>%
	group_by(region, age_group, ed_group) %>%
	mutate(n2 = sum(n)) %>%
	ungroup() %>%
	select(-n, -age) %>%
	distinct(region, age_group, ed_group, .keep_all = T) %>%
	rename(n = n2) %>%
	mutate(ed_group =
				 	factor(ed_group,
				 				 levels = c("Grundskole/Primary", "Secondary", "Tertiary - 2yr",
				 				 "Tertiary - Bachelor", "Tertiary - Masters", "Tertiary - PhD", "Not stated")))

edattain %>%
	count(edlevel, ed_group)

## chart - all DK, horizontal bar age groups, percent stacks percent by deg level

edattain %>%
	filter(region == "Hele landet") %>%
	group_by(age_group) %>%
	mutate(age_total = sum(n)) %>%
	mutate(age_pct = round(n/age_total, 2)) %>%
	select(age_group, ed_group, age_pct) %>%
	{. ->> tmp} %>%
	ggplot(aes(age_pct, fct_rev(age_group), fill = fct_rev(ed_group))) +
	geom_bar(stat = "identity") +
	scale_x_continuous(expand = c(0,0),
										 breaks = c(0, 0.25, 0.50, 0.75, 1),
										 labels = c("0", "25%", "50%", "75%", "100%")) +
	geom_text(data = subset(tmp, age_pct >0.025),
						aes(label = scales::percent(round(age_pct , 2))),
						position = position_stack(vjust = 0.5),
						color= "white", vjust = 0.5, size = 14) +
	scale_fill_brewer(palette = "Set3") +
	labs(title = "Danes under 40 have a higher rate of post-Bachelor educational attainment than other age groups",
			 subtitle = "Highest education level attained by age groups",
			 caption = "*Data from Danmarks Statistik via danstat package*",
			 x = "", y = "Age Group") +
	theme_minimal() +
	theme(legend.position = "bottom", legend.spacing.x = unit(0, 'cm'),
				legend.key.width = unit(2.5, 'cm'), legend.margin=margin(-10, 0, 0, 0),
				legend.text = element_text(size = 10), legend.title = element_text(size = 12),
				plot.title = element_text(hjust = .5, size = 18),
				plot.subtitle = element_text(size = 16),
				plot.caption = element_markdown(size = 12, face = "italic"),
				axis.text.x = element_text(size = 14),
				axis.text.y = element_text(size = 14),
				panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
	guides(fill = guide_legend(label.position = "bottom", reverse = TRUE, direction = "horizontal",
														 nrow = 1,
														 title = "Highest Educational Attainment", title.position = "top")) +
rm(tmp)

ggsave("2025/images/prompt1_2025.jpg", width = 15, height = 8,
			 units = "in", dpi = 300)



