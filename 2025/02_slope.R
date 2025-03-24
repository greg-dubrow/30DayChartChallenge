#Change in educational attainment 2005 to 2023
  #Percent of danes with higher than HS? something else?

library(tidyverse) # to do tidyverse things
library(tidylog) # to get a log of what's happening to the data
library(janitor) # tools for data cleaning
library(danstat) # package to get Danish statistics via api
library(ggtext) # enhancements for text in ggplot
library(patchwork)

# some custom functions
source("~/Data/r/basic functions.R")

LIGEUB1
table_meta <- danstat::get_table_metadata(table_id = "ligeub1", variables_only = TRUE)

# create variable list using the ID value in the variable
variables_ed <- list(
	list(code = "bopomr", values = "000"),
	list(code = "hfudd", values = c("TOT", "H10", "H20", "H30", "H35",
																	"H40", "H50", "H60", "H70", "H80", "H90")),
	list(code = "køn", values = c("TOT")),
	list(code = "alder", values = c("25-29", "30-34", "35-39", "40-44", "45-49",
																	"50-54", "55-59", "60-64", "65-69")),
	list(code = "tid", values = c(2005, 2023)))

edattain1 <- get_data("ligeub1", variables_ed, language = "da") %>%
	as_tibble() %>%
	select(year = TID, age = ALDER, edlevel = HFUDD, n = INDHOLD)

# collapse age and education level groups
edattain <-
	edattain1 %>%
	filter(!edlevel == "I alt") %>%
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
	group_by(year, age_group, ed_group) %>%
	mutate(n2 = sum(n)) %>%
	ungroup() %>%
	select(-n, -age) %>%
	distinct(year, age_group, ed_group, .keep_all = T) %>%
	rename(n = n2) %>%
	group_by(year, age_group) %>%
	mutate(age_group_tot = sum(n)) %>%
	ungroup() %>%
	mutate(age_ed_pct = round(n / age_group_tot, 2)) %>%
	mutate(ed_group =
				 	factor(ed_group,
				 				 levels = c("Grundskole/Primary", "Secondary", "Tertiary - 2yr",
				 				 					 "Tertiary - Bachelor", "Tertiary - Masters",
				 				 					 "Tertiary - PhD", "Not stated")))
glimpse(edattain)

# for less cluttered display,
#    redo education level groups - combine 2 & 4 year undergrad, and masters & phd
edattain2 <- edattain %>%
	filter(!ed_group == "Not stated") %>%
	select(-age_group_tot, -age_ed_pct) %>%
	mutate(ed_group2 =
				 	case_when(
				 		ed_group %in% c("Tertiary - 2yr", "Tertiary - Bachelor") ~ "Tertiary - 2yr/Bach",
				 		ed_group %in% c("Tertiary - Masters", "Tertiary - PhD") ~ "Tertiary - Masters+",
				 		TRUE ~ ed_group)) %>%
	group_by(year, age_group, ed_group2) %>%
	mutate(n2 = sum(n)) %>%
	ungroup() %>%
	select(-ed_group, -n) %>%
	distinct(year, age_group, ed_group2, .keep_all = T) %>%
	rename(n = n2) %>%
	group_by(year, age_group) %>%
	mutate(age_group_tot = sum(n)) %>%
	ungroup() %>%
	mutate(age_ed_pct = round(n / age_group_tot, 2))


# create chart function
slope_graph <- function(plotdf) {
	plotdf %>%
	ggplot(aes(x = year, y = age_ed_pct, group = age_group, color = age_group)) +
		geom_line(size = 1) +
		geom_point(size = .2) +
		scale_x_continuous(breaks = c(2005, 2023),
											 labels = c("2005", "2023")) +
		scale_y_continuous(limits = c(0, .5),
			labels = scales::label_percent()) +
		scale_color_brewer(palette = "Set2") +
		labs(x = "", y = "") +
		theme_minimal() +
		theme(legend.position = "bottom", legend.spacing.x = unit(0, 'cm'),
					legend.key.width = unit(3, 'cm'), legend.margin=margin(-10, 0, 0, 0),
					legend.text = element_text(size = 10), legend.title = element_text(size = 12),
					plot.title = element_text(hjust = .5, size = 16),
					plot.subtitle = element_markdown(size = 14, vjust = -.5),
					plot.caption = element_markdown(size = 12, face = "italic"),
					axis.text.x = element_text(size = 11),
					axis.text.y = element_text(size = 11),
					panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
					strip.background = element_blank()) +
		guides(color = guide_legend(label.position = "bottom", reverse = FALSE, direction = "horizontal",
															 nrow = 1,
															 title = "Age Group", title.position = "top"))
}

edattain2 %>%
	count(ed_group2)

# create individual plots with titles and annotations
plot_grundsk <-
	edattain2 %>%
	filter(ed_group2 == "Grundskole/Primary") %>%
	slope_graph() +
	labs(title = "Primary school (thru grade 10)",
			 subtitle = "*In 2023 Danes of all ages were much less likely to have stopped their education at<br>primary school
			 than they were in 2005.<br>*")

plot_hs <-
	edattain2 %>%
	filter(ed_group2 == "Secondary") %>%
	slope_graph() +
	labs(title = "Gymnasium & Vocational (High School)",
			 subtitle = "*From 2005 to 2023, there was a steep decline in the percentage of Danes<br>
			 aged 25-49 who were finished with education at the high school level, especially<br>
			 Danes under 40. For Danes older than 50 there was a very slight increase.*")

plot_colldegs <-
	edattain2 %>%
	filter(ed_group2 == "Tertiary - 2yr/Bach") %>%
	slope_graph() +
	labs(title = "2-year & Bachelor's Degrees",
			 subtitle = "*For Danes of all age groups, but especially those under 50, there was a<br>
			 noticable increase in the percentage earning 2 or 4 year degrees.*")

plot_masters <-
	edattain2 %>%
	filter(ed_group2 == "Tertiary - Masters+") %>%
	slope_graph() +
	labs(title = "Master's & PhD Degrees",
			 subtitle = "*The percentage of Danes earning a Master's or PhD increased across all age groups between 2005 and 2025;<br>
			 the increase was strongest among Danes under 50.*")

plot_grundsk + plot_hs + plot_colldegs + plot_masters +
	plot_annotation(
		title = "Danes of all ages have become more likely to continue their education beyond primary level. Danes under 50 have over time become likely to earn a Master's.",
		subtitle = "*Highest level of education earned by age groups, in 2005 and 2023.*",
		caption = "*Data from Danmarks Statistik via danstat package. Groups are not longitudinal - age is for the person in the year of data collection.*") &
	theme(plot.title = element_text(size = 16), plot.subtitle = element_markdown(),
		plot.caption = element_markdown(),
		plot.background = element_rect(colour = "grey", fill=NA))

ggsave("2025/images/prompt2_2025.jpg", width = 15, height = 8,
			 units = "in", dpi = 300)


### code ideas not used in final
## faceting plots...not using
# edattain2 %>%
# 	ggplot(aes(x = year, y = age_ed_pct, group = age_group, color = age_group)) +
# 	geom_line(size = 1) +
# 	geom_point(size = .2) +
# 	scale_x_continuous(breaks = c(2005, 2023),
# 										 labels = c("2005", "2023")) +
# 	#	scale_y_continuous(limits = c(0, .5), breaks = seq(0, .5, by = .1)) +
# 	scale_y_continuous(#limits = c(0, .5),
# 		labels = scales::label_percent()) +
# 	scale_color_brewer(palette = "Set2") +
# 	labs(x = "", y = "") +
# 	facet_wrap(vars(ed_group2)) +
# 	theme_minimal() +
# 	theme(legend.position = "bottom", legend.spacing.x = unit(0, 'cm'),
# 				legend.key.width = unit(4, 'cm'), legend.margin=margin(-10, 0, 0, 0),
# 				legend.text = element_text(size = 12), legend.title = element_text(size = 16),
# 				plot.title = element_text(hjust = .5, size = 20),
# 				plot.subtitle = element_text(size = 16),
# 				plot.caption = element_markdown(size = 12, face = "italic"),
# 				axis.text.x = element_text(size = 14),
# 				axis.text.y = element_text(size = 14),
# 				panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
# 				strip.background = element_blank()) +
# 	guides(color = guide_legend(label.position = "bottom", reverse = FALSE, direction = "horizontal",
# 															nrow = 1,
# 															title = "Age Group", title.position = "top"))

# %>%
# 	mutate(age =
# 				 	factor(age,
# 				 				 levels = c("25-29 år", "30-34 år", "35-39 år",
# 				 				 					 "40-44 år", "45-49 år", "50-54 år", "55-59 år",
# 				 				 					 "60-64 år", "65-69 år"),
# 				 				 labels = c("25-29", "30-34", "35-39",
# 				 				 					 "40-44", "45-49", "50-54", "55-59",
# 				 				 					 "60-64", "65-69")))

# slope_graph_m <- function(edgroup, charttitle) {
# 	g <-
# 		ggplot(data, aes(x = year, y = age_ed_pct, group = age_group, color = age_group)) +
# 		geom_line(size = 1) +
# 		geom_point(size = .2) +
# 		scale_x_continuous(breaks = c(2005, 2023),
# 											 labels = c("2005", "2023")) +
# 		scale_y_continuous(labels = scales::label_percent()) +
# 		scale_color_brewer(palette = "Set2") +
# 		labs(x = "", y = "") +
# 		theme(legend.position = "bottom", legend.spacing.x = unit(0, 'cm'),
# 					legend.key.width = unit(4, 'cm'), legend.margin=margin(-10, 0, 0, 0),
# 					legend.text = element_text(size = 12), legend.title = element_text(size = 16),
# 					plot.title = element_text(hjust = .5, size = 20),
# 					plot.subtitle = element_text(size = 16),
# 					plot.caption = element_markdown(size = 12, face = "italic"),
# 					axis.text.x = element_text(size = 14),
# 					axis.text.y = element_text(size = 14),
# 					panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
# 					strip.background = element_blank()) +
# 		guides(fill = guide_legend(label.position = "bottom", reverse = TRUE, direction = "horizontal",
# 															 nrow = 1,
# 															 title = "Age Group", title.position = "top"))
# 	return(g)
# }
#
# edgroups <- unique(edattain2$ed_group2)
#
# patchwork::wrap_plots(
# 	map(edgroups, ~slope_graph_m(edgroup = .x, charttitle = .x)),
# 	widths = 6, heights = 6)
