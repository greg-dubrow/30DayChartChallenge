# government appropriations to space and earth sciences over time
# actual numbers, percent change, percent of total
# in 2024 dollars

library(tidyverse) # to do tidyverse things
library(tidylog) # to get a log of what's happening to the data
library(janitor) # tools for data cleaning
library(tidyverse) # to do tidyverse things
library(danstat) # package to get Danish statistics via api
library(gt) # for making tables
library(gtExtras) # extra functions for gt
library(scales) # helps with labeling scales and text
library(ggtext)


source("~/Data/r/basic functions.R")

# FOUBUD5: Central Government appropriations for R&D by socio-economic objectives and type of appropriation; fixed prices. DKK million
# Unit : m DKK (fixed prices)
table_meta <- danstat::get_table_metadata(table_id = "foubud5", variables_only = TRUE)
table_meta_dk <- danstat::get_table_metadata(table_id = "foubud5", variables_only = TRUE, language = "da")

# create variable list using the ID value in the variable
variables_ed <- list(
	list(code = "forskformaal", values = "*"),
		#values = c("010", "020", "030", "130", "150", "200")),
	list(code = "bevilling1", values = 1),
	list(code = "tid", values = "*"))

funding1 <- get_data("foubud5", variables_ed, language = "en") %>%
	as_tibble() %>%
	clean_names()

glimpse(funding1)

funding1 %>%
	count(forskformaal)

funding_tot <- funding1 %>%
	select(-bevilling1) %>%
	filter(forskformaal == "Total") %>%
	mutate(dkk_mill = as.numeric(indhold)) %>%
	mutate(year_pct_chg = pctchange(dkk_mill)) %>%
	mutate(objective_pct_tot = 1) %>%
	select(objective = forskformaal, year = tid, objective_sum = dkk_mill, objective_pct_tot)

funding <- funding1 %>%
	select(-bevilling1) %>%
	filter(!forskformaal == "Total") %>%
	filter(!forskformaal == "General advancement of knowledge") %>%
	mutate(dkk_mill = as.numeric(indhold)) %>%
	mutate(objective = case_when(
		forskformaal == "Agriculture, forestry, hunting and construction" ~ "Fossils",
		forskformaal ==
			"Mining, trade and industry, building and construction and services" ~ "Fossils",
		forskformaal == "Production and distribution of energy" ~ "Fossils",
		forskformaal == "Exploration and exploitation of Earth and atmosphere" ~ "Stars",
		forskformaal == "Space research" ~ "Stars",
		TRUE ~ "Other"))%>%
	group_by(objective, tid) %>%
	mutate(objective_sum = sum(dkk_mill)) %>%
	ungroup() %>%
	distinct(objective, tid, .keep_all = TRUE) %>%
	select(objective, year = tid, objective_sum) %>%
	group_by(year) %>%
	mutate(objective_pct_tot = objective_sum/sum(objective_sum)) %>%
	ungroup() %>%
	rbind(funding_tot)%>%
	group_by(objective) %>%
	mutate(objective_pctchg = pctchange(objective_sum))

glimpse(funding)

saveRDS(funding, "~/Data/greg_dubrow_io/posts/30-day-chart-challenge-2025/data/prompt2122_funding.Rda")


# all gov't funding - one plot total, one plot pct change, patchwork

## try athletic charts
# three rows, fossils, stars, total
# cols 2023, 2024, yoy change spark lines for all years

spark_df <-
	funding %>%
	filter(!objective == "Other") %>%
	select(objective, year, objective_sum) %>%
	group_by(objective) %>%
	summarise(fund_df = list(objective_sum), .groups = "drop")

funding %>%
	filter(!objective == "Other") %>%
	filter(year %in% c(2023, 2024)) %>%
	select(objective, year, objective_sum) %>%
	pivot_wider(names_from = year, values_from = objective_sum) %>%
	mutate(pct_change = (`2024` - `2023`) / `2023`) %>%
	ungroup() %>%
	merge(spark_df) %>%
	mutate(objective = ifelse(objective == "Total", "All R&D", objective)) %>%
	gt() %>%
	gt_plt_sparkline(fund_df, type = "shaded") %>%
	cols_label(
		objective = (""),
		pct_change = ("% change"),
		fund_df = ("Trend 2007 - 2024")) %>%
	fmt_number(columns = c(`2023`, `2024`), decimals = 0) %>%
	fmt_percent(columns = pct_change, decimals = 1) %>%
	tab_header(
		title = "Government funding for R&D for Fossils & Stars",
		subtitle = md("*In millions of Danish kroner (DKK), constant 2024 amounts.*")) %>%
	tab_source_note(
		(md("*Data from Danmarks Statistik table FOUBUD5 via danstat package* "))) %>%
	gt_theme_nytimes()
	%>%
	gtsave("table1.png", "~/Data/greg_dubrow_io/posts/30-day-chart-challenge-2025/images")


## yearly funding for fossils & stars- one for each, patchwork

funding %>%
	filter(objective %in% c("Fossils", "Stars")) %>%
	filter(year < 2025) %>%
	ggplot(aes(x = year, y = objective_sum, color = objective)) +
	geom_line()+
	geom_line(size = 1.5) +
	scale_y_continuous(limits = c(0, 4500), breaks = seq(0, 4500, by = 500),
		labels = label_comma()) +
	scale_x_continuous(breaks = c(2007, 2010, 2015, 2020, 2024)) +
	labs(x = "", y = "",
		title = "More funding for fossil group than stars group. Groups are made up for the purposes of the chart prompt.",
		subtitle = "*Government budget allocations for R&D, 2007 to 2024. In millions of Danish kroner, constant 2024 amounts.*",
		caption = "*Data from Danmarks Statistik table FOUBUD5 via danstat package*") +
	theme_minimal() +
	theme(
		plot.title = element_markdown(size = 14), plot.subtitle = element_markdown(size = 11),
		plot.caption = element_markdown(size = 8),
		axis.text.x = element_text(size = 7, color = "grey50"),
		axis.text.y = element_text(size = 7, color = "grey50"),
		strip.background.x = element_rect(fill = "grey90", color = "grey95"),
		legend.position = "bottom", legend.justification = "left",
		legend.spacing.x = unit(0, 'cm'),
		legend.key.width = unit(1, 'cm'), legend.margin=margin(-12, 0, 0, 0),
		legend.title = element_blank(), legend.text = element_text(size = 8),
		panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
	guides(fill = guide_legend(label.position = "bottom", nrow = 1, reverse=T,
		title.position = "none", title = ""))

ggsave("2025/images/prompt21_22_1_2025.jpg", width = 15, height = 8,
	units = "in", dpi = 300)

ggsave("~/Data/greg_dubrow_io/posts/30-day-chart-challenge-2025/images/prompt21_22_1_2025.jpg",
	width = 15, height = 8, units = "in", dpi = 300)



# pct change over time
funding %>%
	filter(objective %in% c("Fossils", "Stars")) %>%
	filter(year < 2025) %>%
	ggplot(aes(x = year, y = objective_pctchg, color = objective)) +
	geom_line(size = 1.5) +
	scale_y_continuous(limits = c(-.6, .6), labels = label_percent()) +
	scale_x_continuous(breaks = c(2007, 2010, 2015, 2020, 2024)) +
	labs(x = "", y = "",
		title = "Year-over-year funding for these arbitrarily grouped research areas are kind of volatile.",
		caption = "*Data from Danmarks Statistik table FOUBUD5 via danstat package*") +
	theme_minimal() +
	theme(
		plot.title = element_markdown(size = 14), plot.subtitle = element_markdown(size = 11),
		plot.caption = element_markdown(size = 8),
		axis.text.x = element_text(size = 7, color = "grey50"),
		axis.text.y = element_text(size = 7, color = "grey50"),
		strip.background.x = element_rect(fill = "grey90", color = "grey95"),
		legend.position = "bottom", legend.justification = "left",
		legend.spacing.x = unit(0, 'cm'),
		legend.key.width = unit(1, 'cm'), legend.margin=margin(-12, 0, 0, 0),
		legend.title = element_blank(), legend.text = element_text(size = 8),
		panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
	guides(fill = guide_legend(label.position = "bottom", nrow = 1, reverse=T,
		title.position = "none", title = ""))


ggsave("2025/images/prompt21_22_2_2025.jpg", width = 15, height = 8,
	units = "in", dpi = 300)

ggsave("~/Data/greg_dubrow_io/posts/30-day-chart-challenge-2025/images/prompt21_22_2_2025.jpg",
	width = 15, height = 8, units = "in", dpi = 300)
