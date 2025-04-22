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
	list(code = "alder", values = "TOT"),
	list(code = "tid", values = "*"))

ed_urban1 <- get_data("laby19", variables_ed, language = "en") %>%
	as_tibble() %>%
	clean_names()

glimpse(ed_urban1)

ed_urban1 %>%
	count(hfudd2)

ed_urban_main <- ed_urban1 %>%
	mutate(komgrp = str_remove(komgrp, " municipalities")) %>%
	mutate(alder = ifelse(alder == "Age, total", "Total", alder)) %>%
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
	mutate(hfudd2 = factor(hfudd2,
		levels = c("Primary", "HS-Vocational", "HS-Academic", "Short-cycle coll",
			"Qualifying educ paths", "Bach-Vocational", "Bach-Academic",
			"Masters", "PhD", "Not stated", "Total"))) %>%
	mutate(komgrp = factor(komgrp,
		levels = c("Capital", "Metropolitan", "Commuter", "Provincial", "Rural"))) %>%
	select(muni_grp = komgrp, ed_highest = hfudd2, age = alder, year = tid, N = indhold)

glimpse(ed_urban_main)

ed_urban_main %>%
	count(ed_highest)


# stacked bar chart with pct of population by municipality, years on x axis
ed_urban_main %>%
	filter(ed_highest == "Total") %>%
	select(muni_grp, year, N) %>%
	group_by(year) %>%
	mutate(muni_grp_pct = N /sum(N)) %>%
	ungroup() %>%
	select(-N) %>%
	arrange(year, muni_grp) %>%
	{. ->> tmp} %>%
	ggplot(aes(x = year, y = muni_grp_pct, fill = fct_rev(muni_grp))) +
	geom_bar(position = "stack", stat = "identity") +
	scale_fill_brewer(palette = "Set3") +
	geom_text(aes(label = scales::percent(round(muni_grp_pct, 3))),
		position = position_stack(vjust = 0.5),
		color = "grey30", size = 5) +
	scale_y_continuous(limits = c(0, 1),
		labels = label_percent()) +
	scale_x_continuous(breaks = c(2008, 2011, 2013, 2015, 2017, 2019, 2021, 2023))+
	coord_cartesian(expand = FALSE, clip = "off") +
	labs(x = "", y = "",
		title = "Between 2008-2023 there has been an increase in the percentage of
		people living in larger Danish municipalities.",
		subtitle = "Percentage of people ages 15-69 by municipal group in Denmark, 2008-2023.",
		caption = "*Data from Danmarks Statistik table LABY19 via danstat package*") +
	theme(
		plot.title = element_markdown(size = 16), plot.subtitle = element_markdown(size = 12),
		plot.caption = element_markdown(size = 8),
		legend.position = "bottom", legend.justification = "left",
		legend.spacing.x = unit(0, 'cm'),
		legend.key.width = unit(1, 'cm'), legend.margin=margin(-12, 0, 0, 0),
		legend.title = element_text(size = 8), legend.text = element_text(size = 8),
		panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
	guides(fill = guide_legend(label.position = "bottom",
		title = "Municipality group", title.position = "top"))
	rm(tmp)

ggsave("2025/images/prompt20_1_2025.jpg", width = 15, height = 8,
	units = "in", dpi = 300)

ggsave("~/Data/greg_dubrow_io/posts/30-day-chart-challenge-2025/images/prompt20_1_2025.jpg",
	width = 15, height = 8, units = "in", dpi = 300)


# stacked bar chart with pct of population by municipality, years on x axis
ed_urban_main %>%
	filter(ed_highest == "Total") %>%
	select(muni_grp, year, N) %>%
	group_by(year) %>%
	mutate(muni_grp_pct = N /sum(N)) %>%
	ungroup() %>%
	select(-N) %>%
	arrange(year, muni_grp) %>%
	{. ->> tmp} %>%
	ggplot(aes(x = year, y = muni_grp_pct, fill = muni_grp)) +
	geom_bar(position = "stack", stat = "identity") +
	scale_fill_brewer(palette = "Set3") +
	geom_text(aes(label = scales::percent(round(muni_grp_pct, 3))),
		position = position_stack(vjust = 0.5),
		color = "grey20", size = 5) +
	scale_y_continuous(limits = c(0, 1),
		labels = label_percent()) +
	scale_x_continuous(breaks = c(2008, 2011, 2013, 2015, 2017, 2019, 2021, 2023))+
	coord_cartesian(expand = FALSE, clip = "off") +
	labs(x = "", y = "") +
	theme(
		plot.title = element_markdown(size = 16), plot.subtitle = element_markdown(size = 12),
		plot.caption = element_markdown(size = 8),
		legend.position = "bottom", legend.justification = "left",
		legend.spacing.x = unit(0, 'cm'),
		legend.key.width = unit(1, 'cm'), legend.margin=margin(-12, 0, 0, 0),
		legend.title = element_text(size = 8), legend.text = element_text(size = 8),
		panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
	guides(fill = guide_legend(label.position = "bottom",
		title = "Municipality group", title.position = "top"))
rm(tmp)

ggsave("2025/images/prompt20_no_2025.jpg", width = 15, height = 8,
	units = "in", dpi = 300)

ggsave("~/Data/greg_dubrow_io/posts/30-day-chart-challenge-2025/images/prompt20_no_2025.jpg",
	width = 15, height = 8, units = "in", dpi = 300)

## bar chart with pct ed attain with year on x axis
ed_urban_main %>%
	filter(!ed_highest == "Total") %>%
	select(muni_grp, ed_highest, year, N) %>%
	arrange(year, muni_grp, ed_highest) %>%
	group_by(year, muni_grp) %>%
	mutate(muni_year_N = sum(N)) %>%
	mutate(muni_ed_pct = N /sum(N)) %>%
	ungroup() %>%
	select(-N) %>%
	{. ->> tmp} %>%
	ggplot(aes(x = year, y = muni_ed_pct, fill = fct_rev(ed_highest))) +
	geom_bar(position = "stack", stat = "identity") +
#	scale_fill_brewer(palette = "Set3") +
	scale_fill_manual(values = c(
		"#BC80BD", "#D9D9D9",	"#FCCDE5", "#B3DE69", "#FDB462",
		"#80B1D3", "#FB8072", "#BEBADA", "#FFFFB3", "#8DD3C7")) +
	geom_text(
		data = subset(tmp, year %in% c(2008, 2015, 2023) & muni_ed_pct > 0.01),
		aes(label = scales::percent(muni_ed_pct, accuracy = 1)),
		position = position_stack(vjust = .5),
		color = "grey20", size = 3) +
	scale_y_continuous(labels = label_percent()) +
	scale_x_continuous(breaks = c(2008, 2011, 2013, 2015, 2017, 2019, 2021, 2023))+
	coord_cartesian(expand = FALSE, clip = "off") +
	facet_wrap(~muni_grp, scales = "free") +
	labs(x = "", y = "",
		title = "People in all municipal groups are earning higher levels of education.
		The Capital & Metropolitan groups have higher percentages of people with Bachelors & Masters.",
		subtitle = "Percentage of people ages 15-69 by municipal group in Denmark, 2008-2023.",
		caption = "*Data from Danmarks Statistik table LABY19 via danstat package*") +
	theme(
		plot.title = element_markdown(size = 14), plot.subtitle = element_markdown(size = 11),
		plot.caption = element_markdown(size = 8),
		axis.text.x = element_text(size = 7, color = "grey50"),
		axis.text.y = element_text(size = 7, color = "grey50"),
		strip.background.x = element_rect(fill = "grey90", color = "grey95"),
		legend.position = "bottom", legend.justification = "left",
		legend.spacing.x = unit(0, 'cm'),
		legend.key.width = unit(1, 'cm'), legend.margin=margin(-12, 0, 0, 0),
		legend.title = element_text(size = 8), legend.text = element_text(size = 8),
		panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
	guides(fill = guide_legend(label.position = "bottom", nrow = 1, reverse=T,
		title = "Highest level of education attained", title.position = "top"))


ggsave("2025/images/prompt20_2_2025.jpg", width = 15, height = 8,
	units = "in", dpi = 300)

ggsave("~/Data/greg_dubrow_io/posts/30-day-chart-challenge-2025/images/prompt20_2_2025.jpg",
	width = 15, height = 8, units = "in", dpi = 300)



####

# "#8DD3C7" "#FFFFB3" "#BEBADA" "#FB8072" "#80B1D3" "#FDB462"
# "#B3DE69" "#FCCDE5" "#1F78B4" "#BC80BD"
#
# "#A6CEE3" "#1F78B4" "#B2DF8A" "#33A02C" "#FB9A99" "#E31A1C"
# "#FDBF6F" "#FF7F00" "#CAB2D6" "#6A3D9A"
#


# mutate(muni_pct = N_muni_grp / sum(N_muni_grp)) %>%
# gt() %>%
# cols_label(
# 	muni_grp = "Municipal type",
# 	N_muni_grp = "Pop: 25-69",
# 	muni_pct = "Pct of all DK") %>%
# # tab_style(
# # 	style = cell_text(align = "center"),
# # 	locations = cells_column_labels(
# # 		columns = c(dkk_million_2017, dkk_million_2023, pct_chg_2023))) %>%
# fmt_number(columns = N_muni_grp, sep_mark = ",", decimals = 0) %>%
# fmt_percent(columns = muni_pct, decimals = 1) %>%
# tab_footnote(footnote = md("*Data from Danmarks Statistik table LABY19 via danstat package.*")) %>%
# opt_stylize(style = 5)
