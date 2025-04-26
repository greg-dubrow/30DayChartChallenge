# WHO data on measles vaccination rates
# facet by global regions with lines of countries within. figure out which to highlight
# table MCV2 Indicator Id: 4756


library(tidyverse) # to do tidyverse things
library(tidylog) # to get a log of what's happening to the data
library(janitor) # tools for data cleaning
library(rgho) # gets WHO data
library(ggtext) # enhancements for text in ggplot
library(ggthemes) # more themes and colour palettes
library(ggrepel)
library(scales)
library(patchwork)
library(gt)
library(ggiraph)

# some custom functions
source("~/Data/r/basic functions.R")

# country abbrvs to longer names. join to main table on abbrv
who_country <- get_gho_values(dimension = "COUNTRY") %>%
	clean_names() %>%
	select(country_code = code, country_name = title)

glimpse(who_country)

measles1 <- get_gho_data(code = "MCV2") %>%
	clean_names()

glimpse(measles1)

measles1 %>%
	count(region, parent_location_code, parent_location,
		global, worldbankincomegroup) %>%
	view()

## data notes - region is a filter for region-wide numbers. parent_location is to
# match region to country

measles <- measles1 %>%
	rename(country_code = country) %>%
	left_join(who_country) %>%
	# fix country names
	mutate(country_name = ifelse(country_name == "Netherlands (Kingdom of the)",
		"Netherlands", country_name)) %>%
	# make it easier to filter by region or country level
	mutate(data_level = case_when(
		!is.na(region) ~ "region",
		!is.na(worldbankincomegroup) ~ "worldbank",
		!is.na(global) ~ "global",
		TRUE ~ "country")) %>%
	# add name to region
	mutate(region_name = case_when(
		region == "AFR" ~	 "Africa",
		region == "AMR" ~	"Americas",
		region == "EMR" ~	"Eastern Mediterranean",
		region == "EUR" ~	"Europe",
		region == "SEAR" ~ "South-East Asia",
		region == "WPR" ~ "Western Pacific",
		TRUE ~ NA)) %>%
	# create euro regions based on UN GeoScheme
	  # https://en.wikipedia.org/wiki/United_Nations_geoscheme
	mutate(region_europe = case_when(
		country_name %in% c("Belarus", "Bulgaria", "Czechia", "Hungary", "Poland", "Republic of Moldova",
			"Romania", "Russian Federation", "Slovakia", "Ukraine") ~ "Eastern Europe",
		country_name %in% c("Åland Islands", "Denmark", "Estonia", "Faroe Islands", "Finland", "Iceland",
			"Ireland", "Isle of Man", "Latvia", "Lithuania", "Norway", "Svalbard and Jan Mayen Islands",
			"Sweden", "United Kingdom of Great Britain and Northern Ireland") ~ "Northern Europe",
		country_name %in% c("Albania", "Andorra", "Bosnia and Herzegovina", "Croatia", "Gibraltar", "Greece",
			"Holy See", "Italy", "Malta", "Montenegro", "North Macedonia", "Portugal", "San Marino",
			"Serbia", "Slovenia", "Spain") ~ "Southern Europe",
		country_name %in% c("Austria", "Belgium", "France", "Germany", "Liechtenstein", "Luxembourg", "Monaco",
			"Netherlands", "Switzerland") ~ "Western Europe",
		TRUE ~ "Not Europe")) %>%
	# more country name fixes for easier labeling
	mutate(country_name = ifelse(country_name ==
	"United Kingdom of Great Britain and Northern Ireland",
		"United Kingdom", country_name)) %>%
	mutate(country_name = ifelse(
		country_name == "Republic of Moldova", "Moldova", country_name)) %>%
	# create vacc rate for better axis labels
	mutate(vacc_rate_pct = numeric_value / 100) %>%
	# create tooltip objects for plot
	mutate(tooltip1 = paste0(region_name, "; ", year, "; ", value, "%")) %>%
	mutate(tooltip2 = paste0(country_name, "; ", year, "; ", value, "%")) %>%
	select(id, indicator_code, data_level, parent_location_code, parent_location,
		country_code, country_name, region_code = region, region_name, region_europe,
		year, numeric_value, vacc_rate_pct,
		tooltip1, tooltip2,
		global, worldbankincomegroup, everything())

glimpse(measles)

saveRDS(measles,
	"~/Data/greg_dubrow_io/posts/30-day-chart-challenge-2025/data/measles.Rda")


measles %>%
	filter(parent_location == "Europe") %>%
	count(region_europe, country_name) %>%
	view()

measles %>%
	count(region, region_name) %>%
	view()

measles %>%
	count(parent_location_code, parent_location) %>%
	view()



## by region
# create plot object
plot_region <-
	measles %>%
	filter(data_level == "region") %>%
#	select(region_name, numeric_value, year) %>%
	ggplot(aes(
		x = year, y = vacc_rate_pct, col = region_name, data_id = region_name)) +
	geom_line_interactive(linewidth = 1) +
	geom_point_interactive(
		aes(tooltip = tooltip1),
		size = 2,
		stroke = 1.5) +
	scale_color_brewer(palette = "Set3") +
	scale_x_continuous(breaks = c(2000, 2005, 2010, 2015, 2020, 2023)) +
	scale_y_continuous(
		limits = c(0,1),
		labels = label_percent(),
		breaks = pretty_breaks()) +
	labs(x = "", y = "",
		title = "Measles vaccine 2nd dose, percent covered by local age recomendation. By global region.",
		subtitle = "*Hover over dots to highlight region line and show year & pct vaccinated.*",
		caption = "*Data from World Health Organization, MCV2 Indicator*") +
#	theme_minimal() +
	theme(
		plot.title = element_markdown(size = 14),
		plot.subtitle = element_markdown(size = 12),
		plot.caption = element_markdown(size = 8),
		legend.position = "bottom", legend.justification = "left",
		legend.spacing.x = unit(0, 'cm'),
		legend.key.width = unit(1, 'cm'), legend.margin=margin(-10, 0, 0, 0),
		legend.title = element_text(size = 8), legend.text = element_text(size = 8),
		panel.background = element_rect(fill = "#F2DFCE"),
		panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
	guides(col = guide_legend(
		label.position = "bottom", nrow = 1,
		title = "Region", title.position = "top"))

# crate interactive object of plot
girafe(
	ggobj = plot_europe,
	options = list(
		opts_hover(css = ''), ## CSS code of line we're hovering over
		opts_hover_inv(css = "opacity:0.1;"), ## CSS code of all other lines
		opts_sizing(rescale = FALSE), ## Fixes sizes to dimensions below
		opts_tooltip(
			opacity = 0.8, #opacity of the background box
			css = "background-color:#4c6061; color:white;")),
	height_svg = 6,
	width_svg = 9)

measles %>%
	count(region_europe, country_name) %>%
	view()

## line = country, facet by region
# europe only

# create custom color scale
color_europe <-
	c(
		"Belarus" = "#4E79A7", "Bulgaria" = "#F28E2B", "Czechia" = "#59A14F", "Hungary" = "#FF9D9A",
		"Moldova" = "#B07AA1", "Poland" = "#9D7660", "Romania" = "#499894", "Russian Federation" = "#F1CE63",
		"Slovakia" = "#A0CBE8", "Ukraine" = "#FFBE7D", "Denmark" = "#B6992D", "Estonia" = "#86BCB6",
		"Finland" = "#D4A6C8", "Iceland" = "#4E79A7", "Latvia" = "#F28E2B", "Lithuania" = "#59A14F",
		"Norway" = "#FF9D9A", "Sweden" = "#B07AA1", "United Kingdom" = "#9D7660", "Albania" = "#499894",
		"Andorra" = "#F1CE63", "Bosnia and Herzegovina" = "#A0CBE8", "Croatia" = "#FFBE7D", "Greece" = "#B6992D",
		"Italy" = "#86BCB6", "Malta" = "#D4A6C8", "Montenegro" = "#4E79A7", "North Macedonia" = "#F28E2B",
		"Portugal" = "#59A14F", "San Marino" = "#FF9D9A", "Serbia" = "#B07AA1", "Slovenia" = "#9D7660",
		"Spain" = "#E15759", "Austria" = "#499894", "Belgium" = "#F1CE63", "France" = "#A0CBE8",
		"Germany" = "#FFBE7D", "Luxembourg" = "#B6992D", "Monaco" = "#86BCB6", "Netherlands" = "#D4A6C8",
		"Switzerland" = "#4E79A7")

# create plot object
plot_europe <-
	measles %>%
	filter(data_level == "country") %>%
	filter(!region_europe == "Not Europe") %>%
#	filter(country_name == "Denmark") %>%
	 # select(year, numeric_value, vacc_rate_pct, region_europe, country_name) %>%
	 # 	view()
	ggplot(aes(
		x = year, y = vacc_rate_pct, col = country_name, data_id = country_name)) +
	geom_line_interactive(linewidth = 1) +
	geom_point_interactive(
		aes(tooltip = tooltip2),
		size = 2,
		stroke = 1.5) +
#	scale_color_tableau(palette = "Tableau 20", type = "regular") +
		scale_color_manual(values = color_europe) +
		scale_x_continuous(breaks = c(2000, 2005, 2010, 2015, 2020, 2023)) +
	scale_y_continuous(
		limits = c(0,1),
		labels = label_percent(),
		breaks = pretty_breaks()) +
	facet_wrap(~ region_europe) +
#	theme(legend.position = "none") +
	labs(x = "", y = "",
		title = "Measles vaccine 2nd dose, percent covered by local age recomendation. By Euro regions.",
		subtitle = "*Hover over dots to highlight country line and show country name, year & pct vaccinated.*",
		caption = "*Data from World Health Organization, MCV2 Indicator. Euro regions per UN GeoSchema*") +
	#	theme_minimal() +
	theme(
		plot.title = element_markdown(size = 14),
		plot.subtitle = element_markdown(size = 12),
		plot.caption = element_markdown(size = 8),
		legend.position = "none",
		# legend.justification = "left",
		# legend.spacing.x = unit(0, 'cm'),
		# legend.key.width = unit(1, 'cm'), legend.margin=margin(-10, 0, 0, 0),
		# legend.title = element_text(size = 8), legend.text = element_text(size = 8),
		#panel.background = element_rect(fill = "#F2DFCE"),
		panel.background = element_rect(fill = "white", color = "grey"),
		panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# crate interactive object of plot
girafe(
	ggobj = plot_europe,
	options = list(
		opts_hover(css = ''), ## CSS code of line we're hovering over
		opts_hover_inv(css = "opacity:0.1;"), ## CSS code of all other lines
		opts_sizing(rescale = FALSE), ## Fixes sizes to dimensions below
		opts_tooltip(
			opacity = 0.8, #opacity of the background box
			css = "background-color:#4c6061; color:white;")),
	height_svg = 6,
	width_svg = 9)


###
# "#BC80BD", "#D9D9D9",	"#FCCDE5", "#B3DE69", "#FDB462",
# "#80B1D3", "#FB8072", "#BEBADA", "#FFFFB3", "#8DD3C7"
#
#
# palettes <- ggthemes_data[["tableau"]][["color-palettes"]][["regular"]]
# palettes[[2]]
