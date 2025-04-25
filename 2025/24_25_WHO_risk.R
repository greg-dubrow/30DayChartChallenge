# WHO data on measles vaccination rates
# facet by global regions with lines of countries within. figure out which to highlight
# table MCV2 Indicator Id: 4756


library(tidyverse) # to do tidyverse things
library(tidylog) # to get a log of what's happening to the data
library(janitor) # tools for data cleaning
library(rgho) # gets WHO data
library(ggtext) # enhancements for text in ggplot
library(ggrepel)
library(scales)
library(patchwork)
library(gt)
library(ggiraph)

# some custom functions
source("~/Data/r/basic functions.R")


vignette("a-intro", "rgho")
vignette("b-dimensions", "rgho")
vignette("e-details", "rgho")

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
	# create vacc rate for better axis labels
	mutate(vacc_rate_pct = numeric_value / 100) %>%
	# create tooltip objects for plot
	mutate(tooltip1 = paste0(region_name, "; ", year, "; ", value, "%")) %>%
	select(id, indicator_code, data_level, parent_location_code, parent_location,
		country_code, country_name, region_code = region, region_name, year,
		numeric_value, vacc_rate_pct,
		tooltip1,
		global, worldbankincomegroup, everything())

glimpse(measles)

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
	ggobj = plot_region,
	options = list(
		opts_hover(css = ''), ## CSS code of line we're hovering over
		opts_hover_inv(css = "opacity:0.1;"), ## CSS code of all other lines
		opts_sizing(rescale = FALSE) ## Fixes sizes to dimensions below
	),
	height_svg = 6,
	width_svg = 9
)



## line = country, facet by region
# create plot object
#plot_region <-
	measles %>%
	filter(data_level == "country") %>%
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
	ggobj = plot_region,
	options = list(
		opts_hover(css = ''), ## CSS code of line we're hovering over
		opts_hover_inv(css = "opacity:0.1;"), ## CSS code of all other lines
		opts_sizing(rescale = FALSE) ## Fixes sizes to dimensions below
	),
	height_svg = 6,
	width_svg = 9
)
