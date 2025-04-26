## redo maps from 2024 hazards
## ed attainment rates by region, diff map for each level

library(tidyverse) # to do tidyverse things
library(tidylog) # to get a log of what's happening to the data
library(janitor) # tools for data cleaning
library(danstat) # package to get Danish statistics via api
library(sf) # for mapping
library(ggiraph) # for interactive hoverover
library(giscoR) # get region borders
library(ggtext) # enhancements for text in ggplot
library(patchwork) # stitching plots together
library(classInt)

# don't think i need this
#library(eurostat)

# some custom functions
source("~/Data/r/basic functions.R")

### Get mapping data
# 1. NUTS2 & country shapefile
#-----------------------------
# define longlat projection
crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

nuts3_dk <- giscoR::gisco_get_nuts(
	year = "2021",	resolution = "3",
	nuts_level = "3", country = "DK") |>
	rename(province_name = NAME_LATN) |>
	sf::st_transform(crsLONGLAT)

plot(sf::st_geometry(nuts3_dk))
glimpse(nuts3_dk)

nuts3_dk %>%
	count(NUTS_NAME)

## data
# metadata for table variables, click thru nested tables to find variables and ids for filters
# table_meta <- danstat::get_table_metadata(table_id = "hfudd11", variables_only = TRUE)

## because I want to do by province but that's not available in API, need to load spreadsheet
edattain1 <- readxl::read_excel("2025/data/edattain_province_dk_2023.xlsx") %>%
	clean_names()

glimpse(edattain1)

edattain1 %>%
	count(province)

edattain_maps1 <- edattain1 %>%
	filter(!is.na(province)) %>%
	# fill in missing age group names
	fill(age_group) %>%
	# fix province names
	mutate(province = str_remove(province, "Province ")) %>%
	mutate(province = case_when(
		province == "Byen K¯benhavn" ~ "Byen København",
		province == "K¯benhavns omegn" ~ "Københavns omegn",
		province == "NordsjÊlland" ~ "Nordsjælland",
		province == "Vest- og SydsjÊlland" ~ "Vest- og Sydsjælland",
		province == "ÿstjylland" ~ "Østjylland",
		province == "ÿstsjÊlland" ~ "Østsjælland",
		TRUE ~ province)) %>%
	# create ed level total counts by province
	group_by(province) %>%
	mutate(Primary = sum(h10_primary_education)) %>%
	mutate(`HS Academic` = sum(h20_upper_secondary_education)) %>%
	mutate(`HS Vocational` = sum(h30_vocational_education_and_training_vet)) %>%
	mutate(`Short cycle college` = sum(h40_short_cycle_higher_education)) %>%
	mutate(`Bachelor Vocational` = sum(h50_vocational_bachelors_educations)) %>%
	mutate(`Bachelor Academic` = sum(h60_bachelors_programs)) %>%
	mutate(Masters = sum(h70_masters_programs)) %>%
	mutate(PhD = sum(h80_ph_d_programs)) %>%
	mutate(`Not stated` = sum(h90_not_stated)) %>%
	ungroup() %>%
	distinct(province, .keep_all = TRUE) %>%
	select(province_name = province, Primary:`Not stated`) %>%
	# make the data long
	pivot_longer(!province_name, names_to = "ed_level", values_to = "ed_level_n") %>%
	group_by(province_name) %>%
	mutate(ed_level_pct = ed_level_n /sum(ed_level_n)) %>%
	ungroup() %>%
	mutate(ed_level = factor(ed_level,
		levels = c("Primary", "HS Vocational", "HS Academic", "Short cycle college",
			"Bachelor Vocational", "Bachelor Academic", "Masters", "PhD", "Not stated"))) %>%
	mutate(ed_level_pct_r = round(ed_level_pct, 2) * 100) %>%
	mutate(ed_level_pct_c = as.character(ed_level_pct_r)) %>%
	mutate(tooltip1 = paste0(province_name, "; ", ed_level_pct_c, "%"))

glimpse(edattain_maps1)

edattain_maps1 %>%
	count(ed_level)

edattain_maps2 <- edattain_maps1 %>%
	filter(!province_name == "All Denmark")

edattain_maps <- nuts3_dk %>%
	left_join(edattain_maps2, by = "province_name")

saveRDS(edattain_maps,
	"~/Data/greg_dubrow_io/posts/30-day-chart-challenge-2025/data/edattain_maps.Rda")

glimpse(edattain_maps)

edattain_maps %>%
	count(ed_level)

# test map
#test_map <-
ggplot() +
	geom_sf_interactive(data = subset(edattain_maps, ed_level == "HS Vocational"),
		aes(fill = ed_level_pct, tooltip = tooltip1),
		color = "#FFFFFF", size = 4) +
	geom_sf_text(data = (edattain_maps %>%
			filter(province_name %notin% c("Byen København", "Københavns omegn")) %>%
			filter(ed_level == "HS Vocational")),
		aes(label = province_name), color = "white", size = 3)	+
	ggsflabel::geom_sf_label_repel(data = (edattain_maps %>%
			filter(province_name %in% c("Byen København", "Københavns omegn")) %>%
			filter(ed_level == "HS Vocational")),
		aes(label = province_name),
		force = 1, nudge_x = 1.5, nudge_y = .5, size = 2) +
	# scale_fill_gradient(
	# #	limits = c(min(tmp$crime_per1k), max(tmp$crime_per1k)),
	# 	low = "blue", high = "yellow") +
	scale_fill_gradient(trans = "reverse",
		labels = scales::label_percent()) +
	labs(x = "", y = "", title = "Highest level = HS Vocational") +
	theme_minimal() +
	theme(
		panel.background = element_rect(fill = "grey90", color = "white"),
		panel.grid = element_blank(),
		plot.title = element_text(hjust = .6, vjust = -5),
		axis.line = element_blank(), axis.ticks = element_blank(),
		axis.text.x = element_blank(), axis.text.y = element_blank(),
		#				legend.position = "bottom",
		legend.position = c(.01, .005),
		legend.justification = "left"
		) +
	guides(fill = guide_legend(
		direction = "horizontal",
		keyheight = unit(1.5, units = "mm"),
		keywidth = unit(15, units = "mm"),
		#title.position = "top", title.hjust = .5,
		label.hjust = .5,
		nrow = 1, byrow = T, reverse = F,
		label.position = "bottom",
		title = "Pct HS Vocational", title.position = "top"
	))

girafe(
	ggobj = test_map,
	options = list(
		opts_selection(type = "single", only_shiny = FALSE),
		opts_hover(css = ''), ## CSS code of line we're hovering over
		opts_hover_inv(css = "opacity:0.1;"), ## CSS code of all other lines
		opts_sizing(rescale = FALSE), ## Fixes sizes to dimensions below
		opts_tooltip(
			opacity = 0.8, #opacity of the background box
			css = "background-color:#4c6061; color:white;")),
	height_svg = 6,
	width_svg = 9)

glimpse(crime_2023)

# geom_sf(data = (crime_2023 %>% filter(offence_cat_name== offence)),
# 	aes(fill = crime_per1k), color = "#FFFFFF", size = 3) +

### plot function
dk_edmap <- function(edlevel, maptitle) {
	g <-
		ggplot() +
		geom_sf_interactive(data = subset(edattain_maps, ed_level == edlevel),
			aes(fill = ed_level_pct, tooltip = tooltip1),
			color = "#FFFFFF", size = 4) +
		geom_sf_text(data = (edattain_maps %>%
				filter(province_name %notin% c("Byen København", "Københavns omegn")) %>%
				filter(ed_level == edlevel)),
			aes(label = province_name), color = "white", size = 3)	+
		ggsflabel::geom_sf_label_repel(data = (edattain_maps %>%
				filter(province_name %in% c("Byen København", "Københavns omegn")) %>%
				filter(ed_level == edlevel)),
			aes(label = province_name),
			force = 1, nudge_x = 1.5, nudge_y = .5, size = 2) +
		scale_fill_gradient(trans = "reverse",
			labels = scales::label_percent()) +
		labs(x = "", y = "") +
		ggtitle(maptitle) +
		theme_minimal() +
		theme(
			panel.background = element_rect(fill = "grey90", color = "white"),
			panel.grid = element_blank(),
			plot.title = element_text(hjust = .5, vjust = -5),
			axis.line = element_blank(), axis.ticks = element_blank(),
			axis.text.x = element_blank(), axis.text.y = element_blank(),
			#				legend.position = "bottom",
			legend.position = c(.01, .005),
			legend.justification = "left",
			legend.title = element_text(size = 8),
			legend.text = element_text(size = 8),
		) +
		guides(fill = guide_legend(
			direction = "horizontal",
			keyheight = unit(.75, units = "mm"),
			keywidth = unit(8, units = "mm"),
			label.hjust = .5,
			nrow = 1, byrow = T, reverse = F,
			label.position = "bottom",
			title = "Pct attained level", title.position = "top"
		))

		return(g)
}

# one map
testmap1 <-
dk_edmap("HS Vocational", "HS Vocational")

girafe(
	ggobj = testmap1,
	options = list(
		opts_selection(type = "single", only_shiny = FALSE),
		opts_hover(css = ''), ## CSS code of line we're hovering over
		opts_hover_inv(css = "opacity:0.1;"), ## CSS code of all other lines
		opts_sizing(rescale = FALSE), ## Fixes sizes to dimensions below
		opts_tooltip(
			opacity = 0.8, #opacity of the background box
			css = "background-color:#4c6061; color:white;")),
	height_svg = 6,
	width_svg = 9)


## map over all crime categories
# create list of crime types
edlevcats <- unique(edattain_maps$ed_level)

# create plots, stitch together with patchwork
edmaps <-
wrap_plots(
	map(edlevcats, ~dk_edmap(edlevel = .x, maptitle = .x)),
	widths = 12, heights = 12) +
	plot_annotation(
		title = "Educational attainment levels vary by geography in Denmark.",
		subtitle = "*Percent education attainment level by province in Denmark, 2023, population ages 20 - 69.*",
		caption = "*Data from Danmarks Statistik table HFUDD11*",
		theme = theme(plot.subtitle = element_markdown(),
			plot.caption = element_markdown()))

girafe(
	ggobj = edmaps,
	options = list(
		opts_selection(type = "single", only_shiny = FALSE),
		opts_hover(css = ''), ## CSS code of line we're hovering over
		opts_hover_inv(css = "opacity:0.1;"), ## CSS code of all other lines
		opts_sizing(rescale = FALSE), ## Fixes sizes to dimensions below
		opts_tooltip(
			opacity = 0.8, #opacity of the background box
			css = "background-color:#4c6061; color:white;")),
	height_svg = 10,
	width_svg = 10)
