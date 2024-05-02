# crime by NUTS3 region in denmark
# data from danstat
## code for map from https://github.com/milos-agathon/how-i-make-eurostat-maps/blob/main/R/main.r

library(tidyverse)
library(sf)
library(classInt)
library(giscoR)
library(eurostat)
library(tidylog) # to get a log of what's happening to the data
library(janitor) # tools for data cleaning
library(danstat) # package to get Danish statistics via api
library(ggtext) # enhancements for text in ggplot
library(patchwork)
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
	count(NAME_LATN) %>%
	view()


### data
table_meta_crime <- danstat::get_table_metadata(table_id = "straf11", variables_only = TRUE)
view(table_meta$variables)
crime_meta <- statsDK::sdk_retrieve_metadata("STRAF11")

## because I want to do by province but that's not available in API, need to load spreadsheet
## use Nature of office, total value

# read in file. need to fill down first two columns
# criminal code = sexual offences + violence + property + other
# nature of the offence total = sexual offences + violence + property + other + special acts
#crime_2023_1 <- readxl::read_excel("2024/data/dk_crime_by_province_2023.xlsx") %>%
crime_2023_1 <- readxl::read_excel("~/Data/r/30 Day Chart Challenge/2024/data/dk_crime_by_province_2023.xlsx") %>%
	clean_names() %>%
	fill(offence_cat_code) %>%
	fill(offence_cat_name) %>%
#	filter(offence_cat_code == "TOT") %>%
	rename_with(~ sub("^x", "tot_", .x), starts_with(("x"))) %>%
	mutate(tot_2023 = rowSums(.[c(5:8)])) %>%
	mutate(province_name = str_replace(province_name, "Province ", "")) %>%
	mutate(offence_cat_name = str_replace(offence_cat_name, ", total", "")) %>%
	mutate(offence_cat_name = str_replace(
		offence_cat_name, "Nature of the offence", "All offences")) %>%
	mutate(offence_cat_name = str_replace(
		offence_cat_name, "Offences against property", "Property crime")) %>%
	mutate(offence_cat_name = str_replace(
		offence_cat_name, "Crimes of violence", "Violent crime")) %>%
	mutate(offence_cat_name =
				 	factor(offence_cat_name,
				 				 levels = c("All offences", "Criminal code", "Sexual offenses", "Violent crime", "Property crime",
				 				 					 "Other offences", "Special acts"))) %>%
	select(province_name, offence_cat_name, tot_2023)

glimpse(crime_2023_1)

crime_2023_1 %>%
	count(offence_cat_name)

c("All offences", "Criminal code", "Sexual offenses", "Violent crime", "Property crime",
	"Other offences", "Special acts")

## get population data to normalize - again, need spreadsheet for provinces
# based on total at start of 2023 Q3
pop_2023 <- readxl::read_excel("2024/data/dk_pop_2023_q3.xlsx") %>%
	clean_names() %>%
	mutate(province_name = str_replace(province_name, "Province ", ""))

glimpse(pop_2023)

# merge crime & pop to normalize crime by population: crimes by pop * 100,000

crime_2023_2 <- crime_2023_1 %>%
	left_join(pop_2023) %>%
	mutate(crime_per1k = round(tot_2023 / tot_pop * 100000, 0))

glimpse(crime_2023_2)

## left_join nuts sf object to crime data to get sf object to map from
crime_2023 <- nuts3_dk %>%
	left_join(crime_2023_2, by = "province_name")
%>%
	#filter out aggregate categories
	filter(offence_cat_name %notin% c("Criminal code", "Nature of the offence"))

glimpse(crime_2023)


## map
# test map

ggplot() +
	geom_sf(data = subset(crime_2023, offence_cat_name == "Special acts"),
					aes(fill = crime_per1k), color = "#FFFFFF", size = 4) +
	geom_sf_text(data = (crime_2023 %>%
											 	filter(province_name %notin% c("Byen København", "Københavns omegn")) %>%
											 	filter(offence_cat_name == "Special acts")),
							 aes(label = province_name))	+
	ggsflabel::geom_sf_label_repel(data = (crime_2023 %>%
																				 	filter(province_name %in% c("Byen København", "Københavns omegn")) %>%
																				 	filter(offence_cat_name == "Special acts")),
																 aes(label = province_name),
																 force = 1, nudge_x = 1.5, nudge_y = .5, size = 2) +
	# scale_fill_gradient(
	# #	limits = c(min(tmp$crime_per1k), max(tmp$crime_per1k)),
	# 	low = "blue", high = "yellow") +
	scale_fill_gradient(trans = "reverse") +
	labs(x = "", y = "", title = "Special acts") +
	theme_minimal() +
	theme(panel.grid = element_blank(),
				plot.title = element_text(hjust = .6, vjust = -5),
				axis.line = element_blank(), axis.ticks = element_blank(),
				axis.text.x = element_blank(), axis.text.y = element_blank(),
#				legend.position = "bottom",
		legend.position = c(.4, 0)) +
	guides(fill = guide_legend(
		direction = "horizontal",
		keyheight = unit(1.5, units = "mm"),
		keywidth = unit(15, units = "mm"),
		title.position = "top", title.hjust = .5,
		label.hjust = .5,
		nrow = 1, byrow = T, reverse = F,
		label.position = "bottom"
	))


## map function
# https://yutannihilation.github.io/ggsflabel/
dk_crime_map <- function(offence, maptitle) {
	g <-
		ggplot() +
		geom_sf(data = (crime_2023 %>% filter(offence_cat_name== offence)),
						aes(fill = crime_per1k), color = "#FFFFFF", size = 3) +
		geom_sf_text(data = (crime_2023 %>%
												 	filter(province_name %notin% c("Byen København", "Københavns omegn")) %>%
												 	filter(offence_cat_name == "Special acts")),
								 aes(label = province_name), nudge_x = -.5, size = 2)	+
		ggsflabel::geom_sf_label_repel(data = (crime_2023 %>%
																					 	filter(province_name %in% c("Byen København", "Københavns omegn")) %>%
																					 	filter(offence_cat_name == offence)),
																	 aes(label = province_name), size = 3,
																	 force = 1, nudge_x = 2, nudge_y = 1) +
		scale_fill_gradient(trans = "reverse") +
		labs(x = "") +
		theme_minimal() +
		ggtitle(maptitle) +
		theme(panel.grid = element_blank(),
					plot.title = element_text(size = 12, hjust = .6, vjust = -7),
					axis.line = element_blank(), axis.ticks = element_blank(),
					axis.text.x = element_blank(), axis.text.y = element_blank(),
					legend.position = c(.4, -.05),
					legend.title = element_text(size = 8),
					legend.text = element_text(size = 8)
					) +
		guides(fill = guide_legend(
			title = "Incidents per 100K people",
			direction = "horizontal",
			keyheight = unit(1.5, units = "mm"),
			keywidth = unit(15, units = "mm"),
			title.position = "top",
			title.hjust = .5,
			label.hjust = .5,
			nrow = 1,
			byrow = T,
			reverse = F,
			label.position = "bottom"
		))

	return(g)
}

# one map
dk_crime_map("Special acts", "Special Acts Crimes")

## map over all crime categories
# create list of crime types
crimecats <- unique(crime_2023_2$offence_cat_name)

# create plots, stitch together with patchwork
wrap_plots(
	map(crimecats, ~dk_crime_map(offence = .x, maptitle = .x)),
	widths = 5, heights = 5) +
	plot_annotation(
		title = "Crimes by Type and Province in Denmark, 2023",
		subtitle = "*Total Crimes per 100K people*",
		caption = "*Data from Danmarks Statistik*",
		theme = theme(plot.subtitle = element_markdown(),
									plot.caption = element_markdown()))

# patchwork::wrap_plots(
# 	map(crimecats, ~dk_crime_map3(offence = .x, maptitle = .x) +
# 				theme(plot.margin = margin(rep(15, 4)))))






glimpse(crime_2023)
crime_2023 %>%
	count(province_name)








	## redundant code
# nutsall_dk <- giscoR::gisco_get_nuts(
# 	year = "2021",	resolution = "3",
# 	nuts_level = "all", country = "DK") |>
# 	sf::st_transform(crsLONGLAT)
#
# glimpse(nutsall_dk)

# meta dat
# table_meta_pop <- danstat::get_table_metadata(table_id = "folk1a", variables_only = TRUE)

# province code
# "18404{G}"
# variables_pop <- list(
# 	list(code = "område", values = "084"),
# 	#	list(code = "køn", values = c("M","K")),
# 	list(code = "alder", values = "IALT"),
# 	list(code = "tid", values = "2023K4"))
#
# pop2023 <- danstat::get_data("folk1a", variables_pop, language = "da")

# dk_crime_map <- function(data, var1, var2, offence_cat_name, color = "") {
#
# 	g <-
# 		ggplot() +
# 		geom_sf(data = crime_2023, aes(fill = crime_per1k), color = NA, size = 0) +
# 		theme_minimal() +
# 		labs(x = "", y = "") +
# 		#		title = paste("chart for", crime_2023$offence_cat_name) +
# 		theme(panel.grid = element_blank(),
# 					axis.line = element_blank(), axis.ticks = element_blank(),
# 					axis.text.x = element_blank(), axis.text.y = element_blank(),
# 					legend.position = c(.85, .60)) +
# 		guides(fill = guide_legend(
# 			direction = "horizontal",
# 			keyheight = unit(1.5, units = "mm"),
# 			keywidth = unit(15, units = "mm"),
# 			title.position = "top",
# 			title.hjust = .5,
# 			label.hjust = .5,
# 			nrow = 1,
# 			byrow = T,
# 			reverse = F,
# 			label.position = "bottom"
# 		))
#
# 	return(g)
# }
#
# # works with all of these two calls
# dk_crime_map(data = subset(crime_2023, offence_cat_name == "Special acts, total"))
# dk_crime_map(crime_2023, offence_cat_name = "Special acts, total")
# crime_2023 %>%
# 	filter(offence_cat_name == "Special acts, total") %>%
# 	dk_crime_map()
#
#
# dk_crime_map2 <- function(data, offence_cat_name) {
# 	g <-
# 		ggplot() +
# 		geom_sf(data = crime_2023, aes(fill = crime_per1k), color = NA, size = 0) +
# 		theme_minimal() +
# 		labs(x = "", y = "") +
# 		#		title = paste("chart for", crime_2023$offence_cat_name) +
# 		theme(panel.grid = element_blank(),
# 					axis.line = element_blank(), axis.ticks = element_blank(),
# 					axis.text.x = element_blank(), axis.text.y = element_blank(),
# 					legend.position = c(.85, .60)) +
# 		guides(fill = guide_legend(
# 			direction = "horizontal",
# 			keyheight = unit(1.5, units = "mm"),
# 			keywidth = unit(15, units = "mm"),
# 			title.position = "top",
# 			title.hjust = .5,
# 			label.hjust = .5,
# 			nrow = 1,
# 			byrow = T,
# 			reverse = F,
# 			label.position = "bottom"
# 		))
#
# 	return(g)
# }
#
# # works with all of these two calls
# dk_crime_map2(data = subset(crime_2023, offence_cat_name == "Special acts, total"))
# dk_crime_map2(crime_2023, offence_cat_name = "Special acts, total")
# dk_crime_map2(crime_2023, "Special acts, total")
# crime_2023 %>%
# 	filter(offence_cat_name == "Special acts, total") %>%
# 	dk_crime_map2()
