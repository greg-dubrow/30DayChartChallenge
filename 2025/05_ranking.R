# sub fields in major fields of study. horizontal bar graphs, faceted by major field

library(tidyverse) # to do tidyverse things
library(tidylog) # to get a log of what's happening to the data
library(janitor) # tools for data cleaning
library(danstat) # package to get Danish statistics via api
library(dkstat)
library(ggtext) # enhancements for text in ggplot
library(ggrepel)
library(scales)
library(tidytext)
library(patchwork)


# some custom functions
source("~/Data/r/basic functions.R")

# UDDAKT60
table_meta <- danstat::get_table_metadata(table_id = "uddakt60", variables_only = TRUE)

# create variable list using the ID value in the variable
variables_ed <- list(
	list(code = "uddannelse", values = "*"),
	list(code = "fstatus", values = c("F")),
	list(code = "køn", values = c("M", "K")),
	#list(code = "alder", values = c("TOT")),
	list(code = "tid", values = 2023))

bacdegs1 <- get_data("uddakt60", variables_ed, language = "en") %>%
	as_tibble()

glimpse(bacdegs1)

bacdegs <- bacdegs1 %>%
	mutate(deg_code = str_extract(UDDANNELSE, "^[^ ]+")) %>%
	mutate(deg_name = sub("^\\S+\\s+", '', UDDANNELSE)) %>%
	mutate(deg_name = str_remove(deg_name, ", BACH")) %>%
	mutate(deg_name = str_remove(deg_name, " BACH")) %>%
	mutate(deg_group = case_when(
		deg_code %in% c("H6020", "H6025", "H6030", "H6035", "H6039",
										"H6059", "H6075", "H6080", "H6090") ~ "Main",
		deg_code == "H60" ~ "All",
		TRUE ~ "Sub")) %>%
	mutate(deg_field =  case_when(
		str_detect(deg_code, "H6020") ~ "Education",
		str_detect(deg_code, "H6025") ~ "Humanities",
		str_detect(deg_code, "H6030") ~ "Arts",
		str_detect(deg_code, "H6035") ~ "Science",
		str_detect(deg_code, "H6039") ~ "Social Sciences",
		str_detect(deg_code, "H6059") ~ "Technical sciences",
		str_detect(deg_code, "H6075") ~ "Food/Biotech/Lab Tech",
		str_detect(deg_code, "H6080") ~ "Agriculture/Nature/Environment",
		str_detect(deg_code, "H6090") ~ "Health science",
		TRUE ~ "All")) %>%
	rename(degs_n = INDHOLD, sex = KØN)
# %>%
# 	group_by(deg_field, deg_group) %>%
# 	mutate(deg_field_n = sum(degs_n)) %>%
# 	mutate(deg_field_pct = degs_n/deg_field_n) %>%
# 	ungroup() %>%
# 	group_by(sex, deg_field) %>%
# 	mutate(deg_sex_field_pct = degs_n / deg_field_n)

glimpse(bacdegs)

bacdegs %>%
	count(deg_field)

bacdegs %>%
	filter(deg_group == "Main") %>%
	select(deg_field, degs_n, sex, deg_field_n)


## plots
# "Agriculture/Nature/Environment" X
# Arts" X
# "Education" not doing chart
# "Food/Biotech/Lab Tech"not doing chart
# "Health science" X
# "Humanities" X
# "Science"
# "Social Sciences"
# "Technical sciences"

# not shown is Education, only one major field, 117 women, 17 men and
# Food, Biotech, Lab Tech with 61 total, men 20 women 41 all in general Food & Nutrition science

bacdegs %>%
	filter(deg_group == "Main") %>%
	{. ->> tmp} %>%
	ggplot(aes(x = degs_n, y = reorder_within(deg_name, degs_n, sex), fill = sex)) +
	geom_bar(stat = "identity") +
	scale_fill_manual(values = c("#E66100", "#5D3A9B")) +
	scale_y_reordered() +
	scale_x_continuous(labels = comma) +
	labs(x = "", y = "",
			 title = "All Bachelor degrees by field & sex, 2023",
			 subtitle = glue::glue("*Total degrees earned = 19,199: Men = 8,694, Women = 10,505*"),
			 caption = "*Data from Danmarks Statistik via danstat package*") +
	facet_wrap(~ sex, scales = "free_y") +
	geom_text(data = subset(tmp, degs_n > 3000),
						aes(label = comma(degs_n)), hjust = 1.5, color = "white") +
	geom_text(data = subset(tmp, degs_n < 3000),
						aes(label = comma(degs_n)), hjust = -.5, color = "black") +
	theme_minimal() +
	theme(panel.grid = element_blank(),
				legend.position = "none",
				plot.title = element_markdown(size = 16, hjust = 0),
				plot.subtitle = element_markdown(size = 14, hjust = 0),
				plot.caption = element_markdown(),
				strip.text = element_text(size = 12))

ggsave("2025/images/prompt5_all_2025.jpg", width = 15, height = 8,
			 units = "in", dpi = 300)

ggsave("~/Data/greg_dubrow_io/posts/30-day-chart-challenge-2025/images/prompt5_all_2025.jpg",
			 width = 15, height = 8, units = "in", dpi = 300)


#plot_agvet <-
	bacdegs %>%
	filter(deg_group == "Sub") %>%
	filter(deg_field == "Agriculture/Nature/Environment") %>%
	{. ->> tmp} %>%
	ggplot(aes(x = degs_n, y = reorder_within(deg_name, degs_n, sex), fill = sex)) +
	geom_bar(stat = "identity") +
	scale_fill_manual(values = c("#E66100", "#5D3A9B")) +
	scale_y_reordered() +
	scale_x_continuous(labels = comma) +
	labs(x = "", y = "",
			 title = "Agriculture, Nature, & Environment Science Bachelor degrees by field & sex, 2023",
			 subtitle = glue::glue("*Total degrees earned = 308: Men = 55, Women = 253*")) +
	facet_wrap(~ sex, scales = "free_y") +
	geom_text(data = subset(tmp, degs_n > 100),
						aes(label = comma(degs_n)), hjust = 1.5, color = "white") +
	geom_text(data = subset(tmp, degs_n < 100),
						aes(label = degs_n), hjust = -.5, color = "black") +
	theme_minimal() +
	theme(panel.grid = element_blank(),
				legend.position = "none",
				plot.title = element_markdown(size = 16, hjust = 0),
				plot.subtitle = element_markdown(size = 14, hjust = 0),
				plot.caption = element_markdown(),
				strip.text = element_text(size = 12))

ggsave("2025/images/prompt5_ag_2025.jpg", width = 15, height = 8,
			 units = "in", dpi = 300)

ggsave("~/Data/greg_dubrow_io/posts/30-day-chart-challenge-2025/images/prompt5_ag_2025.jpg",
			 width = 15, height = 8, units = "in", dpi = 300)


#plot_arts <-
	bacdegs %>%
	filter(deg_group == "Sub") %>%
	filter(deg_field == "Arts") %>%
	{. ->> tmp} %>%
	ggplot(aes(x = degs_n, y = reorder_within(deg_name, degs_n, sex), fill = sex)) +
	geom_bar(stat = "identity") +
	scale_fill_manual(values = c("#E66100", "#5D3A9B")) +
	scale_y_reordered() +
	scale_x_continuous(labels = comma) +
	labs(x = "", y = "",
			 title = "Arts Bachelor degrees by field & sex, 2023",
			 subtitle = glue::glue("*Total degrees earned = 619: Men = 270, Women = 349*")) +
	facet_wrap(~ sex, scales = "free_y") +
	geom_text(data = subset(tmp, degs_n > 100),
						aes(label = comma(degs_n)), hjust = 1.5, color = "white") +
	geom_text(data = subset(tmp, degs_n < 100),
						aes(label = degs_n), hjust = -.5, color = "black") +
	theme_minimal() +
	theme(panel.grid = element_blank(),
				legend.position = "none",
				plot.title = element_markdown(size = 16, hjust = 0),
				plot.subtitle = element_markdown(size = 14, hjust = 0),
				plot.caption = element_markdown(),
				strip.text = element_text(size = 12))

ggsave("2025/images/prompt5_art_2025.jpg", width = 15, height = 8,
			 units = "in", dpi = 300)

ggsave("~/Data/greg_dubrow_io/posts/30-day-chart-challenge-2025/images/prompt5_art_2025.jpg",
			 width = 15, height = 8, units = "in", dpi = 300)


#plot_health <-
	bacdegs %>%
	filter(deg_group == "Sub") %>%
	filter(deg_field == "Health science") %>%
	{. ->> tmp} %>%
	ggplot(aes(x = degs_n, y = reorder_within(deg_name, degs_n, sex), fill = sex)) +
	geom_bar(stat = "identity") +
	scale_fill_manual(values = c("#E66100", "#5D3A9B")) +
	scale_y_reordered() +
	scale_x_continuous(labels = comma) +
	labs(x = "", y = "",
			 title = "Health science Bachelor degrees by field & sex, 2023",
			 subtitle = glue::glue("*Total degrees earned = 1,973: Men = 564, Women = 1,409*")) +
	facet_wrap(~ sex, scales = "free_y") +
	geom_text(data = subset(tmp, degs_n > 500),
						aes(label = comma(degs_n)), hjust = 1.5, color = "white") +
	geom_text(data = subset(tmp, degs_n < 500),
						aes(label = degs_n), hjust = -.5, color = "black") +
	theme_minimal() +
	theme(panel.grid = element_blank(),
				legend.position = "none",
				plot.title = element_markdown(size = 16, hjust = 0),
				plot.subtitle = element_markdown(size = 14, hjust = 0),
				plot.caption = element_markdown(),
				strip.text = element_text(size = 12))

ggsave("2025/images/prompt5_health_2025.jpg", width = 15, height = 8,
			 units = "in", dpi = 300)

ggsave("~/Data/greg_dubrow_io/posts/30-day-chart-challenge-2025/images/prompt5_health_2025.jpg",
			 width = 15, height = 8, units = "in", dpi = 300)

#plot_human <-
	bacdegs %>%
	filter(deg_group == "Sub") %>%
	filter(deg_field == "Humanities") %>%
	{. ->> tmp} %>%
	ggplot(aes(x = degs_n, y = reorder_within(deg_name, degs_n, sex), fill = sex)) +
	geom_bar(stat = "identity") +
	scale_fill_manual(values = c("#E66100", "#5D3A9B")) +
	scale_y_reordered() +
	scale_x_continuous(labels = comma) +
	labs(x = "", y = "",
			 title = "Humanities Bachelor degrees by field & sex, 2023",
			 subtitle = glue::glue("*Total degrees earned = 3,363: Men = 1,061, Women = 2,302*")) +
	facet_wrap(~ sex, scales = "free_y") +
	geom_text(data = subset(tmp, degs_n > 300),
						aes(label = comma(degs_n)), hjust = 1.5, color = "white") +
	geom_text(data = subset(tmp, degs_n < 300),
						aes(label = degs_n), hjust = -.5, color = "black") +
	theme_minimal() +
	theme(panel.grid = element_blank(),
				legend.position = "none",
				plot.title = element_markdown(size = 16, hjust = 0),
				plot.subtitle = element_markdown(size = 14, hjust = 0),
				plot.caption = element_markdown(),
				strip.text = element_text(size = 12))

ggsave("2025/images/prompt5_hum_2025.jpg", width = 15, height = 8,
			 units = "in", dpi = 300)

ggsave("~/Data/greg_dubrow_io/posts/30-day-chart-challenge-2025/images/prompt5_hum_2025.jpg",
			 width = 15, height = 8, units = "in", dpi = 300)


#plot_sci <-
	bacdegs %>%
	filter(deg_group == "Sub") %>%
	filter(deg_field == "Science") %>%
	{. ->> tmp} %>%
	ggplot(aes(x = degs_n, y = reorder_within(deg_name, degs_n, sex), fill = sex)) +
	geom_bar(stat = "identity") +
	scale_fill_manual(values = c("#E66100", "#5D3A9B")) +
	scale_y_reordered() +
	scale_x_continuous(labels = comma) +
		labs(x = "", y = "",
				 title = "Science Bachelor degrees by field & sex, 2023",
				 subtitle = glue::glue("*Total degrees earned = 2,485: Men = 1,476, Women = 1,009*")) +
		facet_wrap(. ~ sex, scales = "free_y") +
		geom_text(data = subset(tmp, degs_n > 600),
							aes(label = comma(degs_n)), hjust = 1.5, color = "white") +
		geom_text(data = subset(tmp, degs_n < 200),
							aes(label = comma(degs_n)), hjust = -.5, color = "black") +
		theme_minimal() +
		theme(panel.grid = element_blank(),
					legend.position = "none",
					plot.title = element_markdown(size = 16, hjust = 0),
					plot.subtitle = element_markdown(size = 14, hjust = 0),
					plot.caption = element_markdown(),
					strip.text = element_text(size = 12))

ggsave("2025/images/prompt5_sci_2025.jpg", width = 15, height = 8,
			 units = "in", dpi = 300)

ggsave("~/Data/greg_dubrow_io/posts/30-day-chart-challenge-2025/images/prompt5_sci_2025.jpg",
			 width = 15, height = 8, units = "in", dpi = 300)


#plot_socsci <-
bacdegs %>%
	filter(deg_group == "Sub") %>%
	filter(deg_field == "Social Sciences") %>%
	{. ->> tmp} %>%
	ggplot(aes(x = degs_n, y = reorder_within(deg_name, degs_n, sex), fill = sex)) +
	geom_bar(stat = "identity") +
	scale_fill_manual(values = c("#E66100", "#5D3A9B")) +
	scale_y_reordered() +
	scale_x_continuous(labels = comma) +
	labs(x = "", y = "",
			 title = "Social Science Bachelor degrees by field & sex, 2023",
	subtitle = glue::glue("*Total degrees earned - 7,847: Men = 3,618, Women = 4,229*")) +
	facet_wrap(~ sex, scales = "free_y") +
	geom_text(data = subset(tmp, degs_n > 1000),
						aes(label = comma(degs_n)), hjust = 1.5, color = "white") +
	geom_text(data = subset(tmp, degs_n < 1000),
						aes(label = degs_n), hjust = -.5, color = "black") +
	theme_minimal() +
	theme(panel.grid = element_blank(),
		legend.position = "none",
		plot.title = element_markdown(size = 16, hjust = 0),
		plot.subtitle = element_markdown(size = 14, hjust = 0),
		plot.caption = element_markdown(),
		strip.text = element_text(size = 12))

ggsave("2025/images/prompt5_soc_2025.jpg", width = 15, height = 8,
			 units = "in", dpi = 300)

ggsave("~/Data/greg_dubrow_io/posts/30-day-chart-challenge-2025/images/prompt5_soc_2025.jpg",
			 width = 15, height = 8, units = "in", dpi = 300)


#plot_techci <-
bacdegs %>%
	filter(deg_group == "Sub") %>%
	filter(deg_field == "Technical sciences") %>%
	{. ->> tmp} %>%
	ggplot(aes(x = degs_n, y = reorder_within(deg_name, degs_n, sex), fill = sex)) +
	geom_bar(stat = "identity") +
	scale_fill_manual(values = c("#E66100", "#5D3A9B")) +
	scale_y_reordered() +
	scale_x_continuous(labels = comma) +
	labs(x = "", y = "",
			 title = "Technical sciences Bachelor degrees by field & sex, 2023",
			 subtitle = glue::glue("*Total degrees earned = 2,409: Men = 1,613, Women = 796*")) +
	facet_wrap(~ sex, scales = "free_y") +
	geom_text(data = subset(tmp, degs_n > 400),
						aes(label = comma(degs_n)), hjust = 1.5, color = "white") +
	geom_text(data = subset(tmp, degs_n < 400),
						aes(label = degs_n), hjust = -.5, color = "black") +
	theme_minimal() +
	theme(panel.grid = element_blank(),
				legend.position = "none",
				plot.title = element_markdown(size = 16, hjust = 0),
				plot.subtitle = element_markdown(size = 14, hjust = 0),
				plot.caption = element_markdown(),
				strip.text = element_text(size = 12))

ggsave("2025/images/prompt5_tech_2025.jpg", width = 15, height = 8,
			 units = "in", dpi = 300)

ggsave("~/Data/greg_dubrow_io/posts/30-day-chart-challenge-2025/images/prompt5_tech_2025.jpg",
			 width = 15, height = 8, units = "in", dpi = 300)


###
### getting data via dkstat https://github.com/rOpenGov/dkstat
# uddakt60_meta <- dst_meta("uddakt60", lang = "en")
#
# uddakt60_meta$values
#
# bacdegs1 <- dst_get_data(
# 	table = "uddakt60",
# 	UDDANNELSE = "*",
# 	KØN = c("Men", "Women"),
# 	FSTATUS = 'Completed',
# 	Tid = 2023,
# 	lang = "en",
# 	meta_data = uddakt60_meta
# )
#
# bacdegs1


## tried doing a function to automate plots but takig too long
# plot_degs_field <- function(field){
# 	ggplot(bacdegs %>% 	filter(deg_group == "Sub"),
# 				 aes(x = degs_n, y = reorder_within(deg_name, degs_n, sex), fill = sex)) +
# 		geom_bar(stat = "identity") +
# 		scale_fill_manual(values = c("#E66100", "#5D3A9B")) +
# 		scale_y_reordered() +
# 		scale_x_continuous(labels = comma) +
# 		labs(x = "", y = "") +
# 		facet_wrap(~ sex, scales = "free_y") +
# 		# geom_text(data = bacdegs %>%
# 		# 						filter(deg_group == "Sub") %>%
# 		# 						filter(deg_field == field) %>%
# 		# 						filter(degs_n > 1000),
# 		# 					aes(label = degs_n), hjust = 1.5, color = "white") +
# 		# geom_text(data = bacdegs %>%
# 		# 						filter(deg_group == "Sub") %>%
# 		# 						filter(deg_field == field) %>%
# 		# 						filter(degs_n < 1000),
# 		# 					aes(label = bacdegs$degs_n), hjust = -.5, color = "black") +
# 		theme_minimal() +
# 		theme(legend.position = "none")
# }
#
# fields <- unique(bacdegs$deg_field)
# setdiff(fields, "no")
# patchwork::wrap_plots(map(fields, ~plot_degs_field(field = .x)))
#
