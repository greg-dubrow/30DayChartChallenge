## differences in percent of degrees awarded by discipline
# diverging bars like prompt 5 2024
# one by broad discipline
# one for each discipline by top 10 overall with gender split
# label should include where it ranks by gender?

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
table_meta_dk <- danstat::get_table_metadata(table_id = "uddakt60", variables_only = TRUE, language = "da")

# create variable list using the ID value in the variable
variables_ed <- list(
	list(code = "uddannelse", values = "*"),
	list(code = "fstatus", values = c("F")),
	list(code = "køn", values = c("M", "K")),
	#list(code = "alder", values = c("TOT")),
	list(code = "tid", values = 2023))

bacdegs1 <- get_data("uddakt60", variables_ed, language = "en") %>%
	as_tibble() %>%
	clean_names()

glimpse(bacdegs1)

bacdegs <- bacdegs1 %>%
	select(-fstatus) %>%
	mutate(deg_code = str_extract(uddannelse, "^[^ ]+")) %>%
	mutate(deg_name = sub("^\\S+\\s+", '', uddannelse)) %>%
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
		str_detect(deg_code, "H6080") ~ "Agriculture Nature Environment",
		str_detect(deg_code, "H6090") ~ "Health science",
		TRUE ~ "All")) %>%
	mutate(deg_field = factor(deg_field,
				levels = c("All", "Agriculture Nature Environment", "Arts", "Education", "Food/Biotech/Lab Tech",
				"Health science", "Humanities", "Science", "Social Sciences", "Technical sciences"))) %>%
	rename(degs_n = indhold, sex = kon)
# %>%
# 	group_by(deg_field, deg_group) %>%
# 	mutate(deg_field_n = sum(degs_n)) %>%
# 	mutate(deg_field_pct = degs_n/deg_field_n) %>%
# 	ungroup() %>%
# 	group_by(sex, deg_field) %>%
# 	mutate(deg_sex_field_pct = degs_n / deg_field_n)

glimpse(bacdegs)

bacdegs %>%
	filter(deg_field == "All")
	count(deg_field)



## for faint highlight lines in chart
vlines_df <- data.frame(xintercept = seq(-100, 100, 20))

## by top line group
bacdegs %>%
	filter(deg_group == "Main") %>%
	select(deg_field, sex, degs_n) %>%
	group_by(deg_field) %>%
	mutate(deg_field_tot = sum(degs_n)) %>%
	ungroup() %>%
	group_by(deg_field, sex) %>%
	mutate(deg_sex_pct = round(degs_n /deg_field_tot, 3)) %>%
	mutate(deg_sex_pct = ifelse(sex == "Men", deg_sex_pct *-1, deg_sex_pct)) %>%
	mutate(deg_sex_pct2 = round(deg_sex_pct * 100, 1)) %>%
	ungroup() %>%
	mutate(deg_field = fct_reorder(deg_field, desc(deg_field))) %>%
	{. ->> tmp} %>%
	ggplot() +
	geom_col(aes(x = -50, y = deg_field), width = 0.75, fill = "#e0e0e0") +
	geom_col(aes(x = 50, y = deg_field), width = 0.75, fill = "#e0e0e0") +
	geom_col(aes(x = deg_sex_pct2, y = deg_field, fill = sex, color = sex), width = 0.75) +
	scale_x_continuous(expand = c(0, 0),
		labels = function(x) abs(x), breaks = seq(-100, 100, 20)) +
	geom_vline(data = vlines_df, aes(xintercept = xintercept), color = "#FFFFFF", size = 0.1, alpha = 0.5) +
	coord_cartesian(clip = "off") +
	scale_fill_manual(values = c("#E66100", "#5D3A9B")) +
	scale_color_manual(values = c("white", "white")) +
	geom_text(data = subset(tmp, sex == "Men"),
						aes(x = deg_sex_pct2, y = deg_field, label = paste0(abs(deg_sex_pct2), "%")),
						size = 5, color = "white", position = position_dodge(width = .4), hjust = -0.09) +
	geom_text(data = subset(tmp, sex == "Women"),
						aes(x = deg_sex_pct2, y = deg_field, label = paste0(abs(deg_sex_pct2), "%")),
						size = 5, color = "white", position = position_dodge(width = .4), hjust = 1.1) +
	geom_text(aes(x = -100, y = deg_field, label = paste0("Total awarded = ", comma(deg_field_tot))),
						size = 4, color = "#3b3b3b", hjust = -.1) +
	labs(x = "", y = "",
			 title = "<span style = 'color: #5D3A9B;'>Women</span> earn more Bachelor's degrees than
			 <span style = 'color: #E66100;'>men</span> in all disciplines except Science and Technical sciences",
			 subtitle = glue::glue("*Total degrees awarded = 19,199:
			 											<span style = 'color: #E66100;'>Men = 8,694 (45%)</span>,
			 											<span style = 'color: #5D3A9B;'>Women = 10,505 (55%)</span>*"),
			 caption = "*Data from Danmarks Statistik table UDDAKT60 via danstat package*") +
	theme_minimal() +
	theme(panel.grid = element_blank(),
				plot.title = element_markdown(size = 16),
				plot.subtitle = element_markdown(size = 12),
				plot.caption = element_markdown(),
				legend.position = "none", legend.title = element_blank(),
				axis.text.y = element_text(size = 10))
rm(tmp)

ggsave("2025/images/prompt9_all_2025.jpg", width = 15, height = 8,
			 units = "in", dpi = 300)

ggsave("~/Data/greg_dubrow_io/posts/30-day-chart-challenge-2025/images/prompt9_all_2025.jpg",
			 width = 15, height = 8, units = "in", dpi = 300)

bacdegs %>%
	count(deg_field)

## function for individual discipline tables
disc_top10 <- function(degfield, dfname = "NA") {
dfout <- bacdegs %>%
	filter(deg_group == "Sub") %>%
	filter(deg_field == degfield) %>%
	select(deg_field, deg_name, sex, degs_n) %>%
	mutate(degs_all_tot = sum(degs_n)) %>%
# creates field to use in plot dynamic subtitle
	group_by(sex) %>%
	mutate(degs_all_sex_tot = sum(degs_n)) %>%
	mutate(degs_all_sex_pct = round(degs_all_sex_tot / degs_all_tot, 3) * 100) %>%
	mutate(degs_all_sex_pct = paste0(degs_all_sex_pct, "%")) %>%
	ungroup() %>%
	group_by(deg_name) %>%
	mutate(deg_name_tot = sum(degs_n)) %>%
	ungroup() %>%
	arrange(desc(deg_name_tot), deg_name) %>%
	mutate(rank=row_number()) %>%
#	mutate(across(starts_with("degs_all_"), ~ifelse(rank <3, .x, ""))) %>%
	group_by(deg_name, sex) %>%
	mutate(deg_sex_pct = round(degs_n /deg_name_tot, 3)) %>%
	mutate(deg_sex_pct = ifelse(sex == "Men", deg_sex_pct *-1, deg_sex_pct)) %>%
	mutate(deg_sex_pct2 = round(deg_sex_pct * 100, 1)) %>%
	ungroup() %>%
	# more fields to use in plot dynamic subtitle because I can't get it to work in plot function
	mutate(deg_all_men_tot = ifelse(sex == "Men", degs_all_sex_tot, NA)) %>%
	mutate(deg_all_women_tot = ifelse(sex == "Women", degs_all_sex_tot, NA)) %>%
	mutate(deg_all_men_pct = ifelse(sex == "Men", degs_all_sex_pct, NA)) %>%
	mutate(deg_all_women_pct = ifelse(sex == "Women", degs_all_sex_pct, NA)) %>%
	mutate(deg_field_men_pct = ifelse(sex == "Men", paste0(abs(deg_sex_pct2), "%"), NA)) %>%
	mutate(deg_field_women_pct = ifelse(sex == "Women", paste0(deg_sex_pct2, "%"), NA))  %>%
	group_by(deg_name) %>%
	fill(deg_all_men_tot) %>%
	fill(deg_all_women_tot, .direction = "up") %>%
	fill(deg_all_men_pct) %>%
	fill(deg_all_women_pct, .direction = "up") %>%
	fill(deg_field_men_pct) %>%
	fill(deg_field_women_pct, .direction = "up") %>%
	ungroup()
assign(str_c(dfname, "_pcts"), dfout, envir=.GlobalEnv)
}

# test function on one dicipline
disc_top10("Science", "Science")
glimpse(Science_pcts)

# create list of fields
degfields <- unique(bacdegs$deg_field)

# map over df function
map(degfields, ~disc_top10(degfield = .x, dfname = .x))

# list objects created by function
ls(pattern = "_pcts")
## remove "All_pcts" so it doesn't loop in plot function
rm(All_pcts)
ls(pattern = "_pcts")

totdegdf <-
	Humanities_pcts %>%
	filter(rank == 1) %>%
	select(degs_all_tot)


## plot function
majorplot <- function(plotdf, degfield, plotname = "NA") {

	## for faint highlight lines in chart
	vlines_df <- data.frame(xintercept = seq(-100, 100, 20))

	# df for dynamic title
	totdegdf <- plotdf %>%
		filter(deg_field == degfield) %>%
		filter(rank == 1) %>%
		select(degs_all_tot)

	#create plot
	plotout <- plotdf %>%
		filter(deg_field == degfield) %>%
		filter(rank < 21) %>%
		# to get top 10 for plot sorted in order of total degrees in major
		arrange(desc(deg_name_tot)) %>%
		mutate(deg_name = fct_reorder(deg_name, deg_name_tot)) %>%
		# not used, but this orders top 10 for plot in degree name order
	# mutate(deg_name = fct_reorder(deg_name, desc(deg_name))) %>%
		ggplot() +
	geom_col(aes(x = -50, y = deg_name), width = 0.75, fill = "#e0e0e0") +
	geom_col(aes(x = 50, y = deg_name), width = 0.75, fill = "#e0e0e0") +
	geom_col(aes(x = deg_sex_pct2, y = deg_name, fill = sex, color = sex), width = 0.75) +
	scale_x_continuous(expand = c(0, 0),
										 labels = function(x) abs(x), breaks = seq(-100, 100, 20)) +
	geom_vline(data = vlines_df, aes(xintercept = xintercept), color = "#FFFFFF", size = 0.1, alpha = 0.5) +
	coord_cartesian(clip = "off") +
	scale_fill_manual(values = c("#E66100", "#5D3A9B")) +
	scale_color_manual(values = c("white", "white")) +
	geom_text(data = subset(plotdf, sex == "Men" & rank < 21),
						aes(x = deg_sex_pct2, y = deg_name, label = paste0(abs(deg_sex_pct2), "%")),
						size = 5, color = "white", position = position_dodge(width = .4), hjust = -0.09) +
	geom_text(data = subset(plotdf, sex == "Women" & rank < 21),
						aes(x = deg_sex_pct2, y = deg_name, label = paste0(abs(deg_sex_pct2), "%")),
						size = 5, color = "white", position = position_dodge(width = .4), hjust = 1.1) +
	 geom_text(data = subset(plotdf, sex == "Men" & rank < 21 & deg_sex_pct > -0.85),
		aes(x = -100, y = deg_name, label = paste0("Total awarded = ", comma(deg_name_tot))),
		size = 3.5, color = "#3b3b3b", hjust = -.1) +
		geom_text(data = subset(plotdf, sex == "Men" & rank < 21 & deg_sex_pct < -0.85),
							aes(x = 80, y = deg_name, label = paste0("Total awarded = ", comma(deg_name_tot))),
							size = 3.5, color = "#3b3b3b", hjust = .5) +
		labs(x = "", y = "",
			 caption = "*Data from Danmarks Statistik table UDDAKT60 via danstat package*",
					 subtitle = glue::glue("*Total degrees awarded in {degfield} = {comma(plotdf$degs_all_tot)}:
					 											<span style = 'color: #E66100;'>Men =
					 											{comma(plotdf$deg_all_men_tot)} ({plotdf$deg_all_men_pct})</span>,
					 											<span style = 'color: #5D3A9B;'>Women =
					 											{comma(plotdf$deg_all_women_tot)} ({plotdf$deg_all_women_pct})</span>.
					 											Only top 10 majors displayed.*")) +
	theme_minimal() +
	theme(panel.grid = element_blank(),
				plot.title = element_markdown(size = 16, hjust = 0),
				plot.subtitle = element_markdown(size = 12, hjust = 0),
				plot.caption = element_markdown(),
				legend.position = "none", legend.title = element_blank(),
				axis.text.y = element_text(size = 10))
	# to create individual plot names
	assign(str_c(plotname, "_plot"), plotout, envir=.GlobalEnv)
	# outputs plots to global env
	return(plotout)
}

# test plot function - input df to use, degree field and name for plot that will append to _plot
majorplot(`Technical science_pcts`, "Science", "Science")
Science_plot

# create list of data frames to mutate function over
df_list <- list(mget(ls(pattern = "local")))[[1]]
df_list = mget(ls(pattern = "_pcts"))
df_list

# for degree fields to loop over in function, need a character vector of just major names
# first a df of just the field names
bacdegs_maj <- bacdegs %>%
	filter(deg_group == "Sub") %>%
	mutate(deg_fieldc = as.character(deg_field)) %>%
	select(deg_fieldc)
# then output names to character vector
degfieldsc <- (unique(bacdegs_maj$deg_fieldc)) %>%
	str_sort()

# map list of discipline dataframes over the function.
# 2nd argument after df list list of diciplines to filter on in plot function
# df list is .x, dicipline gets .y for filter and plot name
map2(df_list, degfieldsc, ~majorplot(plotdf = .x, degfield = .y, plotname = .y))

# list objects created by function
ls(pattern = "_plot")
# look at plots
# `Agriculture Nature Environment_plot`done
# Arts_plot done
#Education_plot only one major, no need to look at plot
# Food/Biotech/Lab Tech_plot only one major, no need to look at plot
#`Health science_plot` done
#Humanities_plot # done
# Science_plot done
# `Social Sciences_plot` done
`Technical sciences_plot`


## start to edit plots - annotate with title and subtitle
`Agriculture Nature Environment_plot`+
	labs(title = "Agriculture, Nature & Environment degrees overwhelmingly earned by <span style = 'color: #5D3A9B;'>women</span>.
			 <br>The pattern is consistent across majors, especially Veterinary Med and Animal Science.")

ggsave("2025/images/prompt9_ag_2025.jpg", width = 15, height = 8,
			 units = "in", dpi = 300)

ggsave("~/Data/greg_dubrow_io/posts/30-day-chart-challenge-2025/images/prompt9_ag_2025.jpg",
			 width = 15, height = 8, units = "in", dpi = 300)

Arts_plot	+
labs(title = "<span style = 'color: #5D3A9B;'>Women</span> earned a majority of Arts degrees,
			 but in some majors such as Music, Painting and Skuespiller (acting) <br>
		 the numbers were even or <span style = 'color: #E66100;'>men</span> earned a few more degrees.")

ggsave("2025/images/prompt9_art_2025.jpg", width = 15, height = 8,
			 units = "in", dpi = 300)

ggsave("~/Data/greg_dubrow_io/posts/30-day-chart-challenge-2025/images/prompt9_art_2025.jpg",
			 width = 15, height = 8, units = "in", dpi = 300)

`Health science_plot` +
	labs(title = "<span style = 'color: #5D3A9B;'>Women</span> earned a majority of Health science degrees across all majors. <br>
	*Odontology is dentistry*")

ggsave("2025/images/prompt9_health_2025.jpg", width = 15, height = 8,
			 units = "in", dpi = 300)

ggsave("~/Data/greg_dubrow_io/posts/30-day-chart-challenge-2025/images/prompt9_health_2025.jpg",
			 width = 15, height = 8, units = "in", dpi = 300)

Humanities_plot +
	labs(title = "Humanities degrees mostly went to <span style = 'color: #5D3A9B;'>women</span>.
			 By major, <span style = 'color: #E66100;'>men</span> only earned more degrees in History")

ggsave("2025/images/prompt9_hum_2025.jpg", width = 15, height = 8,
			 units = "in", dpi = 300)

ggsave("~/Data/greg_dubrow_io/posts/30-day-chart-challenge-2025/images/prompt9_hum_2025.jpg",
			 width = 15, height = 8, units = "in", dpi = 300)

Science_plot +
	labs(title = "<span style = 'color: #E66100;'>Men</span> earned 60% of all natural and physical sciences degrees,
			 driven by 70%+ majorities in Math, Physics & Scientific IT.<br>
			 <span style = 'color: #5D3A9B;'>Women</span> earned more degrees in Chemistry & bio sciences.")

ggsave("2025/images/prompt9_sci_2025.jpg", width = 15, height = 8,
			 units = "in", dpi = 300)

ggsave("~/Data/greg_dubrow_io/posts/30-day-chart-challenge-2025/images/prompt9_sci_2025.jpg",
			 width = 15, height = 8, units = "in", dpi = 300)


`Social Sciences_plot` +
	labs(title = "<span style = 'color: #5D3A9B;'>Women</span> earned a slight majority of Social Sciences degrees,
			 with 75%+ majorities in Psychology, Sociology & Anthropology. <br>
			 <span style = 'color: #E66100;'>Men</span> earned more degrees in Business, Political Science & Economy (Economics)")

ggsave("2025/images/prompt9_socsci_2025.jpg", width = 15, height = 8,
			 units = "in", dpi = 300)

ggsave("~/Data/greg_dubrow_io/posts/30-day-chart-challenge-2025/images/prompt9_socsci_2025.jpg",
			 width = 15, height = 8, units = "in", dpi = 300)

`Technical sciences_plot` +
labs(title = "<span style = 'color: #E66100;'>Men</span> earned two-thirds of all Technical sciences degrees,
including 90% of degrees in Engineering & Electronics.
		 <br><span style = 'color: #5D3A9B;'>Women</span> earned more degrees in Bio & Health technology.")

ggsave("2025/images/prompt9_techsci_2025.jpg", width = 15, height = 8,
			 units = "in", dpi = 300)

ggsave("~/Data/greg_dubrow_io/posts/30-day-chart-challenge-2025/images/prompt9_techsci_2025.jpg",
			 width = 15, height = 8, units = "in", dpi = 300)


## reference code
# Humanities_plot_sorder <- Humanities_plot
# Humanities_plot_sorder
# Humanities_plot_norder <-
# Humanities_plot_norder

# prototype of plot on one discipline
# Science_pcts %>%
# 	filter(rank < 21) %>%
# 	mutate(deg_name = fct_reorder(deg_name, desc(deg_name))) %>%
# 	ggplot() +
# 	geom_col(aes(x = -50, y = deg_name), width = 0.75, fill = "#e0e0e0") +
# 	geom_col(aes(x = 50, y = deg_name), width = 0.75, fill = "#e0e0e0") +
# 	geom_col(aes(x = deg_sex_pct2, y = deg_name, fill = sex, color = sex), width = 0.75) +
# 	scale_x_continuous(expand = c(0, 0),
# 										 labels = function(x) abs(x), breaks = seq(-100, 100, 20)) +
# 	geom_vline(data = vlines_df, aes(xintercept = xintercept), color = "#FFFFFF", size = 0.1, alpha = 0.5) +
# 	coord_cartesian(clip = "off") +
# 	scale_fill_manual(values = c("#E66100", "#5D3A9B")) +
# 	#	scale_fill_manual(values = c("#C8102E", "#FFFFFF")) +
# 	#	scale_color_manual(values = c("#C8102E", "#C8102E")) +
# 	scale_color_manual(values = c("white", "white")) +
# 	geom_text(data = subset(Science_pcts, sex == "Men" & rank < 21),
# 						aes(x = deg_sex_pct2, y = deg_name, label = paste0(abs(deg_sex_pct2), "%")),
# 						size = 5, color = "white", position = position_dodge(width = .4), hjust = -0.09) +
# 	geom_text(data = subset(Science_pcts, sex == "Women" &  rank < 21),
# 						aes(x = deg_sex_pct2, y = deg_name, label = paste0(abs(deg_sex_pct2), "%")),
# 						size = 5, color = "white", position = position_dodge(width = .4), hjust = 1.1) +
# 	geom_text(data = subset(Science_pcts, sex == "Men" & rank < 21),
# 						aes(x = -100, y = deg_name, label = paste0("Total awarded = ", comma(deg_name_tot))),
# 						size = 4, color = "#3b3b3b", hjust = -.1) +
# 	labs(x = "", y = "",
# 			 caption = "*Data from Danmarks Statistik table UDDAKT60 via danstat package*") +
# 	theme_minimal() +
# 	theme(panel.grid = element_blank(),
# 				plot.title = element_markdown(size = 16),
# 				plot.subtitle = element_markdown(size = 12),
# 				plot.caption = element_markdown(),
# 				legend.position = "none", legend.title = element_blank(),
# 				axis.text.y = element_text(size = 10))
#
# geom_text(data = subset(tmp, sex == "Men"),
# 					aes(x = deg_sex_pct2, y = deg_field, label = paste0(abs(deg_sex_pct2), "%")),
# 					size = 5, color = "#C8102E",
# 					hjust = 1, nudge_x = -.5) +
# 	geom_text(data = subset(tmp, sex == "Women"),
# 						aes(x = deg_sex_pct2, y = deg_field, label = paste0(abs(deg_sex_pct2), "%")),
# 						size = 5, color = "#C8102E",
# 						hjust = -.25) +
#
