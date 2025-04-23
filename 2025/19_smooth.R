## something on a time series with 3 year rolling average?
## number of degrees by major area for all available years?

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
options(scipen=999)

# UDDAKT60
table_meta <- danstat::get_table_metadata(table_id = "uddakt60", variables_only = TRUE)

# create variable list using the ID value in the variable
variables_ed <- list(
	list(code = "uddannelse", values = "*"),
	list(code = "fstatus", values = c("F")),
	list(code = "køn", values = "*"),
	#list(code = "alder", values = c("TOT")),
	list(code = "tid", values = "*"))

bacdegs1 <- get_data("uddakt60", variables_ed, language = "en") %>%
	as_tibble() %>%
	clean_names()

glimpse(bacdegs1)


bacdegs_main <- bacdegs1 %>%
	mutate(kon = ifelse(kon == "Sex, total", "Total", kon)) %>%
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
		str_detect(deg_code, "H6080") ~ "Agriculture/Nature/Environment",
		str_detect(deg_code, "H6090") ~ "Health science",
		TRUE ~ "All")) %>%
	select(deg_code:deg_field, year = tid, sex = kon, degs_n = indhold)

glimpse(bacdegs_main)

bacdegs_main %>%
	count(deg_name)

bacdegs_main %>%
	filter(deg_name == "Bachelors programs") %>%
	filter(sex == "Total") %>%
	filter(year %in% c(2005, 2024)) %>%
	select(year, degs_n) %>%
	mutate(pct_chg_degs = pctchange(degs_n))

# zoo package https://cran.r-project.org/web/packages/zoo/index.html
bacdegs_all <-
bacdegs_main %>%
	filter(deg_name == "Bachelors programs") %>%
	filter(sex == "Total") %>%
	select(year, degs_n) %>%
	mutate(degs_3yr = zoo::rollmean(degs_n, k = 3, fill = NA)) %>%
	mutate(pct_chg_degs = pctchange(degs_n)) %>%
	mutate(pct_chg_degs2 = ifelse(year > 2005,
		round(pct_chg_degs * 100, 3), pct_chg_degs)) %>%
	mutate(pct_chg_degs2 = ifelse(is.na(pct_chg_degs2), 0, pct_chg_degs2)) %>%
	mutate(cum_change = cumsum(pct_chg_degs2)) %>%
	mutate(norm100 = cum_change + 100)

glimpse(bacdegs_all)


# line plot with all degs & 3 year average
alldegs_plot <-
bacdegs_all %>%
	ggplot(aes(x = year)) +
	geom_line(aes(y = degs_n), color = "#80B1D3", size = 1 ) +
	geom_line(aes(y = degs_3yr), color = "#FDB462", linetype = 2, size = 1) +
	scale_x_continuous(breaks = c(2005, 2008, 2010, 2012, 2014, 2016, 2018, 2020, 2022, 2024))+
	scale_y_continuous(limits = c(8000, 21000),
		labels = label_comma()) +
	theme_minimal() +
	labs(x = "", y = "",
		title = "The number of academic bachelor's degrees awarded increased
		<br>by 77% from 2005-2024",
		subtitle = "<span style = 'color: #80B1D3;'> Blue line is total degrees awarded.</span>
		<span style = 'color: #FDB462;'> Orange dotted line is 3-year rolling average.</span> ",
		caption = "*Data from Danmarks Statistik table UDDAKT60 via danstat package*") +
	theme(
		panel.background = element_rect(fill = "grey95", colour = "grey95"),
		plot.title = element_markdown(size = 14, color = "grey35"),
		plot.subtitle = element_markdown(size = 11),
		plot.caption = element_markdown(size = 8),
		axis.text.x = element_text(size = 9, color = "grey50"),
		axis.text.y = element_text(size = 9, color = "grey50"),
		panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave("2025/images/prompt19_1_2025.jpg", width = 15, height = 8,
	units = "in", dpi = 300)

ggsave("~/Data/greg_dubrow_io/posts/30-day-chart-challenge-2025/images/prompt19_1_2025.jpg",
	width = 15, height = 8, units = "in", dpi = 300)

# with percent change
pctchange_plot <-
bacdegs_all %>%
	ggplot(aes(x = year, y = pct_chg_degs)) +
	geom_line(color = "#80B1D3", size = 1) +
	geom_hline(aes(yintercept = mean(pct_chg_degs, na.rm = TRUE)),
		color="#FDB462") +
	scale_x_continuous(breaks = c(2005, 2008, 2010, 2012, 2014, 2016, 2018, 2020, 2022, 2024)) +
	scale_y_continuous(limits = c(-.2, .2),
		labels = label_percent(),
		breaks = c(-.2, -.15, -.10, -.05, 0, .05, .10, .15, .2)) +
	theme_minimal() +
	labs(x = "", y = "",
		title = "The percent change in number of academic bachelor's degrees awarded has
		<br>fluctuated year-to-year from 2005 to 2024",
		subtitle = glue::glue("<span style = 'color: #80B1D3;'> Blue line is annual % change degrees awarded 2005-2024.</span>
		<span style = 'color: #FDB462;'> Average year-to-year change is
			{round(mean(bacdegs_all$pct_chg_degs, na.rm = TRUE) *100, 2)}% </span> "),
		caption = "*Data from Danmarks Statistik table UDDAKT60 via danstat package*") +
	theme(
		panel.background = element_rect(fill = "grey95", colour = "grey95"),
		plot.title = element_markdown(size = 14, color = "grey35"),
		plot.subtitle = element_markdown(size = 11),
		plot.caption = element_markdown(size = 8),
		axis.text.x = element_text(size = 9, color = "grey50"),
		axis.text.y = element_text(size = 9, color = "grey50"),
		panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave("2025/images/prompt19_2_2025.jpg", width = 15, height = 8,
	units = "in", dpi = 300)

ggsave("~/Data/greg_dubrow_io/posts/30-day-chart-challenge-2025/images/prompt19_1_2025.jpg",
	width = 15, height = 8, units = "in", dpi = 300)


alldegs_plot + pctchange_plot
ggsave("2025/images/prompt19_1&2_2025.jpg", width = 15, height = 8,
	units = "in", dpi = 300)

ggsave("~/Data/greg_dubrow_io/posts/30-day-chart-challenge-2025/images/prompt19_1&2_2025.jpg",
	width = 15, height = 8, units = "in", dpi = 300)


# line plot with norm to 100 NOT DOING

## share of bacs to women over time

bacdegs_main %>%
	count(deg_field)

bacdegs_main %>%
	filter(!deg_name == "Bachelors programs") %>%
	filter(deg_group == "Main") %>%
	filter(!sex == "Total") %>%
	select(year, deg_field, sex, degs_n) %>%
	group_by(year, deg_field) %>%
	mutate(pct_sex_field = degs_n / sum(degs_n)) %>%
	ungroup() %>%
	ggplot(aes(x = year, y = pct_sex_field, color = sex)) +
	geom_point() +
	geom_smooth() +
	# for sex men (orange) & women (purple) colors
	scale_color_manual(values = c("#E66100", "#5D3A9B")) +
	scale_x_continuous(breaks =
			c(2005, 2008, 2010, 2012, 2014, 2016, 2018, 2020, 2022, 2024)) +
	scale_y_continuous(labels = label_percent()) +
	labs(x = "", y = "",
		title = "<span style = 'color: #5D3A9B;'>Women</span> have consistently earned more bachelor's degrees than
		 <span style = 'color: #E66100;'>men</span> in all disciplines except Sciences and Technical Sciences.",
		subtitle = "Bachelor's degrees by discipline and sex, 2005-2024",
		caption = "*Data from Danmarks Statistik table UDDAKT60 via danstat package*") +
	facet_wrap(~ deg_field, scales = "free_x") +
	theme(
		legend.position = "none",
		panel.background = element_rect(fill = "white", colour = "grey95"),
		panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
		plot.title = element_markdown(size = 14, color = "grey35"),
		plot.subtitle = element_markdown(size = 11),
		plot.caption = element_markdown(size = 8),
		axis.text.x = element_text(size = 9, color = "grey50"),
		axis.text.y = element_text(size = 9, color = "grey50"))

ggsave("2025/images/prompt19_3_2025.jpg", width = 15, height = 8,
	units = "in", dpi = 300)

ggsave("~/Data/greg_dubrow_io/posts/30-day-chart-challenge-2025/images/prompt19_3_2025.jpg",
	width = 15, height = 8, units = "in", dpi = 300)


## technical sciences by major
bacdegs_main %>%
	filter(!deg_name == "Bachelors programs") %>%
	filter(deg_group == "Sub") %>%
	filter(!sex == "Total") %>%
	filter(deg_field == "Technical sciences") %>%
	select(year, deg_name, sex, degs_n) %>%
	ggplot(aes(x = year, y = degs_n, color = sex)) +
	geom_point() +
	geom_smooth() +
	labs(x = "", y = "",
		title = "The Electronics & IT major is responsible for most of the gendered split in
		Technical Sciences degrees awarded to
		<span style = 'color: #5D3A9B;'>women</span> and
		<span style = 'color: #E66100;'>men</span>.",
		subtitle = "Technical Sciences bachelor's degrees by major and sex, 2005-2024",
		caption = "*Data from Danmarks Statistik table UDDAKT60 via danstat package*") +
	scale_color_manual(values = c("#E66100", "#5D3A9B")) +
	scale_x_continuous(breaks =
			c(2005, 2008, 2010, 2012, 2014, 2016, 2018, 2020, 2022, 2024)) +
	facet_wrap(~ deg_name, scales = "free") +
	theme(
		legend.position = "none",
		panel.background = element_rect(fill = "white", colour = "grey95"),
		panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
		plot.title = element_markdown(size = 14, color = "grey35"),
		plot.subtitle = element_markdown(size = 11),
		plot.caption = element_markdown(size = 8),
		axis.text.x = element_text(size = 9, color = "grey50"),
		axis.text.y = element_text(size = 9, color = "grey50"))


ggsave("2025/images/prompt19_4_2025.jpg", width = 15, height = 8,
	units = "in", dpi = 300)

ggsave("~/Data/greg_dubrow_io/posts/30-day-chart-challenge-2025/images/prompt19_4_2025.jpg",
	width = 15, height = 8, units = "in", dpi = 300)
