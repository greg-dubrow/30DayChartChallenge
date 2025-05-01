library(tidyverse) # to do tidyverse things
library(tidylog) # to get a log of what's happening to the data
library(janitor) # tools for data cleaning
library(danstat) # package to get Danish statistics via api
library(ggtext) # enhancements for text in ggplot
library(scales)
library(waffle) # to make waffles
library(patchwork)


# some custom functions
source("~/Data/r/basic functions.R")



## waffles of attainment by immigrant type. immigrant type on x axis. 4 charts, one each for 2005, 2011, 2017, 2023

# get data
# LIGEUB1: Educational attainment (15-69 years) by region, ancestry, highest education completed, age and sex
table_meta_attain <- danstat::get_table_metadata(table_id = "ligeub1", variables_only = TRUE)
table_meta_attain_dk <- danstat::get_table_metadata(table_id = "ligeub1", variables_only = TRUE, language = "da")

## get attainment data
variables_attain <- list(
	list(code = "herkomst", values = "*"),
	list(code = "alder", values = "*"),
	list(code = "hfudd", values = "*"),
	list(code = "tid", values = c(2005, 2011, 2017, 2023)))

ed_attain1 <- get_data("ligeub1", variables_attain, language = "en") %>%
	as_tibble() %>%
	clean_names()

glimpse(ed_attain1)

ed_attain1 %>%
	count(herkomst)

ed_attain <- ed_attain1 %>%
	mutate(alder = ifelse(alder == "Age, total", "Total", alder)) %>%
	mutate(alder = str_remove(alder, " years")) %>%
	mutate(hfudd = case_when(
		hfudd == "H10 Primary education" ~ "Primary",
		hfudd == "H20 Upper secondary education" ~
			"HS-Academic",
		hfudd == "H30 Vocational Education and Training (VET)" ~
			"HS-Vocational",
		hfudd == "H35 Qualifying educational programs" ~
			"Qualifying educ paths",
		hfudd == "H40 Short cycle higher education" ~
			"Short-cycle coll",
		hfudd == "H50 Vocational bachelors educations" ~
			"Bach-Vocational",
		hfudd == "H60 Bachelors programs" ~
			"Bach-Academic",
		hfudd == "H70 Masters programs" ~ "Masters",
		hfudd == "H80 PhD programs" ~ "PhD",
		hfudd == "H90 Not stated" ~ "Not stated",
		TRUE ~ hfudd)) %>%
	mutate(hfudd = factor(hfudd,
		levels = c("Primary", "HS-Vocational", "HS-Academic", "Short-cycle coll",
			"Qualifying educ paths", "Bach-Vocational", "Bach-Academic",
			"Masters", "PhD", "Not stated", "Total"))) %>%
	mutate(herkomst = case_when(
		herkomst == "Descendants from non-western countries" ~ "Descendants: non-western",
		herkomst == "Descendants from western countries" ~ "Descendants: western",
		herkomst == "Immigrants from non-western countries" ~ "Immigrants: non-western",
		herkomst == "Immigrants from western countries" ~ "Immigrants: western",
		herkomst == "Persons of Danish origin" ~ "Danish origin",
		TRUE ~ herkomst)) %>%
	mutate(herkomst = factor(herkomst,
		levels = c("Descendants: non-western", "Descendants: western", "Descendants, total",
			"Immigrants: non-western", "Immigrants: western", "Immigrants, total",
			"Danish origin", "Total"))) %>%
	rename(year = tid, N = indhold)

glimpse(ed_attain)

ed_attain %>%
	count(alder)

ed_attain_waffle <- ed_attain %>%
	filter(herkomst %notin% c("Total", "Descendants, total", "Immigrants, total")) %>%
	filter(alder %notin% c("Total", "15-19", "20-24")) %>%
	filter(!hfudd == "Total") %>%
	group_by(year, herkomst) %>%
	mutate(herkomst_yr_sum = sum(N)) %>%
	ungroup() %>%
	group_by(year, herkomst, hfudd) %>%
	mutate(herkomst_ed_yr_sum = sum(N)) %>%
	ungroup() %>%
	mutate(herkomst_ed_yr_pct = herkomst_ed_yr_sum / herkomst_yr_sum) %>%
	mutate(herkomst_ed_yr_pct2 = round(herkomst_ed_yr_pct, 2) * 100) %>%
	distinct(herkomst, hfudd, year, .keep_all = TRUE) %>%
	select(-alder, -N) %>%
	arrange(herkomst, hfudd) %>%
	## fix groups to even out to 100
	mutate(herkomst_ed_yr_pct2 = ifelse((herkomst == "Danish origin" &
			hfudd == "PhD" & year == 2005), 1, herkomst_ed_yr_pct2)) %>%
	mutate(herkomst_ed_yr_pct2 = ifelse((herkomst == "Descendants: western" &
			hfudd == "Not stated" & year == 2011), 11, herkomst_ed_yr_pct2)) %>%
	mutate(herkomst_ed_yr_pct2 = ifelse((herkomst == "Immigrants: non-western" &
			hfudd == "HS-Academic" & year == 2011), 10, herkomst_ed_yr_pct2)) %>%
	mutate(herkomst_ed_yr_pct2 = ifelse((herkomst == "Immigrants: western" &
			hfudd == "Bach-Academic" & year == 2011), 3, herkomst_ed_yr_pct2)) %>%
	mutate(herkomst_ed_yr_pct2 = ifelse((herkomst == "Danish origin" &
		hfudd == "Bach-Vocational" & year == 2011), 17, herkomst_ed_yr_pct2)) %>%
	mutate(herkomst_ed_yr_pct2 = ifelse((herkomst == "Descendants: non-western" &
		hfudd == "Primary" & year == 2017), 29, herkomst_ed_yr_pct2)) %>%
	mutate(herkomst_ed_yr_pct2 = ifelse((herkomst == "Descendants: non-western" &
			hfudd == "Not stated" & year == 2017), 3, herkomst_ed_yr_pct2)) %>%
	mutate(herkomst_ed_yr_pct2 = ifelse((herkomst == "Descendants: western" &
			hfudd == "Masters" & year == 2017), 18, herkomst_ed_yr_pct2)) %>%
	mutate(herkomst_ed_yr_pct2 = ifelse((herkomst == "Danish origin" &
			hfudd == "Short-cycle coll" & year == 2017), 5, herkomst_ed_yr_pct2)) %>%
	mutate(herkomst_ed_yr_pct2 = ifelse((herkomst == "Descendants: western" &
			hfudd == "HS-Academic" & year == 2023), 10, herkomst_ed_yr_pct2)) %>%
	mutate(herkomst_ed_yr_pct2 = ifelse((herkomst == "Immigrants: non-western" &
			hfudd == "Short-cycle coll" & year == 2023), 6, herkomst_ed_yr_pct2)) %>%
	mutate(herkomst_ed_yr_pct2 = ifelse((herkomst == "Danish origin" &
			hfudd == "Masters" & year == 2023), 13, herkomst_ed_yr_pct2))



glimpse(ed_attain_waffle)

ed_attain_waffle %>%
	count(hfudd)

## waffle plot function
waffleplot <- function(plotdf, filter_expr) {

	# Convert the string expression to an actual R expression
	filter_expr <- rlang::parse_expr(filter_expr)
	# Filter the data
	filtered_df <- plotdf %>% filter(!!filter_expr)

	# Create the plot
	ggplot(filtered_df, (aes(fill = hfudd, values = herkomst_ed_yr_pct2))) +
		geom_waffle(na.rm=TRUE, n_rows=10, flip=TRUE, size = 0.33, colour = "white") +
		facet_wrap(~herkomst, nrow=1,strip.position = "bottom", scales = "free_x") +
		scale_x_discrete() +
		scale_y_continuous(labels = function(x) x * 10, # make this multipler the same as n_rows
			expand = c(0,0)) +
		scale_fill_manual(values = c( "#4E79A7", "#F28E2B", "#BEBADA", "#FF9D9A", "#B07AA1",
			"#9D7660", "#FB8072", "#F1CE63", "#A0CBE8", "#FFBE7D")) +
		theme_minimal() +
		theme(legend.position = "bottom", legend.justification = "left",
			legend.spacing.x = unit(0, 'cm'),
			legend.key.width = unit(1, 'cm'), legend.margin=margin(-10, 0, 0, 0),
			legend.title = element_text(size = 8), legend.text = element_text(size = 8),
			panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
		guides(fill = guide_legend(label.position = "bottom", nrow = 1,
			title = "Education level attained", title.position = "top"))
}

plot_2005 <- waffleplot(ed_attain_waffle, "year == 2005")
plot_2011 <- waffleplot(ed_attain_waffle, "year == 2011")
plot_2017 <- waffleplot(ed_attain_waffle, "year == 2017")
plot_2023 <- waffleplot(ed_attain_waffle, "year == 2023")

### add titles to the plots

plot_2005 <-
	plot_2005 +
	labs(subtitle = "Year: 2005")

plot_2011 <-
	plot_2011 +
	labs(subtitle = "Year: 2011")

plot_2017 <-
	plot_2017 +
	labs(subtitle = "Year: 2017")

plot_2023 <-
	plot_2023 +
	labs(subtitle = "Year: 2023")


# stich plots together
plot_2005 + plot_2011 +  plot_2017 + plot_2023 +
	plot_annotation(
		title = "Since 2005, immigrants & descendants are less likely to discontinue education after primary level, and more is known about their education status.",
		subtitle = "Educational attainment for people ages 25-45, by national origin. Each block = 1 %",
		caption = "Data from Danmarks Statistik table LIGEUB1 via danstat package")

ggsave("2025/images/prompt28_waffle_2025.jpg", width = 15, height = 8,
	units = "in", dpi = 300)

ggsave("~/Data/greg_dubrow_io/posts/30-day-chart-challenge-2025/images/prompt28_waffle_2025.jpg",
	width = 15, height = 8, units = "in", dpi = 300)


## waffle plot demo
ed_attain_waffle %>%
	filter(year == 2005) %>%
#	arrange(herkomst, hfudd) %>%
	#	select(parent_ed, child_age_group, child_ed, N, child_age_N, child_age_ed_pct2) %>%
	#	view()
	ggplot(aes(fill = hfudd, values = herkomst_ed_yr_pct2)) +
	geom_waffle(na.rm=TRUE, n_rows=10, flip=TRUE, size = 0.33, colour = "white") +
	facet_wrap(~herkomst, nrow=1,strip.position = "bottom") +
	scale_x_discrete() +
	scale_y_continuous(labels = function(x) x * 10, # make this multiplier the same as n_rows
		expand = c(0,0)) +
	scale_fill_manual(values = c( "#4E79A7", "#F28E2B", "#BEBADA", "#FF9D9A", "#B07AA1",
	"#9D7660", "#FB8072", "#F1CE63", "#A0CBE8", "#FFBE7D")) +
	#scale_fill_brewer(palette = "Set2") +
	labs(title = "For Danes aged 25-29, higher levels of parent educational attainment mean greater likelihood of finishing higher education",
		subtitle = "Higher educational completion status by parent educational attainment and age, 2023.<br>
				 Each block = 1 %",
		caption = "Data from Danmarks Statistik table LIGEUB1 via danstat package",
		x = "", y = "") +
	theme_minimal() +
	theme(legend.position = "bottom", legend.spacing.x = unit(0, 'cm'),
		legend.key.width = unit(1.5, 'cm'), legend.margin=margin(-10, 0, 0, 0),
		plot.title = element_text(hjust = 0), plot.subtitle = element_markdown(),
		plot.caption = element_markdown(),
		panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
	guides(fill = guide_legend(label.position = "bottom", nrow = 1,
		title = "Education level attained", title.position = "top"))


## shares of degree level to immigrant type over time. all years, year on x axis, like in smooth
# UDDAKT12: Educational activity by education, age, ancestry, national origin, sex and status
table_meta_levels <- danstat::get_table_metadata(table_id = "uddakt12", variables_only = TRUE)

variables_edlev <- list(
list(code = "uddannelse", values = c("H20", "H30", "H40", "H50",
	"H60", "H70", "H80")),
	list(code = "fstatus", values = c("F")),
	#list(code = "køn", values = "*"),
	 list(code = "herkomst", values = c(5, 4, 3, 0)),
	 list(code = "herkomst1", values = "*"),
	list(code = "alder", values = c("17", "18", "19", "20", "21", "22", "23",
		"24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35",
		"36", "37", "38", "39", "40-")),
	list(code = "tid", values = "*"))

alldegs1 <- get_data("uddakt12", variables_edlev, language = "en") %>%
	as_tibble() %>%
	clean_names()

glimpse(alldegs1)

alldegs1 %>%
	select(herkomst, herkomst1, indhold) %>%
	view()

alldegs_main <- alldegs1 %>%
	filter(!herkomst1 == "Total") %>%
	mutate(nat_orgin_group = paste0(herkomst, "-", herkomst1)) %>%
	filter(!nat_orgin_group == "Persons of Danish origin-Western countries") %>%
	filter(!nat_orgin_group == "Persons of Danish origin-Non-western countries") %>%
	filter(!nat_orgin_group == "Persons of Danish origin-National origin, not stated") %>%
	filter(!nat_orgin_group == "Unknown origin-Denmark") %>%
	filter(!nat_orgin_group == "Immigrants-Denmark") %>%
	filter(!nat_orgin_group == "Descendant-Denmark") %>%
	filter(!nat_orgin_group == "Descendant-National origin, not stated") %>%
	filter(!nat_orgin_group == "Unknown origin-Non-western countries") %>%
	filter(!nat_orgin_group == "Unknown origin-Western countries") %>%
	mutate(nat_orgin_group = ifelse(
		(nat_orgin_group == "Immigrants-National origin, not stated" & indhold >0),
		"Immigrants-Non-western countries", nat_orgin_group)) %>%
	filter(!nat_orgin_group == "Immigrants-National origin, not stated") %>%
	mutate(nat_orgin_group = case_when(
		nat_orgin_group == "Descendant-Non-western countries" ~ "Descendants: non-western",
		nat_orgin_group == "Descendant-Western countries" ~ "Descendants: western",
		nat_orgin_group == "Immigrants-Non-western countries" ~ "Immigrants: non-western",
		nat_orgin_group == "Immigrants-Western countries" ~ "Immigrants: western",
		nat_orgin_group == "Persons of Danish origin-Denmark" ~ "Danish origin",
		nat_orgin_group == "Unknown origin-National origin, not stated" ~ "Unknown origin",
		TRUE ~ nat_orgin_group)) %>%
	mutate(nat_orgin_group = factor(nat_orgin_group,
		levels = c("Descendants: non-western", "Descendants: western",
			"Immigrants: non-western", "Immigrants: western", "Unknown origin",
			"Danish origin"))) %>%
	mutate(alder = str_remove(alder, (" years-"))) %>%
	mutate(alder = str_remove(alder, (" years"))) %>%
	mutate(age_group = case_when(
		alder %in% c("17", "18", "19", "20") ~ "17-20",
		alder %in% c("21", "22", "23", "24") ~ "21-24",
		alder %in% c("25", "26", "27", "28", "29")
		~ "25-29",
		alder %in% c("30", "31", "32", "33", "34")
		~ "30-34",
		alder %in% c("35", "36", "37", "38", "39")
		~ "35-39",
		alder == "40" ~ "40+")) %>%
	mutate(alder = as.numeric(alder)) %>%
	mutate(uddannelse =
			case_when(uddannelse == "H20 Upper secondary education" ~
					"HS-Academic",
				uddannelse == "H30 Vocational Education and Training (VET)" ~
					"HS-Vocational",
				uddannelse == "H35 Qualifying educational programs" ~
					"Qualifying educ paths",
				uddannelse == "H40 Short cycle higher education" ~
					"Short-cycle college",
				uddannelse == "H50 Vocational bachelors educations" ~
					"Bachelor-Vocational",
				uddannelse == "H60 Bachelors programs" ~
					"Bachelor-Academic",
				uddannelse == "H70 Masters programs" ~ "Masters",
				uddannelse == "H80 PhD programs" ~ "PhD",
				TRUE ~ uddannelse)) %>%
	mutate(uddannelse = factor(uddannelse,
		levels = c("HS-Academic", "HS-Vocational", "Short-cycle college",
			"Bachelor-Vocational", "Bachelor-Academic", "Masters", "PhD"))) %>%
	select(degree = uddannelse, nat_orgin_group, age_group, year = tid, N = indhold,
		herkomst, herkomst1, alder)

glimpse(alldegs_main)

alldegs_main %>%
	count(alder, age_group) %>%
	view()

# number of degs by year facet by degree and nat origin
alldegs_main %>%
	select(nat_orgin_group, degree, alder, year, N) %>%
	group_by(nat_orgin_group, degree, year) %>%
	mutate(degs_natorig_yr = sum(N)) %>%
	distinct(nat_orgin_group, degree, year, .keep_all = TRUE) %>%
	filter(!nat_orgin_group == "Unknown origin") %>%
	ggplot(aes(year, degs_natorig_yr)) +
	geom_bar(stat = "identity", fill = "#C60C30") +
	labs(x = "", y = "",
		title = "Immigrants to Denmark are earning more Master's and vocational degrees over time.",
		subtitle = "Degrees earned by type and national origin, 2005-2024.",
		caption = "*Data from Danmarks Statistik table UDDAKT12 via danstat package.*") +
	scale_x_continuous(breaks = c(2005, 2011, 2017, 2024)) +
	scale_y_continuous(labels = scales::label_comma()) +
	theme(panel.background = element_rect(fill = "white"),
		panel.grid.minor = element_line(color = "grey90"),
		plot.title = element_markdown(size = 14, color = "grey35"),
		plot.subtitle = element_markdown(size = 11),
		plot.caption = element_markdown(size = 8)) +
	facet_grid(nat_orgin_group ~ degree, scales = "free_y")
rm(tmp)

ggsave("2025/images/prompt28_degsall_2025.jpg", width = 15, height = 8,
	units = "in", dpi = 300)

ggsave("~/Data/greg_dubrow_io/posts/30-day-chart-challenge-2025/images/prompt28_degsall_2025.jpg",
	width = 15, height = 8, units = "in", dpi = 300)


# Calculate the weighted average age
# weighted_avg_age <- sum(df$age * df$n_age) / sum(df$n_age)

# line graphs with average age, faceted by degree and nat origin
alldegs_main %>%
	select(nat_orgin_group, degree, alder, year, N) %>%
	group_by(nat_orgin_group, degree, year) %>%
#	filter(degree == "Bachelor-Academic") %>%
	mutate(age_x_n_yr = alder*N) %>%
	mutate(avg_age = sum(age_x_n_yr) / sum(N)) %>%
	filter(N > 0) %>%
	filter(!nat_orgin_group == "Unknown origin") %>%
	mutate(median_age =
			Hmisc::wtd.quantile(alder, weights = N, probs = 0.5, na.rm = TRUE)) %>%
	ungroup() %>%
	distinct(nat_orgin_group, degree, year, .keep_all = TRUE) %>%
	select(nat_orgin_group, degree, year, avg_age, median_age) %>%
	{. ->> tmp} %>%
	ggplot(aes(year, avg_age)) +
	geom_line() +
	scale_x_continuous(breaks = c(2005, 2011, 2017, 2024)) +
	geom_text(
		data = subset(tmp, year == 2005),
		aes(y = avg_age, label = round(avg_age, 1)),
		color = "#C60C30", size = 3, vjust = -1, hjust = -.1) +
	geom_text(
		data = subset(tmp, year == 2024),
		aes(y = avg_age, label = round(avg_age, 1)),
		color = "#C60C30", size = 3, vjust = -1, hjust = 1) +
	labs(x = "", y = "",
		title = "Immigrants to Denmark are older than other groups when they earn the same degree.",
		subtitle = "Average age for degree earners by degree type and national origin, 2005-2024.",
		caption = "*Data from Danmarks Statistik table UDDAKT12 via danstat package.*") +
	theme(panel.background = element_rect(fill = "grey90"),
		panel.grid.minor = element_line(color = "grey90"),
		plot.title = element_markdown(size = 14, color = "grey35"),
		plot.subtitle = element_markdown(size = 11),
		plot.caption = element_markdown(size = 8)) +
	facet_grid(nat_orgin_group ~ degree)

ggsave("2025/images/prompt28_degsage_2025.jpg", width = 15, height = 8,
	units = "in", dpi = 300)

ggsave("~/Data/greg_dubrow_io/posts/30-day-chart-challenge-2025/images/prompt28_degsall_2025.jpg",
	width = 15, height = 8, units = "in", dpi = 300)

## types of vocational and masters degrees earned by immigrants
table_meta_levels <- danstat::get_table_metadata(table_id = "uddakt12", variables_only = TRUE)

variables_voced <- list(
	list(code = "uddannelse", values = c("H30", "H3010", "H3015", "H3020",
		"H3025", "H3030", "H3035", "H3040", "H3045", "H3050", "H3055",
		"H3060", "H3065", "H3090", "H35", "H3510", "H3520", "H3530",
		"H50", "H5020", "H5024", "H5025", "H5030", "H5038", "H5039", "H5058",
		"H5059", "H5075", "H5080", "H5085", "H5089", "H5095",
		"H70", "H7020", "H7025", "H7030", "H7035", "H7039", "H7059", "H7075",
		"H7080", "H7090", "H7095")),
	list(code = "fstatus", values = c("F")),
	list(code = "herkomst", values = c(4, 3)),
	list(code = "herkomst1", values = "*"),
	#	list(code = "køn", values = "*"),
	list(code = "alder", values = "TOT"),
	list(code = "tid", values = "*"))

voced_degs1 <- get_data("uddakt12", variables_voced, language = "en") %>%
	as_tibble() %>%
	clean_names()

glimpse(voced_degs1)

voced_degs <- voced_degs1 %>%
	mutate(deg_code = str_extract(uddannelse, "^[^ ]+")) %>%
	mutate(deg_name = sub("^\\S+\\s+", '', uddannelse)) %>%
	mutate(deg_name = str_remove(deg_name, ", BACH")) %>%
	mutate(deg_name = str_remove(deg_name, " BACH")) %>%
	mutate(deg_name = str_remove(deg_name, ", SCE")) %>%
	mutate(deg_name = str_remove(deg_name, ", TBT")) %>%
	mutate(deg_name = str_remove(deg_name, ", VBE")) %>%
	mutate(deg_name = str_remove(deg_name, ", MASTER")) %>%
	mutate(deg_name = str_replace(deg_name, " \\s*\\([^\\)]+\\)", ""))


glimpse(voced_degs)

voced_degs %>%
	count(deg_name) %>%
	view()













#### probably don't need these
# UDDAKT50 vocational higher ed
table_meta_voched <- danstat::get_table_metadata(table_id = "uddakt50", variables_only = TRUE)


# UDDAKT60 academic higher ed
table_meta_ached <- danstat::get_table_metadata(table_id = "uddakt60", variables_only = TRUE)

bacdegs1 <- get_data("uddakt60", variables_ed, language = "en") %>%
	as_tibble() %>%
	clean_names()


## shares of major fields within degree levels by immigrant type



### old code not needed
# EDA on age
alldegs_main %>%
	#	filter(nat_orgin_group == "Danish origin") %>%
	filter(year %in% c(2005, 2010, 2015, 2019, 2024)) %>%
	filter(degree == "HS-Academic") %>%
	ggplot(aes(x = age_group, y = N)) +
	geom_bar(stat = "identity") +
	facet_grid(year ~ nat_orgin_group, scales = "free")

alldegs_main %>%
	filter(nat_orgin_group == "Danish origin") %>%
	filter(year %in% c(2005, 2010, 2015, 2019, 2024)) %>%
	#	filter(age_group %in% c("17-20", "19-20")) %>%
	filter(degree == "Bachelor-Academic") %>%
	ggplot(aes(x = alder, y = N)) +
	geom_bar(stat = "identity") +
	facet_grid(year ~ nat_orgin_group, scales = "free")

