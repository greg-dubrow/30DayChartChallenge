library(tidyverse) # to do tidyverse things
library(tidylog) # to get a log of what's happening to the data
library(janitor) # tools for data cleaning
library(danstat) # package to get Danish statistics via api
library(waffle) # to make waffles
library(scales)
library(patchwork)
library(ggtext)


source("~/Data/r/basic functions.R")


### No need to load data from StatBank, using same df from prompt 14

# STATUSV2 higher ed status by parent ed
# STATUSU2 does secondary completion by parent ed
table_meta <- danstat::get_table_metadata(table_id = "statusv2", variables_only = TRUE)
table_meta_dk <- danstat::get_table_metadata(table_id = "statusv2", variables_only = TRUE, language = "da")

# create variable list using the ID value in the variable
variables_ed <- list(
	list(code = "statusvid", values = "*"),
	list(code = "alder1", values = "*"),
	list(code = "forudd1", values = "*"),
	list(code = "tid", values = 2023))

ed_attain1 <- get_data("statusv2", variables_ed, language = "en") %>%
	as_tibble() %>%
	clean_names()

ed_attain_main <- ed_attain1 %>%
	mutate(statusvid =
			ifelse(statusvid == "Educational graduation statement, total", "Total", statusvid)) %>%
	mutate(alder1 =
			ifelse(alder1 == "Age, total", "Total", alder1)) %>%
	mutate(forudd1 =
			case_when(forudd1 == "H10 Primary education" ~ "Primary",
				forudd1 == "H20 Upper secondary education" ~
					"HS-Academic",
				forudd1 == "H30 Vocational Education and Training (VET)" ~
					"HS-Vocational",
				forudd1 == "H35 Qualifying educational programs" ~
					"Qualifying educ paths",
				forudd1 == "H40 Short cycle higher education" ~
					"Short-cycle coll",
				forudd1 == "H50 Vocational bachelors educations" ~
					"Bach-Vocational",
				forudd1 == "H60 Bachelors programs" ~
					"Bach-Academic",
				forudd1 == "H70 Masters programs" ~ "Masters",
				forudd1 == "H80 PhD programs" ~ "PhD",
				forudd1 == "H90 Not stated" ~ "Not stated",
				TRUE ~ forudd1)) %>%
	mutate(statusvid = ifelse(
		statusvid == "No registrated education", "No registered education", statusvid)) %>%
	mutate(child_age_group =
			case_when(
				alder1 %in% c("25 years", "26 years", "27 years", "28 years", "29 years") ~ "25-29",
				alder1 %in% c("30 years", "31 years", "32 years", "33 years", "34 years") ~ "30-34",
				alder1 %in% c("35 years", "36 years", "37 years", "38 years", "39 years") ~ "35-39",
				alder1 %in% c("40 years", "41 years", "42 years",
					"43 years", "44 years", "45 years") ~ "40-45",
				TRUE ~ alder1)) %>%
	select(parent_ed = forudd1, child_age = alder1, child_age_group, child_ed = statusvid, N = indhold)

glimpse(ed_attain_main)

ed_attain_main %>%
	count(parent_ed)

saveRDS(ed_attain_main,
	"~/Data/greg_dubrow_io/posts/30-day-chart-challenge-2025/data/ed_attain_main.Rda")

ed_attain_main <-
	readRDS("~/Data/greg_dubrow_io/posts/30-day-chart-challenge-2025/data/ed_attain_main.Rda")

glimpse(ed_attain_main)


## x axis is parent ed level (the facet group), fill is child higher ed achievement,
## values are pct of each parent ed level in higher ed attain group
## facet grid for each age group? separate plot for each age group and patchwork all together?
## df needs: parent ed, child ed group, age group (include total)

ed_attain_main %>%
	filter(!parent_ed == "Total") %>%
	filter(!child_ed == "Total") %>%
	filter(!child_age_group == "Total") %>%
	group_by(child_age_group, parent_ed) %>%
	mutate(age_grp_par_ed_sum = sum(N)) %>%
	ungroup() %>%
	group_by(child_age_group, parent_ed, child_ed) %>%
	mutate(age_par_ed_child_ed_sum = sum(N)) %>%
	ungroup() %>%
	mutate(age_par_ed_child_ed_pct = age_par_ed_child_ed_sum / age_grp_par_ed_sum) %>%
	distinct(parent_ed, child_age_group, child_ed, .keep_all = TRUE) %>%
	select(-child_age) %>%
	view()

ed_attain_waffle <- ed_attain_main %>%
	filter(!parent_ed == "Total") %>%
	filter(!parent_ed == "Qualifying educ paths") %>%
	filter(!child_ed == "Total") %>%
	filter(!child_age_group == "Total") %>%
	group_by(child_age_group, parent_ed) %>%
	mutate(age_grp_par_ed_sum = sum(N)) %>%
	ungroup() %>%
	group_by(child_age_group, parent_ed, child_ed) %>%
	mutate(age_par_ed_child_ed_sum = sum(N)) %>%
	ungroup() %>%
	mutate(age_par_ed_child_ed_pct = age_par_ed_child_ed_sum / age_grp_par_ed_sum) %>%
	mutate(age_par_ed_child_ed_pct2 = round(age_par_ed_child_ed_pct, 2) * 100) %>%
	distinct(parent_ed, child_age_group, child_ed, .keep_all = TRUE) %>%
	select(-child_age, -N) %>%
	mutate(parent_ed = factor(parent_ed,
		levels = c("Primary", "HS-Vocational", "HS-Academic", "Short-cycle coll",
			"Bach-Vocational", "Bach-Academic", "Masters", "PhD", "Not stated"))) %>%
	mutate(child_ed = factor(child_ed,
		levels = c("Completed education", "Undergoing education", "Discontinued education",
			"No registered education"))) %>%
	# clean up pct vals to make charts even out
	mutate(age_par_ed_child_ed_pct2 =
			ifelse(parent_ed == "HS-Vocational" & child_ed == "Completed education" & child_age_group == "25-29",
				40, age_par_ed_child_ed_pct2)) %>%
	mutate(age_par_ed_child_ed_pct2 =
			ifelse(parent_ed == "Short-cycle coll" & child_ed == "No registered education"
				& child_age_group == "25-29", 31, age_par_ed_child_ed_pct2)) %>%
	mutate(age_par_ed_child_ed_pct2 =
			ifelse(parent_ed == "Masters" & child_ed == "Completed education" & child_age_group == "25-29",
		69, age_par_ed_child_ed_pct2)) %>%
	mutate(age_par_ed_child_ed_pct2 =
			ifelse(parent_ed == "HS-Academic" & child_ed == "Discontinued education"
				& child_age_group == "30-34", 9, age_par_ed_child_ed_pct2)) %>%
	mutate(age_par_ed_child_ed_pct2 =
			ifelse(parent_ed == "Short-cycle coll" & child_ed == "Completed education"
				& child_age_group == "30-34", 60, age_par_ed_child_ed_pct2)) %>%
	mutate(age_par_ed_child_ed_pct2 =
			ifelse(parent_ed == "PhD" & child_ed == "Discontinued education"
				& child_age_group == "30-34", 4, age_par_ed_child_ed_pct2)) %>%
	mutate(age_par_ed_child_ed_pct2 =
			ifelse(parent_ed == "Primary" & child_ed == "No registered education"
				& child_age_group == "35-39", 68, age_par_ed_child_ed_pct2)) %>%
	mutate(age_par_ed_child_ed_pct2 =
			ifelse(parent_ed == "HS-Academic" & child_ed == "No registered education"
				& child_age_group == "35-39", 39, age_par_ed_child_ed_pct2)) %>%
	mutate(age_par_ed_child_ed_pct2 =
			ifelse(parent_ed == "Short-cycle coll" & child_ed == "Discontinued education"
				& child_age_group == "35-39", 7, age_par_ed_child_ed_pct2)) %>%
	mutate(age_par_ed_child_ed_pct2 =
			ifelse(parent_ed == "Bach-Vocational" & child_ed == "Discontinued education"
				& child_age_group == "35-39", 5, age_par_ed_child_ed_pct2)) %>%
	mutate(age_par_ed_child_ed_pct2 =
			ifelse(parent_ed == "Primary" & child_ed == "No registered education"
				& child_age_group == "40-45", 67, age_par_ed_child_ed_pct2)) %>%
	mutate(age_par_ed_child_ed_pct2 =
			ifelse(parent_ed == "HS-Academic" & child_ed == "No registered education"
				& child_age_group == "40-45", 39, age_par_ed_child_ed_pct2)) %>%
	mutate(age_par_ed_child_ed_pct2 =
			ifelse(parent_ed == "Bach-Vocational" & child_ed == "Undergoing education"
				& child_age_group == "40-45", 1, age_par_ed_child_ed_pct2)) %>%
	mutate(age_par_ed_child_ed_pct2 =
			ifelse(parent_ed == "Not stated" & child_ed == "Undergoing education"
				& child_age_group == "40-45", 1, age_par_ed_child_ed_pct2))

glimpse(ed_attain_waffle)

waffleplot <- function(plotdf, filter_expr) {

	# Convert the string expression to an actual R expression
		filter_expr <- rlang::parse_expr(filter_expr)
		# Filter the data
		filtered_df <- plotdf %>% filter(!!filter_expr)

		# Create the plot
		ggplot(filtered_df, (aes(fill = child_ed, values = age_par_ed_child_ed_pct2))) +
				geom_waffle(na.rm=TRUE, n_rows=10, flip=TRUE, size = 0.33, colour = "white") +
				facet_wrap(~parent_ed, nrow=1,strip.position = "bottom", scales = "free_x") +
				scale_x_discrete(labels = ) +
				scale_y_continuous(labels = function(x) x * 10, # make this multipler the same as n_rows
					expand = c(0,0)) +
				scale_fill_brewer(palette = "Set2") +
				theme_minimal() +
				theme(legend.position = "bottom", legend.justification = "left",
					legend.spacing.x = unit(0, 'cm'),
					legend.key.width = unit(1, 'cm'), legend.margin=margin(-10, 0, 0, 0),
					legend.title = element_text(size = 8), legend.text = element_text(size = 8),
					panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
				guides(fill = guide_legend(label.position = "bottom",
					title = "Higher education completion status", title.position = "top"))
}

ed_attain_waffle %>%
	count(child_age_group)

plot_2529 <- waffleplot(ed_attain_waffle, "child_age_group == '25-29'")
plot_3034 <- waffleplot(ed_attain_waffle, "child_age_group == '30-34'")
plot_3539 <- waffleplot(ed_attain_waffle, "child_age_group == '35-39'")
plot_4045 <- waffleplot(ed_attain_waffle, "child_age_group == '40-45'")

### add titles to the plots

plot_2529 <-
plot_2529 +
	labs(subtitle = "Age group 25-29")

plot_3034 <-
	plot_3034 +
	labs(subtitle = "Age group 30-34")

plot_3539 <-
plot_3539 +
	labs(subtitle = "Age group 35-39")

plot_4045 <-
plot_4045 +
	labs(subtitle = "Age group 40-45")

plot_2529 + plot_3034 +  plot_3539 + plot_4045 +
	plot_annotation(
		title = "In Denmark, regardless of age, the likelihood of completing higher education increases as level of parent education increases.",
		subtitle = "Higher education completion status for people ages 25-45, by age group and parent educational attainment, 2023. Each block = 1 %",
		caption = "Data from Danmarks Statistik table STATUSV2 via danstat package")

ggsave("2025/images/prompt15_2025.jpg", width = 15, height = 8,
	units = "in", dpi = 300)

ggsave("~/Data/greg_dubrow_io/posts/30-day-chart-challenge-2025/images/prompt15_2025.jpg",
	width = 15, height = 8, units = "in", dpi = 300)


## plot demo
ed_attain_waffle %>%
	filter(child_age_group == "25-29") %>%
#	select(parent_ed, child_age_group, child_ed, N, child_age_N, child_age_ed_pct2) %>%
#	view()
	ggplot(aes(fill = child_ed, values = age_par_ed_child_ed_pct2)) +
	geom_waffle(na.rm=TRUE, n_rows=10, flip=TRUE, size = 0.33, colour = "white") +
	facet_wrap(~parent_ed, nrow=1,strip.position = "bottom") +
	scale_x_discrete() +
	scale_y_continuous(labels = function(x) x * 10, # make this multipler the same as n_rows
		expand = c(0,0)) +
	scale_fill_brewer(palette = "Set2") +
	labs(title = "For Danes aged 25-29, higher levels of parent educational attainment mean greater likelihood of finishing higher education",
		subtitle = "Higher educational completion status by parent educational attainment and age, 2023.<br>
				 Each block = 1 %",
		caption = "Data from Danmarks Statistik table STATUSV2 via danstat package",
		x = "", y = "") +
	theme_minimal() +
	theme(legend.position = "bottom", legend.spacing.x = unit(0, 'cm'),
		legend.key.width = unit(1.5, 'cm'), legend.margin=margin(-10, 0, 0, 0),
		plot.title = element_text(hjust = 0), plot.subtitle = element_markdown(),
		plot.caption = element_markdown(),
		panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
	guides(fill = guide_legend(label.position = "bottom",
		title = "Higher education completion status", title.position = "top"))

