## table RDCE05 Expenses to R&D by sector, field of research and developement and time 2023 and 2017
## gt table with percent change by sector and field
## scatterplot with percent share of total funding by field x axis
 # and y axis percent change for combo of sector-field
# fields come from https://en.wikipedia.org/wiki/Frascati_Manual
 # https://en.wikipedia.org/wiki/Fields_of_Science_and_Technology
# scatterplot inspired by nicola rennie's cluster chart
   # https://github.com/nrennie/30DayChartChallenge/blob/main/2025/README.md#day-13-clusters-made-with-r

library(tidyverse) # to do tidyverse things
library(tidylog) # to get a log of what's happening to the data
library(janitor) # tools for data cleaning
library(danstat) # package to get Danish statistics via api
library(gt) # for tables
library(ggiraph) # interactive plot with tooltip
library(ggtext) # add markdown to plot labels

source("~/Data/r/basic functions.R")


# RDCE05
table_meta <- danstat::get_table_metadata(table_id = "rdce05", variables_only = TRUE)
table_meta_dk <- danstat::get_table_metadata(table_id = "rdce05", variables_only = TRUE, language = "da")

# create variable list using the ID value in the variable
variables_ed <- list(
	list(code = "sektor", values = "*"),
	list(code = "videnhoved", values = "*"),
	list(code = "tid", values = c(2017, 2023)))

r_and_d1 <- get_data("rdce05", variables_ed, language = "en") %>%
	as_tibble() %>%
	clean_names()

glimpse(r_and_d1)

r_and_d1 %>%
	count(videnhoved)

r_and_d_main <- r_and_d1 %>%
	mutate(sektor = str_remove(sektor, " sector")) %>%
	mutate(sektor = case_when(
		sektor == "Total source of funding" ~ "Total funding",
		sektor == "Business enterprise" ~ "Business",
		TRUE ~ sektor)) %>%
	mutate(videnhoved = case_when(
		videnhoved == "Agricultural and veterinary sciences" ~ "Agri & veterinary sci",
		videnhoved == "Engineering and technology" ~ "Eng & tech",
		videnhoved == "Humanities and the arts" ~ "Humanities & arts",
		videnhoved == "Medical and health sciences" ~ "Medical & health sci",
		TRUE ~ videnhoved)) %>%
	select(sector = sektor, field = videnhoved, year = tid, dkk_million = indhold)

glimpse(r_and_d_main)

r_and_d_main %>%
	count(field)

saveRDS(r_and_d_main,
	"~/Data/greg_dubrow_io/posts/30-day-chart-challenge-2025/data/r_and_d_main.Rda")


# gt tables with percent change in total by field and total by sector

r_and_d_main %>%
	filter(sector == "Total funding") %>%
	group_by(field) %>%
	mutate(pct_chg_funding = pctchange(dkk_million)) %>%
	ungroup() %>%
	pivot_wider(names_from = year, values_from = c(dkk_million, pct_chg_funding)) %>%
	mutate(pct_chg_2023 = round(pct_chg_funding_2023, 3)) %>%
	select(field, dkk_million_2017, dkk_million_2023, pct_chg_2023) %>%
	arrange(field) %>%
	gt() %>%
	cols_label(
		field = "Research area",
		dkk_million_2017 = "2017",
		dkk_million_2023 = "2023",
		pct_chg_2023 = md("% change<br>  2017 to 2023")) %>%
	tab_style(
		style = cell_text(align = "center"),
		locations = cells_column_labels(
			columns = c(dkk_million_2017, dkk_million_2023, pct_chg_2023))) %>%
	fmt_number(columns = c(dkk_million_2017, dkk_million_2023),
		sep_mark = ",", decimals = 0) %>%
	fmt_percent(columns = pct_chg_2023, decimals = 1) %>%
	tab_header(
		title = md("Agriculture/Veterinary & Medical/Health sciences had biggest increases in
		R&D expenses 2017-2023. <br>Engineering/Tech & Medical have highest R&D expenses."),
		subtitle = "Expenses for R&D by field of research in Denmark, 2017 & 2023") %>%
	opt_align_table_header(align = "left") %>%
	tab_footnote(footnote = md("*All values in millions of Danish kroner (DKK).*")) %>%
	tab_footnote(footnote = md("*Data from Danmarks Statistik table RDCE05 via danstat package.*")) %>%
	opt_stylize(style = 5)

r_and_d_main %>%
	filter(!sector == "Total funding") %>%
	arrange(sector, year) %>%
	group_by(sector, year) %>%
	mutate(funding_year = sum(dkk_million)) %>%
	distinct(sector, year, .keep_all = TRUE) %>%
	select(-field, -dkk_million) %>%
	ungroup() %>%
	mutate(pct_chg_funding = pctchange(funding_year)) %>%
	ungroup() %>%
	pivot_wider(names_from = year, values_from = c(funding_year, pct_chg_funding)) %>%
	mutate(pct_chg_2023 = round(pct_chg_funding_2023, 3)) %>%
	select(sector:funding_year_2023, pct_chg_2023) %>%
	gt() %>%
	cols_label(
		sector = "Sector",
		funding_year_2017 = "2017",
		funding_year_2023 = "2023",
		pct_chg_2023 = md("% change<br>  2017 to 2023")) %>%
	tab_style(
		style = cell_text(align = "center"),
		locations = cells_column_labels(
			columns = c(funding_year_2017, funding_year_2023, pct_chg_2023))) %>%
	fmt_number(columns = c(funding_year_2017, funding_year_2023),
		sep_mark = ",", decimals = 0) %>%
	fmt_percent(columns = pct_chg_2023, decimals = 1) %>%
	tab_header(
		title = md("Private non-profit sector had biggest increase in
		R&D expenses 2017-2023. <br>Business sector by far has highest R&D expenses."),
		subtitle = "Expenses for R&D by sector in Denmark, 2017 & 2023") %>%
	opt_align_table_header(align = "left") %>%
	tab_footnote(footnote = md("*All values in millions of Danish kroner (DKK).*")) %>%
	tab_footnote(footnote = md("*Data from Danmarks Statistik table RDCE05 via danstat package.*")) %>%
	opt_stylize(style = 5)

r_and_d_main %>%
	count(sector)

# scatterplot

## colors for geom_points
bg_col <- "grey97"
text_col <- "black"
highlight_col <- "#0072B2"
highlight_col2 <- "#CC79A7"

# data for plot
r_and_d_plotdf <-
r_and_d_main %>%
	filter(!sector == "Total funding") %>%
	group_by(sector, field) %>%
	mutate(pct_chg_funding = pctchange(dkk_million)) %>%
	ungroup() %>%
	pivot_wider(names_from = year, values_from = c(dkk_million, pct_chg_funding)) %>%
	select(-pct_chg_funding_2017) %>%
	mutate(dkk_million_2023_c = formatC(dkk_million_2023, format = "d", big.mark = ",")) %>%
	mutate(dkk_million_2023_c = paste0(dkk_million_2023_c, " mill DKK")) %>%
	group_by(sector) %>%
	mutate(sector_funding  = sum(dkk_million_2023)) %>%
	mutate(sector_field_funding_pct  = dkk_million_2023 / sum(sector_funding)) %>%
	ungroup() %>%
	mutate(sector_funding_pct  = sector_funding / sum(dkk_million_2023)) %>%
	mutate(sector_short = case_when(
		sector == "Government" ~ "Gov't",
		sector == "Higher education" ~ "Higher ed",
		sector == "Private non-profit" ~ "NGO",
		TRUE ~ sector)) %>%
	mutate(sector_field = paste0(sector_short, "/", field)) %>%
	filter(!is.na(pct_chg_funding_2023)) %>%
	filter(dkk_million_2017 > 0) %>%
	mutate(col = case_when(
		pct_chg_funding_2023 > 0 ~ highlight_col2,
		pct_chg_funding_2023 == 0 ~ "grey60",
		pct_chg_funding_2023 < 0 ~ highlight_col))

glimpse(r_and_d_plotdf)

saveRDS(r_and_d_plotdf,
	"~/Data/greg_dubrow_io/posts/30-day-chart-challenge-2025/data/r_and_d_plotdf.Rda")


r_and_d_plotdf %>%
	count(pct_chg_funding_2023, col)

## add interactive hover-over dots with sector/field name, 2023 funding,

r_and_d_plotdf %>%
	ggplot() +
	geom_hline(aes(yintercept=0), color="grey70") +
	geom_vline(aes(xintercept = median(sector_field_funding_pct)),
		color="grey70")+
	geom_point(
		data = r_and_d_plotdf,
		mapping = aes(x = sector_field_funding_pct, y = pct_chg_funding_2023,
		fill = col, colour = col),
	pch = 21, size = 3) +
	scale_x_continuous(limits = c(-.0002, .15),
		labels = label_percent()) +
	scale_y_continuous(limits = c(-1.05, 2.5),
		labels = label_percent()) +
	scale_fill_identity() +
	scale_colour_identity() +
	coord_cartesian(expand = FALSE, clip = "off") +
	annotate("text", x = .1, y = 1.5,
		label = stringr::str_wrap("Above median in expense share, expenses increased from 2017-2023", 30),
		size = 5, color="grey30") +
	annotate("text", x = .01, y = -.25,
		label = stringr::str_wrap("Below median in expense share, expenses decreased from 2017-2023", 30),
		size = 3.5, color="grey30") +
	labs(
		title = "Which combination of sector and field of study spent the most on R&D?",
		subtitle = "*Expenses include all R&D activity & personnel, regardless of funding source.*",
		caption = "*Data from Danmarks Statistik table STATUSV2 via danstat package*",
		x = "Sector/field % of all expenses in 2023",
		y = "Sector/field % change 2017-2023") +
	theme_minimal() +
	theme(legend.position = "none",
		panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
		plot.title = element_text(size = 18, color = "grey20"),
		plot.subtitle = element_markdown(size = 14, color = "grey20"),
		plot.caption = element_markdown(size = 7, color = "grey50"),
		axis.text.x = element_text(size = 8, color = "grey50"),
		axis.text.y = element_text(size = 8, color = "grey50"),
		axis.title.x = element_text(size = 8, color = "grey50", vjust = -2),
		axis.title.y = element_text(size = 8, color = "grey50"))

ggsave("2025/images/prompt13_2025.jpg", width = 15, height = 8,
	units = "in", dpi = 300)


rdplot <-
	ggplot(
		data = r_and_d_plotdf,
		mapping = aes(
			x = sector_field_funding_pct, y = pct_chg_funding_2023,
			fill = col, colour = col,
			# here we add interactive aesthetics
			tooltip = paste0(sector_field, "; ", dkk_million_2023_c), data_id = sector),
		pch = 21, alpha = 0.3, size = 3) +
	geom_point_interactive(
		size = 3, hover_nearest = FALSE) +
	geom_hline(aes(yintercept=0), color="grey70") +
	geom_vline(aes(xintercept = median(sector_field_funding_pct)),
		color="grey70") +
	scale_x_continuous(limits = c(-.0002, .15),
		labels = label_percent()) +
	scale_y_continuous(limits = c(-1.05, 2.5),
		labels = label_percent()) +
	scale_fill_identity() +
	scale_colour_identity() +
	coord_cartesian(expand = FALSE, clip = "off") +
	annotate("text", x = .1, y = 1.5,
		label = stringr::str_wrap("Above median in expense share, expenses increased from 2017-2023", 30),
		size = 3, color="grey50") +
	annotate("text", x = .01, y = -.25,
		label = stringr::str_wrap("Below median in expense share, expenses decreased from 2017-2023", 25),
		size = 2, color="grey50") +
	labs(
		title = "Which combination of sector and field of study spent the most on R&D?",
		subtitle = "*Expenses include all R&D activity & personnel, regardless of funding source. Hover over for sector/field & 2023 expenses in millions of DKK.*",
		caption = "*Data from Danmarks Statistik table STATUSV2 via danstat package*",
		x = "Sector/field % of all expenses in 2023",
		y = "Sector/field % change 2017-2023") +
	theme_minimal() +
	theme(legend.position = "none",
		panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
		plot.title = element_text(size = 10, color = "grey40"),
		plot.subtitle = element_markdown(size = 8, color = "grey40"),
		plot.caption = element_markdown(size = 5, color = "grey60", vjust = -2),
		axis.text.x = element_text(size = 6, color = "grey60"),
		axis.text.y = element_text(size = 6, color = "grey60"),
		axis.title.x = element_text(size = 6, color = "grey60", vjust = -1),
		axis.title.y = element_text(size = 6, color = "grey60"))


# turn as girafe - in markdown don't use svg widths
girafe(ggobj = rdplot,
	width_svg = 8, height_svg = 5)

