# Income in log scale and actual scale for comparison
# by education level & degree, sex, sector

# explainer for income types
# https://www.dst.dk/Site/Dst/SingleFiles/GetArchiveFile.aspx?fi=903848007114&fo=0&ext=kvaldel

library(tidyverse) # to do tidyverse things
library(tidylog) # to get a log of what's happening to the data
library(janitor) # tools for data cleaning
library(danstat) # package to get Danish statistics via api
library(ggtext) # enhancements for text in ggplot
library(ggrepel)
library(scales)
library(patchwork)


# some custom functions
source("~/Data/r/basic functions.R")

# lons11
table_meta <- danstat::get_table_metadata(table_id = "lons11", variables_only = TRUE)

# create variable list using the ID value in the variable
variables_ed <- list(
	list(code = "uddannelse", values = "*"),
	list(code = "sektor", values = c(1000, 1016, 1020, 1025, 1046)),
	list(code = "lønmål", values = "MDRSNIT"), # avg monthly
	list(code = "køn", values = "*"),
	list(code = "afloen", values = c("TIME", "FAST")),
	list(code = "tid", values = "*"))


sal1 <- get_data("lons11", variables_ed, language = "en") %>%
	as_tibble() %>%
	clean_names()

glimpse(sal1)

sal1 %>%
	count(sektor)
sal1 %>%
	count(afloen)
sal1 %>%
	count(indhold) %>%
	view()


sal_main <- sal1 %>%
	mutate(income = as.numeric(indhold)) %>%
	mutate(income = round(income, 2)) %>%
	mutate(kon = ifelse(kon == "Men and women, total", "Total", kon)) %>%
	mutate(afloen = ifelse(
		afloen == "Fixed salary-earners", "Salaried", "Hourly")) %>%
	mutate(sector = case_when(
		sektor == "Corporations and organizations" ~ "Private sector",
		sektor == "Government including social security funds" ~ "Gov't - National",
		sektor == "Municipal government" ~ "Gov't - Municipal",
		sektor == "Regional government" ~ "Gov't - Regional",
		TRUE ~ sektor)) %>%
	mutate(sector =
			factor(sector,
				levels = c("All sectors", "Gov't - Municipal", "Gov't - Regional",
					"Gov't - National", "Private sector"))) %>%
	mutate(ed_level =
			case_when(
				str_detect(uddannelse, "H10") ~ "Primary",
				str_detect(uddannelse, "H20") ~ "HS-Academic",
				str_detect(uddannelse, "H30") ~ "HS-Vocational",
				str_detect(uddannelse, "H35") ~ "Qualifying Programs",
				str_detect(uddannelse, "H40") ~ "Short-cycle college",
				str_detect(uddannelse, "H50") ~ "Bachelor-Vocational",
				str_detect(uddannelse, "H60") ~ "Bachelor-Academic",
				str_detect(uddannelse, "H70") ~ "Masters",
				str_detect(uddannelse, "H80") ~ "PhD",
				str_detect(uddannelse, "H90") ~ "Not stated",
				uddannelse == "Total" ~ "Total")) %>%
	mutate(ed_level =
			factor(ed_level,
				levels = c("Primary", "HS-Academic", "HS-Vocational",
					"Qualifying Programs", "Short-cycle college",
					"Bachelor-Vocational", "Bachelor-Academic",
					"Masters", "PhD", "Not stated", "Total"))) %>%
	mutate(deg_code = str_extract(uddannelse, "^[^ ]+")) %>%
	mutate(deg_name = sub("^\\S+\\s+", '', uddannelse)) %>%
	mutate(deg_name = str_remove(deg_name, ", BACH")) %>%
	mutate(deg_name = str_remove(deg_name, " BACH")) %>%
	mutate(deg_name = str_remove(deg_name, ", SCE")) %>%
	mutate(deg_name = str_remove(deg_name, ", VBE")) %>%
	mutate(deg_name = str_remove(deg_name, ", MASTER")) %>%
	mutate(deg_name = str_replace(
		deg_name, "Educational, PhD", "Education")) %>%
	mutate(deg_name = str_remove(deg_name, ", PhD")) %>%
	mutate(deg_name = str_remove(deg_name, " PhD")) %>%
	mutate(deg_name = str_replace(deg_name, " \\s*\\([^\\)]+\\)", "")) %>%
	mutate(deg_name = str_replace(
		deg_name, "Upper secondary education", "Upper secondary")) %>%
	mutate(deg_name = str_replace(
		deg_name, "upper secondary education", "upper secondary")) %>%
	mutate(deg_name = str_replace(
		deg_name, "The technology area, ", "Technology-")) %>%
	mutate(deg_name = str_replace(
		deg_name, "in general", "general")) %>%
	mutate(deg_name = ifelse(uddannelse ==
		"H2010 Upper secondary education, General (stx, hf, student courses)",
		"Upper secondary, stx, hf", deg_name)) %>%
	mutate(deg_name = ifelse(uddannelse ==
		"H2020 Upper secondary education, General (hhx, htx)",
	"Upper secondary, hhx, htx", deg_name)) %>%
	mutate(deg_name = str_replace(
		deg_name, "Educational", "Education")) %>%
	mutate(deg_name = ifelse(
		deg_code %in% c("H5097","H6097","H7097","H8097"),
		"Not specified", deg_name)) %>%
	mutate(deg_name = case_when(
		deg_code == "H1010" ~ "up to 6th grade",
		deg_code == "H1020" ~ "7th-9th grade",
		deg_code == "H1030" ~ "10th grade",
		TRUE ~ deg_name)) %>%
	mutate(deg_name = str_replace(deg_name,
	"Food, biotechnology and laboratory technology",
	"Food/Biotech/Lab Tech")) %>%
	mutate(deg_name = str_replace(deg_name,
		"educations", "education")) %>%
	mutate(deg_level = case_when(
		deg_code %in%
			c("H10", "H20", "H30", "H35", "H40", "H50", "H60", "H70", "H80") ~ "Top level",
		deg_code == "Total" ~ "Total",
		TRUE ~ "Sub level")) %>%
	select(year = tid, income, afloen, sector, sex = kon, ed_level,
		deg_code, deg_level, deg_name)

glimpse(sal_main)

sal_main %>%
	count(indhold, income) %>%
	view()

sal_main %>%
	count(deg_code, ed_level, deg_level, deg_name, uddannelse) %>%
	view()

sal_main %>%
	count(year)

## charts - do one regular, one log10 patchwork

# color choices
"#8DD3C7" "#FFFFB3" "#BEBADA" "#FB8072" "#80B1D3" "#FDB462"
"#B3DE69" "#FCCDE5" "#1F78B4" "#BC80BD"

"#A6CEE3" "#1F78B4" "#B2DF8A" "#33A02C" "#FB9A99" "#E31A1C"
"#FDBF6F" "#FF7F00" "#CAB2D6" "#6A3D9A"

# all income, facet by worker type.
plot_income_all <-
	sal_main %>%
	filter(sector == "All sectors") %>%
	filter(sex == "Total") %>%
	filter(ed_level == "Total") %>%
	select(year, income, afloen) %>%
	{. ->> tmp} %>%
	ggplot(aes(x = year)) +
	geom_line(aes(y = income), size = 2, color = "#1F78B4") +
	geom_text(
		data = subset(tmp, year %in% c(2015, 2019, 2023)),
		aes(y = income, label = scales::comma(round(income, 0))),
		color = "#1F78B4", vjust = 2) +
	scale_x_continuous(breaks = c(2015, 2017, 2019, 2021, 2023)) +
	scale_y_continuous(limits = c(20000, 60000),
		breaks = pretty_breaks(),
		labels = label_comma()) +
	labs(x = "", y = "",
		subtitle = "Monthly standardized income by worker type, 2015-2023;
		in Danish kroner (DKK)") +
	facet_wrap(~ afloen) +
	theme_minimal() +
	theme(
		plot.title = element_markdown(size = 14),
		plot.subtitle = element_markdown(size = 12),
		plot.caption = element_markdown(size = 9),
		axis.text.x = element_text(size = 9, color = "grey50"),
		axis.text.y = element_text(size = 9, color = "grey50"),
		strip.background.x = element_rect(fill = "grey90", color = "grey95"),
		strip.text = element_text(size = 11),
		panel.border = element_rect(color = "grey50", fill = NA, size = 1),
		panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# log10 income
plot_log_income_all <-
	sal_main %>%
	filter(sector == "All sectors") %>%
	filter(sex == "Total") %>%
	filter(ed_level == "Total") %>%
	select(year, income, afloen) %>%
	ggplot(aes(x = year)) +
	geom_line(aes(y = income), size = 2, color = "#1F78B4", alpha = .6) +
	geom_text(
		data = subset(tmp, year %in% c(2015, 2019, 2023)),
		aes(y = income, label = scales::comma(round(income, 0))),
		color = "#1F78B4", vjust = 2) +
	scale_x_continuous(breaks = c(2015, 2017, 2019, 2021, 2023)) +
	labs(x = "", y = "",
		subtitle = "Monthly standardized income by worker type with log10 axis transformation, 2015-2023;
		in Danish kroner (DKK)",
		caption = "*Data from Danmarks Statistik table LØNS11 via danstat package*") +
	scale_y_log10(limits = c(20000, 60000),
		breaks = pretty_breaks(),
		labels = label_comma()) +
	facet_wrap(~ afloen) +
	theme_minimal() +
	theme(
		plot.title = element_markdown(size = 14),
		plot.subtitle = element_markdown(size = 12),
		plot.caption = element_markdown(size = 8),
		axis.text.x = element_text(size = 9, color = "grey50"),
		axis.text.y = element_text(size = 9, color = "grey50"),
		strip.background.x = element_rect(fill = "grey90", color = "grey95"),
		strip.text = element_text(size = 11),
		panel.border = element_rect(color = "grey50", fill = NA, size = 1),
		panel.grid.major = element_blank(), panel.grid.minor = element_blank())

plot_income_all / plot_log_income_all +
	plot_annotation(
		title = "Salaried workers have higher monthly earnings than salaried workers. Log10 axis transformation does not have much effect because increase in wages is steady.")


ggsave("2025/images/prompt23_incall_2025.jpg", width = 15, height = 8,
	units = "in", dpi = 300)

ggsave("~/Data/greg_dubrow_io/posts/30-day-chart-challenge-2025/images/prompt23_incall_2025.jpg",
	width = 15, height = 8, units = "in", dpi = 300)


# all income by sex, facet by worker type.
plot_income_sex_all <-
	sal_main %>%
	filter(sector == "All sectors") %>%
	filter(!sex == "Total") %>%
	filter(ed_level == "Total") %>%
	select(year, income, sex, afloen) %>%
	{. ->> tmp} %>%
	ggplot(aes(x = year)) +
	geom_line(
		data = subset(tmp, sex == "Men"),
		aes(y = income), size = 2, color = "#5D3A9B") +
	geom_text(
		data = subset(tmp, sex == "Men" & year == 2015),
		aes(y = income, label = "Men"), color = "#5D3A9B", vjust = -1) +
	geom_text(
		data = subset(tmp, sex == "Men" & year %in% c(2015, 2019, 2023)),
		aes(y = income, label = scales::comma(round(income, 0))),
		color = "#5D3A9B", vjust = 2) +
	geom_line(
		data = subset(tmp, sex == "Women"),
		aes(y = income), size = 2, color = "#E66100") +
	geom_text(
		data = subset(tmp, sex == "Women" & year == 2015),
		aes(y = income, label = "Women"), color = "#E66100", vjust = -1) +
	geom_text(
		data = subset(tmp, sex == "Women" & year %in% c(2015, 2019, 2023)),
		aes(y = income, label = scales::comma(round(income, 0))),
		color = "#E66100", vjust = 2) +
	scale_x_continuous(breaks = c(2015, 2017, 2019, 2021, 2023)) +
	scale_y_continuous(limits = c(20000, 60000),
		breaks = pretty_breaks(),
		labels = label_comma()) +
	labs(x = "", y = "",
		subtitle = "Monthly standardized income by sex and worker type, 2015-2023;
		in Danish kroner (DKK)") +
	facet_wrap(~ afloen) +
	theme_minimal() +
	theme(
		plot.title = element_markdown(size = 14),
		plot.subtitle = element_markdown(size = 12),
		plot.caption = element_markdown(size = 9),
		axis.text.x = element_text(size = 9, color = "grey50"),
		axis.text.y = element_text(size = 9, color = "grey50"),
		strip.background.x = element_rect(fill = "grey90", color = "grey95"),
		strip.text = element_text(size = 11),
		panel.border = element_rect(color = "grey50", fill = NA, size = 1),
		panel.grid.major = element_blank(), panel.grid.minor = element_blank())
rm(tmp)

# log10 income
plot_log_income_sex_all <-
	sal_main %>%
	filter(sector == "All sectors") %>%
	filter(!sex == "Total") %>%
	filter(ed_level == "Total") %>%
	select(year, income, sex, afloen) %>%
	{. ->> tmp} %>%
	ggplot(aes(x = year)) +
	geom_line(
		data = subset(tmp, sex == "Men"),
		aes(y = income), size = 2, color = "#5D3A9B", alpha = .6) +
	geom_text(
		data = subset(tmp, sex == "Men" & year == 2015),
		aes(y = income, label = "Men"), color = "#5D3A9B", vjust = -1) +
	geom_text(
		data = subset(tmp, sex == "Men" & year %in% c(2015, 2019, 2023)),
		aes(y = income, label = scales::comma(round(income, 0))),
		color = "#5D3A9B", vjust = 2) +
	geom_line(
		data = subset(tmp, sex == "Women"),
		aes(y = income), size = 2, color = "#E66100", alpha = .6) +
	geom_text(
		data = subset(tmp, sex == "Women" & year == 2015),
		aes(y = income, label = "Women"), color = "#E66100", vjust = -1) +
	geom_text(
		data = subset(tmp, sex == "Women" & year %in% c(2015, 2019, 2023)),
		aes(y = income, label = scales::comma(round(income, 0))),
		color = "#E66100", vjust = 2) +
	scale_x_continuous(breaks = c(2015, 2017, 2019, 2021, 2023)) +
	scale_y_log10(limits = c(20000, 60000),
		breaks = pretty_breaks(),
		labels = label_comma()) +
	labs(x = "", y = "",
		subtitle = "Monthly standardized income by sex & worker type with log10 axis transformation, 2015-2023;
		in Danish kroner (DKK)",
		caption = "*Data from Danmarks Statistik table LØNS11 via danstat package*") +
	facet_wrap(~ afloen) +
	theme_minimal() +
	theme(
		plot.title = element_markdown(size = 14),
		plot.subtitle = element_markdown(size = 12),
		plot.caption = element_markdown(size = 8),
		axis.text.x = element_text(size = 9, color = "grey50"),
		axis.text.y = element_text(size = 9, color = "grey50"),
		strip.background.x = element_rect(fill = "grey90", color = "grey95"),
		strip.text = element_text(size = 11),
		panel.border = element_rect(color = "grey50", fill = NA, size = 1),
		panel.grid.major = element_blank(), panel.grid.minor = element_blank())
rm(tmp)

plot_income_sex_all / plot_log_income_sex_all +
	plot_annotation(
		title = "Without controlling for education level or age, men earn higher monthly wages than women.")

ggsave("2025/images/prompt23_inc_sex_2025.jpg", width = 15, height = 8,
	units = "in", dpi = 300)

ggsave("~/Data/greg_dubrow_io/posts/30-day-chart-challenge-2025/images/prompt23_inc_sex_2025.jpg",
	width = 15, height = 8, units = "in", dpi = 300)


# all income by sex, facet by worker type and education level.
#plot_income_sex_ed_all <-
	sal_main %>%
	filter(sector == "All sectors") %>%
	filter(!sex == "Total") %>%
	filter(deg_level == "Top level") %>%
	filter(ed_level %notin% c("Qualifying Programs")) %>%
	select(year, income, sex, ed_level, afloen) %>%
	{. ->> tmp} %>%
	ggplot(aes(x = year)) +
	geom_line(
		data = subset(tmp, sex == "Men"),
		aes(y = income), size = 2, color = "#5D3A9B") +
	# geom_text(
	# 	data = subset(tmp, sex == "Men" & year == 2015),
	# 	aes(y = income, label = "Men"), color = "#5D3A9B", vjust = -1) +
		geom_text(
			data = subset(tmp, sex == "Men" & year %in% c(2015)),
			aes(y = income, label = scales::comma(round(income, 0))),
			color = "#5D3A9B", size = 3, vjust = -2, hjust = -.05) +
		geom_text(
		data = subset(tmp, sex == "Men" & year %in% c(2019)),
		aes(y = income, label = scales::comma(round(income, 0))),
		color = "#5D3A9B", size = 3, vjust = -2) +
		geom_text(
			data = subset(tmp, sex == "Men" & year %in% c(2023)),
			aes(y = income, label = scales::comma(round(income, 0))),
			color = "#5D3A9B", size = 3, vjust = -2, hjust = 1) +
		geom_line(
		data = subset(tmp, sex == "Women"),
		aes(y = income), size = 2, color = "#E66100") +
	# geom_text(
	# 	data = subset(tmp, sex == "Women" & year == 2015),
	# 	aes(y = income, label = "Women"), color = "#E66100", vjust = -1) +
		geom_text(
			data = subset(tmp, sex == "Women" & year %in% c(2015)),
			aes(y = income, label = scales::comma(round(income, 0))),
			color = "#E66100", size = 3, vjust = 2, hjust = -.05) +
		geom_text(
			data = subset(tmp, sex == "Women" & year %in% c(2019)),
			aes(y = income, label = scales::comma(round(income, 0))),
			color = "#E66100", size = 3, vjust = 2) +
		geom_text(
			data = subset(tmp, sex == "Women" & year %in% c(2023)),
			aes(y = income, label = scales::comma(round(income, 0))),
			color = "#E66100", size = 3, vjust = 3, hjust = 1) +
		scale_x_continuous(breaks = c(2015, 2017, 2019, 2021, 2023)) +
	scale_y_continuous(limits = c(20000, 80000),
		breaks = pretty_breaks(),
		labels = label_comma()) +
	labs(x = "", y = "",
		title = "Though pay increases as education level increases, the gender pay gap between <span style = 'color: #E66100;'>men</span>
		and <span style = 'color: #5D3A9B;'>women</span> persists but varies depending on degree level
		and worker type.",
		subtitle = "Monthly standardized income by sex and worker type, 2015-2023;
		in Danish kroner (DKK)") +
	facet_grid(afloen ~ ed_level, axes = "all", axis.labels = "all_x") +
	#theme_minimal() +
	theme(
		plot.title = element_markdown(size = 13),
		plot.subtitle = element_markdown(size = 12),
		plot.caption = element_markdown(size = 9),
		axis.text.x = element_text(size = 7, color = "grey50"),
		axis.text.y = element_text(size = 9, color = "grey50"),
		strip.background.x = element_rect(fill = "grey90", color = "grey95"),
		strip.text = element_text(size = 11),
		panel.border = element_rect(color = "grey50", fill = NA, size = 1),
		panel.grid.major = element_blank(), panel.grid.minor = element_blank())

rm(tmp)

ggsave("2025/images/prompt23_inc_sex_deg_2025.jpg", width = 15, height = 8,
	units = "in", dpi = 300)

ggsave("~/Data/greg_dubrow_io/posts/30-day-chart-challenge-2025/images/prompt23_inc_sex_deg_2025.jpg",
	width = 15, height = 8, units = "in", dpi = 300)


