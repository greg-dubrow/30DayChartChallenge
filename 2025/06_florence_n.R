# Nursing and SOSU degrees by gender and immigrant status

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

# UDDAKT35
table_meta <- danstat::get_table_metadata(table_id = "uddakt35", variables_only = TRUE)
table_meta_dk <- danstat::get_table_metadata(table_id = "uddakt35", variables_only = TRUE, language = "da")

# create variable list using the ID value in the variable
variables_ed <- list(
	list(code = "uddannelse", values = c("H30", "H3010", "H301020")),
	list(code = "fstatus", values = c("F")),
	list(code = "køn", values = "*"),
	list(code = "herkomst", values = "*"),
	list(code = "tid", values = c(2019, 2020, 2021, 2022, 2023, 2024)))

sosu1 <- get_data("uddakt35", variables_ed, language = "en") %>%
	as_tibble() %>%
	clean_names()

glimpse(sosu1)

sosu_all <- sosu1 %>%
	rename(sex = kon, national_origin = herkomst, year = tid, n = indhold) %>%
	select(-fstatus) %>%
	mutate(deg_code = str_extract(uddannelse, "^[^ ]+")) %>%
	mutate(deg_name = case_when(
		deg_code == "H30" ~ "VET All",
		deg_code == "H3010" ~ "Health care/Educ All",
		deg_code == "H301020" ~ "SOSU")) %>%
	mutate(national_origin = ifelse(
		national_origin == "Persons of Danish origin", "Danish origin", national_origin)) %>%
	mutate(national_origin =
				 	factor(national_origin,
				 				 levels = c("Total", "Danish origin", "Descendant",
				 				 					 "Immigrants", "Unknown origin"))) %>%
	mutate(sex = ifelse(sex == "Sex, total", "Total", sex))

glimpse(sosu_all)

sosu_all %>%
	count(deg_code)
sosu_all %>%
	count(national_origin)


# change in number of total sosu degrees over time
sosu_all %>%
	filter(deg_name == "SOSU") %>%
	filter(sex == "Total") %>%
	filter(national_origin == "Total") %>%
	select(year, n) %>%
	ggplot(aes(year, n)) +
	geom_bar(stat = "identity", fill = '#C60C30') +
	scale_x_continuous(breaks = c(2019, 2020, 2021, 2022, 2023, 2024)) +
	scale_y_continuous(labels = comma) +
	geom_text(aes(label = comma(n)), vjust = 1.5,
						size = 5, color = "white") +
	labs(x = "", y = "",
			 title = "SOSU degrees awarded increased by greater than 200% from 2019 to 2024",
			 subtitle = "*Vocational program: H301020 Social- og sundhedsuddannelsen*",
			 caption = "*Data from Danmarks Statistik table UDDAKT35 via danstat package*") +
	theme_minimal() +
	theme(panel.grid = element_blank(),
				legend.position = "none",
				plot.title = element_markdown(size = 16, hjust = 0),
				plot.subtitle = element_markdown(size = 12, hjust = 0),
				plot.caption = element_markdown())

ggsave("2025/images/prompt6_sosu_all_2025.jpg", width = 15, height = 8,
			 units = "in", dpi = 300)

ggsave("~/Data/greg_dubrow_io/posts/30-day-chart-challenge-2025/images/prompt6_sosu_all_2025.jpg",
			 width = 15, height = 8, units = "in", dpi = 300)

# sosu as percent of all H30 VET
sosu_all %>%
	filter(deg_name %in% c("VET All", "SOSU")) %>%
	filter(sex == "Total") %>%
	filter(national_origin == "Total") %>%
	select(year, deg_name, n) %>%
	pivot_wider(names_from = deg_name, values_from = n) %>%
	rename(vet_all = `VET All`) %>%
	mutate(sosu_pct = SOSU/sum(vet_all)) %>%
	{. ->> tmp} %>%
	ggplot(aes(year, sosu_pct)) +
	geom_bar(stat = "identity", fill = '#C60C30') +
	scale_x_continuous(breaks = c(2019, 2020, 2021, 2022, 2023, 2024)) +
	scale_y_continuous(limits = c(0, .04), labels = label_percent()) +
	geom_text(aes(label = percent(sosu_pct, 2)),
						size = 4.5, color = "white", vjust = 1.5) +
	labs(x = "", y = "",
			 title = "SOSU degrees doubled as pct of all H30 vocational degrees from 2019 to 2024,
			 accounting for more than the total increase in H30 degrees",
			 subtitle = "*Total H30 degrees: 2019 = 28,672, 2024 = 30,570, increase of 1,898 (6.6%)*",
			 caption = "*Data from Danmarks Statistik table UDDAKT35 via danstat package*") +
	theme_minimal() +
	theme(panel.grid = element_blank(),
				legend.position = "none",
				plot.title = element_markdown(size = 16, hjust = 0),
				plot.subtitle = element_markdown(size = 12, hjust = 0),
				plot.caption = element_markdown())
rm(tmp)

ggsave("2025/images/prompt6_sosu_pct_vet_2025.jpg", width = 15, height = 8,
			 units = "in", dpi = 300)

ggsave("~/Data/greg_dubrow_io/posts/30-day-chart-challenge-2025/images/prompt6_sosu_pct_vet_2025.jpg",
			 width = 15, height = 8, units = "in", dpi = 300)


# stacked bar graph by sex

sosu_all %>%
	filter(deg_name == "SOSU") %>%
	filter(!sex == "Total") %>%
	filter(national_origin == "Total") %>%
	select(year, sex, n) %>%
	group_by(year) %>%
	mutate(sex_pct = n/sum(n)) %>%
	ungroup() %>%
	ggplot(aes(year, sex_pct, fill = sex)) +
	geom_bar(stat = "identity") +
	scale_fill_manual(values = c("#E66100", "#5D3A9B")) +
	scale_x_continuous(breaks = c(2019, 2020, 2021, 2022, 2023, 2024)) +
	scale_y_continuous(labels = label_percent()) +
	geom_text(aes(label = percent(sex_pct, 2)),
						position = position_stack(vjust = 0.5),
						size = 4.5, color = "white") +
	labs(x = "", y = "",
			 title = "Women earn the vast majority of SOSU degrees awarded",
			 subtitle = "*<span style='color: #E66100;'>Men</span> --
			 <span style='color: #5D3A9B;'>Women</span>*",
			 caption = "*Data from Danmarks Statistik table UDDAKT35 via danstat package*") +
	theme_minimal() +
	theme(panel.grid = element_blank(),
				legend.position = "none",
				plot.title = element_markdown(size = 16, hjust = 0),
				plot.subtitle = element_markdown(size = 12, hjust = 0),
				plot.caption = element_markdown())

ggsave("2025/images/prompt6_sosu_sex_2025.jpg", width = 15, height = 8,
			 units = "in", dpi = 300)

ggsave("~/Data/greg_dubrow_io/posts/30-day-chart-challenge-2025/images/prompt6_sosu_sex_2025.jpg",
			 width = 15, height = 8, units = "in", dpi = 300)


# stacked bar graph by nat origin
sosu_all %>%
	filter(deg_name == "SOSU") %>%
	filter(sex == "Total") %>%
	filter(!national_origin == "Total") %>%
	select(year, national_origin, n) %>%
	group_by(year) %>%
	mutate(nat_org_pct = n/sum(n)) %>%
	ungroup() %>%
	{. ->> tmp} %>%
	ggplot(aes(year, nat_org_pct, fill = national_origin)) +
	geom_bar(stat = "identity") +
	scale_fill_manual(values = c("#CC79A7", "#D55E00", "#0072B2", "#F0E442")) +
	scale_x_continuous(breaks = c(2019, 2020, 2021, 2022, 2023, 2024)) +
	scale_y_continuous(labels = label_percent()) +
	geom_text(data = subset(tmp, nat_org_pct > 0.01),
		aes(label = percent(nat_org_pct, 2)),
						position = position_stack(vjust = 0.5),
						size = 4.5, color = "white") +
	labs(x = "", y = "",
			 title = "Slight increase in percent of SOSU degrees awarded to immigrants",
			subtitle = "*<span style='color: #CC79A7;'>Danish origin = at least 1 parent born in & citizen of DK</span>;
			<span style='color: #D55E00;'>Descendant = born in DK, neither parent citizen or born in DK</span>;
						<span style='color: #0072B2;'>Immigrant = born abroad, neither parent citizen or born in DK</span>*",
			 caption = "*Data from Danmarks Statistik table UDDAKT35 via danstat package*") +
	theme_minimal() +
	theme(panel.grid = element_blank(),
				legend.position = "none", legend.title = element_blank(),
				plot.title = element_markdown(size = 16, hjust = 0),
				plot.subtitle = element_markdown(size = 11, hjust = 0),
				plot.caption = element_markdown())
rm(tmp)

ggsave("2025/images/prompt6_sosu_nat_org_2025.jpg", width = 15, height = 8,
			 units = "in", dpi = 300)

ggsave("~/Data/greg_dubrow_io/posts/30-day-chart-challenge-2025/images/prompt6_sosu_nat_org_2025.jpg",
			 width = 15, height = 8, units = "in", dpi = 300)


### don't really need it, differences not stark by nat origin & sex than just nat origin
# stacked bar graph by nat origin facet by sex
sosu_all %>%
	filter(deg_name == "SOSU") %>%
	filter(!sex == "Total") %>%
	filter(!national_origin == "Total") %>%
	select(year, sex, national_origin, n) %>%
	group_by(year, sex) %>%
	mutate(nat_org_pct = n/sum(n)) %>%
	ungroup() %>%
	{. ->> tmp} %>%
	ggplot(aes(year, nat_org_pct, fill = national_origin)) +
	geom_bar(stat = "identity") +
	scale_fill_manual(values = c("#CC79A7", "#D55E00", "#0072B2", "#F0E442")) +
	scale_x_continuous(breaks = c(2019, 2020, 2021, 2022, 2023, 2024)) +
	scale_y_continuous(labels = label_percent()) +
	geom_text(data = subset(tmp, nat_org_pct > 0.01),
						aes(label = percent(nat_org_pct, 2)),
						position = position_stack(vjust = 0.5),
						size = 4.5, color = "white") +
	facet_wrap(~ sex) +
	labs(x = "", y = "",
			 title = "Slight increase in percent of SOSU degrees awarded to immigrants",
			 subtitle = "*<span style='color: #CC79A7;'>Danish origin = at least 1 parent born in & citizen of DK</span>;
			<span style='color: #D55E00;'>Descendant = born in DK, neither parent citizen or born in DK</span>;
						<span style='color: #0072B2;'>Immigrant = born abroad, neither parent citizen or born in DK</span>*",
			 caption = "*Data from Danmarks Statistik table UDDAKT35 via danstat package*") +
	theme_minimal() +
	theme(panel.grid = element_blank(),
				legend.position = "none", legend.title = element_blank(),
				plot.title = element_markdown(size = 16, hjust = 0),
				plot.subtitle = element_markdown(size = 11, hjust = 0),
				plot.caption = element_markdown())
rm(tmp)

ggsave("2025/images/prompt6_sosu_nat_org_sex_2025.jpg", width = 15, height = 8,
			 units = "in", dpi = 300)

ggsave("~/Data/greg_dubrow_io/posts/30-day-chart-challenge-2025/images/prompt6_sosu_nat_org_sex_2025.jpg",
			 width = 15, height = 8, units = "in", dpi = 300)


## all vet by sex
sosu_all %>%
	filter(deg_name == "VET All") %>%
	filter(!sex == "Total") %>%
	filter(national_origin == "Total") %>%
	select(year, sex, n) %>%
	group_by(year) %>%
	mutate(sex_pct = n/sum(n)) %>%
	ungroup() %>%
	ggplot(aes(year, sex_pct, fill = sex)) +
	geom_bar(stat = "identity") +
	scale_fill_manual(values = c("#E66100", "#5D3A9B")) +
	scale_x_continuous(breaks = c(2019, 2020, 2021, 2022, 2023, 2024)) +
	scale_y_continuous(labels = label_percent()) +
	geom_text(aes(label = percent(sex_pct, 2)),
						position = position_stack(vjust = 0.5),
						size = 4.5, color = "white") +
	labs(x = "", y = "",
			 title = "While 90% of SOSU degrees are awarded to women, men earn slightly higher percent of all H30 vocational degrees",
			 subtitle = "*<span style='color: #E66100;'>Men</span> --
			 <span style='color: #5D3A9B;'>Women</span>*",
			 caption = "*Data from Danmarks Statistik table UDDAKT35 via danstat package*") +
	theme_minimal() +
	theme(panel.grid = element_blank(),
				legend.position = "none",
				plot.title = element_markdown(size = 16, hjust = 0),
				plot.subtitle = element_markdown(size = 12, hjust = 0),
				plot.caption = element_markdown())
rm(tmp)

ggsave("2025/images/prompt6_allvet_sex_2025.jpg", width = 15, height = 8,
			 units = "in", dpi = 300)

ggsave("~/Data/greg_dubrow_io/posts/30-day-chart-challenge-2025/images/prompt6_allvet_sex_2025.jpg",
			 width = 15, height = 8, units = "in", dpi = 300)


## all vet by nat origin

sosu_all %>%
	filter(deg_name == "VET All") %>%
	filter(sex == "Total") %>%
	filter(!national_origin == "Total") %>%
	select(year, national_origin, n) %>%
	group_by(year) %>%
	mutate(nat_org_pct = n/sum(n)) %>%
	ungroup() %>%
	{. ->> tmp} %>%
	ggplot(aes(year, nat_org_pct, fill = national_origin)) +
	geom_bar(stat = "identity") +
	scale_fill_manual(values = c("#CC79A7", "#D55E00", "#0072B2", "#F0E442")) +
	scale_x_continuous(breaks = c(2019, 2020, 2021, 2022, 2023, 2024)) +
	scale_y_continuous(labels = label_percent()) +
	geom_text(data = subset(tmp, nat_org_pct > 0.01),
						aes(label = percent(nat_org_pct, 2)),
						position = position_stack(vjust = 0.5),
						size = 4.5, color = "white") +
	labs(x = "", y = "",
			 title = "Immigrant share of all H30 vocational degrees less than half the share of SOSU degrees",
			 subtitle = "*<span style='color: #CC79A7;'>Danish origin = at least 1 parent born in & citizen of DK</span>;
			<span style='color: #D55E00;'>Descendant = born in DK, neither parent citizen or born in DK</span>;
						<span style='color: #0072B2;'>Immigrant = born abroad, neither parent citizen or born in DK</span>*",
			 caption = "*Data from Danmarks Statistik table UDDAKT35 via danstat package*") +
	theme_minimal() +
	theme(panel.grid = element_blank(),
				legend.position = "none", legend.title = element_blank(),
				plot.title = element_markdown(size = 16, hjust = 0),
				plot.subtitle = element_markdown(size = 11, hjust = 0),
				plot.caption = element_markdown())
rm(tmp)

ggsave("2025/images/prompt6_allvet_natorg_2025.jpg", width = 15, height = 8,
			 units = "in", dpi = 300)

ggsave("~/Data/greg_dubrow_io/posts/30-day-chart-challenge-2025/images/prompt6_allvet_natorg_2025.jpg",
			 width = 15, height = 8, units = "in", dpi = 300)

