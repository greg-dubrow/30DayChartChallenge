## 07 outliers - most and least popular vocational degrees 2024
## 08 histogram of all VET to show distribution.

library(tidyverse) # to do tidyverse things
library(tidylog) # to get a log of what's happening to the data
library(janitor) # tools for data cleaning
library(danstat) # package to get Danish statistics via api
library(ggtext) # enhancements for text in ggplot
library(scales)
library(tidytext)

# some custom functions
source("~/Data/r/basic functions.R")

# UDDAKT35
table_meta <- danstat::get_table_metadata(table_id = "uddakt35", variables_only = TRUE)
table_meta_dk <- danstat::get_table_metadata(table_id = "uddakt35", variables_only = TRUE, language = "da")

# create variable list using the ID value in the variable
# getting all education programs, pare down
variables_ed <- list(
	list(code = "uddannelse", values = "*"),
	list(code = "fstatus", values = c("F")),
#	list(code = "køn", values = "*"),
#	list(code = "herkomst", values = "*"),
	list(code = "tid", values = 2024))

vet1 <- get_data("uddakt35", variables_ed, language = "en") %>%
	as_tibble() %>%
	clean_names()

glimpse(vet1)

vet_all <- vet1 %>%
	mutate(deg_code = str_extract(uddannelse, "^[^ ]+")) %>%
	filter(!grepl("H29", deg_code)) %>%
	filter(!grepl("Total", deg_code)) %>%
	mutate(deg_level = if_else(str_length(deg_code) == 5, "Main", "Sub")) %>%
	mutate(deg_level = if_else(deg_code == "H30", "All", deg_level)) %>%
	mutate(deg_name = sub("^\\S+\\s+", '', uddannelse)) %>%
	mutate(deg_outlier_calc = ifelse(deg_level == "Sub" & indhold > 0, 1 ,0)) %>%
	mutate(deg_outlier = ifelse(deg_outlier_calc == 1
															& (indhold>mean(indhold)+2*sd(indhold)), "outlier", "normal"))

glimpse(vet_all)

vet_all %>%
	filter(deg_level == "Sub") %>%
	filter(indhold > 0) %>%
	summarise(mean_degs = mean(indhold),
						med_degs = median(indhold),
						min_degs = min(indhold),
						max_degs = max(indhold),
						std_degs = sd(indhold))

vet_majors %>%
	select(deg_name, indhold) %>%
	arrange(desc(indhold))

# histogram for prompt 8
vet_majors %>%
	ggplot(aes(indhold)) +
	geom_histogram(fill = "#CC79A7", bins = 100) +
	geom_vline(aes(xintercept = mean(indhold)), color="black") +
	geom_vline(aes(xintercept = median(indhold)), color="#A9A9A9") +
	labs(x = "", y = "",
			 title = "Eight programs with +1,000 degrees awarded accounted for 43% of the H30 Vocational degrees awarded in 2024",
			 subtitle = "*Total H30 degrees awarded in 2024 = 30,570*",
			 caption = "*Data from Danmarks Statistik table UDDAKT35 via danstat package*") +
	scale_y_continuous(expand = c(.001, 0), limits = c(0, 20),
										 breaks = c(0, 1, 3, 6, 9, 12, 15, 18, 20)) +
	scale_x_continuous(breaks = c(0, 500, 1000, 2000, 3000, 4000, 5000, 6000),
		labels = comma) +
	theme_minimal() +
	theme(plot.title = element_markdown(size = 16, hjust = 0),
				plot.subtitle = element_markdown(size = 14, hjust = 0),
				plot.caption = element_markdown(),
				legend.title = element_blank(),
				legend.position = c(.98, .05),
				legend.justification = c("right", "bottom"),
				legend.direction = "horizontal",
				legend.box.just = "right",
				legend.margin = margin(6, 6, 6, 6),
				panel.grid = element_blank(),
				panel.border = element_blank())

ggsave("2025/images/prompt7&8_hist_2025.jpg", width = 15, height = 8,
			 units = "in", dpi = 300)

ggsave("~/Data/greg_dubrow_io/posts/30-day-chart-challenge-2025/images/prompt7&8_hist_2025.jpg",
			 width = 15, height = 8, units = "in", dpi = 300)


# outliers for prompt 7. need df with only major + 0 degrees
vet_majors <- vet_all %>%
	filter(deg_level == "Sub") %>%
	filter(indhold > 0) %>%
	mutate(degs_mean = mean(indhold),
				 degs_std = sd(indhold),
				 degs_pctl10 = quantile(indhold, 0.10),
				 degs_pctl90 = quantile(indhold, 0.90)) %>%
	mutate(outlier = case_when(
		indhold >= degs_pctl90 ~ "outlier+ > 90thpctl",
		indhold <= degs_pctl10 ~ "outlier- < 10thpctl",
		TRUE ~ "not outlier")) %>%
	mutate(outlier = factor(outlier,
													levels = c("outlier+ > 90thpctl", "not outlier", "outlier- < 10thpctl")))

# horizontal bar with fills based on outlier status, bars for mean and median
# for annotation,
caption1 <- paste(strwrap("Health, education, business (office, retail, trading (mercantile)), and
												 craft trades (electrician, carpentry, mechanic) are by far the most popular vocational degree choices.",
													75), collapse = "\n")

caption2 <- glue::glue(
	"<span style = 'color:#A9A9A9;'>Median = 97   </span><span style = 'color:#000000;'>
	Mean = 392</span>"
)

vet_majors %>%
	ggplot(aes(x = indhold, y = reorder(deg_name, indhold), fill = outlier)) +
	geom_bar(stat = "identity") +
	scale_fill_manual(values = c("#0072B2", "#CC79A7", "#D55E00")) +
	geom_vline(aes(xintercept = mean(indhold)), color="black") +
	geom_vline(aes(xintercept = median(indhold)), color="#A9A9A9") +
#	geom_vline(aes(xintercept = quantile(indhold, 0.10)), color="black") +
	scale_x_continuous(limits = ~ c(0, max(.x) + 0.1), breaks = pretty_breaks(),
										 expand = c(0, 0), labels = comma) +
	labs(x = "", y = "",
			 title = "Total H30 degrees awarded in 2024 = 30,570",
			 caption = "*Data from Danmarks Statistik table UDDAKT35 via danstat package*") +
	annotate("text", x = 2000, y = "Storage, port and terminal education",
					 label = caption1, size = 5, hjust = 0) +
	annotate("richtext", x = 4700, y = "Train preparation educations",
					 label = caption2,
					 size = 4.5, label.color = NA) +
	theme_minimal() +
	theme(plot.title = element_markdown(size = 16),
				plot.caption = element_markdown(),
				axis.text.y = element_text(size = 7),
				legend.title = element_blank(),
				legend.position = c(.98, .05),
				legend.justification = c("right", "bottom"),
				legend.direction = "horizontal",
				legend.box.just = "right",
				legend.margin = margin(6, 6, 6, 6),
				panel.grid = element_blank(),
				panel.border = element_blank())

ggsave("2025/images/prompt7&8_outl_2025.jpg", width = 15, height = 8,
			 units = "in", dpi = 300)

ggsave("~/Data/greg_dubrow_io/posts/30-day-chart-challenge-2025/images/prompt7&8_outl_2025.jpg",
			 width = 15, height = 8, units = "in", dpi = 300)


## notes - translations, eg Gartner Education = Gardening Education
# Trading education is commerce or merchant. Logistics and sourcing.
  # Handler...vinhandler = wine merchant
# Smoth worker is working with steel in any context...machines, windmills, etc







### code not being used in final


### first attempt, not using
# UDDAKT20
# table_meta <- danstat::get_table_metadata(table_id = "uddakt20", variables_only = TRUE)
#
# variables_ed <- list(
# 	list(code = "alder", values = "*"),
# 	list(code = "fstatus", values = "B"),
# 	list(code = "tid", values = 2024))
#
# # past variable list along with table name.
# primary <- get_data("uddakt20", variables_ed, language = "en") %>%
# 	as_tibble() %>%
# 	clean_names()
#
# glimpse(primary)
#
# primary %>%
# 	count(alder)
#
# primary %>%
# 	filter(!alder == "Age, total") %>%
# 	filter(!alder == "-5 years") %>%
# 	mutate(age = str_remove(alder, " years")) %>%
# 	mutate(age = str_remove(age, " and over")) %>%
# 	mutate(age = as.numeric(age)) %>%
# 	ggplot(aes(age, indhold)) +
# 	geom_bar(stat = "identity")
#
