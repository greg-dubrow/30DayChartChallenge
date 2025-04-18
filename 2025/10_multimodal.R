# adult education survey - participation in formal or informal education after initial entry into the labor market.
# if i understand methodology correctly, ed level is at the time of survey, survey asks about participation
  ## in last 12 months
# EU harmonized, runs in multiple countries. national comparison data at eurostat
# https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Adult_Education_Survey_(AES)_methodology
# denmark https://www.dst.dk/en/Statistik/dokumentation/documentationofstatistics/adult-education-survey--aes-/statistical-presentation

# is a talbe a chart? i think so.

library(tidyverse) # to do tidyverse things
library(tidylog) # to get a log of what's happening to the data
library(janitor) # tools for data cleaning
library(danstat) # package to get Danish statistics via api
library(infer) # tidy statistical inference
library(gt)
library(gtsummary)
library(gtExtras)
library(ggtext) # enhancements for text in ggplot
library(ggrepel)
library(scales)
library(tidytext)
library(patchwork)

source("~/Data/r/basic functions.R")
#options(scipen=999)

# AES1A
table_meta <- danstat::get_table_metadata(table_id = "aes1a", variables_only = TRUE)
table_meta_dk <- danstat::get_table_metadata(table_id = "aes1a", variables_only = TRUE, language = "da")

# create variable list using the ID value in the variable
variables_ed <- list(
	list(code = "uddniv", values = "*"),
	list(code = "koen", values = "*"),
	list(code = "alder", values = "*"),
	list(code = "uformel", values = "*"),
	list(code = "formel", values = "*"),
	list(code = "tid", values = 2022))

adulted1 <- get_data("aes1a", variables_ed, language = "en") %>%
	as_tibble() %>%
	clean_names()

glimpse(adulted1)

adulted1 %>%
	count(koen)

adulted <- adulted1 %>%
	mutate(alder = str_remove(alder, " years (2022-)")) %>%
	mutate(alder = str_remove(alder, " years (-2016)")) %>%
	mutate(across(contains("formel"), ~ifelse(. ==  "All Denmark", "Total", .))) %>%
	mutate(alder = ifelse(alder == "Age total", "Total", alder)) %>%
	select(ed_attain = uddniv, sex = koen, age_group = alder, formal = formel,
				 informal = uformel, count = indhold)

glimpse(adulted)

## Table 1 All people, formal, informal, formal + informal, with pcts, no chi sq
adulted %>%
	mutate(totstat = ifelse(
		(sex == "Total" & age_group == "Total" & ed_attain == "Total"), 1, 0)) %>%
	mutate(adult_ed_status = case_when(
		(totstat == 1 & formal == "Has participated in formal education" &
			informal == "Has not participated in non-formal education") ~ "Formal Only",
		(totstat == 1 & formal == "Has not participated in formal education" &
		 	informal == "Has participated in non-formal education") ~ "Informal Only",
		(totstat == 1 & formal == "Has participated in formal education" &
		 	informal == "Has participated in non-formal education") ~ "Both",
		(totstat == 1 & formal == "Has not participated in formal education" &
		 	informal == "Has not participated in non-formal education") ~ "Neither",
		(totstat == 1 & formal == "Total" & informal == "Total" ~ "Total 25-64")
		)) %>%
	mutate(adult_ed_status =
				 	factor(adult_ed_status,
				 				 levels = c("Formal Only", "Informal Only", "Both", "Neither", "Total 25-64"))) %>%
	select(formal, informal, adult_ed_status, count, totstat) %>%
	filter(totstat == 1 & !is.na(adult_ed_status)) %>%
	arrange(adult_ed_status) %>%
	mutate(adult_ed_status_pct = count / last(count)) %>%
	select(adult_ed_status, count, adult_ed_status_pct) %>%
	gt()






adulted %>%
	filter(!formal == "Total") %>%
	filter(!informal == "Total") %>%
	filter(sex == c("Men", "Women")) %>%
	select(sex, informal, formal, count) %>%
	as.vector() %>%
	glimpse()


#adulted_inform <-
adulted %>%
	filter(!formal == "Total") %>%
	filter(!informal == "Total") %>%
#	filter(sex == c("Men", "Women")) %>%
#	filter(sex == "Total") %>%
	filter(age_group == "Total") %>%
	filter(ed_attain == "Total") %>%
	select(formal, informal, sex, count)
#%>%
	# mutate(formal = ifelse(str_detect(formal, "Has participated"), "Yes", "No")) %>%
	# mutate(informal = ifelse(str_detect(informal, "Has participated"), "Yes", "No")) %>%
	# xtabs(count ~ formal + informal, data = .) %>%
	# chisq.test(.) %>%
	#pivot_wider(names_from = formal, values_from = count)
# %>%
# 	gt()

glimpse(adulted_inform)

table_matrix <- xtabs(count ~ formal + informal, data = adulted_inform)
glimpse(table_matrix)

chi_result1 <- chisq.test(table_matrix)
chi_result1$statistic <- round(chi_result1$statistic, 1)
chi_result1$statistic
round(chi_result1$p.value, 25)

chi_result2 <- infer::chisq_test(adulted_inform, formal ~ informal)
glimpse(chi_result2)

chi_result2$statistic
chi_result2$p_value

chi_result <- data.frame(
	statistic = unname(chi_result1$statistic),
	df = unname(chi_result1$parameter),
	p_value = unname(chi_result1$p.value)
)
rownames(chi_result) <- 1

chi_result <- as_tibble(chi_result) %>%
	mutate(p_value_txt = ifelse(p_value == 0, "< 0.01", p_value))

glimpse(chi_result)

expected_df <- as.data.frame(as.table(chi_result1$expected))
names(expected_df) <- c("formal", "informal", "expected")

gss_df <- infer::gss

glimpse(gss_df)

gss_df %>%
	count(college, finrela)

chisq_test(gss, college ~ finrela)

# Step 6: Extract residuals
	residuals_df <- as.data.frame(as.table(chi_result1$residuals))
	names(residuals_df) <- c("formal", "informal", "residual")
# Step 4: Tidy table into data frame for gt
table_df <- as.data.frame.matrix(table_matrix) %>%
	tibble::rownames_to_column(var = "Formal Education")

# Step 5: Display with gt, include chi-square stat as summary
gt_table <- table_df %>%
	gt() %>%
	tab_header(
		title = "Participation in Formal vs Non-Formal Education"
	) %>%
	tab_footnote(
		footnote = paste0("Chi-squared = ", round(chi_result$statistic, 2),
											", df = ", chi_result$parameter,
											", p-value = ", signif(chi_result$p.value, 3)),
		locations = cells_title(groups = "title")
	)

# View the gt table
print(gt_table)

