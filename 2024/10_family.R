## sankey diagram of parent ed level to student attainment for 35-45 year olds in 2022

library(tidyverse) # to do tidyverse things
library(tidylog) # to get a log of what's happening to the data
library(janitor) # tools for data cleaning
library(danstat) # package to get Danish statistics via api
library(ggtext) # enhancements for text in ggplot

# metadata for table variables STATUS42
table_meta <- danstat::get_table_metadata(table_id = "status42", variables_only = TRUE)


# create variable list
variables_ed <- list(
	list(code = "statusvid", values = c("110", "210", "310", "350", "360")),
	list(code = "forudd1", values = c("H10", "H20", "H30", "H35",
																	"H40", "H50", "H60", "H70", "H80", "H90")),

	list(code = "alder1", values = c("35", "36", "37", "38", "39",
																	"40", "41", "42", "43", "44", "45")),
	list(code = "tid", values = 2022))

# past variable list along with table name
edattain1 <- get_data("status42", variables_ed, language = "da") %>%
	as_tibble() %>%
	select(attain_child = STATUSVID, attain_parent = FORUDD1, age = ALDER1,
				 year = TID, n = INDHOLD) %>%
	mutate(attain_child = str_replace(
		attain_child, "FULDFØRT VIDEREGÅENDE UDDANNELSE", "Completed")) %>%
	mutate(attain_child = str_replace(
		attain_child, "IGANGVÆRENDE VIDEREGÅENDE UDDANNELSE", "Currently attending")) %>%
	mutate(attain_child = str_replace(
		attain_child, "INGEN VIDEREGÅENDE UDDANNELSE","No Higher Ed")) %>%
	mutate(attain_child = str_replace(
		attain_child, "AFBRUDT VIDEREGÅENDE UDDANNELSE", "Discontinued")) %>%
	mutate(attain_child = str_replace(
		attain_child,  "UOPLYST", "Unknown")) %>%
	mutate(attain_child = factor(
		attain_child, levels = c("Completed", "Currently attending", "Discontinued", "No Higher Ed", "Unknown"))) %>%
	mutate(attain_parent =
				 	factor(attain_parent,
				 				 levels = c("H10 Grundskole", "H20 Gymnasiale uddannelser",
				 				 					 "H30 Erhvervsfaglige uddannelser", "H35 Adgangsgivende uddannelsesforløb",
				 				 					 "H40 Korte videregående uddannelser, KVU", "H50 Mellemlange videregående uddannelser, MVU",
				 				 					 "H60 Bacheloruddannelser, BACH", "H70 Lange videregående uddannelser, LVU",
				 				 					 "H80 Ph.d. og forskeruddannelser", "H90 Uoplyst mv."),
				 				 labels = c("Grundskole/Primary", "Gymnasium",
				 				 					 "Erhvervsfaglige/Vocational HS", "Adgangsgivende/Qualifying",
				 				 					 "KVU/2-year college", "MVU/Professional BA",
				 				 					 "Bachelor", "LVU/Masters", "PhD", "Not stated" ))) %>%
	select(attain_parent, attain_child, age, n) %>%
	arrange(attain_parent, attain_child, age)

glimpse(edattain1)

edattain1 %>%
	count(attain_parent)

edattain <- edattain1 %>%
	group_by(attain_parent, attain_child) %>%
	mutate(total= sum(n)) %>%
	ungroup() %>%
	distinct(attain_parent, attain_child, .keep_all = TRUE) %>%
	select(attain_parent, attain_child, total)

nodes <- data.frame(name =
											c(edattain$attain_parent, edattain$attain_child) %>%
											unique()
)

