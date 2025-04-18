# sankey diagram with parent - child education path
# can do from aggregated data in networkd3
# using this as guide https://www.youtube.com/watch?v=3OvsW8OI1wo&ab_channel=NHS-RCommunity
# freq df with parent_ed child_ed as columns, with freq column
# create nodes df with names of ed levels. should be as data.frame so sankey works
    # nodes_df <- as.data.frame(ed_path_freqs %>%
    #   select(parent_ed, child_ed) %>%
    #   pivot_longer(c("parent_ed", "child_ed"), names_to = "col_name", values_to = "name_match") %>%
    # # this removes dupes keeps only name match
    #   select("name_match) %>% distinct())  # may need to clean names for labels to look nice
# create ID df for order of nodes from ed_path_freqs & nodes; -1 ensures 0 index
    # sankey_plot_id <- as.dta.frame(ed_path_freqs %>%
    #   mutate(IDIn = match(parent_ed, nodes$name_match), -1,
    #          IDOut = match(parent_ed, nodes$name_match), -1))
# create graph
  # sankeyNetwork(Links = sankey_plot_id, Nodes = nodes_df,
  #               Source = "IDIn", Target = "IDOut",
  #               Value = "Freq", NodeID = "name", # "Freq" from freq df, "name" from nodes_df
  #               sinksRight = FALSE) can also change font_Size, font_Family, nodeWidth =
# to change colors
  # node_color <- 'd3.scaleOrdinal().domain(["", ""]) # color names in "" one for each ed level
  #                                 .range(["", ""])' # hex values for each color
  # nodes_df <- nodes_df %>%
  # mutate(ColorGroup = case_when(name = "ed_level_name" ~ "color name"))
# format colors in graph
# create graph
# sankeyNetwork(Links = sankey_plot_id, Nodes = nodes_df,
#               Source = "IDIn", Target = "IDOut",
#               Value = "Freq", NodeID = "name", # "Freq" from freq df, "name" from nodes_df
#               sinksRight = FALSE,
#               colorScale = node_color, NodeGroup = "ColorGroup") # can also change height = width =



library(tidyverse) # to do tidyverse things
library(tidylog) # to get a log of what's happening to the data
library(janitor) # tools for data cleaning
library(danstat) # package to get Danish statistics via api
library(infer) # tidy statistical inference
library(scales)
library(networkD3) # for Sankey plots
# library(ggsankey) # another sankey package
# library(ggalluvial)
library(htmlwidgets) # html widgets helps to handle the networkD3 objects
library(htmltools) # for formatting html code
library(gt)

source("~/Data/r/basic functions.R")


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

glimpse(ed_attain1)

ed_attain1 %>%
	filter(!forudd1 == "Total") %>%
	filter(alder1== "Age, total") %>%
	filter(statusvid == "Educational graduation statement, total") %>%
	select(forudd1, indhold) %>%
	gt()


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
				 							"Short-cycle college",
				 						forudd1 == "H50 Vocational bachelors educations" ~
				 							"Bachelor-Vocational",
				 						forudd1 == "H60 Bachelors programs" ~
				 							"Bachelor-Academic",
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

saveRDS(ed_attain_main,
	"~/Data/greg_dubrow_io/posts/30-day-chart-challenge-2025/data/ed_attain_main.Rda")


ed_attain_main %>%
	count(parent_ed)

ed_attain_main %>%
	filter(!parent_ed == "Total") %>%
	filter(child_age == "Total") %>%
	filter(child_ed == "Total") %>%
	select(parent_ed, N) %>%
	mutate(pct = N/ sum(N)) %>%
	gt() %>%
	cols_label(parent_ed = "Parent education level") %>%
	fmt_number(columns = N, sep_mark = ",", decimals = 0) %>%
	fmt_percent(columns = pct, decimals = 1) %>%
	opt_stylize(style = 5)

ed_attain_main %>%
	filter(parent_ed == "Total") %>%
	filter(child_age == "Total") %>%
	filter(!child_ed == "Total") %>%
	select(child_ed, N) %>%
	mutate(pct = N/ sum(N)) %>%
	gt() %>%
	cols_label(child_ed = "Higher ed status") %>%
	fmt_number(columns = N, sep_mark = ",", decimals = 0) %>%
	fmt_percent(columns = pct, decimals = 1) %>%
	opt_stylize(style = 5)

# gt tables of pct of total each parent ed main group child ed underneath?

# sankey diagram for all ages
ed_attain_allage <- ed_attain_main %>%
	filter(!parent_ed == "Total") %>%
	filter(!parent_ed == "H35 Qualifying educational programs") %>%
	filter(child_age == "Total") %>%
	filter(!child_ed == "Total") %>%
	select(parent_ed, child_ed, N) %>%
	mutate(parent_ed = factor(parent_ed,
		levels = c("Primary", "HS-Vocational", "HS-Academic", "Short-cycle college",
								"Bachelor-Vocational", "Bachelor-Academic", "Masters", "PhD", "Not stated"))) %>%
	mutate(child_ed = factor(child_ed,
		levels = c("Completed education", "Undergoing education", "Discontinued education",
			"No registered education")))

glimpse(ed_attain_allage)

ed_attain_allage %>%
	count(child_ed)

ed_attain_allage %>%
	count(parent_ed)

nodes_df <- as.data.frame(ed_attain_allage %>%
	select(parent_ed, child_ed) %>%
	pivot_longer(c("parent_ed", "child_ed"),
	names_to = "col_name", values_to = "name_match") %>%
	# this removes dupes keeps only name match
	# may need to clean names for labels to look nice
	select("name_match") %>%
	distinct())

nodes_df %>%
	count(name_match)

glimpse(nodes_df)

# create ID df for order of nodes from ed_path_freqs & nodes; -1 ensures 0 index
sankey_plot_id <- as.data.frame(ed_attain_allage %>%
  mutate(IDIn = match(parent_ed, nodes_df$name_match) -1,
  			 IDOut = match(child_ed, nodes_df$name_match)-1))

glimpse(sankey_plot_id)

sankey_plot_id %>%
	count(parent_ed)

## save sankey_plot_id and nodes_df for quiet loading into blog post
saveRDS(sankey_plot_id,
"~/Data/greg_dubrow_io/posts/30-day-chart-challenge-2025/data/sankey_plot_id.Rda")

saveRDS(nodes_df,
	"~/Data/greg_dubrow_io/posts/30-day-chart-challenge-2025/data/nodes_df.Rda")


# create graph
sankeyNetwork(Links = sankey_plot_id, Nodes = nodes_df,
              Source = "IDIn", Target = "IDOut",
							# "Freq"
							#from freq df, "name" from nodes_df
							Value = "N", NodeID = "name_match",
              sinksRight = FALSE, iterations = 0,
	fontSize = 16, fontFamily = "Arial", nodeWidth = 20)
# can also change font_Size, font_Family, nodeWidth =









### old code, not using
##### using ggsankey

ed_attain_allage_df <- as.data.frame(ed_attain_allage)

ed_attain_allage_ggs <- ed_attain_allage_df %>%
	make_long(parent_ed, child_ed, N)

glimpse(ed_attain_allage_ggs)

ggplot(ed_attain_allage_ggs,
	aes(x = x, next_x = next_x, node = node, next_node = next_node,
		fill = factor(node), label = node)) +
	geom_sankey() +
	theme_minimal() +
	theme(legend.position = "none")


ed_attain_allage_df2 <- ed_attain_allage_df %>%
	filter(parent_ed %in% c("Primary", "HS-Academic", "HS-Vocational")) %>%
	filter(child_ed %in% c("Completed education", "Undergoing education"))

ed_attain_allage_ggs2 <- ed_attain_allage_df2 %>%
	make_long(parent_ed, child_ed, N)

ggplot(ed_attain_allage_ggs2,
	aes(x = x, next_x = next_x, node = node, next_node = next_node,
		fill = factor(node), label = node)) +
	geom_sankey() +
	theme_minimal() +
	theme(legend.position = "none")


### with ggalluvial

glimpse(ed_attain_allage_df)

ggplot(ed_attain_allage_ggs,
	aes(y = N, axis1 = parent_ed, axis2 = child_ed)) +
	geom_alluvial() +
	geom_stratum()
