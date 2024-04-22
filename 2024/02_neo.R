## eurostat new births by year by country?

# https://ec.europa.eu/eurostat/databrowser/view/tps00204/default/table?lang=en
#
# Live births and crude birth rate
# Online data code: tps00204
# DOI: 10.2908/tps00204
#

# Live births are the births of children that showed any sign of life.
# The crude birth rate is the ratio of the number of live births during the year to the average population in that year.
# The value is expressed per 1 000 persons.

library(tidyverse)
library(tidylog)
library(janitor)
library(patchwork)
library(ggtext)

births <- eurostat::get_eurostat("tps00204", type = "label", time_format = "num") %>%
	select(-freq) %>%
	rename(year = TIME_PERIOD)

glimpse(births)

births %>%
	filter(year == 2022) %>%
	view()

## patchwork two charts - total births, birth rates, sorted descending by value

# geom_text(
# 	aes(label = y, y = y + 0.05),
# 	position = position_dodge(0.9),
# 	vjust = 0
# ) #						position = position_stack(vjust = 0.5),

total_births <-
births %>%
	filter(year == 2022) %>%
	filter(indic_de == "Live births - number") %>%
	filter(!grepl("Euro",geo)) %>%
	arrange(desc(values), geo) %>%
	mutate(geo = forcats::fct_inorder(geo)) %>%
	{. ->> tmp} %>%
	ggplot(aes(values, fct_rev(geo))) +
	geom_bar(stat = "identity", fill = "#FFCC00") +
	scale_y_discrete(position = "right") +
	# need to subset data to get highest label to align similar to the rest
	geom_text(data = subset(tmp, values <800000),
		aes(label = format(values, big.mark=",")),
						color= "#003399", size = 5,
						hjust = -.25, nudge_x = .25) +
	geom_text(data = subset(tmp, values >800000),
						aes(label = format(values, big.mark = ",")),
						color= "#003399", size = 5,
						hjust = 1, nudge_x = .5) +
	labs(y = "", x = "Total Live Births - 2022") +
	theme_minimal() +
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

births_per1k <-
births %>%
	filter(year == 2022) %>%
	filter(indic_de == "Crude birth rate - per thousand persons") %>%
	filter(!grepl("Euro",geo)) %>%
	arrange(desc(values), geo) %>%
	mutate(geo = forcats::fct_inorder(geo)) %>%
	ggplot(aes(values, fct_rev(geo))) +
	geom_bar(stat = "identity", fill = "#003399") +
	geom_text(aes(label = values),
						position = position_stack(vjust = 0.5),
						color= "#FFCC00", vjust = 0.5, size = 6) +
	labs(y = "", x = "Births per 1000 persons - 2022") +
	theme_minimal() +
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

total_births + births_per1k + plot_annotation(
	title = 'Turkey tops in both total births and births / 1000 people',
	subtitle = 'France only other country top 10 in both measures. San Marino bottom in both',
	caption = 'Data from EuroStats using eurostat package')
