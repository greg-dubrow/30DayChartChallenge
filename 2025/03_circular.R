# bachelors degs awarded 2023 by major field of study (big circles) and subfields (small circles)

library(tidyverse) # to do tidyverse things
library(tidylog) # to get a log of what's happening to the data
library(janitor) # tools for data cleaning
library(danstat) # package to get Danish statistics via api
library(ggtext) # enhancements for text in ggplot
library(ggrepel)
library(scales)
library(packcircles)
#library(patchwork)

# some custom functions
source("~/Data/r/basic functions.R")

# UDDAKT60
table_meta <- danstat::get_table_metadata(table_id = "uddakt60", variables_only = TRUE)

# create variable list using the ID value in the variable
variables_ed <- list(
	list(code = "uddannelse", values = c("H6020", "H6025", "H6030", "H6035",
																			 "H6039", "H6059", "H6075", "H6080", "H6090")),
	list(code = "fstatus", values = c("F")),
	#list(code = "køn", values = 10),
	#list(code = "alder", values = c("TOT")),
	list(code = "tid", values = 2023))

degs1 <- get_data("uddakt60", variables_ed, language = "en") %>%
	as_tibble() %>%
	mutate(deg_field = case_when(UDDANNELSE == "H6020 Educational, BACH" ~ "Educ.",
															 UDDANNELSE == "H6025 Humanities and theological, BACH" ~ "Humanities",
															 UDDANNELSE == "H6030 Arts, BACH" ~ "Arts",
															 UDDANNELSE == "H6035 Science, BACH" ~ "Science",
															 UDDANNELSE == "H6039 Social Sciences, BACH" ~ "Social Science",
															 UDDANNELSE == "H6059 Technical sciences, BACH" ~ "Tech Science",
															 UDDANNELSE == "H6075 Food, biotechnology and laboratory technology, BACH"
															 ~ "Food/Biotech/LabTech",
															 UDDANNELSE == "H6080 Agriculture, nature and environment, BACH"
															 ~ "Agricultural Science",
															 UDDANNELSE == "H6090 Health science, BACH" ~ "Health Sciences")) %>%
	mutate(text = paste("name: ",deg_field, "\n", "value:", INDHOLD, "\n", "You can add a story here!"))

degs2 <- degs1 %>%
	select(group = deg_field, value = INDHOLD, text)

degs_packing <- circleProgressiveLayout(degs2$value, sizetype='area')
degs3 <- cbind(degs2, degs_packing)
degs.gg <- circleLayoutVertices(degs_packing, npoints=50)

ggplot() +
	geom_polygon_interactive(
		data = degs.gg,
		aes(x, y, group = id, fill=id, tooltip = data$text[id], data_id = id),
		colour = "black", alpha = 0.6) +
	scale_fill_viridis() +
	geom_text(data = degs3 %>% filter(value > 6000),
						aes(x, y, label = paste0(group, "\n", format(value, big.mark=","))),
						size=7, color="black") +
	geom_text(data = degs3 %>% filter(between(value, 2410, 3350)),
						aes(x, y, label = paste0(group, "\n", format(value, big.mark=","))),
				size=7, color="black") +
	geom_text(data = degs3 %>% filter(between(value, 600, 2410)),
						aes(x, y, label = paste0(group, "\n", format(value, big.mark=","))),
						size=6, color="black") +
	geom_text_repel(data = degs3 %>% filter(between(value, 200, 400)),
						aes(x, y, label = paste0(group, "\n", format(value, big.mark=","))),
						size=4, color="black",
						max.overlaps = Inf, nudge_x = -110, nudge_y = 50,
						segment.curvature = 0,
						segment.ncp = 8,
						segment.angle = 30) +
	labs(x = "", y = "",
			 title = "Social Sciences were the most popular Bachelor's degrees awarded by Danish universities in 2023",
			 subtitle = "*Labels not diplayed: Education = 134, Food Science = 61*",
			 caption = "*Data from Danmarks Statistik via danstat package*") +
#	annotate("text", x = -60, y = 100, label = "Some text") +
	theme_void() +
	theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm"),
				plot.title = element_markdown(size = 16),
				plot.subtitle = element_markdown(size = 12),
				plot.caption = element_markdown(size = 8)) +
	coord_equal()

ggsave("2025/images/prompt3_2025.jpg", width = 15, height = 8,
			 units = "in", dpi = 300)

ggsave("~/Data/greg_dubrow_io/posts/30-day-chart-challenge-2025/images/prompt3_2025.jpg",
			 width = 15, height = 8, units = "in", dpi = 300)
