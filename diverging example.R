library(extrafont)
library(ggpattern)
sysfonts::font_add_google(name = "Asap Condensed")
sysfonts::font_add_google(name = "Asap")
font <- "Asap"
font2 <- "Asap Condensed"


# Numbers reflect the percentage who believe climate change IS caused by humans
climate_skep_sex <- read_delim("climate_skep_sex.txt", delim = "\t", locale = comma_delim) %>%
	pivot_longer(cols = -yr) %>%
	rename(sex = name, perc = value)

climate_skep_sex <- readxl::read_excel("~/Downloads/folk-mener-klimaendringe.xlsx") %>%
	pivot_longer(cols = -yr) %>%
	rename(sex = name, perc = value)


vlines_df <- data.frame(xintercept = seq(-100, 100, 20))

climate_skep_sex_p <-
	climate_skep_sex %>%
	mutate(perc = 100 - perc) %>%
	mutate(yr = as.factor(yr)) %>%
	mutate(yr = factor(yr, levels = rev(levels(yr)))) %>%
	mutate(perc = ifelse(sex == "Male", -1 * perc, perc))

#p <-
	climate_skep_sex_p %>%
	ggplot() +
	geom_col(aes(x = -50, y = yr), width = 0.75, fill = "#e0e0e0") +
	geom_col(aes(x = 50, y = yr), width = 0.75, fill = "#e0e0e0") +
	geom_col(aes(x = perc, y = yr, fill = sex), width = 0.75) +
	theme_void() +
	theme(
		legend.position = "none",
		axis.text.y = element_text(size = 5, color = "#999999", hjust = 1, margin = margin(r = -10)),
		axis.text.x = element_text(size = 5, color = "#999999", margin = margin(t = 1)),
		axis.title.x = element_markdown(size = 5.5, color = "#555555", margin = margin(t = 1)),
		plot.title = element_markdown(size = 12, color = "#444444"),
		plot.subtitle = element_text(size = 5.5, color = "#444444"),
		plot.background = element_rect(fill = "#FFFFFF", color = NA),
		panel.background = element_rect(fill = "#FFFFFF", color = NA),
		axis.ticks.length = unit(1, "mm"),
		axis.ticks.x = element_line(size = 0.2, color = "#e0e0e0"),
		plot.caption = element_text(size = 4, color = "#9C9C9C")
	) +
	scale_fill_manual(values = c("#619b90", "#ce7382")) +
	scale_x_continuous(labels = function(x) abs(x), breaks = seq(-100, 100, 20)) +
	geom_vline(data = vlines_df, aes(xintercept = xintercept), color = "#FFFFFF", size = 0.1, alpha = 0.5) +
	labs(
		title = "**A gender in denial**",
		subtitle = "Questioned whether climate change is to a large extent caused by human actions, only\n70% of Norwegian adults answer 'yes'. Men are more likely to be in denial than women,\nand the overall trend is quite stable, but the % of men in denial is slightly decreasing.\n",
		x = expression("Percentage who do **not** believe climate change is caused by humans"),
		caption = "Source: Norsk Medborgerpanel (energiogklima.no/nyhet/klimaskepsis) "
	) +
	coord_cartesian(clip = "off") +
#	annotation_custom(grob = ggplotGrob(flag), xmin = 90, xmax = 90, ymin = 14.5, ymax = 14.5) +
	geom_text(data = climate_skep_sex_p %>% filter(sex == "Male"), aes(x = perc, y = yr, label = paste0(abs(perc), "%")), size = 1.8, color = "#FFFFFF", family = font, position = position_nudge(x = 5)) +
	geom_text(data = climate_skep_sex_p %>% filter(sex == "Female"), aes(x = perc, y = yr, label = paste0(abs(perc), "%")), size = 1.8, color = "#FFFFFF", family = font, position = position_nudge(x = -5)) +
	geom_segment(aes(x = mean((climate_skep_sex_p %>% filter(sex == "Male"))$perc), xend = -1, y = 9.6, yend = 9.6), size = 0.35, color = "#ce7382") +
	annotate(geom = "text", label = "Males", family = font2, size = 2, x = -2, y = 9.7, vjust = 0, hjust = 1, color = "#ce7382") +
	geom_segment(aes(x = 1, xend = mean((climate_skep_sex_p %>% filter(sex == "Female"))$perc), y = 9.6, yend = 9.6), size = 0.35, color = "#619b90") +
	annotate(geom = "text", label = "Females", family = font2, size = 2, x = 2, y = 9.7, vjust = 0, hjust = 0, color = "#619b90") +
	annotate(geom = "text", label = "Mean % in denial last 10 years", family = font, size = 1.5, x = 35, y = 10.5, vjust = 0, hjust = 0, color = "#999999")
