## code for 30 Day Chart Challenge 2024, day 6 OECD
## educational attainment via OECD package
## nordics, by sex 25 to 64, 2020-2022

library(tidyverse) # to do tidyverse things
library(tidylog) # to get a log of what's happening to the data
library(janitor) # tools for data cleaning
library(OECD) # package to get OECD data via api
library(ggtext) # enhancements for text in ggplot

# table from website
# https://www.oecd-ilibrary.org/education/data/education-at-a-glance/educational-attainment-and-labour-force-status_889e8641-en


edattdata <- "OECD.CFE.EDS,DSD_REG_EDU@DF_ATTAIN,1.0"
edattfilter <- "A.CTRY.SWE+NOR+ISL+FIN+DNK...Y25T64.F+M.ISCED11_0T2+ISCED11_5T8+ISCED11_3_4.."


oecd1 <- get_dataset(edattdata, edattfilter, start_time = 2020, end_time = 2022)

glimpse(oecd1)

oecd1 %>%
	count(COUNTRY)

## get metadata
oecd_metadata <- get_data_structure(edattdata)
str(oecd_metadata, max.level = 1)

oecd_metadata$VAR_DESC
view(oecd_metadata$CL_EDUCATION_LEV_ISCED11)

datalabs_ct <- oecd_metadata$CL_REGIONAL %>%
	rename(COUNTRY = id)

## to speed up blog post rendering saving this as data object to load into env
saveRDS(datalabs_ct, file = "2024/data/oecd_datalabs_ct.rds")


edlev %>%	count(label)

# keep only necessary vars, change some to factors & add labels
oecd <- oecd1 %>%
	mutate(EDUCATION_LEV =
				 	factor(EDUCATION_LEV,
				 				 levels = c("ISCED11_0T2", "ISCED11_3_4", "ISCED11_5T8"),
				 				 labels = c("Pre-primary thru lower secondary",
				 				 					 "Upper secondary and non-degree tertiary", "Tertiary education"))) %>%
	mutate(SEX = case_when(SEX == "M" ~ "Male",
												 SEX == "F" ~ "Female")) %>%
	mutate(pct_attain2 = as.numeric(ObsValue)) %>%
	mutate(pct_attain = pct_attain2 / 100) %>%
	left_join(datalabs_ct) %>%
	select(country = label,  year = TIME_PERIOD, SEX, EDUCATION_LEV, pct_attain, pct_attain2) %>%
	clean_names()

glimpse(oecd)


## chart...horizontal stacked bar

oecd %>%
	filter(year == "2022") %>%
	ggplot(aes(pct_attain, fct_rev(country), fill = fct_rev(education_lev))) +
	geom_bar(stat = "identity") +
	scale_x_continuous(expand = c(0,0),
										 breaks = c(.01, 0.25, 0.50, 0.75, .97),
										 labels = c("0", "25%", "50%", "75%", "100%")) +
	facet_wrap(~ sex, nrow = 1) +
	geom_text(aes(label = scales::percent(round(pct_attain , 2))),
						position = position_stack(vjust = 0.5),
						color= "white", vjust = 0.5, size = 5) +
	labs(title = "In Nordic countries, women age 25-64 more likely than men to complete college.<br><br>
			 Finns have lowest percentage of attainment stopping at lower secondary",
			 subtitle = "<br>*Educational Attainment in Nordic Countries, by Sex, ages 25-64 combined, 2022*",
			 caption = "*Data from OECD, via oecd package for r*",
			 x = "", y = "") +
	scale_fill_brewer(palette = "Set2") +
	theme_minimal() +
	theme(legend.position = "bottom", legend.spacing.x = unit(0, 'cm'),
				legend.key.width = unit(1.5, 'cm'), legend.margin=margin(-10, 0, 0, 0),
				plot.title = element_markdown(), plot.subtitle = element_markdown(),
				plot.caption = element_markdown(),
				axis.text.y = element_text(size = 12),
				panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
	guides(fill = guide_legend(label.position = "bottom", reverse = TRUE,
														 title = "Education Levels", title.position = "top"))

