library(packcircles)
library(ggplot2)
library(viridis)
library(ggiraph)

# Create data
data <- data.frame(
	group=paste("Group_", sample(letters, 70, replace=T),
							sample(letters, 70, replace=T), sample(letters, 70, replace=T), sep="" ),
	value=sample(seq(1,70),70))

# Add a column with the text you want to display for each bubble:
data$text <- paste("name: ",data$group, "\n", "value:", data$value, "\n", "You can add a story here!")

# Generate the layout
packing <- circleProgressiveLayout(data$value, sizetype='area')
data <- cbind(data, packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)

# Make the plot with a few differences compared to the static version:
p <- ggplot() +
	geom_polygon_interactive(
		data = dat.gg,
		aes(x, y, group = id, fill=id, tooltip = data$text[id], data_id = id),
		colour = "black", alpha = 0.6) +
	scale_fill_viridis() +
	geom_text(data = data, aes(x, y, label = gsub("Group_", "", group)), size=2, color="black") +
	theme_void() +
	theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm") ) +
	coord_equal()

# Turn it interactive
widg <- ggiraph(ggobj = p, width_svg = 7, height_svg = 7)
# widg

# save the widget
# library(htmlwidgets)
# saveWidget(widg, file=paste0( getwd(), "/HtmlWidget/circular_packing_interactive.html"))
