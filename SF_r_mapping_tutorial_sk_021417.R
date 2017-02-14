# R MAPPING TUTORIAL ------------------------------------------------------
# Simon Kassel & Ken Steif

# GLOBAL OPTIONS ----------------------------------------------------------

options(scipen = "999")
options(stringsAsFactors = FALSE)

# PACKAGES ----------------------------------------------------------------
for (p in c("ggplot2", "ggmap", "RColorBrewer", "maptools",
            "ggthemes", "scales", "broom", "rgeos", "dplyr",
            "plyr", "lubridate", "grid", "gridExtra",
            "reshape2")) {
  if (!requireNamespace(p, quietly = TRUE))
    install.packages(p)
  suppressMessages(library(p, character.only = TRUE))
} 

# DATA --------------------------------------------------------------------
# Home sales
sf <- read.csv("https://raw.githubusercontent.com/simonkassel/Visualizing_SF_home_prices_R/master/Data/SF_home_sales_demo_data.csv")
# Neighborhood polygons
neighb <- readShapePoly("SF_neighborhoods")

# Remove outliers
sf <- sf[which(sf$SalePrice < mean(sf$SalePrice) + (2.5 * sd(sf$SalePrice))), ]
sf$SaleYr <- as.factor(sf$SaleYr)

# DEFINE THEMES -----------------------------------------------------------
plotTheme <- function(base_size = 12, base_family = "Courier") {
  theme(
    text = element_text(family = "Courier", color = "black"),
    plot.title = element_text(hjust = 0.5, size = 18, vjust = 3, 
                              colour = "black"),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(face = "italic"),
    axis.text = element_text(face = "italic"),
    axis.title = element_text(face = "italic"),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic"))
}

mapTheme <- function(base_size = 12, base_family = "Courier") {
  plotTheme() %+replace% 
    theme(
      axis.text = element_blank(),
      axis.title = element_blank())
}
# BASEMAP -----------------------------------------------------------------
bbox <- neighb@bbox

sf_bbox <- c(left = bbox[1, 1] - .01, bottom = bbox[2, 1] - .005, 
             right = bbox[1, 2] + .01, top = bbox[2, 2] + .005)

basemap <- get_stamenmap(
  bbox = sf_bbox,
  zoom = 13,
  maptype = "toner-lite")

bmMap <- ggmap(basemap) + mapTheme() + ggtitle("SF Basemap")
# MAPPING POINTS ----------------------------------------------------------
prices_mapped_by_year <- ggmap(basemap) + 
  geom_point(data = sf, aes(x = long, y = lat, color = SalePrice), 
             size = 0.6, alpha = 0.75) +
  facet_wrap(~SaleYr, scales = "fixed", ncol = 4) +
  coord_map() +
  mapTheme() + theme(legend.position = c(.85, .25)) +
  scale_color_gradientn("Sale Price", 
                        colors = c("#73A475", "#FFC861","#C84663"),
                        labels = scales::dollar_format(prefix = "$")) +
  ggtitle("San Francisco home sale prices (2009 - 2015)")

prices_mapped_by_year

# PLOTTING POLYGONS --------------------------------------------------------
# Convert neighborhood sp object to a format ggplot can handle
neighb.fort <- tidy(neighb, region = c('nbrhood'))
neighb.fort$nbrhood <- neighb.fort$id
neighb.fort$Neighborhood <- neighb.fort$id

# Summarize the data by neighborhood and sale year
sf.summarized <- ddply(sf, c("Neighborhood", "SaleYr"), summarise, 
                       medianPrice = median(SalePrice),
                       saleCount = length(SaleYr),
                       sdPrice = sd(SalePrice),
                       minusSd = medianPrice - sdPrice,
                       plusSD = medianPrice + sdPrice,
                       .progress = "text")

# Take anpther step to calculate the average yearly sales by neighborhood
yearly_sales <- ddply(sf.summarized, ~Neighborhood, summarise, avg.yearly.sales = mean(saleCount))
sf.summarized <- left_join(sf.summarized, yearly_sales)

# Find the percent change from 2009 - 2015
medByYear <- dcast(sf.summarized, Neighborhood ~ SaleYr, value.var = "medianPrice")
medByYear$pctChange <- (medByYear$`2015` - medByYear$`2009`) / medByYear$`2009`

# Combine into one neighborhood dataset
sf.summarized <- join(sf.summarized, medByYear[,c("Neighborhood", "pctChange")], by = "Neighborhood")
sf.summarized$pctChange <- ifelse(sf.summarized$avg.yearly.sales < 10, NA, sf.summarized$pctChange)
# for ggplot polygon mapping
sf.summarized_tidy <- join(sf.summarized, neighb.fort, by = "Neighborhood", match = "all")

# Plot neighborhood median home value over time
neighb_map <- ggmap(basemap) +
  geom_polygon(data = sf.summarized_tidy, aes(x = long, y = lat, group = group, fill = medianPrice), 
               colour = "white", alpha = 0.75, size = 0.25) + 
  scale_fill_gradientn("Neighborhood \nMedian \nSale Price", 
                       colors = c("#73A475", "#FFC861","#C84663"),
                       labels = scales::dollar_format(prefix = "$")) +
  mapTheme() + theme(legend.position = c(.85, .25)) + coord_map() +
  ggtitle("Home price by neighborhood") + facet_wrap(~SaleYr, nrow = 2)
neighb_map

# Plot the percent change in neighborhood median home value over time
change_map <- ggmap(basemap) +
  geom_polygon(data = sf.summarized_tidy[which(sf.summarized_tidy$SaleYr == 2015), ], 
               aes(x = long, y = lat, group = group, fill = pctChange), 
               colour = "white", alpha = 0.75, size = 0.25) + 
  coord_map() +
  scale_fill_gradientn("% Change", colors = c("#73A475", "#FFC861","#C84663"),
                       labels = scales::percent_format()) +
  mapTheme() + 
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.key.width = unit(1, "in")) +
  ggtitle("Neighborhood Median Home \nPrice Change (2009-2015)") 
change_map


# SAMPLE NEIGHBORHOOD TIME SERIES -----------------------------------------
# Select the neighborhoods with the highest rates of change
topPctChange <- unique(sf.summarized$pctChange) %>% sort(decreasing = TRUE) %>% head(9)
sf.changed <- sf.summarized[which(sf.summarized$pctChange %in% topPctChange), ] 

# Create a data frame to use for time series plot
sfForTimeSeries <- sf.summarized[which(sf.summarized$Neighborhood %in% unique(sf.changed$Neighborhood)), ] 
sfForTimeSeries <- join(sfForTimeSeries, sf.changed[c("Neighborhood", "medianPrice", "minusSd", "plusSD")], 
                        by = "Neighborhood", type = "left", match = "first")

# Plot time series
time.series <- ggplot(sfForTimeSeries, aes(x = SaleYr)) +
  geom_line(aes(y = medianPrice)) +
  geom_ribbon(aes(ymin = minusSd, ymax = plusSD, fill = Neighborhood), alpha = 0.75) +
  facet_wrap(~Neighborhood, scales = "fixed", nrow = 3) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$")) +
  ylab("District median home price") + xlab(NULL) +
  plotTheme() +
  theme(
    legend.position = "none",
    legend.title.align = 0.3,
    panel.spacing.y = unit(1, "lines"),
    strip.background = element_rect(fill = "grey95"),
    plot.margin=unit(c(0.5,.5,.75,.5),"cm")) +
  scale_fill_brewer("SF Realtor District", type = "qual", palette = "Set3") +
  ggtitle("Neighborhood Home price \nDistribution (2009 - 2015)")

# Plot the neighborhoods
sampleNeighborhoods <- ggmap(basemap) + 
  geom_polygon(data = neighb.fort[which(neighb.fort$Neighborhood %in% sfForTimeSeries$Neighborhood), ], 
               aes(x = long, y = lat, group = group, fill = Neighborhood), 
               colour = "white") +
  coord_map() +
  scale_fill_brewer("SF Neighborhood", type = "qual", palette = "Set3") +
  scale_color_brewer("SF Neighborhood", type = "qual", palette = "Set3") +
  mapTheme() + 
  theme(legend.position = "right", legend.direction = "vertical", legend.key.size = unit(0.15, "in")) +
  guides(fill=guide_legend(ncol = 1))
sampleNeighborhoods


# Create grob list
gs <- list(time.series, sampleNeighborhoods)
# Create layout matrix
lm <- rbind(c(1,1,1),
            c(1,1,1),
            c(1,1,1),
            c(NA,2,2))
# Arrange plots
grid.arrange(grobs = gs, layout_matrix = lm)



