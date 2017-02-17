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

# DEFINE THEMES -----------------------------------------------------------
plotTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 18,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=12),
    axis.title = element_text(size=8),
    axis.text = element_text(size=8),
    axis.title.x = element_text(hjust=1),
    axis.title.y = element_text(hjust=1),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic"))
}

mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 18,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    strip.text = element_text(size=12),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey80", color = "white"),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic"))
}

#were going to use these custom palettes
pallete_9_colors <- c("#0DA3A0","#2999A9","#458FB2","#6285BB","#7E7CC4","#9A72CD","#B768D6","#D35EDF","#F055E9")
pallete_8_colors <- c("#0DA3A0","#2D97AA","#4D8CB4","#6E81BF","#8E76C9","#AF6BD4","#CF60DE","#F055E9")
pallete_7_colors <- c("#2D97AA","#4D8CB4","#6E81BF","#8E76C9","#AF6BD4","#CF60DE","#F055E9")
pallete_1_colors <- c("#0DA3A0")

# DATA --------------------------------------------------------------------
# Home sales
sf <- read.csv("https://raw.githubusercontent.com/simonkassel/Visualizing_SF_home_prices_R/master/Data/SF_home_sales_demo_data.csv")

#convert sale year to a factor
sf$SaleYr <- as.factor(sf$SaleYr)

# Download polygon shapefile from github
URL <- "https://github.com/simonkassel/Visualizing_SF_home_prices_R/raw/master/Data/SF_neighborhoods.zip"
download.file(URL, "SF_neighborhoods.zip")
unzip("SF_neighborhoods.zip")
neighb <- readShapePoly("SF_neighborhoods")

#Let's look at the distribution of home values
home_value_hist <- ggplot(sf, aes(SalePrice)) + 
  geom_histogram(fill=pallete_1_colors) +
  xlab("Sale Price($)") + ylab("Count") +
  scale_fill_manual(values=pallete_1_colors) +
  plotTheme() + 
  labs(x="Sale Price($)", y="Count", title="Distribution of San Francisco home prices",
     subtitle="Nominal prices (2009 - 2015)", 
     caption="Source: San Francisco Office of the Assessor-Recorder\n@KenSteif & @SimonKassel") +
  scale_x_continuous(labels = comma) + scale_y_continuous(labels = comma)
home_value_hist
ggsave("plot1_histogram.png", home_value_hist, width = 8, height = 4, device = "png")

# Seems like there are some outliers so lets remove anything greater than 2.5 st. deviations from the mean
sf <- sf[which(sf$SalePrice < mean(sf$SalePrice) + (2.5 * sd(sf$SalePrice))), ]
  
#violin plots
home_value_violin <- ggplot(sf, aes(x=SaleYr, y=SalePrice, fill=SaleYr)) + geom_violin(color = "grey50") +
  xlab("Sale Price($)") + ylab("Count") +
  scale_fill_manual(values=pallete_7_colors) +
  stat_summary(fun.y=mean, geom="point", size=2, colour="white") +
  plotTheme() + theme(legend.position="none") +
  scale_y_continuous(labels = comma) +
  labs(x="Year",y="Sale Price($)",title="Distribution of San Francisco home prices",
       subtitle="Nominal prices (2009 - 2015); Sale price means visualized as points",
       caption="Source: San Francisco Office of the Assessor-Recorder\n@KenSteif & @SimonKassel")
home_value_violin
ggsave("plot2_violin.png", home_value_violin, width = 8, height = 4, device = "png")


# BASEMAP -----------------------------------------------------------------

# Create a bouding box
bbox <- neighb@bbox

# add a margin between the extent of our data the edge of the basemap
sf_bbox <- c(left = bbox[1, 1] - .01, bottom = bbox[2, 1] - .005, 
             right = bbox[1, 2] + .01, top = bbox[2, 2] + .005)

basemap <- get_stamenmap(
  bbox = sf_bbox,
  zoom = 13,
  maptype = "toner-lite")

bmMap <- ggmap(basemap) + mapTheme() + 
  labs(title="San Francisco basemap"
       )
bmMap
ggsave("plot3_basemap.png", bmMap, width = 6, height = 6, device = "png")


# MAPPING POINTS ----------------------------------------------------------
#let's map the sale prices per year
prices_mapped_by_year <- ggmap(basemap) + 
  geom_point(data = sf, aes(x = long, y = lat, color = SalePrice), 
             size = .25, alpha = 0.6) +
  facet_wrap(~SaleYr, scales = "fixed", ncol = 4) +
  coord_map() +
  mapTheme() + theme(legend.position = c(.85, .25)) +
  scale_color_gradientn("Sale Price", 
                        colors = pallete_8_colors,
                        labels = scales::dollar_format(prefix = "$")) +
  labs(title="Distribution of San Francisco home prices",
       subtitle="Nominal prices (2009 - 2015)",
       caption="Source: San Francisco Office of the Assessor-Recorder\n@KenSteif & @SimonKassel")
prices_mapped_by_year
ggsave("plot4_point map.png", prices_mapped_by_year, width = 8, height = 4, device = "png")


#Thats a lot of information. Let's subset to get just two years. We'll stack them this time and increase the point size
prices_mapped_2009_2015 <- ggmap(basemap) + 
  geom_point(data = subset(sf, sf$SaleYr == 2015 | sf$SaleYr == 2009), aes(x = long, y = lat, color = SalePrice), 
             size = 1, alpha = 0.75) +
  facet_wrap(~SaleYr, scales = "fixed", ncol = 1) +
  coord_map() +
  mapTheme() +
  scale_color_gradientn("Sale Price", 
                        colors = pallete_8_colors,
                        labels = scales::dollar_format(prefix = "$")) +
  labs(title="Distribution of San Francisco home prices",
       subtitle="Nominal prices (2009 & 2015)",
       caption="Source: San Francisco Office of the Assessor-Recorder\n@KenSteif & @SimonKassel")
prices_mapped_2009_2015
ggsave("plot5_point map selected years.png", prices_mapped_2009_2015, width = 7, height = 5, device = "png")


# PLOTTING POLYGONS --------------------------------------------------------

# Convert neighborhood sp object to a format ggplot can handle
neighb.fort <- tidy(neighb, region = c('nbrhood'))
neighb.fort$nbrhood <- neighb.fort$id
neighb.fort$Neighborhood <- neighb.fort$id

# Summarize the data by neighborhood and sale year
#----what are you calculating and why?
sf.summarized <- ddply(sf, c("Neighborhood", "SaleYr"), summarise, 
                       medianPrice = median(SalePrice),
                       saleCount = length(SaleYr),
                       sdPrice = sd(SalePrice),
                       minusSd = medianPrice - sdPrice,
                       plusSD = medianPrice + sdPrice,
                       .progress = "text")

# Take anpther step to calculate the average yearly sales by neighborhood
yearly_sales <- ddply(sf.summarized, ~Neighborhood, summarise, 
                      avg.yearly.sales = mean(saleCount))

#join them
sf.summarized <- left_join(sf.summarized, yearly_sales)

# Find the median percent change from 2009 - 2015
medByYear <- dcast(sf.summarized, Neighborhood ~ SaleYr, value.var = "medianPrice")
medByYear$pctChange <- (medByYear$`2015` - medByYear$`2009`) / medByYear$`2009`

# Combine into one neighborhood dataset
sf.summarized <- join(sf.summarized, medByYear[,c("Neighborhood", "pctChange")], by = "Neighborhood")

# Remove the percentage change values for neighborhoods with low rates of transactions
sf.summarized$pctChange <- ifelse(sf.summarized$avg.yearly.sales < 10, NA, sf.summarized$pctChange)

# for ggplot polygon mapping - why?#########
sf.summarized_tidy <- join(sf.summarized, neighb.fort, by = "Neighborhood", match = "all")

# Plot neighborhood median home value over time
neighb_map <- ggmap(basemap) +
  geom_polygon(data = sf.summarized_tidy, aes(x = long, y = lat, group = group, fill = medianPrice), 
               colour = "white", alpha = 0.75, size = 0.25) + 
  scale_fill_gradientn("Neighborhood \nMedian \nSale Price", 
                       colors = pallete_8_colors,
                       labels = scales::dollar_format(prefix = "$")) +
  mapTheme() + theme(legend.position = c(.85, .25)) + coord_map() +
  facet_wrap(~SaleYr, nrow = 2) +
  labs(title="Median home price by neighborhood, San Francisco ",
       subtitle="Nominal prices (2009 - 2015)",
       caption="Source: San Francisco Office of the Assessor-Recorder\n@KenSteif & @SimonKassel")
neighb_map
ggsave("plot6_neighborhood home value.png", neighb_map, width = 8, height = 6, device = "png")

# Plot the percent change in neighborhood median home value over time
change_map <- ggmap(basemap) +
  geom_polygon(data = sf.summarized_tidy[which(sf.summarized_tidy$SaleYr == 2015), ], 
               aes(x = long, y = lat, group = group, fill = pctChange), 
               colour = "white", alpha = 0.75, size = 0.25) + 
  coord_map() +
  scale_fill_gradientn("% Change", colors = pallete_8_colors,
                       labels = scales::percent_format()) +
  mapTheme() + 
  theme(legend.position = "bottom", 
        legend.direction = "horizontal", 
        legend.key.width = unit(.5, "in")) +
  labs(title="Percent change in median home prices, San Francisco",
       subtitle="Nominal prices (2009 - 2015)",
       caption="Source: San Francisco Office of the Assessor-Recorder\nNeighborhoods with too few annual transactions are withheld\n@KenSteif & @SimonKassel")
change_map
ggsave("plot7_change over time.png", change_map, width = 7, height = 7, device = "png")

# SAMPLE NEIGHBORHOOD TIME SERIES -----------------------------------------
# Select the neighborhoods with the highest rates of change
#can you explain the tidy happening here
topPctChange <- unique(sf.summarized$pctChange) %>% sort(decreasing = TRUE) %>% head(8)
sfForTimeSeries <- sf.summarized[which(sf.summarized$pctChange %in% topPctChange), ] 

# Scatterplot of home pct change vs. inital home price
# just 2009 sales
sf.2009 <- sf.summarized[which(sf.summarized$SaleYr == 2009), ] %>% na.omit()

# create scatterplot
change_scatterplot <- ggplot(sf.2009, aes(x = pctChange, y = medianPrice, label = Neighborhood)) + 
  geom_label(data = sf.2009[which(!sf.2009$pctChange %in% topPctChange),], 
             aes(label = Neighborhood), fill = "grey20", size = 2, color = "white") +
  geom_label(data = sf.2009[which(sf.2009$pctChange %in% topPctChange),], 
             aes(label = Neighborhood, fill = Neighborhood), size = 2, color = "white") +
  scale_fill_manual(values=pallete_9_colors) +
  geom_smooth(method = "lm", se = FALSE) +
  plotTheme() + theme(legend.position = "none") +
  scale_y_continuous(labels = dollar_format(prefix = "$")) + 
  scale_x_continuous(labels = percent) +
  labs(x="Change", y="Median Sale Price",
       title="Change in home price as a function of initial price",
       subtitle="Median price; Change between 2009 - 2015",
       caption="Source: San Francisco Office of the Assessor-Recorder\n@KenSteif & @SimonKassel")
change_scatterplot
ggsave("plot8_scatterplot.png", change_scatterplot, width = 8, height = 6, device = "png")

# Plot time series
time.series <- ggplot(sfForTimeSeries, aes(x = SaleYr, group=Neighborhood)) +
  geom_line(aes(y = medianPrice)) +
  geom_ribbon(aes(ymin = minusSd, ymax = plusSD, fill = Neighborhood), alpha = 0.75) +
  facet_wrap(~Neighborhood, scales = "fixed", nrow = 4) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$")) +
  ylab("Neighborhood median home price") + xlab(NULL) +
  plotTheme() +
  theme(
    legend.position = "none",
    panel.spacing.y = unit(1, "lines")
       ) +
  scale_fill_manual(values=pallete_8_colors) +
  labs(title="Time series for highest growth neighborhoods, San Francisco",
       subtitle="Nominal prices (2009-2015); Median; Ribbon indicates 1 standard deviation",
       caption="Source: San Francisco Office of the Assessor-Recorder\n@KenSteif & @SimonKassel")
time.series 
ggsave("plot9_time series.png", time.series, width = 6, height = 8, device = "png")


# Plot the neighborhoods
sampleNeighborhoods <- ggmap(basemap) + 
  geom_polygon(data = neighb.fort, aes(x = long, y = lat, group = group), 
               colour = NA, fill="black", alpha = 1) +
  geom_polygon(data = neighb.fort[which(neighb.fort$Neighborhood %in% sfForTimeSeries$Neighborhood), ], 
               aes(x = long, y = lat, group = group, fill = Neighborhood), 
               colour = "black") +
  coord_map() +
  scale_fill_manual(values=pallete_9_colors)+
  mapTheme() + 
  theme(legend.position = "bottom", 
        legend.direction = "horizontal", 
        legend.key.size = unit(0.15, "in"),
        legend.title=element_blank()) +
  guides(fill=guide_legend(ncol = 2))
sampleNeighborhoods
ggsave("plot10_sample neighborhoods.png", sampleNeighborhoods, width = 6, height = 6, device = "png")

#Create grob list
# we'll create a blank grob
blank <- grid.rect(gp=gpar(col="white"))
# create list of our plots
gs <- list(time.series,blank, sampleNeighborhoods)
# and lay them out.
lay <- rbind(c(1,1,2),
             c(1,1,2),
             c(1,1,3),
             c(1,1,3),
             c(1,1,2),
             c(1,1,2))
arranged_plot <- grid.arrange(grobs = gs, layout_matrix = lay)
arranged_plot
ggsave("plot11_time series with map.png", g, width = 11.6, height = 10, device = "png")
