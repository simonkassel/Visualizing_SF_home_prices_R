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
# The ggplot2 theme system allows you to customize most visual elements of
# a plot. As you can see, we will want to modify many theme elements for
# each plot. Rather than repeat this code for every plot or map we will 
# define functions that specify theme templates for a plot.

# Define one that we will use for plots
plotTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 18,colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
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

# And another that we will use for maps
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

# We will also use a specific palette including the colors listed below. 
# As you will find out, we will need palettes of different lengths for
# different purposes. We can define them below and call later in the script.
palette_9_colors <- c("#0DA3A0","#2999A9","#458FB2","#6285BB","#7E7CC4","#9A72CD","#B768D6","#D35EDF","#F055E9")
palette_8_colors <- c("#0DA3A0","#2D97AA","#4D8CB4","#6E81BF","#8E76C9","#AF6BD4","#CF60DE","#F055E9")
palette_7_colors <- c("#2D97AA","#4D8CB4","#6E81BF","#8E76C9","#AF6BD4","#CF60DE","#F055E9")
palette_1_colors <- c("#0DA3A0")

# DATA --------------------------------------------------------------------
# Read in a csv of home sale transactions directly from github.
sf <- read.csv("https://raw.githubusercontent.com/simonkassel/Visualizing_SF_home_prices_R/master/Data/SF_home_sales_demo_data.csv")

# We will need to consider Sale Year as a categorical variable so we convert 
# it from a numeric variable to a factor
sf$SaleYr <- as.factor(sf$SaleYr)

# The other dataset we will need is a shapefile of polygons representing 
# realtor-defined nighborhoods in San Francisco. This dataset comes from the 
# San Francisco Association of realtors via SF OpenData. To make things simpler,
# I have uploaded the shapefile to github as well as a zipped file. 
#   define the URL of the zipped shapefile
URL <- "https://github.com/simonkassel/Visualizing_SF_home_prices_R/raw/master/Data/SF_neighborhoods.zip"
#   download the shapefile to your working directory
download.file(URL, "SF_neighborhoods.zip")
#   unzip it
unzip("SF_neighborhoods.zip")
#   and read it into R as a spatial polygons data frame, we will use this 
#   later on
neighb <- readShapePoly("SF_neighborhoods")

# HOME SALE PRICE DISTIRBUTION --------------------------------------------
# Let's look at the distribution of home prices in our whole dataset by 
# creating the plot:
home_value_hist <- ggplot(sf, aes(SalePrice)) + 
  geom_histogram(fill=palette_1_colors) +
  xlab("Sale Price($)") + ylab("Count") +
  scale_fill_manual(values=palette_1_colors) +
  plotTheme() + 
  labs(x="Sale Price($)", y="Count", title="Distribution of San Francisco home prices",
       subtitle="Nominal prices (2009 - 2015)", 
       caption="Source: San Francisco Office of the Assessor-Recorder\n@KenSteif & @SimonKassel") +
  scale_x_continuous(labels = comma) + scale_y_continuous(labels = comma)
# Plotting it:
home_value_hist
# And saving it to the working directory:
ggsave("plot1_histogram.png", home_value_hist, width = 8, height = 4, device = "png")

# It seems like there are some outliers so lets remove anything greater than 
# 2.5 st. deviations from the mean
sf <- sf[which(sf$SalePrice < mean(sf$SalePrice) + (2.5 * sd(sf$SalePrice))), ]

# Now we will look at the overall distribution of home prices by year using 
# violin plots (create plot, plot it and save it)
home_value_violin <- ggplot(sf, aes(x=SaleYr, y=SalePrice, fill=SaleYr)) + 
  geom_violin(color = "grey50") +
  xlab("Sale Price($)") + ylab("Count") +
  scale_fill_manual(values=palette_7_colors) +
  stat_summary(fun.y=mean, geom="point", size=2, colour="white") +
  plotTheme() + theme(legend.position="none") +
  scale_y_continuous(labels = comma) +
  labs(x="Year",y="Sale Price($)",title="Distribution of San Francisco home prices",
       subtitle="Nominal prices (2009 - 2015); Sale price means visualized as points",
       caption="Source: San Francisco Office of the Assessor-Recorder\n@KenSteif & @SimonKassel")
home_value_violin
ggsave("plot2_violin.png", home_value_violin, width = 8, height = 4, device = "png")

# BASEMAPS ----------------------------------------------------------------
# Now we will move into mapping but before we do that we will need to 
# download some basemaps that fit the geographic boundaries of the data. 
# We can download basemaps from Stamen using the package ggmap.

# The first step is to define the boundaries of the map in latitude and lonigtude
#   One of the slots in the neighborhood spatial polygons dataframe that
#   we created earlier includes a matrix that defines the bounding box for 
#   this shapefile. Extract it:
bbox <- neighb@bbox

# Then we will manipulate these values slightly so that we get some padding 
# on our basemap between the edge of the data and the edge of the map
sf_bbox <- c(left = bbox[1, 1] - .01, bottom = bbox[2, 1] - .005, 
             right = bbox[1, 2] + .01, top = bbox[2, 2] + .005)
# Download the basemap
basemap <- get_stamenmap(
  bbox = sf_bbox,
  zoom = 13,
  maptype = "toner-lite")

# Map it
bmMap <- ggmap(basemap) + mapTheme() + 
  labs(title="San Francisco basemap")
bmMap 
ggsave("plot3_basemap.png", bmMap, width = 6, height = 6, device = "png")

# MAPPING POINTS ----------------------------------------------------------
# Let's map the sale prices per year
prices_mapped_by_year <- ggmap(basemap) + 
  geom_point(data = sf, aes(x = long, y = lat, color = SalePrice), 
             size = .25, alpha = 0.6) +
  facet_wrap(~SaleYr, scales = "fixed", ncol = 4) +
  coord_map() +
  mapTheme() + theme(legend.position = c(.85, .25)) +
  scale_color_gradientn("Sale Price", 
                        colors = palette_8_colors,
                        labels = scales::dollar_format(prefix = "$")) +
  labs(title="Distribution of San Francisco home prices",
       subtitle="Nominal prices (2009 - 2015)",
       caption="Source: San Francisco Office of the Assessor-Recorder\n@KenSteif & @SimonKassel")
prices_mapped_by_year
ggsave("plot4_point map.png", prices_mapped_by_year, width = 8, height = 4, device = "png")

# Thats a lot of information. Let's subset to get just two years. We'll 
# stack them this time and increase the point size.
prices_mapped_2009_2015 <- ggmap(basemap) + 
  geom_point(data = subset(sf, sf$SaleYr == 2015 | sf$SaleYr == 2009), 
             aes(x = long, y = lat, color = SalePrice), 
             size = 1, alpha = 0.75) +
  facet_wrap(~SaleYr, scales = "fixed", ncol = 1) +
  coord_map() +
  mapTheme() +
  scale_color_gradientn("Sale Price", 
                        colors = palette_8_colors,
                        labels = scales::dollar_format(prefix = "$")) +
  labs(title="Distribution of San Francisco home prices",
       subtitle="Nominal prices (2009 & 2015)",
       caption="Source: San Francisco Office of the Assessor-Recorder\n@KenSteif & @SimonKassel")
prices_mapped_2009_2015
ggsave("plot5_point map selected years.png", prices_mapped_2009_2015, width = 7, 
       height = 5, device = "png")

# Let's look at just one neighborhood: The Mission District
#   Data set of just sales for the "Inner Mission neighborhood"
missionSales <- sf[which(sf$Neighborhood == "Inner Mission"), ]

#   We'll need a new basemap at the appropriate scale
centroid_lon <- median(missionSales$long)
centroid_lat <- median(missionSales$lat)
missionBasemap <- get_map(location = c(lon = centroid_lon, lat = centroid_lat), 
                          source = "stamen",maptype = "toner-lite", zoom = 15)

#   Map these by year
mission_mapped_by_year <- ggmap(missionBasemap) + 
  geom_point(data = missionSales, aes(x = long, y = lat, color = SalePrice), 
             size = 2) +
  facet_wrap(~SaleYr, scales = "fixed", ncol = 4) +
  coord_map() +
  mapTheme() + theme(legend.position = c(.85, .25)) +
  scale_color_gradientn("Sale Price", 
                        colors = palette_8_colors,
                        labels = scales::dollar_format(prefix = "$")) +
  labs(title="Distribution of Mission District home prices",
       subtitle="Nominal prices (2009 - 2015)",
       caption="Source: San Francisco Office of the Assessor-Recorder\n@KenSteif & @SimonKassel")
mission_mapped_by_year
ggsave("plot6_point map Mission District.png", mission_mapped_by_year, width = 8, 
       height = 4, device = "png")

# DATA WRANGLING ----------------------------------------------------------
# For the next step we will move on from the relatively simple plotting points
# to the slightly more complicated plotting of polygons. In order to do this,
# we will do some data wrangling.

# We are going to transform our data from the property to the neighborhood
# level. For simplicity's sake, we already joined the sales points to our
# neighborhood shapefile so each point has an attribute indicataing which 
# neighborhood it is in. Since we still want to look at the time-space trend
# of home sales by neighborhood we want to summarize the data not just by
# neighborhood but by year. Using the ddply command we can create a dataframe
# in which each row corresponds to a specific year of a neighborhood and each 
# column includes a different summary statistic
sf.summarized <- ddply(sf, c("Neighborhood", "SaleYr"), summarise, 
                       medianPrice = median(SalePrice),
                       saleCount = length(SaleYr),
                       sdPrice = sd(SalePrice),
                       minusSd = medianPrice - sdPrice,
                       plusSD = medianPrice + sdPrice,
                       .progress = "text")
# Take a look at the resulting dataset
head(sf.summarized, 10)

# We will use another ddply command to calculate the neighborhood's
# average annual home sale count, which will be one value for each neighborhood
yearly_sales <- ddply(sf.summarized, ~Neighborhood, summarise, 
                      avg.yearly.sales = mean(saleCount))

# Then we will use a left join to join the average sale count data frame
# to the original summarized data frame. We will be joining one valur to many
# so each of the 7 rows for any neighborhood will have the same value
sf.summarized <- left_join(sf.summarized, yearly_sales, by = "Neighborhood")

# Next up we will calculate the % change in neighborhood median home value 
# from 2009 to 2015. In order to do this we will need to reshape the data 
# again. This time we will us the dcast function in the package reshape2. 
medByYear <- dcast(sf.summarized, Neighborhood ~ SaleYr, value.var = "medianPrice")
# Check out the reshaped data frame
head(medByYear)

# We now have a data frame in which each row is a neighborhood, each column
# is a year and the values are the corresponding median prices. Now we can easily
# calculate the % change from 2009 to 2015.
medByYear$pctChange <- (medByYear$`2015` - medByYear$`2009`) / medByYear$`2009`
# And join it back to our sf.summarized dataset
sf.summarized <- left_join(sf.summarized, medByYear[,c("Neighborhood", "pctChange")], 
                           by = "Neighborhood")

# You may have noticed that some neighborhoods have very low numbers of sales
# every year. These are likely neighborhoods that are primarily commercial or
# industrial. The low rates of commercial activity will not make the rates of 
# home price change comparable to more robust markets so we will remove these 
# values from the dataset by convertnig them to NA.
sf.summarized$pctChange <- ifelse(sf.summarized$avg.yearly.sales < 10, NA, 
                                  sf.summarized$pctChange)

# Remember the neighborhood shapefile we imported at the beginning? In order to
# plot it we must convert it into a format that ggplot can handle. We do 
# this using the tidy function which converts the polygon into a data frame with
# one row for each vertex of each polygon and contains a field grouping the 
# vertices. The region parameter tells the function how to group the vertices.
neighb.tidy <- tidy(neighb, region = c('nbrhood'))
# Look at the resulting data frame to see how it has been transformed
head(neighb.tidy)

# Create an identical field to 'id' but with a name that will allow us to 
# join the data frame to our summarized price data
neighb.tidy$Neighborhood <- neighb.tidy$id

# Now we're going to join these data frames together so that when we map
# the neighborhood polygons we will be able to symbolize them using the summary
# stats we created
sf.summarized_tidy <- join(sf.summarized, neighb.tidy, by = "Neighborhood", match = "all")


# PLOTTING POLYGONS -------------------------------------------------------
# Plot neighborhood median home value over time
neighb_map <- ggmap(basemap) +
  geom_polygon(data = sf.summarized_tidy, 
               aes(x = long, y = lat, group = group, fill = medianPrice), 
               colour = "white", alpha = 0.75, size = 0.25) + 
  scale_fill_gradientn("Neighborhood \nMedian \nSale Price", 
                       colors = palette_8_colors,
                       labels = scales::dollar_format(prefix = "$")) +
  mapTheme() + theme(legend.position = c(.85, .25)) + coord_map() +
  facet_wrap(~SaleYr, nrow = 2) +
  labs(title="Median home price by neighborhood, San Francisco ",
       subtitle="Nominal prices (2009 - 2015)",
       caption="Source: San Francisco Office of the Assessor-Recorder\n@KenSteif & @SimonKassel")
neighb_map
ggsave("plot7_neighborhood home value.png", neighb_map, width = 8, height = 6, device = "png")

# Plot the percent change in neighborhood median home value over time
change_map <- ggmap(basemap) +
  geom_polygon(data = sf.summarized_tidy[which(sf.summarized_tidy$SaleYr == 2015), ], 
               aes(x = long, y = lat, group = group, fill = pctChange), 
               colour = "white", alpha = 0.75, size = 0.25) + 
  coord_map() +
  scale_fill_gradientn("% Change", colors = palette_8_colors,
                       labels = scales::percent_format()) +
  mapTheme() + 
  theme(legend.position = "bottom", 
        legend.direction = "horizontal", 
        legend.key.width = unit(.5, "in")) +
  labs(title="Percent change in median home prices, San Francisco",
       subtitle="Nominal prices (2009 - 2015)",
       caption="Source: San Francisco Office of the Assessor-Recorder\nNeighborhoods with too few annual transactions are withheld\n@KenSteif & @SimonKassel")
change_map
ggsave("plot8_change over time.png", change_map, width = 7, height = 7, device = "png")

# SAMPLE NEIGHBORHOOD TIME SERIES -----------------------------------------
# The next plots we create will be small multiple time series plots showing
# the trends in home prices over time for a specific neighborhood. However, 
# at the scale we're working at, there are too many neighborhoods to plot 
# a time series for each one. Instead, we will take a sample of the eight 
# neighborhoods with the highest rates of change between 2009 and 2015.
# Extracting this dataset will take a few more lines of data wrangling code

# We will create a list of the 8 highest percentages by chaining together
# three functions with forward piping (%>%). The line of code below finds
# all unique percentage change values, sorts them decreasing and then takes
# the first 8 from the list
topPctChange <- unique(sf.summarized$pctChange) %>% sort(decreasing = TRUE) %>% head(8)
# Check out what those are:
topPctChange

# Now we want to use this vector of percentages to subset our sf.summarized
# data frame to only include the neighborhoods we're interested in. We can
# use piping again with the %in% operator to find group membership. In this 
# case, it will only include row's which have a pctChange value that is in 
# the top 8 percentages. We'll use this dataset later on.
sfForTimeSeries <- sf.summarized[which(sf.summarized$pctChange %in% topPctChange), ] 

# Now we will create a scatterplot of the initial median home prices (2009)
# for each neighborhood against that neighborhood's ultimate percent change
# over the study period. 

# First create a data frame of just  the year 2009, removing neighborhoods 
# that we gave an NA value for pctChange due to on insufficient sale volume
sf.2009 <- sf.summarized[which(sf.summarized$SaleYr == 2009), ] %>% na.omit()

# Then create the scatterplot using neighborhood name labels instead of points
change_scatterplot <- ggplot(sf.2009, aes(x = pctChange, y = medianPrice, label = Neighborhood)) + 
  geom_label(data = sf.2009[which(!sf.2009$pctChange %in% topPctChange),], 
             aes(label = Neighborhood), fill = "grey20", size = 2, color = "white") +
  geom_label(data = sf.2009[which(sf.2009$pctChange %in% topPctChange),], 
             aes(label = Neighborhood, fill = Neighborhood), size = 2, color = "white") +
  scale_fill_manual(values=palette_9_colors) +
  geom_smooth(method = "lm", se = FALSE) +
  plotTheme() + theme(legend.position = "none") +
  scale_y_continuous(labels = dollar_format(prefix = "$")) + 
  scale_x_continuous(labels = percent, limits = c(min(sf.2009$pctChange - .04), max(sf.2009$pctChange + .025)) ) +
  labs(x="% Change", y="Median Sale Price (2009)",
       title="Change in home price as a function of initial price",
       subtitle="Median price; Change between 2009 - 2015",
       caption="Source: San Francisco Office of the Assessor-Recorder\n@KenSteif & @SimonKassel")
change_scatterplot
ggsave("plot9_scatterplot.png", change_scatterplot, width = 8, height = 6, device = "png")

# Use the sfForTimeSeries data frame to create the time series plots for our 
# 8 sample neighborhoods
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
  scale_fill_manual(values=palette_8_colors) +
  labs(title="Time series for highest growth neighborhoods, San Francisco",
       subtitle="Nominal prices (2009-2015); Median; Ribbon indicates 1 standard deviation",
       caption="Source: San Francisco Office of the Assessor-Recorder\n@KenSteif & @SimonKassel")
time.series 
ggsave("plot10_time series.png", time.series, width = 6, height = 8, device = "png")

# Create a locator map for these neighborhoods
sampleNeighborhoods <- ggmap(basemap) + 
  geom_polygon(data = neighb.tidy, aes(x = long, y = lat, group = group), 
               colour = NA, fill="black", alpha = 1) +
  geom_polygon(data = neighb.tidy[which(neighb.tidy$Neighborhood %in% sfForTimeSeries$Neighborhood), ], 
               aes(x = long, y = lat, group = group, fill = Neighborhood), 
               colour = "black") +
  coord_map() +
  scale_fill_manual(values=palette_9_colors)+
  mapTheme() + 
  theme(legend.position = "bottom", 
        legend.direction = "horizontal", 
        legend.key.size = unit(0.15, "in"),
        legend.title=element_blank()) +
  guides(fill=guide_legend(ncol = 2))
sampleNeighborhoods
ggsave("plot11_sample neighborhoods.png", sampleNeighborhoods, width = 6, height = 6, device = "png")

# ARRANGE MULTIPLE PLOTS --------------------------------------------------
# Our final step will be too arrange the previous two plots using the 'grid'
# and 'gridExtra' packages. 

# First create a blank grob (or grid object). This will act as the canvas that 
# we lay the plots out on. 
blank <- grid.rect(gp=gpar(col="white"))

# Create a an ordered list of the plots we want to use, including 'blank' 
# which we will place in the plot to create white space.
gs <- list(time.series, blank, sampleNeighborhoods)
# Then create a matrix that will dictate the layout of the plots. Each number
# in the matrix refers to a position in the list of plots.
lay <- rbind(c(1,1,2),
             c(1,1,2),
             c(1,1,3),
             c(1,1,3),
             c(1,1,2),
             c(1,1,2))

# Arrange the plot
arranged_plot <- grid.arrange(grobs = gs, layout_matrix = lay)
ggsave("plot12_time series with map.png", arranged_plot, width = 11.6, height = 10, device = "png")
