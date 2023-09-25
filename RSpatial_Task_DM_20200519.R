#Author: Deidre Mensah

#get packages.
require(tidyverse); require(magrittr);
require(sp);require(sf);require(rgdal)
require(classInt);require(RColorBrewer)
require(ggplot2);require(ggmap);require(leaflet);require(tmap)
library(ggpubr); library(devtools);
require(htmlwidgets); require(mapview)
library(ggpubr); library(gridExtra)

###---------------
#1.	Read the NYS_restaurants.csv file and create a sf object from it. Set the correct CRS.

#reads in csv need to add coords because it's not a spatial format
restaurants <-read.csv("nys_restaurants.csv")

#coverts csv to sf object
restaurants_sf<- restaurants %>%
  st_as_sf(coords=c("X", "Y"))

#assign WGS84 projection - note: you can't transform for an sf object that's missing a coordinate system entirely
st_crs(restaurants_sf) <- 4326


#1B - Read in shapefile

#read in shapefule and then convert it to sf object 
nyc_restaurants_sf <- sf::st_read("NYC_restaurants.shp") %>% sf::st_transform(4326)

###---------------
#2.	Read the NYS county Shapefile into a sf object and plot the county boundaries with a basemap from ggmap.

#read in shapefile and then converts it to sf object.
county_sf<- sf::st_read("NYS_Counties.shp") %>% 
  sf::st_transform(4326) #sf class

#read in shapefile and then converts it to sf object.
nyc_county_sf<- sf::st_read("NYC_Counties.shp") %>% 
  sf::st_transform(4326) #sf class

#----------------create basemaps--------------
mapBound <- county_sf %>% sf::st_transform(4326) %>% 
  st_bbox() %>% st_as_sfc() %>% st_buffer(0.03) %>%
  st_bbox() %>% as.numeric()

mapBound1 <- nyc_county_sf %>% sf::st_transform(4326) %>% 
  st_bbox() %>% st_as_sfc() %>% st_buffer(0.03) %>%
  st_bbox() %>% as.numeric()

nysbasemap <- ggmap::get_stamenmap(bbox = mapBound, zoom = 8, maptype = 'terrain-background')
nycbasemap <- ggmap::get_stamenmap(bbox = mapBound1, zoom = 8, maptype = 'terrain-background')
#----------------create basemap---------------

#transforms coordinate system of counties shapefile

#creates country boundaries map
nys <- ggmap(nysbasemap) +
  geom_sf(data = county_sf, inherit.aes = FALSE)+
  labs(x="Longitude",y="Latitude", title="County Boundaries", caption = "Data Source: GTECH 785") +
  coord_sf(crs = st_crs(4326))
nys

#3.	Use spatial join to find out the total number of fast food restaurants in each NYS county. 
#Add these numbers as a new column in the NYS county sf object. 

#aggregates retail food by county
county_by_restaurants <- county_sf %>% 
  #spatial join
  st_join(restaurants_sf) %>%
  #column to be aggregated by county name
  group_by(NAME) %>% 
  #creats new column with summarized data
  summarize(Restaurants=n())

#binds the column of the restaurants column to the county sf object and renames column
merged <- cbind(county_sf, county_by_restaurants$Restaurants) %>% 
  rename(restaurants = county_by_restaurants.Restaurants)

nyc_merged <- dplyr::filter(county_by_restaurants, NAME == "Kings"| NAME =="Bronx" | NAME =="Richmond" | NAME == "New York" | NAME =="Queens")

#4.	Use ggplot to show the numbers of restaurants in the counties with seven discrete colors with darker 
#colors for higher values and lighter colors for lower values. 
# get quantile breaks. Add .00001 offset to catch the lowest value
breaks_qt <- classIntervals(c(min(merged$restaurants) - .00001, #we do this because the intervales will get the minimum value up to but not including the min value, sowe want to catch the lowest value
                              merged$restaurants), n = 7, style = "quantile")

#divide the range of the restaurants column into different values. in this case it will be divided by the breaks we defined above
merged_trans <- mutate(merged, restaurants = cut(restaurants, breaks_qt$brks)) 

#note to self = ggplot has syntax that adds parameters by plus sign, so you can add on top of each other. plot accepts arguments in parentheses. just look above for an example
#creates restaurant total map
restaurantsMap <- ggmap(nysbasemap) +
  geom_sf(data = merged_trans, aes(fill=restaurants), inherit.aes = FALSE) +
  scale_fill_brewer(palette ="PuBu", name="Restaurants in NYS") +
  labs(x="Longitude",y="Latitude", title="Total Restaurants in NYS", caption = "Data Source: GTECH 785") +
  coord_sf(crs = st_crs(4326))
restaurantsMap

#5.	Filter the state data down to NYC, i.e., the five counties of NYC and restaurants in them. 
restaurantsMap2 <- ggmap(nycbasemap) +
  geom_sf(data = merged_trans %>% dplyr::filter(NAME == "Kings"| NAME =="Bronx" | NAME =="Richmond" | NAME == "New York" | NAME =="Queens"), aes(fill=restaurants), inherit.aes = FALSE) +
  scale_fill_brewer(palette ="PuBu", name="Restaurants in NYC") +
  labs(x="Longitude",y="Latitude", title="Total Restaurants in NYC", caption = "Data Source: GTECH 785") +
  coord_sf(crs = st_crs(4326))
restaurantsMap2

#6.	Make a basic interactive web map with leaflet or tmap to show the number of restaurants in 
#each borough as well as each individual restaurant. The numbers and the name of individual restaurants
#should be shown either by moving mouse over them or clicking on them. 

#-----------CREATE WEB-BASED INTERACTIVE MAP FOR COVID-19 DATA WITH LEAFLET PACKAGE--------------

#colors and their total number of classes
pal_fun <- colorQuantile("PuBu", NULL, n = 7)

#specifies information  for popup
my_popup <- paste0("<strong> County: </strong>",
                   nyc_merged$NAME,
                   "<br/>",
                   "<strong>Total Restaurants: </strong>", 
                   nyc_merged$Restaurants%>%round(0)%>%format(nsmall = 0),
                   sep="")

polyHighlightOption <- leaflet::highlightOptions(opacity = 2.0, fillColor = "cyan")
polyLabelOption <-labelOptions()
  
htmlMap <- leaflet(dplyr::filter(nyc_merged, NAME == "Kings"| NAME =="Bronx" | NAME =="Richmond" | NAME == "New York" | NAME =="Queens")) %>% 
  #adds polygons to be used
  addPolygons(
    #omits border
    stroke = FALSE, 
    #specifies which field we want to symbolize by
    fillColor = ~pal_fun(Restaurants),
    #sets transparency
    fillOpacity = 0.7, smoothFactor = 0.5,
    #passes through the popup settings we specified above
    popup = my_popup,
    #specify highlight options
    highlightOptions = polyHighlightOption) %>%
  #adds tile basemap, which is from OSM by default
  addTiles() %>%
  #creates legend
  addLegend("topleft", 
            #creates legend with same classes and colors - we have to specify this (and make sure the number of classes matches how we displayed it
            #when we added the polygons with addPolygons (the colorQuantile)
            colors = brewer.pal(7, "PuBu"), 
            labels = paste0("up to ", format(breaks_qt$brks[-1], digits = 2)),
            title =  "Total Restaurants") %>%
  #adds circle markers for each business
  addCircleMarkers(
    data = nyc_restaurants_sf,
    radius = 4,
    color = NA,
    fillColor = "orange",
    group = "Fast Food",
    fillOpacity = 0.6,
    label = paste0(nyc_restaurants_sf$Name),
    labelOptions = polyLabelOption)  
htmlMap

# save html as single file
htmlwidgets::saveWidget(htmlMap, 'NYC_Restaurants.html')

