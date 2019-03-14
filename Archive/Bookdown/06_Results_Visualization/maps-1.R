if(!require("ggplot2")){install.packages("ggplot2")}
if(!require("ggmap")){install.packages("ggmap")}
if(!require("sp")){install.packages("sp")}
if(!require("mapproj")){install.packages("mapproj")}
if(!require("maps")){install.packages("maps")}
if(!require("osmdata")){install.packages("osmdata")}
if(!require("rgdal")){install.packages("rgdal")}

library("ggplot2")
library("ggmap")
library("sp")
library("maps")
library("mapproj")
library("osmdata")
library("rgdal")
library("dplyr")
library("readxl")

#Get Berlin Boundaries from OSM data
ber = getbb("berlin")

# Automatic bbox cut some parts of berlin out, that is why I manually adjust x 
# coordinate. Adjustments are defined manually.
ber_adj = ber
ber_adj["x","min"] = ber_adj["x","min"]-0.18
ber_adj["x","max"] = ber_adj["x","max"]+0.22
ber_adj["y","min"] = ber_adj["y","min"]-0.02

# Upload map-teils from Stamen Maps
map = get_stamenmap(ber_adj, maptype="toner-lite", zoom = 11)

# Download polygons with districts borders 
# source: https://data.technologiestiftung-berlin.de/dataset/bezirksgrenzen        # add space after #
test = readOGR("./06_Results_Visualization/bezirksgrenzen.kml")
Bezirk = fortify(test)
Bezirk$District = case_when(Bezirk$id == "0" ~ "Reinickendorf",                   # changes spaces between ==
                            Bezirk$id=="1" ~ "Charlottenburg-Wilmersdorf",
                            Bezirk$id=="10" ~ "Friedrichshain-Kreuzberg",
                            Bezirk$id=="11" ~ "Tempelhof-Schoneberg",
                            Bezirk$id=="2" ~ "Treptow-Kopenick",
                            Bezirk$id=="3" ~ "Pankow",
                            Bezirk$id=="5" ~ "Lichtenberg",
                            Bezirk$id=="4" ~ "Neukolln",
                            Bezirk$id=="6" ~ "Marzahn-Hellersdorf",
                            Bezirk$id=="7" ~ "Spandau",
                            Bezirk$id=="8" ~ "Steglitz-Zehlendorf",
                            Bezirk$id=="9" ~ "Mitte",
                            TRUE ~ "other")

#=================================FUNCTIONS=====================================
DistricToFullName = function (column){
  column[grepl("mit",  column, ignore.case = TRUE)] = "Mitte" 
  column[grepl("fri",  column, 
               ignore.case = TRUE)] = "Friedrichshain-Kreuzberg"
  column[grepl("pank", column, ignore.case = TRUE)] = "Pankow"
  column[grepl("mar",  column, ignore.case = TRUE)] = "Marzahn-Hellersdorf" 
  column[grepl("char", column,
               ignore.case = TRUE)] = "Charlottenburg-Wilmersdorf"
  column[grepl("spa",  column, ignore.case = TRUE)] = "Spandau" 
  column[grepl("ste",  column, ignore.case = TRUE)] = "Steglitz-Zehlendorf"  
  column[grepl("tem",  column, ignore.case = TRUE)] = "Tempelhof-Schoneberg" 
  column[grepl("trep", column, ignore.case = TRUE)] = "Treptow-Kopenick" 
  column[grepl("neu",  column, ignore.case = TRUE)] = "Neukolln" 
  column[grepl("lich", column, ignore.case = TRUE)] = "Lichtenberg" 
  column[grepl("rein", column, ignore.case = TRUE)] = "Reinickendorf"
  
  return(column)
}

#==========================READ INDEX AND RENT DATA=============================
# Read Livability Index
IndDt = read.csv2("./06_Results_Visualization/SPL_BerlinDst_Liv_Index.csv", 
               sep = ";", 
               dec = ",")

# Read rent data
RntDt = read_excel(paste0("./06_Results_Visualization/statistic_id259905",
                          "_mietpreise-in-berlin-2017-nach-bezirken.xlsx"),
                   sheet = "Daten",
                   skip = 5,
                   col_names = FALSE) %>%
  setNames(c("District", "Rent")) %>%
  filter(!District == "Berlin Durchschnitt") %>%
  
# Unify district names from retn data with standart version used before  
RntDt$District = DistricToFullName(RntDt$District)

# Merge Index and Rent Data with District borders
IndRntDt = Bezirk %>% 
    merge(IndDt, by = "District") %>%
    merge(RntDt, by = "District")

#centroids
LablesDt = aggregate(cbind(long, lat) ~ District, 
                     data=Bezirk, 
                     mean) %>%
  merge(IndDt, by = "District") %>%
  merge(RntDt, by = "District")


#================================MAPS===========================================

install.packages("ggthemes")
library(ggthemes)


#=========================DISTRICTS MAP=========================================
p1 <- ggmap(map, extent = "normal", maprange = FALSE) +
  geom_polygon(data = IndRntDt, 
               aes(long, lat, group = group, fill=District),
               colour = "darkcyan", 
               alpha = 0.2) + 
  theme_bw() +
  coord_map(projection="mercator",
            xlim = c(attr(map, "bb")$ll.lon, attr(map, "bb")$ur.lon),
            ylim = c(attr(map, "bb")$ll.lat, attr(map, "bb")$ur.lat))

print(p1)

#=====================TOTAL INDEX - RENT HEATMAP================================
install.packages("ggrepel")
library(ggrepel)

p2 = ggplot(IndRntDt, 
           aes(long, lat, group = group))+
  geom_polygon(aes(fill = TotalIn),
               colour = "bisque4")+
  scale_fill_gradient(low = "snow1", 
                      high = "darkcyan",
                      name = "Liveability Index") +
  theme_bw() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line  = element_blank(),
        axis.ticks = element_blank(),
        axis.text  = element_blank(),
        axis.title = element_blank())+
  scale_color_gradient(low = "paleturquoise", 
                       high = "rosybrown2", 
                       guide=FALSE) +
  geom_label_repel(data = LablesDt, 
             aes(long, lat, label = District, group = District), 
             size = 3,
             box.padding = 2,
             segment.color = 'grey24') + 
  geom_point(data = LablesDt, 
             aes(long, lat, 
                 colour = Rent,
                 group = District),
             size = 13) +
  geom_point(data = LablesDt, 
             aes(long, lat, 
                 group = District),
             size = 13,
             shape = 1,
             colour = "grey24") + 
  geom_text(data = LablesDt, 
            aes(long, lat, label = paste0(Rent,"€"), group = District), 
            size = 3)

print(p2)

#=======================INDEX RENT ANALYSIS=====================================

ggplot(LablesDt, aes(x=Rent, y=TotalIn)) +
  geom_point(size = 3, 
             colour = "steelblue") +  # Use hollow circles
  geom_smooth(method = lm,  # Add linear regression line                           #space = 
              se = FALSE) +  # Don't add shaded confidence region
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(y = "Liveability Index") +
  geom_label_repel(data = LablesDt, 
                   aes(Rent, TotalIn, label = District, group = District), 
                   size = 4,
                   box.padding = 0.5) 

#calculate correlation between Rent and Total Index
cor(LablesDt$TotalIn, LablesDt$Rent)

#run regression of rent on calculated total index and print regression summary
LablesDt %>% 
  lm(Rent~TotalIn, .) %>%
  summary()


#===================EAST-WEST BERLIN COMPARISON=================================
#Berlin wall data
#Source: https://opendata-esri-de.opendata.arcgis.com/datasets/ef3c99e6dfcf4f84a90ae91820b08d7a_2

#read and transform KML border of West Berlin
wall = readOGR("./06_Results_Visualization/Berliner_Mauer_Hinterlandmauer.kml")
Wall = fortify(wall)  
  
# Define lists of districts which belonged to east and west Berlin 
# (Omit Mitte and Fridrichshein-Kreuzberg as they are in both East and West)
east = c("Pankow", 
         "Lichtenberg", 
         "Treptow-Kopenick", 
         "Marzahn-Hellersdorf")
west = c("Tempelhof-Schoneberg", 
         "Reinickendorf", 
         "Charlottenburg-Wilmersdorf", 
         "Steglitz-Zehlendorf", 
         "Neukolln")

# Define centroids of east and west Berlin 
LablesCnt = LablesDt %>%
  mutate(Part = ifelse(District %in% east, "East Berlin", 
                       ifelse(District %in% west, "West Berlin", NA))) %>%
  group_by(Part) %>%
  summarize(lat = mean(lat),
            long = mean(long)) %>%
  filter(!is.na(Part))

# For repeated plots I write a function with all the settings
EastWest = function(variable, title) {
plot = ggplot(IndRntDt, 
              aes(long, lat, group = group)) +
  geom_polygon(aes(fill = variable), 
               colour = "darkcyan") +
  geom_path(data = Wall, aes(x=long, y=lat),                                    # SPACE ====
            color="bisque4",
            size = 1.5) +
  scale_fill_gradient(low = "snow1", 
                      high = "darkcyan",
                      name = title) +
  theme_bw() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line  = element_blank(),
        axis.ticks = element_blank(),
        axis.text  = element_blank(),
        axis.title = element_blank()) +
  geom_label(data = LablesCnt, 
                   aes(long, lat, label = Part, group = Part), 
                   size = 6)
return(plot)
}

# Plot Index together with Berlin Wall
EastWest(IndRntDt$TotalIn, "Liveability Index")

# Plot Rent with Berlin wall
EastWest(IndRntDt$Rent, "Rent")

# Plot subindixes and Berlin Wall
EastWest(IndRntDt$Phy1In, "Housing")
EastWest(IndRntDt$Phy2In, "Infrastructure")
EastWest(IndRntDt$SocIn,  "Social Index")
EastWest(IndRntDt$EcoIn,  "Economic Index")
EastWest(IndRntDt$EnvIn,  "Environmental Index")

#Test if the difference in characteristics is significant

#ideas: calculate group means and compare them with regard to West and East 
#Berlin border (for index and for each variable)
#test if vectors of East-West means are significantly different

#citation: https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0140197
#



