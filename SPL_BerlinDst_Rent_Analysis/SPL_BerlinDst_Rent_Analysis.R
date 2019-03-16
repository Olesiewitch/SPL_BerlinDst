#=======================LOAD PACKAGES===========================================
# Install packages if not installed before
if(!require("ggplot2")){install.packages("ggplot2")}
if(!require("sp")){install.packages("sp")}
if(!require("mapproj")){install.packages("mapproj")}
if(!require("maps")){install.packages("maps")}
if(!require("rgdal")){install.packages("rgdal")}
if(!require("ggthemes")){install.packages("ggthemes")}
if(!require("ggmap")){install.packages("ggmap")}
if(!require("osmdata")){install.packages("osmdata")}
if(!require("ggrepel")){install.packages("ggrepel")}

# Load libraries
library("ggplot2")
library("ggmap")
library("sp")
library("maps")
library("mapproj")
library("osmdata")
library("rgdal")
library("dplyr")
library("readxl")
library("ggthemes")
library("ggrepel")

#======================RUN HELPFULL FUNCTIONS ==================================
# Use function from Malgorzata Olesiewitz
DistricToFullName = function (column){
    # Author: Malgorzata Olesiewitz
    # Function re-names the districts of Berlin with their full names 
    # without special signs. The function identifies the district by 
    # three or four letters of its name. 
    #
    # Args:
    #      column: vector or a column containing names of Berlin
    #      districts.Name can have any form, special signs can be
    #      use and additional information can be added to it. Vector 
    #      must be character type.Only first 3-4letters need to be correct.  
    #      
    # Returns: 
    #        Vector of replaced district names by its official names  
    #        without special signs.  
    
    column[grepl("mit", column, 
                 ignore.case = TRUE)] = "Mitte" 
    column[grepl("fri", column, 
                 ignore.case = TRUE)] = "Friedrichshain-Kreuzberg"
    column[grepl("pank", column,
                 ignore.case = TRUE)] = "Pankow"
    column[grepl("mar", column,
                 ignore.case = TRUE)] = "Marzahn-Hellersdorf" 
    column[grepl("char", column,
                 ignore.case = TRUE)] = "Charlottenburg-Wilmersdorf"
    column[grepl("spa", column,
                 ignore.case = TRUE)] = "Spandau" 
    column[grepl("ste", column,
                 ignore.case = TRUE)] = "Steglitz-Zehlendorf"  
    column[grepl("tem", column,
                 ignore.case = TRUE)] = "Tempelhof-Schoneberg" 
    column[grepl("trep", column,
                 ignore.case = TRUE)] = "Treptow-Kopenick" 
    column[grepl("neu", column,
                 ignore.case = TRUE)] = "Neukolln" 
    column[grepl("lich", column,
                 ignore.case = TRUE)] = "Lichtenberg" 
    column[grepl("rein", column,
                 ignore.case = TRUE)] = "Reinickendorf"
    
    return(column)
}

#=======================DOwNLOAD AND TRANSFORM DATA=============================
# Get Berlin Boundaries from OSM data
ber = getbb("berlin")

# Automatic bbox cut some parts of berlin out, that is why I manually adjust x 
# coordinate. Adjustments are defined manually.
ber_adj = ber
ber_adj["x","min"] = ber_adj["x","min"]-0.18
ber_adj["x","max"] = ber_adj["x","max"]+0.22
ber_adj["y","min"] = ber_adj["y","min"]-0.02

# Download map teils from stamen maps
map = get_stamenmap(ber_adj, maptype="toner-lite", zoom = 11)

# Download and transform polygon data for district borders
test = readOGR("./SPL_BerlinDst_Rent_Analysis/bezirksgrenzen.kml")
Bezirk = fortify(test)

# Read and transform KML border of West Berlin
wall = readOGR(paste0("./SPL_BerlinDst_Rent_Analysis",
                      "/Berliner_Mauer_Hinterlandmauer.kml"))
Wall = fortify(wall)  

# Assign names of districts
Bezirk$District = case_when(Bezirk$id == "0" ~ "Reinickendorf",                   
                            Bezirk$id == "1" ~ "Charlottenburg-Wilmersdorf",
                            Bezirk$id == "10" ~ "Friedrichshain-Kreuzberg",
                            Bezirk$id == "11" ~ "Tempelhof-Schoneberg",
                            Bezirk$id == "2" ~ "Treptow-Kopenick",
                            Bezirk$id == "3" ~ "Pankow",
                            Bezirk$id == "5" ~ "Lichtenberg",
                            Bezirk$id == "4" ~ "Neukolln",
                            Bezirk$id == "6" ~ "Marzahn-Hellersdorf",
                            Bezirk$id == "7" ~ "Spandau",
                            Bezirk$id == "8" ~ "Steglitz-Zehlendorf",
                            Bezirk$id == "9" ~ "Mitte",
                            TRUE ~ "other")

# Download livability index table created in previous quantlet
IndDt = read.csv2("./SPL_BerlinDst_Rent_Analysis/SPL_BerlinDst_Liv_Index.csv", 
                  sep = ";", 
                  dec = ",")

# Download rent data
RntDt = read_excel(paste0("./SPL_BerlinDst_Rent_Analysis/statistic_id259905",
                          "_mietpreise-in-berlin-2017-nach-bezirken.xlsx"),
                   sheet = "Daten",
                   skip = 5,
                   col_names = FALSE) %>%
    setNames(c("District", "Rent")) %>%
    filter(!District == "Berlin Durchschnitt")

# Change districts name to the standard writing
RntDt$District = DistricToFullName(RntDt$District)

# Merge polygon data with index and rent
IndRntDt = Bezirk %>% 
    merge(IndDt, by = "District") %>%
    merge(RntDt, by = "District")

# Calculate coordinates of centroids for each district
LablesDt = aggregate(cbind(long, lat) ~ District, 
                     data=Bezirk, 
                     mean) %>%
    merge(IndDt, by = "District") %>%
    merge(RntDt, by = "District")

#=========================DISTRICTS MAP=========================================
p1 <- ggmap(map, extent = "normal", maprange = FALSE) +
    geom_polygon(data = IndRntDt, 
                 aes(long, lat, group = group, fill=District),
                 colour = "red", 
                 alpha = 0.2) + 
    theme_bw() +
    coord_map(projection="mercator",
              xlim = c(attr(map, "bb")$ll.lon, attr(map, "bb")$ur.lon),
              ylim = c(attr(map, "bb")$ll.lat, attr(map, "bb")$ur.lat))

# Save the plot
ggsave("BerlinDistrictsMap.png", 
       plot = p1,
       scale = 1, 
       device = "png", 
       path = "SPL_BerlinDst_Rent_Analysis/")

dev.off()

#=====================TOTAL INDEX - RENT HEATMAP================================
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

# Save the plot
ggsave("IndexRent.png", 
       plot = p2,
       scale = 1, 
       device = "png", 
       path = "SPL_BerlinDst_Rent_Analysis/")

dev.off()

#=======================INDEX RENT ANALYSIS=====================================

ggplot(LablesDt, aes(x=Rent, y=TotalIn)) +
    geom_point(size = 3, 
               colour = "steelblue") +  # Use hollow circles
    geom_smooth(method = lm,  # Add linear regression line                        
                se = FALSE) +  # Don't add shaded confidence region
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    labs(y = "Liveability Index") +
    geom_label_repel(data = LablesDt, 
                     aes(Rent, TotalIn, label = District, group = District), 
                     size = 4,
                     box.padding = 0.5) 

# Save the plot
ggsave("IndexRentScatter.png", 
       plot = last_plot(),
       scale = 1, 
       device = "png", 
       path = "SPL_BerlinDst_Rent_Analysis/")

dev.off()


#calculate correlation between Rent and Total Index
cor(LablesDt$TotalIn, LablesDt$Rent)

#run regression of rent on calculated total index and print regression summary
LablesDt %>% 
    lm(Rent~TotalIn, .) %>%
    summary()

#===================EAST-WEST BERLIN COMPARISON=================================

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
EastWest = function(variable, title, name, leg = TRUE) {
    # Author: Aleksandra Kudaeva
    # Description: function creates a heatmap of Berlin with highlighted 
    #              Berlin Wall border according to the given parameter
    # Input: Table and column, which will determine coloring,
    #        Title of legend
    #        Name of png file which is going to be saved
    #        Legend: by default - TRUE
    # Output: Heatmap, printed and saved in png
    plot = ggplot(IndRntDt,  
                  aes(long, lat, group = group)) +
        geom_polygon(aes(fill = variable), 
                     colour = "darkcyan",
                     show.legend = leg) +
        geom_path(data = Wall, aes(x = long, y = lat),                                 
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
    ggsave(paste0(name, ".png"), 
           plot = plot,
           scale = 1,
           device = "png",
           path = "SPL_BerlinDst_Rent_Analysis/")
    
    return(plot)
}

# Plot Index together with Berlin Wall
EastWest(IndRntDt$TotalIn, "Liveability Index", "IndexEastWest", leg = TRUE)

# Plot Rent with Berlin wall, without legend
EastWest(IndRntDt$Rent, "Rent", "RentEastWest", leg = FALSE)

# Plot subindixes and Berlin Wall, without legend
EastWest(IndRntDt$Phy1In, "Housing", "HousingEastWest", leg = FALSE )
EastWest(IndRntDt$Phy2In, "Infrastructure", "InfrEastWest",leg = FALSE)
EastWest(IndRntDt$SocIn,  "Social Index", "SocEastWest", leg = FALSE)
EastWest(IndRntDt$EcoIn,  "Economic Index", "EconEastWest", leg = FALSE)
EastWest(IndRntDt$EnvIn,  "Environmental Index", "EcolEastWest", leg = FALSE)





