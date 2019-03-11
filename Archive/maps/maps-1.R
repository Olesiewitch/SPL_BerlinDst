if(!require("ggplot2")){install.packages("ggplot2")}
if(!require("ggmap")){install.packages("ggmap")}
if(!require("sp")){install.packages("sp")}
if(!require("mapproj")){install.packages("mapproj")}
if(!require("maps")){install.packages("maps")}
if(!require("osmdata")){install.packages("osmdata")}
if(!require("rgdal")){install.packages("rgdal")}
install.packages("broom")

library("ggplot2")
library("ggmap")
library("sp")
library("maps")
library("mapproj")
library("osmdata")
library("rgdal")
library("dplyr")
library("broom")


#ber <- c(left = 13, bottom = 52.3, right = 13.8, top = 52.7)
#Get Berlin Boundaries from OSM data
ber = getbb("berlin")

#automatic bbox cut some parts of berlin out, that is why I manually adjust x coordinate. Adjustments are defined manually.
ber_adj = ber
ber_adj["x","min"] = ber_adj["x","min"]-0.18
ber_adj["x","max"] = ber_adj["x","max"]+0.22
ber_adj["y","min"] = ber_adj["y","min"]-0.02

map = get_stamenmap(ber_adj, maptype="toner-lite", zoom = 11)

#source: https://data.technologiestiftung-berlin.de/dataset/bezirksgrenzen
test = readOGR("./06_Results_Visualization/bezirksgrenzen.kml")
Bezirk = fortify(test)
Bezirk$District = case_when(Bezirk$id=="0" ~ "Reinickendorf",
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

#==========================READ INDEX DATA======================================

Bezirk_m<-merge(Bezirk, Results, by.x="District", by.y="district", all=TRUE)
Berlin_r<-merge(Bezirk, Results_Rent, by.x="District", by.y="district", all.x=TRUE)


p <- ggmap(map, extent = "normal", maprange = FALSE) +
  geom_polygon(data = Bezirk, aes(long, lat, group = group, fill=District),
               colour = "red", alpha = 0.2) +
  theme_bw() +
  coord_map(projection="mercator",
            xlim=c(attr(map, "bb")$ll.lon, attr(map, "bb")$ur.lon),
            ylim=c(attr(map, "bb")$ll.lat, attr(map, "bb")$ur.lat))

print(p)

p1 <- ggmap(map, extent = "normal", maprange = FALSE) +
  geom_polygon(data = Bezirk_m, aes(long, lat, group = group, fill=Bezirk_m$'Physical Index1'),
               colour = "red", alpha = 0.2) +
  theme_bw() +
  coord_map(projection="mercator",
            xlim=c(attr(map, "bb")$ll.lon, attr(map, "bb")$ur.lon),
            ylim=c(attr(map, "bb")$ll.lat, attr(map, "bb")$ur.lat))

print(p1)


p2<-ggplot(Berlin_r, aes(long, lat, group=group))+geom_polygon(aes(fill=Total.Index.Score))+
  scale_fill_gradient(low = "darkblue", high = "white")
print(p2)

p3<-ggplot(Berlin_r, aes(long, lat, group=group))+geom_polygon(aes(fill=mietepreise_arr..Mietpreis.in.Euro.pro.m².))+
  scale_fill_gradient(low = "lightgreen", high = "darkgreen")
print(p3)
