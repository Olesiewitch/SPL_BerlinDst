##### MERGING THE DATA SETS

### preparing the enviroment
rm(list = ls()) ## cleaning the enviroment
setwd("C:/Users/Malgorzata/Desktop/R/SPL-Project-New/Dirstrict Data") ### setting up working directory
library(readr)
library(readxl)
library(tidyr)
library(dplyr)

### loading the cleaned data sets

fbinter_stadt_berlin_daten =(read_excel("fbinter.stadt-berlin- daten.xls"))
berliner_verkehr_zahlen_daten = read_csv("berliner_verkehr_zahlen_daten.csv")
statistik_berlin_brandenburg_daten= read_delim("statistik-berlin-brandenburg-daten.csv", ";", escape_double = FALSE, trim_ws = TRUE)
Nr_of_house_doctor_per_10_000_people_ = read_excel("Nr.of house doctor per 10,000 people .xls")
kriminalitatsatlas_berlin_daten = read_csv("kriminalitatsatlas_berlin_daten.csv")
laden 
bus
restautant

### Arragning the data sets according to alphabetical order
fbinter_ar=arrange(fbinter_stadt_berlin_daten,Districts)
berliner_ar=arrange(berliner_verkehr_zahlen_daten,District)
stat_dis=separate(stat2[-c(13),],col=District,into= c("Nr", "District"), sep =" " )
stat_arr=arrange(stat_dis,District)
doctor_arr= arrange(Nr_of_house_doctor_per_10_000_people_, Districts)
kriminal_arr= arrange(kriminalitatsatlas_berlin_daten, districts)

###Mergin data sets

final_data_frame = data.frame(stat_arr,fbinter_ar$`Street crossings`,fbinter_ar$`Lenthg of cycling lines`,berliner_ar$`Parking Spaces`,doctor_arr$`Nr.of house doctor per 10,000 people`,kriminal_arr$crime)
colnames(final_data_frame)=c(colnames(stat_arr),colnames(fbinter_ar[2]), colnames(fbinter_ar[3]), colnames(berliner_ar[3]),colnames(doctor_arr[2]), colnames(kriminal_arr[3]))

final_data=arrange(final_data_frame,Nr)

### LIVIBILITY INDEX INDICATORS : CALCULATION

### Creating useful functions
per_capita = function(x)
  {x/final_data_frame$Population
 }

per_ha = function(x)
  {x/final_data_frame$`District Size`
  }
###
colnames(livibility_index)= c("district","liv_spc", "house_av","dens","hous_allow","trans_stops","bike_lines","e-car_charge","park_spc","tourist","hotel_occ", "bars", "sport_club", "pupils","avg_grade","child<3_dc","child3-6_dc", "nr_docs", "act_sen", " act_yth", "traf_acc", "str_cross", "soc_help", "handi", "employ", "comp","tax_rev",
"bankr","green_spc", "agr_res", "trees", "so_2", "no_2_x", "pm_2.5_10")

livibility_index =data.frame(final_data$District,liv_spc)                             

liv_spc
house_av 
dens=per_capita(final_data$`District Size`*10000)

