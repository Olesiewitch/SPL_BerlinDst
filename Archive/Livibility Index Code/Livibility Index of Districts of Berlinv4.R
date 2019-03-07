##### MERGING THE DATA SETS

### preparing the enviroment
rm(list = ls()) ## cleaning the enviroment
setwd("~/SPL-Project/Dirstrict Data") ### setting up working directory
library(readr)
library(readxl)
library(tidyr)
library(dplyr)

### loading the cleaned data sets

fbinter_stadt_berlin_daten =(read_xls("fbinter.stadt-berlin- daten.xls"))#streets crossings and length of cyclyings
berliner_verkehr_zahlen_daten = read.csv("berliner_verkehr_zahlen_daten.csv",header=TRUE,sep=",", stringsAsFactors=FALSE)
statistik_berlin_brandenburg_daten = read.csv("statistik-berlin-brandenburg-daten.csv", header=TRUE, sep=";", stringsAsFactors=FALSE, colClasses =c("character","numeric", "numeric","numeric","numeric","numeric","numeric", "numeric","numeric","numeric","numeric","numeric", "numeric","numeric","numeric","numeric","numeric", "numeric","numeric","numeric","numeric","numeric", "numeric","numeric","numeric","numeric","numeric", "numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric")) 
nr_of_house_doctor= read_excel("Nr.of house doctor per 10,000 people .xls")
kriminalitatsatlas_berlin_daten = read.csv("kriminalitatsatlas_berlin_daten.csv", header=TRUE)
statistik_daten=read.csv("statistik_daten.csv",header=TRUE)
charging_stations=read.csv("chargingstations.csv") 
bus_stops=read.csv("count_bus_stops.csv")
restaurant=read.csv("Nr_of_restaurant.csv") 
air_pollution ## Alexandra please add

### Arragning the data sets according to alphabetical order of districts and converting to numeric level 

## Street Crossings and Length of  
fbinter_arr=arrange(fbinter_stadt_berlin_daten,Districts) ## arrangind data according to the district
fbinter_arr=sapply(fbinter_arr[,-c(1)], as.numeric) ## making the values numeric and removing the district names 

## Parking spaces
berliner_arr=arrange(berliner_verkehr_zahlen_daten,District)
berliner_arr=as.numeric(berliner_arr$Parking.Spaces)

### Data set from Statistik Berlin Brandenburg 
stat_dis=separate(statistik_berlin_brandenburg_daten[-c(13),],col=District,into= c("Nr", "District"), sep =" " ) ### Removing "Berlin" observation and deviding 
## the name of the district into two part : number and name
stat_arr=arrange(stat_dis,District)### arranging according to the district name

## Doctors
doctor_arr= arrange(nr_of_house_doctor, Districts)
doctor_arr=as.numeric(as.character(doctor_arr$`Nr.of house doctor per 10,000 people`))

## Crime 
kriminal_arr= arrange(kriminalitatsatlas_berlin_daten, districts)
kriminal_arr=as.numeric(kriminal_arr$crime)

### Trees, Dogs, Green Space 

statistik_arr=arrange(statistik_daten,districts)
statistik_arr= statistik_arr[-(1:2)]
statistik_arr=data.frame((as.numeric(statistik_arr$trees.km)),as.numeric(statistik_arr$open.green.space))
#### please arrange the data accoriding to the alpabetical order of District and make sure the number are as.numeric

#Charging station
charging_stations_arr=arrange(charging_stations,Bezirk)
charging_stations_arr=as.numeric(charging_stations_arr$count)

#Bus stops
bus_stops_arr=arrange(bus_stops,districts)
bus_stops_arr=as.numeric(bus_stops_arr$count_stops)

#rRestaurants
restaurant_arr=arrange(restaurant,Nr_of_restaurant.district.1.12.)
restaurant_arr=as.numeric(restaurant_arr$Nr_of_restaurant.X__3.1.12.)

###Merging data sets
final_data_frame = data.frame(stat_arr,fbinter_arr,berliner_arr,doctor_arr,kriminal_arr,statistik_arr,
                              charging_stations_arr,bus_stops_arr,restaurant_arr) #### please add remaining columns & their names
colnames(final_data_frame)=c(colnames(stat_arr),colnames(fbinter_stadt_berlin_daten[2:3]),
                             colnames(berliner_verkehr_zahlen_daten[3]),
                             colnames(nr_of_house_doctor[2]), colnames(kriminalitatsatlas_berlin_daten[3]),
                             colnames(statistik_daten[4]),colnames(statistik_daten[6]),"charging_station","bus_stops","Nr_of_restaurants")

final_data=arrange(final_data_frame,Nr)

### LIVIBILITY INDEX INDICATORS : CALCULATION

### Creating useful functions
per_capita = function(x){
  x/final_data_frame$Population
 }

per_ha = function(x){
  x/final_data_frame$District.Size
  }
#### Calculating each indicator (following the google spreadsheet link in Markdown)
liv_spc =per_capita(final_data$Total.living.space)
house_av=per_ha(final_data$Flats)
dens=final_data$Population/final_data$District.Size
hous_allow=final_data$Housing.allowance.housholds
trans_stops=per_ha(final_data$bus_stops)*100 ### Qi
bike_lines= final_data$`Lenthg of cycling lines`
e_car_charge=per_ha(final_data$charging_station)*1000 ## Qi
park_spc=final_data$Parking.Spaces
tourist=final_data$Tourist.guests
hotel_occ =(final_data$Hotel.beds*365)-final_data$Overnight.stays
restaurant_ha=per_ha(final_data$Nr_of_restaurants)*100 ##Qi
sport_club=final_data$Sport.Clubs
pupils=final_data$Puplis.K
avg_grade=final_data$Puplis/final_data$Grades
child_three_dc=final_data$X..Children.in.daycare.under.3
child3_six_dc= final_data$X..Children.in.daycare.3..under.6
nr_docs=final_data$`Nr.of house doctor per 10,000 people`
act_sen=final_data$Senior.Members.of.Sport.Clubs
act_yth=final_data$Junior.Members.of.Sport.Clubs 
traf_acc=final_data$Street.traffic.accidents
str_cross=final_data$`Street crossings`
soc_help=final_data$Social.Help.Recipients
handi=final_data$Severely.handicapped.per.1000.inhabitants
employ=final_data$Employed
comp=final_data$Companies
tax_rev=final_data$Taxable.Revenues
bankr=final_data$Bankruptcies
green_spc=per_ha(final_data$open.green.space)
agr_res=final_data$Agricultural.surface/final_data$Residential.and.traffic.surface
trees=final_data$trees.km

so_2 ### Alexandra
no_2_x
pm_2.5_10


livibility_index =data.frame(final_data$Nr, final_data$District,liv_spc,house_av,trans_stops, dens,hous_allow, bike_lines,e_car_charge,  park_spc, tourist,hotel_occ,restaurant_ha, sport_club,pupils,avg_grade, child_three_dc, child3_six_dc,nr_docs,act_sen, act_yth,traf_acc, str_cross,soc_help, handi, employ, comp,tax_rev, bankr,green_spc, agr_res, trees)                             
colnames(livibility_index)= c("nr", "district","liv_spc", "house_av","trans_stops","dens","hous_allow","bike_lines","e_car_charge", "park_spc","tourist","hotel_occ","restaurant_ha", "sport_club", "pupils","avg_grade","child_three_dc","child3_six_dc", "nr_docs", "act_sen", " act_yth", "traf_acc", "str_cross", "soc_help", "handi", "employ", "comp","tax_rev",
"bankr","green_spc", "agr_res", "trees")

#### Calculating the score for every indicator

create_score_col
  for (i in 1:(ncol(livibility_index)-2)){
    livibility_index[[paste0(i, "_score")]]=livibility_index(i,2)
  }

### Creating 4 categories (Physical, Social, Economic, Enviromental)


### Calculating the index according to the categories 


### Calculating total index


#### Creating heatmaps



###Comparing to rent data



### Doing some fancy analysis 
