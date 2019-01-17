##### MERGING THE DATA SETS

### preparing the enviroment
rm(list = ls()) ## cleaning the enviroment
setwd("~/SPL-Project/Dirstrict Data") ### setting up working directory
library(readr)
library(readxl)
library(tidyr)
library(dplyr)
library(normalr)

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
air_pollution=read.csv2("luftverschmutzung.csv")

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

#Air-polution
air_pollution_arr=arrange(air_pollution, district)


###Merging data sets
final_data_frame = data.frame(stat_arr,fbinter_arr,berliner_arr,doctor_arr,kriminal_arr,statistik_arr,
                              charging_stations_arr,bus_stops_arr,restaurant_arr, air_pollution_arr[c(2,3)]) #### please add remaining columns & their names
colnames(final_data_frame)=c(colnames(stat_arr),colnames(fbinter_stadt_berlin_daten[2:3]),
                             colnames(berliner_verkehr_zahlen_daten[3]),
                             colnames(nr_of_house_doctor[2]), colnames(kriminalitatsatlas_berlin_daten[3]),
                             colnames(statistik_daten[4]),colnames(statistik_daten[6]),"charging_station","bus_stops","Nr_of_restaurants", "PM10", "PM25")

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
crime=final_data$crime
soc_help=final_data$Social.Help.Recipients
handi=final_data$Severely.handicapped.per.1000.inhabitants
employ=final_data$Employed
comp=final_data$Companies
tax_rev=final_data$Taxable.Revenues
bankr=final_data$Bankruptcies
green_spc=per_ha(final_data$open.green.space)
agr_res=final_data$Agricultural.surface/final_data$Residential.and.traffic.surface
trees=final_data$trees.km
pm_25=final_data$PM25
pm_10=final_data$PM10


###Alex look down 

livibility_index =data.frame(final_data$Nr, 
                             final_data$District,
                             liv_spc,
                             house_av,
                             dens,hous_allow,
                             trans_stops, 
                             bike_lines,
                             e_car_charge,  
                             park_spc, 
                             tourist,
                             hotel_occ,
                             restaurant_ha, 
                             sport_club,
                             pupils,
                             avg_grade, 
                             child_three_dc, 
                             child3_six_dc,
                             nr_docs,
                             act_sen, 
                             act_yth,
                             traf_acc, 
                             str_cross,
                             crime, 
                             soc_help, 
                             handi, 
                             employ, 
                             comp,
                             tax_rev, 
                             bankr,
                             green_spc, 
                             agr_res, 
                             trees, 
                             pm_25, 
                             pm_10)
                                                    
colnames(livibility_index)= c("nr", 
                              "district",
                              "liv_spc", 
                              "house_av",
                              "dens",
                              "hous_allow",
                              "trans_stops",
                              "bike_lines",
                              "e_car_charge", 
                              "park_spc",
                              "tourist",
                              "hotel_occ",
                              "restaurant_ha", 
                              "sport_club", 
                              "pupils",
                              "avg_grade",
                              "child_three_dc",
                              "child3_six_dc", 
                              "nr_docs", 
                              "act_sen", 
                              " act_yth", 
                              "traf_acc", 
                              "str_cross",
                              "crime", 
                              "soc_help", 
                              "handi", 
                              "employ", 
                              "comp",
                              "tax_rev",
                              "bankr",
                              "green_spc", 
                              "agr_res", 
                              "trees", 
                              "pm_25", 
                              "pm_10")

### Creating 4 categories (Physical, Social, Economic, Enviromental)

### Positive vs Negative indicator 
a=which(colnames(livibility_index)=="dens")
b=which(colnames(livibility_index)=="traf_acc")
c=which(colnames(livibility_index)=="bankr")
d=which(colnames(livibility_index)=="crime")
e=which(colnames(livibility_index)=="pm_25")
f=which(colnames(livibility_index)=="pm_10")

negative=c(a,b,c,d,e,f) 

#### Calculating the score for every indicator

### positive and  negative score function 

normalized_positive = function(x){
  (x-min(x))/(max(x)-min(x))}

normalized_negative=function(x){
  (max(x)-x)/(max(x)-min(x))
  }

### creating columns for scores and calculating them according to category

  for (i in 1:(ncol(livibility_index)-2)){
    if ((i+2) %in% negative) {
      livibility_index[,paste(colnames(livibility_index[i+2]),"_score")]=normalized_negative(livibility_index[i+2])}
  else {
      livibility_index[,paste(colnames(livibility_index[i+2]),"_score")]=normalized_positive(livibility_index[i+2])
  }
   }

### Weights of each Index
phys1_weight=0.10
phys2_weight=0.15
social_weight=0.25
economic_weight=0.25
env_weight=0.25

### INDEX ACCORDING TO THE CATEGORIES

### number of indicators for each category
p1_ind_nr=4
p2_ind_nr=4
soc_ind_nr=16
eco_ind_nr=4
env_ind_nr=5 ## Alex

Physical_index_1=apply(livibility_index[36:39],1,sum)
Physical_index_2=apply(livibility_index[40:43], 1,sum)
Social_index=apply(livibility_index[44:59],1,sum)
Economic_index=apply(livibility_index[60:63],1,sum)
Enviromental_index= apply(livibility_index[64:68],1,sum) ### Alex

### TOTAL INDEX SCORE

Total_Index_Score=((Physical_index_1*phys1_weight)+(Physical_index_2*phys2_weight)+(Social_index*social_weight)+(Economic_index*economic_weight)+(Enviromental_index*env_weight))

Results=data.frame(livibility_index$nr,livibility_index$district, Physical_index_1,Physical_index_2, Social_index, Economic_index,Enviromental_index, Total_Index_Score)
colnames(Results)= c(colnames(livibility_index[1:2]),"Physical Index1", "Physical Index 2", "Social Index", "Economic Index", "Enviromental Index", "Total Index Score")

max_score=(p1_ind_nr*phys1_weight)+(p2_ind_nr*phys2_weight) +(soc_ind_nr*social_weight)+(eco_ind_nr*economic_weight)+(env_ind_nr*env_weight)        

#### Creating heatmaps



###Comparing to rent data



### Doing some fancy analysis 
