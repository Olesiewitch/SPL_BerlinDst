#===================== PREPARING THE ENVIROMENT ================================


library(readr)
library(readxl)
library(tidyr)
library(dplyr)

#================ READING IN ALL PREPARED DATA SETS ============================

dt1 = read.csv("./SPL_BerlinDst_Liv_Index_Calc/SPL-BerlinDst_Prep_1.csv",
               sep = ";", row.names = 1)

dt2 = read.csv("./SPL_BerlinDst_Liv_Index_Calc/SPL_BerlinDst_Data_prep2_qi.csv", 
               row.names = 1)

dt3 = read.csv ("./SPL_BerlinDst_Liv_Index_Calc/SPL-BerlinDst_Prep_3.csv", 
                sep = ";")


#==================== MERGE THE DATA SETS ======================================

lvbInDt = merge(dt1, dt2, by.y = "District") %>% 
    merge(., dt3, by.y = "District")

#====================PREPERATION OF USEFUL FUNCTIONS =========================== 

PerCapita = function(x){
    # Function devides the data per number of inhabitants of given district to 
    # obtain per capita value 
    #
    #Args: 
    #    x: the column of which the data should devided per number of citizens
    #
    # Returns:
    # The vector of data x devided per number of inhabitants of given district
    
  x/final_data_frame$Population
}

PerHa = function(x){
    # Function devides the data by size in ha of given district to obtain 
    # per ha value
    #
    #Args: 
    #    x: the column of which the data should devided by size of the district
    #
    # Returns:
    # The vector of data x devided by size of the district in ha
    
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
mietpreise_in_berlin_2017_nach_bezirken= read_excel("statistic_id259905_mietpreise-in-berlin-2017-nach-bezirken.xlsx", 
                                                    sheet = "Daten", col_types = c("text", 
                                                                                   "numeric"), skip = 4)

mietepreise_arr=arrange(mietpreise_in_berlin_2017_nach_bezirken[!mietpreise_in_berlin_2017_nach_bezirken$X__1=="Berlin Durchschnitt",], X__1)

#Results_Rent=merge(Results, mietepreise_arr, by.x="district",by.y="X__1")
results_arr=arrange(Results, district)

Results_Rent=data.frame(results_arr,mietepreise_arr$`Mietpreis in Euro pro m²`)


### Doing some fancy analysis 






