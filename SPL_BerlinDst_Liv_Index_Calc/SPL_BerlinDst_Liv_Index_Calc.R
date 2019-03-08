#===================== PREPARING THE ENVIROMENT ================================


library(readr)
library(readxl)
library(tidyr)
library(dplyr)

#================ READING IN ALL PREPARED DATA SETS ============================

dt1 = read.csv("./SPL_BerlinDst_Liv_Index_Calc/SPL-BerlinDst_Prep_1.csv",
               sep = ";", dec = ",", row.names = 1, stringsAsFactors = FALSE )

dt2 = read.csv("./SPL_BerlinDst_Liv_Index_Calc/SPL_BerlinDst_Data_prep2_qi.csv", 
               row.names = 1, stringsAsFactors = FALSE)

dt3 = read.csv ("./SPL_BerlinDst_Liv_Index_Calc/SPL-BerlinDst_Prep_3.csv", 
                sep = ";", dec = ",", stringsAsFactors = FALSE)


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
    
  x/lvbInDt$Population
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
    
  x/lvbInDt$District.Size
}


NormalizePositive = function(x){
    # Function normalizes the data in the vector x , by assigning the values 
    # between 0 and 1. The highest value among the vector recives 1, the lowest 
    # 0.    
    # 
    # Args: 
    #      x: the vectors of which values should be normalized. The data type 
    #      must be numeric. 
    #
    # Returns: 
    # Vector of normalised data from vector x 
    (x-min(x))/(max(x)-min(x))
}

NormalizeNegative=function(x){
    # Function normalizes the data in the vector x , by assigning the values 
    # between 0 and 1. The highest value among the vector recives 0, the lowest 
    # 1.    
    # 
    # Args: 
    #      x: the vectors of which values should be normalized. The data type 
    #      must be numeric. 
    #
    # Returns: 
    # Vector of normalised data from vector x 
    (max(x)-x)/(max(x)-min(x))
}
#======================CALCULATING INDEX INDICATORS============================= 

InDt = list(lvSpc = PerCapita(lvbInDt$Total.living.space),  # Calc. liv space/cap.
            hsAv  = PerHa(lvbInDt$Flats),  # Calc. nr of flats per ha
            dns   = PerHa(lvbInDt$Population),  # Calc. population per ha
            hsAl  = lvbInDt$Housing.allowance.households, # Choose hous. all. dt
            trnDn = PerHa(lvbInDt$Bus.Stop.Number),  # Calc. den. of public trans.
            bkLn  = lvbInDt$cycling.length,  # Choose cycling lines lenght
            crChr = PerHa(lvbInDt$Nr..of.charging.stations),  #  Calc.e-car char.stat./ha  
            prkSp = PerHa(lvbInDt$Parking.Spaces),  # Calc. nr parking spc. per ha
            trs   = lvbInDt$Tourist.guests,  # Choose nr of tourist guests
            # Calculate the hotel occupancy by multipling the total num. of bed  
            # by 365 days in the year (total capacity) and deducting the total 
            # num. of stays
            htlOc = (lvbInDt$Hotel.beds*365) - lvbInDt$Overnight.stays, 
            sprCl = lvbInDt$Sports.Club,  # Choose nr of sport clubs
            std   = lvbInDt$Pupils,  # Choose nr of pupils per K of inhabitants 
            grdSz = lvbInDt$Pupils/lvbInDt$Grades,  # Cal. avg. grade size
            chU3  =    
            
            
        
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

 rst   = PerHa(lvbInDt$)

 IndDt = 

#============================NEGATIVE INDICATORS================================

# Most of the indicators contribute positively to the index, the higher the 
# value the better the district should be rank. Here are 6 indicators, which
# contribute negatively to the index calculation

 
NgtInd = list(a=which(colnames(IndDt)=="dens"), 
              b=which(colnames(IndDt)=="traf_acc"),
              c=which(colnames(IndDt)=="bankr"),
              d=which(colnames(IndDt)=="crime"),
              e=which(colnames(IndDt)=="pm_25"),
              f=which(colnames(IndDt)=="pm_10"))

 
#=========================NORMALIZING THE DATA =================================

# We normalise the data according to whether they contribute positivly or 
# negatively to the Index. For each     

for (i in 1:(ncol(IndDt))){
    if ((i+2) %in% NgtInd){
        IndDt[,paste(colnames(IndDt[i]),"Scr")] = 
             NormalizeNegative(IndDt[i])
        } else {
          IndDt[,paste(colnames(InsDt[i]),"Scr")] =
               NormalizePositive(InsDt[i])
        }
    }

#==============  CREATING FINAL DATA FRAME FOR INDEX CALCULATION =============== 

#======================== WEIGHTS OF EACH PILLAR ===============================

phys1 = c(0.10)
phys2 = c(0.15)
soc   = c(0.25)
eco   = c(0.25)
env   = c(0.25)


#=======================DE

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






