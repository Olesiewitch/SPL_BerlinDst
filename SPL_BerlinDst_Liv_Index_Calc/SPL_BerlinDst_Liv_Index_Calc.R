#===================== PREPARING THE ENVIROMENT ================================

library(xlsx)
library(tidyr)
library(dplyr)

#==================== READING IN THE  DATA SETS ================================

lvbInDt = read.csv("SPL_BerlinDst_Liv_Index_Calc/SPL_BerlinDst_Data_Desc.csv",
               sep = ";", dec = ",", row.names = 1, stringsAsFactors = FALSE)

 
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
    
  x/lvbInDt$Size
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

DistricToFullName = function (column){
    # Function re-names the districts of Berlin with their full names without 
    # special signs. The function identifies the district by three or four 
    # letters of its name. 
    #
    # Args:
    #      column: vector or a column containing names of Berlin districts.
    #      Name can have any form, special signs can be use and additional 
    #      information can be added to it. Only first 3-4 letters need ro be
    #      correct. 
    #
    #Returns: 
    #Vector of replaced district names by its official names  
    
    column[grepl("mit",column, ignore.case = TRUE)] = "Mitte" 
    column[grepl("fri",column, 
                 ignore.case = TRUE)] = "Friedrichshain-Kreuzberg"
    column[grepl("pank",column,ignore.case = TRUE)] = "Pankow"
    column[grepl("mar",column,ignore.case = TRUE)] = "Marzahn-Hellersdorf" 
    column[grepl("char",column,
                 ignore.case = TRUE)] = "Charlottenburg-Wilmersdorf"
    column[grepl("spa",column,ignore.case = TRUE)] = "Spandau" 
    column[grepl("ste",column,ignore.case = TRUE)] = "Steglitz-Zehlendorf"  
    column[grepl("tem",column,ignore.case = TRUE)] = "Tempelhof-Schoneberg" 
    column[grepl("trep",column,ignore.case = TRUE)] = "Treptow-Kopenick" 
    column[grepl("neu",column,ignore.case = TRUE)] = "Neukolln" 
    column[grepl("lich",column,ignore.case = TRUE)] = "Lichtenberg" 
    column[grepl("rein",column,ignore.case = TRUE)] = "Reinickendorf"
    
    return(column)
}

#======================CALCULATING INDEX INDICATORS============================= 

lvSpc = PerCapita(lvbInDt$SpacePC)  # Calc. liv space/cap.
hsAv  = PerCapita(lvbInDt$Flats)  # Calc. nr of flats/cap.
dns   = PerHa(lvbInDt$Population)  # Calc. population per ha
hsAl  = lvbInDt$Hausehold # Choose hous. all. 
trnDn = PerHa(lvbInDt$Transport)  # Calc. den. of public trans.
bkLn  = PerHa(lvbInDt$Cycle)  # Calc. den. of cycling lines
crChr = PerHa(lvbInDt$Charging)  #  Calc.e-car char.stat./ha  
prkSp = PerCapita(lvbInDt$Parking)  # Calc. nr parking spc./ cap
trs   = PerCapita(lvbInDt$Tourists)  # Calc. nr of tourist per cap.    # rewrite comment
# Calculate the hotel occupancy by multipling the total num. of bed  
# by 365 days in the year (total capacity) and deducting the total 
            # num. of stays
htlOc = lvbInDt$Stays/(lvbInDt$Hotel*365)
sprCl = PerHa(lvbInDt$Sport)  # Choose nr of sport clubs per ha
res   = PerHa(lvbInDt$Restaurants) # Cal. nr of restaurants per ha
std   = lvbInDt$Pupils  # Choose nr of pupils per K of inhabitants 
grdSz = lvbInDt$Pupils/lvbInDt$Grades  # Cal. avg. grade size          
chU3  = lvbInDt$Child3  # Choose % of kids under 3y/o in daycare 
chU6  = lvbInDt$Child6  # Choose % of kids 3-6y/o in daycare 
doc   = PerCapita(lvbInDt$Doctors)  # Cal. number of doctors/cap
actSn = lvbInDt$SenSport  # Choose % of active seniors 
actJn = lvbInDt$JunSport  # Choose % of active juniors
trf   = PerCapita(lvbInDt$Accidents)  # Calc. nr of traff. acc/cap 
strCr = PerHa(lvbInDt$Crossings)  # Calc.nr street cross/ha
crm   = lvbInDt$Crime  # Choose nr of criminal offences per 10K ppl
socHl = PerCapita(lvbInDt$SocHelp)  # Cal. social help rec. / cap
dsb   = lvbInDt$Disabled1000 # Choose sev. handicapped/ 1K ppl
emp   = lvbInDt$Employment  # Choose emp./ 1K ppl capable of emp.
comp  = lvbInDt$Company  # Choose nr of companies
txRv  = lvbInDt$Revenue  # Choose taxable revenue 
bnk   = (lvbInDt$Bankruptcy)/(lvbInDt$Company) # Cal. nr bankr./nr.comp. 
grSp  = PerHa(lvbInDt$GreenSp)  #Cal. green space / ha
# Cal. ratio of agricultural surface vs. residential & traffic surf.
agrRe = (lvbInDt$AgrSurface)/(lvbInDt$RsSurface)
tr    = lvbInDt$Trees # Choose nr of trees per km of the road
pm10  = lvbInDt$PM10  # Choose PM10 avg. level
pm25  = lvbInDt$PM25  # # Choose PM2.5 avg. level

InDt = data.frame()
#============================NEGATIVE INDICATORS================================

# Most of the indicators contribute positively to the index, the higher the 
# value the better the district should be rank. Here are 6 indicators, which
# contribute negatively to the index calculation

 
NgtInd = list(a = which(colnames(IndDt)=="dns"), 
              b = which(colnames(IndDt)=="trf"),
              c = which(colnames(IndDt)=="bnk"),
              d = which(colnames(IndDt)=="crm"),
              e = which(colnames(IndDt)=="grdSz"),
              f = which(colnames(IndDt)=="pm25"),
              g = which(colnames(IndDt)=="pm10"))

 
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


#==================DEVIDING INDICATORS ACCORDING TO THE PILLARS=================

phys1Inc = cbind( )
phys2Inc = cbind( )
ecoInc = cbind( )
envInc = cbind( )

### number of indicators for each category
p1_ind_nr=4
p2_ind_nr=4
soc_ind_nr=16
eco_ind_nr=4
env_ind_nr=5 ## Alex

# ======================CALCULATING SUB-INDICATORS============================== 


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





