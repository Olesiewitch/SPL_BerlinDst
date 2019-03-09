#===================== PREPARING THE ENVIROMENT ================================

library(xlsx)
library(tidyr)
library(dplyr)
library(xtable)
options(xtable.floating = FALSE)
options(xtable.timestamp = "")
library(reshape2)
library(ggplot2)
library(ggthemes)

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

#======================CALCULATING INDEX INDICATORS============================= 

#Create indicators data frame 

indDt = data.frame(lvSpc = PerCapita(lvbInDt$SpacePC),  # Calc. liv space/cap.
                  hsAv  = PerCapita(lvbInDt$Flats),  # Calc. nr of flats/cap.
                  dns   = PerHa(lvbInDt$Population),  # Calc. population per ha
                  hsAl  = lvbInDt$Hausehold, # Choose hous. all. 
                  trnDn = PerHa(lvbInDt$Transport),  # Calc. den. of pub. trans.
                  bkLn  = PerHa(lvbInDt$Cycle),  # Calc. den. of cycling lines
                  crChr = PerHa(lvbInDt$Charging),  #  Calc.e-car char.stat./ha  
                  prkSp = PerCapita(lvbInDt$Parking),  # Calc.nr park. spc./ cap
                  trs   = PerCapita(lvbInDt$Tourists),  # Calc.nr of tour. /cap.    
                  # Calculate the hotel occupancy by deviding the total num.   
                  # of stays by total capacity; number of beds multiplied   
                  # by 365 days in the year
                  htlOc = lvbInDt$Stays/(lvbInDt$Hotel*365), 
                  sprCl = PerHa(lvbInDt$Sport),  # Choose nr of sport clubs/ ha
                  res   = PerHa(lvbInDt$Restaurants), # Cal.nr of restaurants/ha
                  std   = lvbInDt$Pupils,  # Choose nr of pupils per K of ppl
                  grdSz = lvbInDt$Pupils/lvbInDt$Grades,  # Cal. avg. grade size          
                  chU3  = lvbInDt$Child3,  # Choose % of kids < 3y/o in daycare 
                  chU6  = lvbInDt$Child6,  # Choose % of kids 3-6y/o in daycare 
                  doc   = PerCapita(lvbInDt$Doctors),  # Cal.num. of doctors/cap
                  actSn = lvbInDt$SenSport,  # Choose % of active seniors 
                  actJn = lvbInDt$JunSport,  # Choose % of active juniors
                  trf   = PerCapita(lvbInDt$Accidents),  # Calc. traff. acc/cap 
                  strCr = PerHa(lvbInDt$Crossings),  # Calc.nr street cross/ha
                  crm   = lvbInDt$Crime,  # Choose criminal offences per 10K ppl
                  socHl = PerCapita(lvbInDt$SocHelp),  # Cal.soc. help rec. /cap
                  dsb   = lvbInDt$Disabled1000, # Choose sev. handicapped/1K ppl
                  emp   = lvbInDt$Employment,  # Choose emp./ 1K ppl cap. of emp.
                  comp  = lvbInDt$Company,  # Choose nr of companies
                  txRv  = lvbInDt$Revenue,  # Choose taxable revenue 
                  # Cal. nr bankr./nr.comp. 
                  bnk   = (lvbInDt$Bankruptcy)/(lvbInDt$Company),
                  grSp  = PerHa(lvbInDt$GreenSp),  #Cal. green space / ha
                  # Cal. ratio of agr surface vs. residential & traffic surf.
                  agrRe = (lvbInDt$AgrSurface)/(lvbInDt$RsSurface),
                  tr    = lvbInDt$Trees, # Choose nr of trees per km of the road
                  pm10  = lvbInDt$PM10,  # Choose PM10 avg. level
                  pm25  = lvbInDt$PM25)  # # Choose PM2.5 avg. level


#============================NEGATIVE INDICATORS================================

# Most of the indicators contribute positively to the index, the higher the 
# value the better the district should be rank. Here are 7 indicators, which
# contribute negatively to the index calculation:

 
ngtInd = list(a = which(colnames(indDt)=="dns"), 
              b = which(colnames(indDt)=="trf"),
              c = which(colnames(indDt)=="bnk"),
              d = which(colnames(indDt)=="crm"),
              e = which(colnames(indDt)=="grdSz"),
              f = which(colnames(indDt)=="pm25"),
              g = which(colnames(indDt)=="pm10"))

 
#=========================NORMALIZING THE DATA =================================

# Normalize the data according to whether the indicators contribute positivly or 
# negatively to the Index. for every indicatore, strore the score in new column 
# where letters "Scr" are added to the original name of the indicator

for (i in 1:(ncol(indDt))){
    if ((i) %in% ngtInd){
        indDt[,paste(colnames(indDt[i]),"Scr")] = 
             NormalizeNegative(indDt[i])
        } else {
          indDt[,paste(colnames(indDt[i]),"Scr")] =
               NormalizePositive(indDt[i])
        }
    }

#==============  CREATING FINAL DATA FRAME FOR INDEX CALCULATION =============== 

Nr = lvbInDt$Nr  # Choose district numbers  

District = lvbInDt$District  # Choose district names

# Create the final data frame and select only the variable scores and not 
# absolute values 

indScrDt = data.frame(Nr, District,indDt,stringsAsFactors = FALSE) %>% 
    select("Nr", "District", grep("Scr", names(.)))


write.csv2(indScrDt, "SPL_BerlinDst_Liv_Index_Calc/Index_Sore_Data.csv")

# Read in best with: 
# read.csv("SPL_BerlinDst_Liv_Index_Calc/Index_Sore_Data.csv", 
# sep = ";", dec = ",", row.names = 1, stringsAsFactors = FALSE)
 
#======================== WEIGHTS OF EACH SUB-INDEX ============================

phys1W = c(0.10)
phys2W = c(0.15)
socW   = c(0.25)
ecoW   = c(0.25)
envW   = c(0.25)

#==================DEVIDING INDICATORS ACCORDING TO THE PILLARS=================


phy1Ind = indScrDt %>%
    select("lvSpc.Scr","hsAv.Scr","dns.Scr","hsAl.Scr")

phy2Ind = indScrDt %>%
    select( "trnDn.Scr","bkLn.Scr","crChr.Scr", "prkSp.Scr")

socInd = indScrDt %>%
    select("trs.Scr","htlOc.Scr","sprCl.Scr","res.Scr","std.Scr",
           "grdSz.Scr","chU3.Scr","chU6.Scr", "doc.Scr",
           "actSn.Scr", "actJn.Scr","trf.Scr","strCr.Scr","crm.Scr", 
           "socHl.Scr", "dsb.Scr")

ecoInd = indScrDt %>%
    select("emp.Scr","comp.Scr","txRv.Scr","bnk.Scr")

envInd = indScrDt %>%
    select("grSp.Scr", "agrRe.Scr","tr.Scr","pm10.Scr","pm25.Scr")

# ======================CALCULATING SUB-INDICATORS============================== 

Phy1In = apply(phy1Ind, 1, sum)
Phy2In = apply(phy2Ind, 1, sum)
SocIn   = apply(socInd, 1, sum)
EcoIn   = apply(ecoInd, 1, sum)
EnvIn  = apply(envInd, 1, sum)


# ======================CALCULATING LIVIBILITY INDEX ===========================

# Calculate the contribution of each pilar to the Liveability Index 

Pllrs= data.frame(PhyPl= (Phy1In*phys1W + Phy2In*phys2W), 
                    SocPl = SocIn*socW,
                    EcoPl = EcoIn*ecoW,
                    EnvPl = EnvIn*envW)

# Calculate total Liveability Index 
    
TotalIn = apply(Pllrs,1, FUN = sum)   

RsltDt = data.frame(District,
                     Phy1In,  
                     Phy2In, 
                     SocIn,  
                     EcoIn,  
                     EnvIn,
                     Pllrs,
                     TotalIn)

#==================== CALCULATE MAXIMUM SCORE ==================

MaxScoreIndex = sum(length(phy1Ind)*phys1W,length(phy2Ind)*phys2W,
                    length(socInd)*socW, length(ecoInd)*ecoW,
                    length(envInd)*envW)

    
MaxScore = data.frame("Max Score",
                      length(phy1Ind),
                      length(phy2Ind), 
                      length(socInd),
                      length(ecoInd),
                      length(envInd),
                      (length(phy1Ind)*phys1W + length(phy2Ind)*phys2W),
                      length(socInd)*socW, 
                      length(ecoInd)*ecoW,
                      length(envInd)*envW,
                      MaxScoreIndex,
                      stringsAsFactors = FALSE) 

<<<<<<< HEAD
<<<<<<< HEAD
Results=data.frame(livibility_index$nr,
                   livibility_index$district, 
                   Physical_index_1,
                   Physical_index_2, 
                   Social_index, 
                   Economic_index,
                   Enviromental_index, 
                   Total_Index_Score)

colnames(Results)= c(colnames(livibility_index[1:2]),
                     "Physical Index1", 
                     "Physical Index 2", 
                     "Social Index", 
                     "Economic Index", 
                     "Enviromental Index", 
                     "Total Index Score")

#===============================Analysis========================================

max_score=(p1_ind_nr*phys1_weight)+
          (p2_ind_nr*phys2_weight) +
          (soc_ind_nr*social_weight)+
          (eco_ind_nr*economic_weight)+
          (env_ind_nr*env_weight)        

=======
colnames(MaxScore) = colnames(RESULTS)
>>>>>>> 1e78107ca8e4b2a08bd9c4c6bfb6d4cfce70e8f8
=======
colnames(MaxScore) = colnames(RsltDt)
>>>>>>> 983c5196e054878324beacdffe06d348cf08257e

write.csv2(RsltDt, "SPL_BerlinDst_Liv_Index_Calc/SPL_BerlinDst_Liv_Index.csv")

# Read in best with: 
# read.csv("SPL_BerlinDst_Liv_Index_Calc/SPL_BerlinDst_Liv_Index.csv", 
# sep = ";", dec = ",", row.names = 1, stringsAsFactors = FALSE)
#============================= RESULTS =========================================


View(RESULTS[, 1:6])  # See results for sub-Indexes

xtable(rbind(RESULTS[, 1:6], MaxScore[1:6]))  # Get the latex code for the report (table XX )

View(RESULTS[,-(2:6)])  # See final Index results 

xtable(RESULTS[, -(2:6)])  # Get the latex code for the report (table XX )



#============================= BOXPLOT =========================================


ggplot(data = melt(RESULTS[, -c(1,7:11)]), aes(x=variable,y=value)) +
    geom_boxplot(aes(fill = variable)) + theme_bw() +
    theme(panel.border = element_blank(), panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(), 
          axis.line = element_line(colour = "black")) + coord_flip()


#============================ BAR PLOT =========================================

data.m = melt(RESULTS[, -c(7:11)],id.vars = "District")

ggplot(data.m, aes(x = District,y = value, 
                   fill = variable)) + geom_bar(stat = "identity",
                                                width = 0.5)+ theme_bw() +
    scale_fill_economist() + theme(panel.border = element_blank(),
                                panel.grid.minor = element_blank(),
                                panel.grid.major = element_blank(),
                                legend.position = "bottom",
                                legend.box = "horizontal",
                                axis.title.x=element_blank()) +
    labs(title="Berlin District Liveability Index") +
    guides(fill=guide_legend(title="Sub-Indexes: ")) + 
    coord_flip() 
    
   




    geom_bar(aes(fill = drv), position = position_stack(reverse = TRUE)) +
    coord_flip() +
    theme(legend.position = "top")

