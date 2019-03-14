Name of Quantlet: SPL_BerlinDst_Liv_Index_Calc

Published in: 'Statistical Programming Languages - Student Project on ''Livability of Berlin Districts: Comparative Analysis'' '

Description: Calculation of Berlin Districts Livability Index based on previously prepared data. Calculation of Sub-Indexes, Pillars Contributions and Total Livability Index. Visualisation of final results.

Keywords: index calculation, data normalization, data visualisation, boxplot, barplot  

Author: Malgorzata Paulina Olesiewicz

See also: other Quantlets in this project

Submitted: 15.03.2019

Datafile: SPL_BerlinDst_Data_Desc.csv

Output:  
- Index_Score_Data.csv
- SPL_BerlinDst_Liv_Index.csv
- Sub-Index Boxplot.png
- Total Index BarPlot.png
- Per Pillar BarPlot.png

![alt text](https://github.com/Olesiewitch/SPL_BerlinDst/blob/master/SPL_BerlinDst_Liv_Index_Calc/Per%20Pillar%20BarPlot.png)
![alt text](https://github.com/Olesiewitch/SPL_BerlinDst/blob/master/SPL_BerlinDst_Liv_Index_Calc/Sub-Index%20Boxplot.png)
![alt text](https://github.com/Olesiewitch/SPL_BerlinDst/blob/master/SPL_BerlinDst_Liv_Index_Calc/Total%20Index%20BarPlot.png)

## R Code 
```
#===================== PREPARING THE ENVIROMENT ================================
#setwd("~/SPL_BerlinDst")
# The code was tested at HU PC Pool 25

install.packages("ggplot2")
install.packages("reshape2")
install.packages("rlang")
install.packages("dplyr")
install.packages("tidyr")
install.packages("xlsx")
install.packages("ggthemes")
install.packages("xtable")
install.packages("magrittr")   # Install pakages required 

library(ggplot2)
library(magrittr)
library(rlang)
library(dplyr)
library(tidyr)
library(ggthemes)
library(xlsx)
library(reshape2)
library(xtable)  # Read in required packages

options(xtable.floating = FALSE)
options(xtable.timestamp = "")
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
    # between 0 and 1 for every observation. The highest value among the
    # vector recives 1, the lowest 0.    
    # 
    # Args: 
    #      x: the vectors of which values are to be normalized. The data type 
    #      must be numeric. 
    #
    # Returns: 
    # Vector of normalized data of vector x 
    (x-min(x))/(max(x)-min(x))
}

NormalizeNegative=function(x){
    # Function normalizes the data in the vector x , by assigning the values 
    # between 0 and 1 for every observation.. The highest value among the vector
    # recives 0, the lowest 1.   
    #  
    # Args: 
    #      x: the vectors of which values are to be normalized. The data type 
    #      must be numeric. 
    #
    # Returns: 
    # Vector of normalised data of vector x 
    (max(x)-x)/(max(x)-min(x))
}

IsOutlier =  function(y) {
    # Function checks if the given observation in vector y is an outlier or not.
    # The outlier is defined as an observation smaller than the first quantile 
    # minus 1,5 times the interquartile range of the vector OR bigger than than
    # the third quantile plus the 1.5 times the interquartile range of the vector.  
    # The interquartile range is calculated using IQR function. 
    # For more information see ?IQR.  
    #
    # Agrs: 
    #      y: vector of data for which outliers are to be found. The type of 
    #         data must be numeric.  
    #
    # Returns: Logical statment "TRUE" for the outlier, "FALSE"  otherwise
    
    return(y < quantile(y,
                        0.25) - 1.5 * IQR(y) | 
               y > quantile(y, 0.75) + 1.5 * IQR(y))
}
#======================CALCULATING INDEX INDICATORS============================= 

#Create indicators data frame 

indDt = data.frame(lvSpc = PerCapita(lvbInDt$SpacePC),  # Calc. liv space/cap.
                  hsAv   = PerCapita(lvbInDt$Flats),  # Calc. nr of flats/cap.
                  dns    = PerHa(lvbInDt$Population),  # Calc. population per ha
                  hsAl   = lvbInDt$Hausehold, # Choose hous. all. 
                  trnDn  = PerHa(lvbInDt$Transport),  # Calc. den. of pub. trans.
                  bkLn   = PerHa(lvbInDt$Cycle),  # Calc. den. of cycling lines
                  crChr  = PerHa(lvbInDt$Charging),  #  Calc.e-car char.stat./ha  
                  prkSp  = PerCapita(lvbInDt$Parking),  # Calc.nr park. spc./ cap
                  trs    = PerCapita(lvbInDt$Tourists),  # Calc.nr of tour. /cap.    
                  # Calculate the hotel occupancy by deviding the total num.   
                  # of stays by total capacity; number of beds multiplied   
                  # by 365 days in the year
                  htlOc  = lvbInDt$Stays/(lvbInDt$Hotel*365), 
                  sprCl  = PerHa(lvbInDt$Sport),  # Choose nr of sport clubs/ ha
                  res    = PerHa(lvbInDt$Restaurants),  # Cal.nr of restaur./ha
                  std    = lvbInDt$Puplis1000,  # Choose nr of pupils/ K of ppl
                  grdSz  = lvbInDt$Pupils/lvbInDt$Grades,  # Cal. avg. grade size          
                  chU3   = lvbInDt$Child3,  # Choose % of kids < 3y/o in daycare 
                  chU6   = lvbInDt$Child6,  # Choose % of kids 3-6y/o in daycare 
                  doc    = PerCapita(lvbInDt$Doctors),  # Cal.num. of doctors/cap
                  actSn  = lvbInDt$SenSport,  # Choose % of active seniors 
                  actJn  = lvbInDt$JunSport,  # Choose % of active juniors
                  trf    = PerCapita(lvbInDt$Accidents),  # Calc. traff. acc/cap 
                  strCr  = PerHa(lvbInDt$Crossings),  # Calc.nr street cross/ha
                  crm    = lvbInDt$Crime,  # Choose criminal offences per 10K ppl
                  socHl  = PerCapita(lvbInDt$SocHelp),  # Cal.soc. help rec. /cap
                  dsb    = lvbInDt$Disabled1000, # Choose sev. handicapped/1K ppl
                  emp    = lvbInDt$Employment,  # Choose emp./ 1K ppl cap. of emp.
                  comp   = lvbInDt$Company,  # Choose nr of companies
                  txRv   = lvbInDt$Revenue,  # Choose taxable revenue 
                  # Cal. nr bankr./nr.comp. 
                  bnk    = (lvbInDt$Bankruptcy)/(lvbInDt$Company),
                  grSp   = PerHa(lvbInDt$GreenSp),  #Cal. green space / ha
                  # Cal. ratio of agr surface vs. residential & traffic surf.
                  agrRe  = (lvbInDt$AgrSurface)/(lvbInDt$RsSurface),
                  tr     = lvbInDt$Trees, # Choose nr of trees per km of the road
                  pm10   = lvbInDt$PM10,  # Choose PM10 avg. level
                  pm25   = lvbInDt$PM25)  # # Choose PM2.5 avg. level


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
# negatively to the Index. For each indicatore, strore the score in new column 
# where letters "Scr" are added to the original name of the indicator.

for (i in 1:(ncol(indDt))){  # Run function for every column in the indDt file
    if ((i) %in% ngtInd){
        indDt[,paste(colnames(indDt[i]),"Scr")] =  # create new col. with "score"
             NormalizeNegative(indDt[i])  # If indicator in ngtInd use Neg. fun
    } else {
          indDt[,paste(colnames(indDt[i]),"Scr")] = 
               NormalizePositive(indDt[i]) # If not in ngtInd use Pos. fun
    }
}

#==============  CREATING FINAL DATA FRAME FOR INDEX CALCULATION =============== 

Nr = lvbInDt$Nr  # Choose district numbers  

District = lvbInDt$District  # Choose district names

# Create the final data frame and select only the variables' scores and not 
# absolute values 

indScrDt = data.frame(Nr, District,indDt,stringsAsFactors = FALSE) %>% 
    select("Nr", "District", grep("Scr", names(.)))

# Write csv file with the data 

write.csv2(indScrDt,"SPL_BerlinDst_Liv_Index_Calc/Index_Sore_Data.csv")

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
    select("lvSpc.Scr",
           "hsAv.Scr",
           "dns.Scr",
           "hsAl.Scr")  # Select Indicators of Physical 1 Sub-Index 

phy2Ind = indScrDt %>%
    select( "trnDn.Scr",
            "bkLn.Scr",
            "crChr.Scr", 
            "prkSp.Scr") # Select Indicators of Physical 2 Sub-Index 

socInd = indScrDt %>%
    select("trs.Scr",
           "htlOc.Scr",
           "sprCl.Scr",
           "res.Scr",
           "std.Scr",
           "grdSz.Scr",
           "chU3.Scr",
           "chU6.Scr",
           "doc.Scr",
           "actSn.Scr",
           "actJn.Scr",
           "trf.Scr",
           "strCr.Scr",
           "crm.Scr", 
           "socHl.Scr",
           "dsb.Scr")  # Select Indicators of Social Sub-Index 

ecoInd = indScrDt %>%
    select("emp.Scr",
           "comp.Scr",
           "txRv.Scr",
           "bnk.Scr")  # Select Indicators of Economic Sub-Index 

envInd = indScrDt %>%
    select("grSp.Scr",
           "agrRe.Scr",
           "tr.Scr",
           "pm10.Scr",
           "pm25.Scr")  # Select Indicators of Enviromental Sub-Index 

# =======================CALCULATING SUB-INDEXES================================ 

Phy1In = apply(phy1Ind, 1, sum)
Phy2In = apply(phy2Ind, 1, sum)
SocIn  = apply(socInd, 1, sum)
EcoIn  = apply(ecoInd, 1, sum)
EnvIn  = apply(envInd, 1, sum)


# ======================CALCULATING LIVIBILITY INDEX ===========================

# Calculate the contribution of each pillar to the Liveability Index 

Pllrs= data.frame(PhyPl   = (Phy1In*phys1W + Phy2In*phys2W), 
                    SocPl = SocIn*socW,
                    EcoPl = EcoIn*ecoW,
                    EnvPl = EnvIn*envW)

# Calculate total Liveability Index 
    
TotalIn = apply(Pllrs,1, FUN = sum)   

# Creat final data frame with the results 

RsltDt = data.frame(District,
                     Phy1In,  
                     Phy2In, 
                     SocIn,  
                     EcoIn,  
                     EnvIn,
                     Pllrs,
                     TotalIn)
# Write csv file

write.csv2(RsltDt, "SPL_BerlinDst_Liv_Index_Calc/SPL_BerlinDst_Liv_Index.csv")

# Read in best with: 
# read.csv("SPL_BerlinDst_Liv_Index_Calc/SPL_BerlinDst_Liv_Index.csv", 
# sep = ";", dec = ",", row.names = 1, stringsAsFactors = FALSE)

#======================= CALCULATE MAXIMUM SCORE ===============================

MaxScoreIndex = sum(length(phy1Ind)*phys1W,length(phy2Ind)*phys2W,
                    length(socInd)*socW, length(ecoInd)*ecoW,
                    length(envInd)*envW)  # Calculate max score of Total Index

# Creat data frame with max score for each result 

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

colnames(MaxScore) = colnames(RsltDt)  #  sure that the col. names are the same

#============================= RESULTS =========================================
# See results for sub-Indexes, Pilars and TotaL Liveability Index

View(RsltDt)  

# Get the latex code for the report

xtable(RsltDt)
xtable(MaxScore)  


#============================= BOXPLOT =========================================

subMlt = melt(RsltDt[, -c(7:11)],id.vars = "District")  # Melt Sub-Index Results

Districtlable = as.character(subMlt$District)  # Convert district names to chr. 

# Create final data frame for plotting, group the data according to the
# Sub-Index (variable) and creat new column which checks for outliers and
# if "TRUE" returns the name of the District

subMltDt = data.frame(Districtlable,subMlt[, -1],stringsAsFactors = FALSE)%>%
    group_by(variable) %>%
    mutate(Outlier = ifelse(IsOutlier(value), Districtlable , ""))


ggplot(subMltDt, aes(x=variable,y=value, group = variable)) +  # Create ggplot 
    geom_boxplot(aes(fill = variable)) +  # Create boxplot for every Sub-Index
    # Add District lebels for outliers
    geom_text(aes(label = Outlier), size = 3,  vjust = - 0.5) + # Plot the outliers   
    theme_bw() +  # Choose black and white theme 
    scale_fill_economist(labels = c("Housing","Infrastructure",  # Choose color palet
                                    "Social", "Economic", 
                                    "Enviromental")) +  # Names the labes
    labs(x = "Sub-Index", y = "Score") +  # Name the axises
    theme(panel.grid.minor = element_blank(),  # Remove the minor grid
          panel.grid.major = element_blank(),  # Remove the major grid
          axis.title.x     = element_text(size = 10), # X axis lebel font size
          axis.title.y     = element_text(size = 10), # Y axis lebel font size
          axis.text.x      = element_blank(),  # Remove x axis labels
          legend.title     = element_blank(), # Remove legent title
          legend.text      = element_text(size = 9),  # Legend font size
          legend.position  = "bottom",  # Place the legend on the graph
          legend.box       = "horizontal")  # Horizontally
 
# Print the plot   

ggsave("Sub-Index Boxplot.png", plot = last_plot(),scale = 1, device = "png", 
       path = "SPL_BerlinDst_Liv_Index_Calc/")

dev.off()

#============================ BAR PLOTS ========================================

TtlMltDt = melt(RsltDt[, -c(2:6,11)],id.vars = "District") # Melt Pillars Data


# Creat showing all individual pillars 

ggplot(TtlMltDt, aes(x = District,y = value, fill = variable)) +  # Creat ggplot 
    geom_bar(stat = "identity", position = "dodge", width = 0.8)+  # Creat barplot 
    theme_bw() +  # Use black and white theme
    scale_fill_economist(labels = c("Physical","Social",# Chose color palet
                                    "Economic", "Enviromental")) +  # Chose color palet 
    labs(y = "Liveability Index: Pillars Comparison") +  # Add x axis label 
    theme(panel.grid.minor = element_blank(),  # Remove the minor grid
          panel.grid.major = element_blank(),  # Remove the major grid
          legend.position  = "bottom",  # Place the legend on the graph
          legend.box       = "horizontal",  # Horizontally
          axis.title.x     = element_text(size = 10), # X axis lebel font size
          legend.title     = element_text(size = 8),  # Legend title font size
          legend.text      = element_text(size = 8),  # Legend font size
          axis.title.y     = element_blank()) +  # Remove y axis labels
    guides(fill=guide_legend(title="Pillars:")) +  # Add legent title
    coord_flip()  # Flip the chart to be horizontal

# Print the plot

ggsave("Per Pillar BarPlot.png", plot = last_plot(),scale = 1, device = "png", 
       path = "SPL_BerlinDst_Liv_Index_Calc/")

dev.off()

# Creat Cummulative Index Plot

ggplot(TtlMltDt, aes(x = District,y = value, fill = variable)) +  # Creat ggplot 
    geom_bar(stat = "identity", width = 0.5)+  # Creat barplot 
    theme_bw() +  # Use black and white theme
    scale_fill_economist(labels = c("Physical", "Social",  # Chose color palet
                                    "Economic", "Enviromental")) +  # Name labels   
    labs(y = "Total Liveability Index") +  # Add x axis label 
    theme(panel.grid.minor = element_blank(),  # Remove the minor grid
          panel.grid.major = element_blank(),  # Remove the major grid
          legend.position  = "bottom",  # Place the legend on the graph
          legend.box       = "horizontal",  # Horizontally
          axis.title.x     = element_text(size = 10), # X axis lebel font size
          legend.title     = element_text(size = 8),  # Legend title font size
          legend.text      = element_text(size = 8),  # Legend font size
          axis.title.y     = element_blank()) +  # Remove y axis labels
    guides(fill=guide_legend(title="Pillars:")) +  # Add legent title
    coord_flip()  # Flip the chart to be horizontal

# Print the plot

ggsave("Total Index BarPlot.png", plot = last_plot(),scale = 1, device = "png", 
       path = "SPL_BerlinDst_Liv_Index_Calc/")

dev.off()

# ========================END OF THE SCRIPT=====================================
# ==============================================================================
```
