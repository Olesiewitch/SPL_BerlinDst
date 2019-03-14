Name of Quantlet: SPL_BerlinDst_Data_Prep_2

Published in: 'Statistical Programming Languages - Student Project on ''Livability in Berlin Districts: Comparative Analysis'' '

Description: Generates csv file for livability calculation from the raw data and web data

Keywords: reading and writing data, web scraping  

Author: Wu Qi

See also: other Quantlets in this project

Submitted: 15.03.2019

Datafile: 
- Radverkehrsanlagen.xls
- ZuordnungderBezirkezuPostleitzahlen.xls
- ladestationen.xls

Output:  
- SPL_BerlinDst_Data_Prep_2.csv


# R Code
```
#========================= Set working directory================================

setwd("~/SPL_BerlinDst")

#=========================install and run pacages===============================

packages = c('rvest','readxl','magrittr','dplyr') 
install.packages('rvest')
install.packages('readxl')
install.packages('magrittr')
install.packages('dplyr')
install.packages("xlsx")
library(rvest)
library(readxl)
library(magrittr)
library(dplyr)
library(xlsx)

#=============================list district names===============================
Dstc = c("Mitte",
         "Friedrichshain-Kreuzberg",
         "Pankow",
         "Charlottenburg-Wilmersdorf",
         "Spandau",
         "Steglitz-Zehlendorf",
         "Tempelhof-Schoeneberg",
         "Neukoelln",
         "Treptow-Koepenick",
         "Marzahn-Hellersdorf",
         "Lichtenberg",
         "Reinickendorf"
)
Dstc = as.data.frame(Dstc)
colnames(Dstc) = c("District"); Dstc

#======================Ortsteil in every district ==============================

# Ortsteil is the German word for subdistricts

# using package rvest to read internet page
Oteil = read_html(paste0("https://de.wikipedia.org/wiki/",
                         "Liste_der_Bezirke_und_Ortsteile_Berlins"))  

Oteil = Oteil %>% 
    html_nodes("table")%>% 
    .[[3]] %>%  #the third table in this website give us a dataframe 
    # lists all Orststeile in Berlin and their corresponding districts
    html_table()

# define a function to replace all german letter which could causes issues
Replace = function(clmn) {
    # author: Aleksandra Kudaeva
    # Input:  column where you want to replace umlauts
    # Output: column without umlauts
    # check if at least one element of a vector has any umlauts in it
    # replaces umlauts until there are no one left
    while(any(grepl("ä|ö|ü|ß| ",clmn)) == TRUE) {
        clmn  %<>% 
            sub("ä", "ae", .) %<>% 
            sub("ö", "oe", .) %<>% 
            sub("ü", "ue", .) %<>% 
            sub("ß", "ss", .) %<>%
            sub(" ", "-", .) # also replace all space with '-'
    }
    return(clmn)
}

# replace all the german letters in the dataframe
Oteil$Bezirk = Replace(Oteil$Bezirk)
Oteil$Ortsteil = Replace(Oteil$Ortsteil)

# create a dataframe OteilD which contains Ortsteil in 12 districts
OteilD = data.frame(matrix(ncol = length(Dstc$District),
                           nrow = 20))  
# nrow can be any number bigger than the maxi number of Ortsteil in district

j = 1  # start with the first observation in dataframe Oteil

for (i in 1:length(Dstc$District)){  # a loop with number of districts
    
    j = j  # every loop start with current j value where last while loop ends
    k = 1  # k start from 1 for every column in the new dataframe OteilD
    
    while(Oteil[j,'Bezirk'] == Dstc[i,"District"]){
        OteilD[k,i] = Oteil[j,'Ortsteil']
        k = k+1
        j = j+1
    }
}  # ignore the error message hier : missing value where TRUE/FALSE needed

colnames(OteilD) = Dstc$District
View(OteilD)

#============================charging station===================================

Cgst = read_excel("SPL_BerlinDst_Data_Prep_2/ladestationen.xls")
# if error appears here, then use:
#read.xlsx("SPL_BerlinDst_Data_Prep_2/ladestationen.xls",
#          encoding = "UTF-8",
#          as.data.frame = TRUE)

# extract post code in column 'Cgst$Adresse', 
# where information listed as e.g."Malteserstrasse 136<U+2013>138, 12249 Berlin" 
# and create a new column only with post code

Adss = strsplit(Cgst$Adresse,"[,]")  # returns a list

# next step:extract info we need from the list and create a vector to store them

Adsscl = c()  # create a column to store the postcode together with 'Berlin' 

for(i in 1:length(Adss)) {
    Adsscl[i]= Adss[[i]][2]  # the second element has the postcode information 
}   ;  Adsscl                      

Adsscl = as.character(Adsscl)  # need charachter for fct strsplit

Adsscl = strsplit(Adsscl," ") # split postcode and 'Berlin', return a list

# same as before extract info from list and create a vector to store them
Pcd = c()
for(i in 1:length(Adss)) {
    Pcd[i] = Adsscl[[i]][2] # the second element has the postcode information
}   

#create new dataframe with name CgstN with the new column of post code
CgstN = data.frame(Cgst,Pcd)

# extract post code of district from file 'ZuordnungderBezirkezuPostleitzahlen'

Pscd = read_excel(paste0("SPL_BerlinDst_Data_Prep_2/",
                         "ZuordnungderBezirkezuPostleitzahlen.xls"))
Pscd

# if error appears here, then use:
#read.xlsx(paste0("SPL_BerlinDst_Data_Prep_2/",
#                 "ZuordnungderBezirkezuPostleitzahlen.xls"),
#          encoding = "UTF-8",
#          as.data.frame = TRUE)


# create 12 vector to store the Post code for each district
# 3:12 are the columns with post code infomation
Dstc01 = as.numeric(unlist(as.list(Pscd[6:8,3:12]))) 
Dstc02 = as.numeric(unlist(as.list(Pscd[10:11,3:12])))  
# ignore warning of NA since it doesn't effect our use of the data
Dstc03 = as.numeric(unlist(as.list(Pscd[13:15,3:12])))
Dstc04 = as.numeric(unlist(as.list(Pscd[17:20,3:12])))
Dstc05 = as.numeric(unlist(as.list(Pscd[22:23,3:12])))
Dstc06 = as.numeric(unlist(as.list(Pscd[25:27,3:12])))
Dstc07 = as.numeric(unlist(as.list(Pscd[29:32,3:12])))
Dstc08 = as.numeric(unlist(as.list(Pscd[34:36,3:12])))
Dstc09 = as.numeric(unlist(as.list(Pscd[38:39,3:12])))
Dstc10 = as.numeric(unlist(as.list(Pscd[41:42,3:12])))
Dstc11 = as.numeric(unlist(as.list(Pscd[44:45,3:12])))
Dstc12 = as.numeric(unlist(as.list(Pscd[47:48,3:12])))

# ignore warning since we don't mind repetition of same value in the same column
Pscd= as.data.frame(cbind(Dstc01,Dstc02,Dstc03,Dstc04,
                          Dstc05,Dstc06,Dstc07,Dstc08,
                          Dstc09,Dstc10,Dstc11,Dstc12))  

# assign districts to every charging station and count for each districs

# compare post code of each Charging station, assgin a district to each 
DsLd = rep(0,length(CgstN$Pcd))

for(i in 1:length(CgstN$Pcd)){
    for(j in 1:12){
        if(CgstN$Pcd[i] %in% Pscd[,j]){
            DsLd[i]=j
        }
    }
}
# check if all charging stations is assigned to a district
(DsLd !=0) == length(CgstN$Pcd) # the result should all be False

# count the number of charging station in each district
Nrct=rep(0,12)
for(i in 1:12){
    for(j in 1:length(DsLd)){
        if(DsLd[j]==i)
            Nrct[i]=Nrct[i]+1
    }
};Nrct


#=================================restaurant====================================

Steil = read_html("https://www.berlin.de/restaurants/stadtteile/")

SteilList = Steil %>% html_nodes("br+ .decoda-list a") %>% html_text() 

SteilList = Replace(SteilList); SteilList

R_html = c()
for (i in 1: length(SteilList)){
    R_html[i] = paste0("https://www.berlin.de/restaurants/stadtteile/",
                       SteilList[i],"/")
}
Nrrs = rep(0,length(SteilList))

system.time(  # this takes a short while so I calculated the system time
    for (i in 1: length(SteilList)){
        Nrrs[i] = length(read_html(R_html[i]) %>% 
                             html_nodes(".main-content .list--arrowlist a") %>%
                             html_text() )
        if(Nrrs[i]==0){
            Nrrs[i] = length(read_html(R_html[i]) %>% 
                                 html_nodes(".basis .heading a") %>%
                                 html_text() )
        }
        if(Nrrs[i]==1){
            Nrrs[i] = length(read_html(R_html[i]) %>% 
                                 html_nodes(".basis .heading a") %>%
                                 html_text() )
        }
    }); Nrrs  # Nrrs gives the number of each restaurant in each Ortsteil
RestO = cbind(SteilList, Nrrs)

# calculate nr. of restaurants in every district
Rest = numeric(length = 12)
for(i in 1:length(Nrrs)){
    for(j in 1:12){
        if(RestO[i,'SteilList'] %in% OteilD[,j])
            Rest[j] = Rest[j] + as.numeric(RestO[i,'Nrrs'])
    }
}; Rest 


#===============================cycling length==================================

# data obtained from <http://www.stadtentwicklung.berlin.de//geoinformation/fis-broker/>

Rad = read_excel(paste0("SPL_BerlinDst_Data_Prep_2/Radverkehrsanlagen.xls"))

# if error appears here, then use:
#read.xlsx(paste0("SPL_BerlinDst_Data_Prep_2/",
#                 "Radverkehrsanlagen.xls"),
#          encoding = "UTF-8",
#          as.data.frame = TRUE)

View(Rad)  # have a look at the data 
unique(Rad$`RVA-Typ`)  # have a better understanding of the data

# select the column in dataset we need
Rad = Rad %>% select('Bezirk','Länge [m]')

Rad$Bezirk = Replace(Rad$Bezirk)  # replace all German letters in Bezirk

CyllVec = list()  # define a list to store all cycling length in districts

for(i in 1:length(Dstc$District)){
    CyllVec[[i]] = Rad$`Länge [m]`[which(Rad$Bezirk == Dstc$District[i])]
}

Cyll = sapply(CyllVec,sum)  # total length in each districts by summing up
Cyll

#===============================nr.doctor=======================================

# Nr. of house doctor per 10,000 people 
# data obtained from 
# <https://www.berlin.de/ba-lichtenberg/auf-einen-blick/buergerservice/gesundheit/artikel.596005.php>

Nrdr = c(66.7,66.6,65.3,86.7,63.2,67.3,73.5,59.5,53.2,61.8,51.8,64.2)  

#===========================street crossings====================================

# data obtained from 
# <http://www.stadtentwicklung.berlin.de//geoinformation/fis-broker/>
Nrsc = c(33, 39, 54, 35, 22, 31, 19, 25, 32, 39, 22, 30)  

#============================merging the data===================================

Nr = 1:12
QiDt = data.frame(Nr,Dstc$District, Nrct, Rest, Cyll, Nrdr, Nrsc)
colnames(QiDt) = c("Nr",
                   "District",
                   "Charging",
                   "Restaurants",
                   "Cycle",
                   "Doctors",
                   "Crossings")

QiDt = as.data.frame(QiDt); QiDt

write.csv(QiDt,"SPL_BerlinDst_Data_Prep_2.csv")



```
