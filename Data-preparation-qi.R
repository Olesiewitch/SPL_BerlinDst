---
title: "Data-preparation-qi"
author: "Wu Qi"
date: "3/3/2019"
---
    
##install and run pacages
    
packages = c("rvest","readxl","magrittr","dplyr") 
for (i in packages) {                          
    if(!require(i, character.only=TRUE))
    {install.packages(i, character.only=TRUE)}
    library(i, character.only=TRUE)
}

## list district names
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

# get Ortsteil of district using package rvest
Oteil = read_html(paste0("https://de.wikipedia.org/wiki/",
                            "Liste_der_Bezirke_und_Ortsteile_Berlins"))  

Oteil = Oteil %>% 
              html_nodes("table")%>% 
              .[[3]] %>%  #the third table in this website give us a dataframe 
              # witch lists all Orststeile in Berlin and their corresponding districts
              html_table()

OteilD = c()
i=3
for (i in 1:12){ 
    OteilD[i] = c()
    j = 1
    while(Oteil[j,'Bezirk'] %in% Dstc[i,"District"]){
        OteilD[i] = rbind(OteilD[i],Oteil[j,'Ortsteil'])
        j =j+1
    }; OteilD[i] = as.vector(OteilD[i])
}


OteilD = c()
j = 1
while(Oteil[j,'Bezirk'] %in% Dstc[1,"District"]){
    OteilD = rbind(OteilD,Oteil[j,'Ortsteil'])
    j =j+1
};OteilD = as.vector(OteilD)


# input: district names
# output: standard names: lower case & no space & no Umlauts 
#DistNm = function(NmClmn){
#    NmClmn = tolower(NmClmn) 
#    sub = c("ö","ä","ß","ü"," ")
#    Pattern = paste(sub, collapse="|")
#    for(grepl(Pattern, NmClmn)){
#        gsub("ö","oe",NmClmn)
#        gsub("ö","ue",NmClmn)
#        gsub("ö","ae",NmClmn)
#        gsub("ö","ss",NmClmn)
#        gsub(" ","-",NmClmn)
#    }
#}

# Set working directory
wd = "~/SPL-Project/Archive/Dirstrict Data/"

## charging station data

Cgst = read_excel(paste0(wd,"ladestationen.xls"))
# extract post code in column 'ladestationen$Adresse', where information is listed as 
# e.g. "Malteserstrasse 136<U+2013>138, 12249 Berlin", and create a new column with post code

Adss=strsplit(Cgst$Adresse,"[,]")
Adss1=c()
for(i in 1:length(Adss)) {
    Adss1[i]<- Adss[[i]][2] 
}   ;  Adss1                           
Adss1<-as.character(Adss1)
Adss1<-strsplit(Adss1," ")
Adss11<-c()
for(i in 1:length(Adss)) {
    Adss11[i]<- Adss1[[i]][2] 
}   

#create a new dataframe with the name ladestationen1 with the new column of post code
Cgst1=data.frame(Cgst,Adss11)

#extract the post code of each district from the original excel file 'ZuordnungderBezirkezuPostleitzahlen'

Pscd = read_excel(paste0("~/SPL-Project/Archive/Dirstrict Data/"
                         ,"ZuordnungderBezirkezuPostleitzahlen.xls"))

Dstc01<-as.numeric(unlist(as.list(Pscd[6:8,3:12])))
Dstc02<-as.numeric(unlist(as.list(Pscd[10:11,3:12])))
Dstc03<-as.numeric(unlist(as.list(Pscd[13:15,3:12])))
Dstc04<-as.numeric(unlist(as.list(Pscd[17:20,3:12])))
Dstc05<-as.numeric(unlist(as.list(Pscd[22:23,3:12])))
Dstc06<-as.numeric(unlist(as.list(Pscd[25:27,3:12])))
Dstc07<-as.numeric(unlist(as.list(Pscd[29:32,3:12])))
Dstc08<-as.numeric(unlist(as.list(Pscd[34:36,3:12])))
Dstc09<-as.numeric(unlist(as.list(Pscd[38:39,3:12])))
Dstc10<-as.numeric(unlist(as.list(Pscd[41:42,3:12])))
Dstc11<-as.numeric(unlist(as.list(Pscd[44:45,3:12])))
Dstc12<-as.numeric(unlist(as.list(Pscd[47:48,3:12])))

Pscd<- as.data.frame(cbind(Dstc01,Dstc02,Dstc03,Dstc04,
                           Dstc05,Dstc06,Dstc07,Dstc08,
                           Dstc09,Dstc10,Dstc11,Dstc12))


##assign districts to every charging station and count for each districs

#compare the post code of each ladestation, and assgin a district to each ladestation
DsLd= rep(0,length(Cgst1$Adss11))
for(i in 1:length(Cgst1$Adss11)){
    for(j in 1:12){
        if(Cgst1$Adss11[i] %in% Pscd[,j]){
            DsLd[i]=j
        }
    }
}
sum(DsLd!=0)

#count the number of ladestation in each district
Nrct=rep(0,12)
for(i in 1:12){
    for(j in 1:length(DsLd)){
        if(DsLd[j]==i)
            Nrct[i]=Nrct[i]+1
    }
};Nrct

#count_chargingsta<- data.frame(Dstc,Nrct)
#write.csv(count_chargingsta,"~/SPL-Project/Dirstrict Data/chargingstations.csv")


## restaurant 

stadtteile = read_html("https://www.berlin.de/restaurants/stadtteile/")

stadtteileList = stadtteile %>% html_nodes("br+ .decoda-list a") %>% html_text() 
stadtteileList[12] = "Prenzlauer-Berg"


stadtteileList = gsub("ö", "oe", stadtteileList)  # substitute germanletters
stadtteileList = gsub("ß","ss",stadtteileList)  # substitute germanletters
stadtteileList

restaurant_html = c()
for (i in 1: length(stadtteileList)){
    restaurant_html[i] = paste0("https://www.berlin.de/restaurants/stadtteile/",
                                stadtteileList[i],"/")
}
Nrrs=rep(0,length(stadtteileList))


for (i in 1: length(stadtteileList)){
    Nrrs[i] = length(read_html(restaurant_html[i]) %>% 
                         html_nodes(".main-content .list--arrowlist a") %>%
                         html_text() )
    if(Nrrs[i]==0){
        Nrrs[i] = length(read_html(restaurant_html[i]) %>% 
                             html_nodes(".basis .heading a") %>%
                             html_text() )
    }
    if(Nrrs[i]==1){
        Nrrs[i] = length(read_html(restaurant_html[i]) %>% 
                             html_nodes(".basis .heading a") %>%
                             html_text() )
    }
}; Nrrs


# assgin 23 stadtteile to 12 districts



StDt = read_html("https://reise.naanoo.de/berlin/berliner-bezirke")
StDt = StDt %>% html_nodes("p+ p , p strong") %>% html_text()
#StDt %<>% html_nodes("p+ p , p strong") %<>% html_text()
StDt

Dstc[1]
Inx = which(grepl("Bezirk",StDt)) 
# 1  3  4  6  7  9 10 12 13 15 16 18 19 21 22 24 25 27 28 30 31 33 34

for(i in Inx){
    StDt= StDt[Inx[i]+1]
}


# summing up from stadtteile to districts


## cycling length

Cyll = c(169677.40,90134.7,161242.6,169577.9,165372.5,194599.2,
         148588.8,104444, 202059.2, 118019.1, 100257, 140794.4
)

## nr.doctor

# Nr. of house doctor per 10,000 people 
Nrdr<-c(66.7,66.6,65.3,86.7,63.2,67.3,73.5,59.5,53.2,61.8,51.8,64.2)  

## street crossings

# street crossings per district 
Nrsc<-c(33, 39, 54, 35, 22, 31, 19, 25, 32, 39, 22, 30)  

## merging the data

#QiDt = cbind(Nrct, Nrrs, Cyll, Nrdr, Nrsc)
Nr = 1:12
QiDt = data.frame(Nr,Dstc$District, Nrct, Cyll, Nrdr, Nrsc)
colnames(QiDt) = c("District Nr.",
                   "District",
                   "Nr. of charging stations",
                   #"Nr. of restaurants",
                   "cycling length",
                   "Nr. of doctors",
                   "Nr. of street crossings"
)

QiDt = as.data.frame(QiDt); QiDt

write.csv(QiDt,"SPL_BerlinDst_Data_prep2_qi.csv")
