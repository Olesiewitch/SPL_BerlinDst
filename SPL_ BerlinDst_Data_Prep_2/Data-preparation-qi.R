
# title: "Data-preparation-qi"
# author: "Wu Qi"
# date: "3/3/2019"

    
#=========================install and run pacages===============================
    
packages = c('rvest','readxl','magrittr','dplyr') 

for (i in packages) {                          
    if(!require(i, character.only=TRUE))
    {install.packages(i, character.only=TRUE)}
    library(i, character.only=TRUE)
}

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

#using package rvest to read internet page
Oteil = read_html(paste0("https://de.wikipedia.org/wiki/",
                            "Liste_der_Bezirke_und_Ortsteile_Berlins"))  

Oteil = Oteil %>% 
              html_nodes("table")%>% 
              .[[3]] %>%  #the third table in this website give us a dataframe 
              # lists all Orststeile in Berlin and their corresponding districts
              html_table()

#define a function to replace all german letter which could causes issues
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

#replace all the german letters in the dataframe
Oteil$Bezirk = Replace(Oteil$Bezirk)
Oteil$Ortsteil = Replace(Oteil$Ortsteil)

#create a dataframe OteilD which contains Ortsteil in 12 districts
OteilD = data.frame(matrix(ncol = length(Dstc$District),
                           nrow = 20))  
# nrow can be any number bigger than the maximum number of Ortsteil in district

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

#========================= Set working directory================================
#working directory for loading the data
wddt = "~/SPL-Project/SPL_ BerlinDst_Data_Prep_2/"

#============================charging station===================================

Cgst = read_excel(paste0(wddt,"ladestationen.xls"))
# extract post code in column 'ladestationen$Adresse', 
# where information listed as e.g."Malteserstrasse 136<U+2013>138, 12249 Berlin" 
# and create a new column with post code

Adss = strsplit(Cgst$Adresse,"[,]")
Adss1 = c()
for(i in 1:length(Adss)) {
    Adss1[i]= Adss[[i]][2] 
}   ;  Adss1                           
Adss1 = as.character(Adss1)
Adss1 = strsplit(Adss1," ")
Adss11 = c()
for(i in 1:length(Adss)) {
    Adss11[i] = Adss1[[i]][2] 
}   

#create new dataframe with name ladestationen1 with the new column of post code
Cgst1 = data.frame(Cgst,Adss11)

# extract post code of district from file 'ZuordnungderBezirkezuPostleitzahlen'

Pscd = read_excel(paste0(wddt,"ZuordnungderBezirkezuPostleitzahlen.xls"))

# Post code for each district
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

Pscd= as.data.frame(cbind(Dstc01,Dstc02,Dstc03,Dstc04,
                           Dstc05,Dstc06,Dstc07,Dstc08,
                           Dstc09,Dstc10,Dstc11,Dstc12))  
# ignore warning since we don't mind repeating of same value in the same column

##assign districts to every charging station and count for each districs

#compare post code of each ladestation, assgin a district to each ladestation
DsLd = rep(0,length(Cgst1$Adss11))
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

#calculate nr. of restaurants in every district
Rest = numeric(length = 12)
for(i in 1:length(Nrrs)){
    for(j in 1:12){
        if(RestO[i,'SteilList'] %in% OteilD[,j])
            Rest[j] = Rest[j] + as.numeric(RestO[i,'Nrrs'])
    }
}; Rest 


#===============================cycling length==================================

#data obtained from <http://www.stadtentwicklung.berlin.de//geoinformation/fis-broker/>

Rad = read_excel(paste0(wddt,"Radverkehrsanlagen.xls"))
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
#data obtained from 
#<https://www.berlin.de/ba-lichtenberg/auf-einen-blick/buergerservice/gesundheit/artikel.596005.php>

Nrdr = c(66.7,66.6,65.3,86.7,63.2,67.3,73.5,59.5,53.2,61.8,51.8,64.2)  

#===========================street crossings====================================

#data obtained from 
#<http://www.stadtentwicklung.berlin.de//geoinformation/fis-broker/>
Nrsc = c(33, 39, 54, 35, 22, 31, 19, 25, 32, 39, 22, 30)  

#============================merging the data===================================

#QiDt = cbind(Nrct, Nrrs, Cyll, Nrdr, Nrsc)
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

write.csv(QiDt,paste0(wddt,"SPL_BerlinDst_Data_Prep_2.csv"))
