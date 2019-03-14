Name of Quantlet: SPL_BerlinDst_Data_Prep_1

Published in: 'Statistical Programming Languages - Student Project on ''Livability of Berlin Districts: Comparative Analysis'' '

Description: Generates the required data set from the raw data

Keywords: data preparation, variable transformation, data description, data acquisition  

Author: Malgorzata Paulina Olesiewicz

See also: other Quantlets in this project

Submitted: 15.03.2019

Datafile:
- bezirksgrenzen.kml
- Fallzahlen&HZ 2012-2017.xls
- public transportation stops.csv
- SB_B05-01-00_2018j01_BE.xls
- statistic_id652680_strassenbaeume-in-berlin-nach-bezirken-2017.xls
- statistic_id652716_oeffentliche-gruenflaechen-in-berlin-nach-bezirken-2017.xlsx

Output:  
- SPL_BerlinDst_Data_Prep_1.csv

## RCode

```
#=========================PREPARING THE ENVIROMENT==============================

#setwd("~/SPL_BerlinDst")
# The code was tested at HU PC Pool 25

install.packages("rvest")
install.packages("magrittr")
install.packages("rlang")
install.packages("dplyr")
install.packages("tidyr")
install.packages("rgdal")
install.packages("xlsx")
install.packages("maptools")# Install required pakages  
  
library(rvest)
library(magrittr)
library(rlang)
library(dplyr)
library(tidyr)
library(rgdal)
library(xlsx)
library(maptools) # Load required packages


#========FUNCTIONS FOR CLEANING AND ARRANGING THE DATA========================

GetDataUnderURL = function(URL){
    # Function returns the data table from under the URL address.
    # Firstly, the URL address is read (webpage). Secondly, the data of 
    # the type "table" is extracted from the HTML document with the CSS 
    # selector (?html_nodes). The data table is read, stored and
    # returned as a table.  
    #
    # Args:
    #   URL: URL address under which the required data is 
    #        stored. The table which contains the data must  
    #        be the first table [[1]] on the page. 
    #
    # Returns: 
    # Matrix with the data stored in the first table under from 
    # the URL address 
    
    webpage = read_html(URL)    
    table = html_table(html_nodes(webpage, "table")[[1]], 
                       fill = TRUE,trim = TRUE)
}

DataToNumeric = function(column){
    # Function cleans up data in chosen column by replacing "," with "." as a 
    # decimel symbol, removes empty space between numbers and converst the 
    # data to numeric type
    # 
    # Args:
    #     column: name of the column in the dataframe which values suppose to 
    #     be cleaned up and converted to numeric type
    #
    # Returns: 
    # Vector with numeric values of converted data 
    commatodot = (gsub(",", ".", column)) 
    num = as.numeric(gsub("[[:space:]]", "", commatodot))
    return(num)
}

DistricToFullName = function (column){
    # Function re-names the districts of Berlin with their full names 
    # without special signs. The function identifies the district by 
    # three or four letters of its name. 
    #
    # Args:
    #      column: vector or a column containing names of Berlin
    #      districts.Name can have any form, special signs can be
    #      use and additional information can be added to it. Vector 
    #      must be character type.Only first 3-4letters need to be correct.  
    #      
    # Returns: 
    #        Vector of replaced district names by its official names  
    #        without special signs.  
    
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

NrofStops= function(district){
    # Function checkes, based on the geografical coordinates, how many public
    # transportation stops (in file bsStp) are in the chosen Berlin district
    # and counts their number. 
    #
    # Args:
    #     district: names of the polygon file with the coordinates of the 
    #     given Berlin district.The longitude coordinates  must be the 
    #     first column of the file, the latitude the second column. 
    #
    # Returns: 
    # Number of public transportation stops in the given district
    # or on its border ( in whichstops function 1 = in the 
    # polygon, 2 = on the border of the polygon ,0 = not in the 
    # polygon).  
    whichstops = point.in.polygon(bsStp$stop_lon, bsStp$stop_lat, 
                                  district[, 1],district[, 2]) 
    nrstops = length(whichstops[whichstops != 0])
    
    return(nrstops)
}

#========ACCESING THE DATA FROM STATISTIK BERLIN BRANDENBURG WEBSITE==========

# The data on Statistik Berlin Brandenburg Website are stored under several 
# URL adresses. Creat a list with each URL as its element:

lstUrl = list(pplt = paste0("https://www.statistik-berlin-brandenburg.de/re",
                            "gionalstatistiken/r-gesamt_neu.asp?Ptyp=410&Sa",
                            "geb=13003&creg=BBB&anzwer=6"),
              
              sz   = paste0("https://www.statistik-berlin-brandenburg.de/regi",
                            "onalstatistiken/r-gesamt_neu.asp?Ptyp=410&Sageb=",
                            "33000&creg=BBB&anzwer=8"),
              
              srf  = paste0("https://www.statistik-berlin-brandenburg.de/regio",
                            "nalstatistiken/r-gesamt_neu.asp?Ptyp=410&Sageb=33",
                            "000&creg=BBB&anzwer=8"),
              
              hshl = paste0("https://www.statistik-berlin-brandenburg.de/regio",
                            "nalstatistiken/r-gesamt_neu.asp?Ptyp=410&Sageb=12",
                            "011&creg=BBB&anzwer=5"),
              
              stdt = paste0("https://www.statistik-berlin-brandenburg.de/regio",
                            "nalstatistiken/r-gesamt_neu.asp?Ptyp=410&Sageb=21",
                            "001&creg=BBB&anzwer=5&fbclid=IwAR0d7Ebm0uLtl5cc81",
                            "O49tvFXeW0PRU_jEFh2qaCLWl1G2XSFv7mnb-SmwU"),
              
              immb = paste0("https://www.statistik-berlin-brandenburg.de/regio",
                            "nalstatistiken/r-gesamt_neu.asp?Ptyp=410&Sageb=31",
                            "000&creg=BBB&anzwer=0"),
              
              trsm = paste0("https://www.statistik-berlin-brandenburg.de/regio",
                            "nalstatistiken/r-gesamt_neu.asp?Ptyp=410&Sageb=45",
                            "005&creg=BBB&anzwer=7"),
              
              socl = paste0("https://www.statistik-berlin-brandenburg.de/regio",
                            "nalstatistiken/r-gesamt_neu.asp?Ptyp=410&Sageb=22",
                            "001&creg=BBB&anzwer=5"),
              
              chld = paste0("https://www.statistik-berlin-brandenburg.de/regio",
                            "nalstatistiken/r-gesamt_neu.asp?Ptyp=410&Sageb=22",
                            "005&creg=BBB&anzwer=9"),
              
              hndy = paste0("https://www.statistik-berlin-brandenburg.de/regio",
                            "nalstatistiken/r-gesamt_neu.asp?Ptyp=410&Sageb=22",
                            "007&creg=BBB&anzwer=10"),
              
              cmp  = paste0("https://www.statistik-berlin-brandenburg.de/regio",
                            "nalstatistiken/r-gesamt_neu.asp?Ptyp=410&Sageb=52",
                            "001&creg=BBB&anzwer=5"),
              
              bnkr = paste0("https://www.statistik-berlin-brandenburg.de/regio",
                            "nalstatistiken/r-gesamt_neu.asp?Ptyp=410&Sageb=52",
                            "004&creg=BBB&anzwer=7"),
              
              accd = paste0("https://www.statistik-berlin-brandenburg.de/regio",
                            "nalstatistiken/r-gesamt_neu.asp?Ptyp=410&Sageb=46",
                            "002&creg=BBB&anzwer=6"),
              
              allw = paste0("https://www.statistik-berlin-brandenburg.de/regio",
                            "nalstatistiken/r-gesamt_neu.asp?Ptyp=410&Sageb=22",
                            "003&creg=BBB&anzwer=7"))


# Apply the DatafromURL function to all the previously listed URL adresses and 
# split each data table from the tblLst into individual data frame in the 
# Global Enviroment

tblLst = lapply(lstUrl, GetDataUnderURL) %>%
    list2env(envir = .GlobalEnv)


#======== PREPARING DATA FRAME FROM STATISTIK WEBSITE FOR MERGING ============

# The data tables aquired from the website are very poorly structured and  
# columns are not directly linked with their names. Therefore hardcoding of  
# numbers of rows and columns is required. 

X1 = c(pplt[5:16, 1])  # choose district name data
X2 = c(pplt[5:16, 2])  # choose population  size data
X3 = c(sz[4:15, 2])  # choose district size data
X4 = c(srf[4:15, 3])  # choose residential and traffic surface data
X5 = c(srf[4:15, 4]) %>%  # choose agricultural surface data
    replace(1, "0")  # replace the first element "-" with 0
X6 = c(pplt[5:16, 3])  # choose employment data
X7 = c(pplt[5:16, 5])  # choose non-professional persons data
X8 = c(hshl[4:15, 2])  # choose  nr of private hauseholds data
X9 = c(hshl[4:15, 3])  # choose persons/household data
X10 = c(stdt[4:15, 2])  # choose nr of schools data
X11 = c(stdt[4:15, 3])   # choose  nr of grades data
X12 = c(stdt[4:15, 4])  # choose  nr of pupils data
X13 = c(stdt[4:15, 5])  # choose  nr of pupils dat / K
X14 = c(immb[4:15, 2])  # choose  nr of residential buildings data
X15 = c(immb[4:15, 3])  # choose  nr of flats
X16 = c(immb[4:15, 4])  # choose  total living space
X17 = c(immb[4:15, 5])  # choose living space per capita
X18 = c(trsm[4:15, 3])  # choose  nr of hotel beds
X19 = c(trsm[4:15, 4])  # choose  nr of tourist guests
X20 = c(trsm[4:15, 5])  # choose  nr of overnight stays
X21 = c(socl[5:16, 2])  # choose  nr of social help recipients
X22 = c(chld[4:15, 3])  # choose  nr of children in daycare
X23 = c(chld[4:15, 4])  # choose % of children in daycare under 3
X24 = c(chld[4:15, 5])  # choose  nr of %Children in daycare 3 - under 6
X25 = c(hndy[4:15, 2])  # choose  nr of severely handicapped
X26 = c(hndy[4:15, 3])  # choose  nr of severely handicapped/1K
X27 = c(cmp[4:15, 2])  # choose  nr of companies
X28 = c(cmp[4:15, 3])  # choose  nr of taxable revenues
X29 = c(bnkr[5:16, 2])  # choose  nr of bankruptcies
X30 = c(accd[4:15, 4])  # choose  nr of street traffic accidents /10K
X31 = c(allw[5:16, 2])  # choose  nr of housing allowance households


# Merge the data vectors together into one data frame and split the number 
# and the name of the district in X1 into two seperate columns

wbsDt = data.frame(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,
                   X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31, 
                   stringsAsFactors = FALSE) %>%
    separate(col = X1,into = c("X0", "X1"),sep = " ")%>%
    mutate_at(vars(X1), DistricToFullName) %>%
    mutate_at(vars("X2":"X31"),DataToNumeric) 


# Name the columns of wbsDt
colnames(wbsDt) = c("Nr",
                    "District",
                    "Population",
                    "Size",
                    "RsSurface",
                    "AgrSurface",
                    "Employment",
                    "NonProf",
                    "Hausehold",
                    "PplHausehold",
                    "Schools",
                    "Grades",
                    "Pupils",
                    "Puplis1000",
                    "Buildings",
                    "Flats",
                    "Space",
                    "SpacePC",
                    "Hotel",
                    "Tourists",
                    "Stays",
                    "SocHelp",
                    "Daycare",
                    "Child3",
                    "Child6",
                    "Disabled",
                    "Disabled1000",
                    "Company",
                    "Revenue",
                    "Bankruptcy",
                    "Accidents",
                    "Allowance")

#==================READING IN THE SPORT DATA FILES==============================

# Data source paset0("https://www.statistik-berlin-brandenburg.de/Statistiken/",
#"statistik_SB.asp?Ptyp=700&Sageb=21006&creg=BBB&anzwer=10")

# Read in sport clubs memberships data and convert columns to numeric.

sprtMb = read.xlsx("SPL_BerlinDst_Data_Prep_1/SB_B05-01-00_2018j01_BE.xls",
                   sheetName = "T9", startRow = 4, encoding = "UTF-8",
                   as.data.frame = TRUE) %>%
    mutate_at(vars("bis6":"mehrund61"),DataToNumeric) %>%  
    mutate_at(vars("Bezirk"),as.character)  

# Read in sport clubs numbers data and rename the columns
# Convert columns to numeric and name the districts properly

# Ignore Warning message: "In (function (column)  : NAs introduced by coercion"

sprtCl = read.xlsx("SPL_BerlinDst_Data_Prep_1/SB_B05-01-00_2018j01_BE.xls",
                   sheetName = "G3", encoding = "UTF-8", startRow = 2,
                   as.data.frame = TRUE) %>% 
    mutate_at(vars("Sportvereine"),DataToNumeric) %>% 
    mutate_at(vars("Bezirk"),as.character)  

#=================== PREPARING SPORT DATA FOR MERGING ==========================

# Sport Club Members 

mbDst = c(sprtMb$Bezirk[33:44])%>%  # Chose district names 
    DistricToFullName(.)  # Uniformise the disctrict names


# Take mean of 3 age groups (up to 6, 7-14 and 15-18) to obtain average % of  
# active sport club members age 0-18

jnrMb = c(rowMeans(sprtMb[33:44, 4:6])) 


snrMb = c(sprtMb$mehrund61[33:44])  # Choose active sport club memb. age 61+

spMbDt = data.frame(mbDst, jnrMb, snrMb,
                    stringsAsFactors = FALSE)  # create a data frame

colnames(spMbDt) = c("District","JunSport","SenSport")  # name the columns

# Sport Clubs

clbDst = c(sprtCl$Bezirk[1:12]) %>%  # Chose sport club district name data
    DistricToFullName(.)

clbNm = c(sprtCl$Sportvereine[1:12])  


clbsDt = data.frame(clbDst, 
                    clbNm, 
                    stringsAsFactors = FALSE)  # Create data frame


colnames(clbsDt) = c("District", "Sport")  # Name the column


#====================== READING IN BUS STOP DATA  ==============================


# Read in district borders coordinates
# Data source: https://data.technologiestiftung-berlin.de/dataset/bezirksgrenzen 
# Ignore warnings: no altitude values for KML object 1 - 16


dstrBrd = getKMLcoordinates("SPL_BerlinDst_Data_Prep_1/bezirksgrenzen.kml")

# Read in bus stops data
# Source: https://daten.berlin.de/datensaetze/vbb-fahrplandaten-gtfs

bsStp = read.csv("SPL_BerlinDst_Data_Prep_1/public transportation stops.csv")  


#=================== PREPARING BUS STOP DATA FOR MERGING =======================

# create a list of all the polygons assigned to corresponding districts

dstLst = list( rein = dstrBrd [[1]],  # chose polygon of Reinickendorf
               char = dstrBrd[[2]],  # chose polygon of Charlottenburg-Wilmersd.
               trep = dstrBrd[[3]],  # chose polygon of Treptow-Köpenic
               pank = rbind(dstrBrd[[4]], dstrBrd[[5]], dstrBrd[[6]],
                            dstrBrd[[7]], dstrBrd[[8]]), # chose polys.of Pankow
               #
               neu  = dstrBrd[[9]],  # chose polygon of Neukolln
               lich = dstrBrd[[10]],  # chose polygon of Lichtenberg
               marz = dstrBrd[[11]],  # chose polygon of Marzahn-Hellersdorf
               spa  = dstrBrd[[12]],  # chose polygon of Spandau
               steg = dstrBrd[[13]],  # chose polygon of Steglitz-Zehlendorf
               mit  = dstrBrd[[14]],  # chose polygon of Mitte
               fri  = dstrBrd[[15]],  # chose polygon of Friedrichshain-Kreu.
               tem  = dstrBrd[[16]])  # chose polygon of Tempelhof-Schöneberg


# Apply NrofBusStops function to all the districts: 

bsLst = lapply(dstLst, NrofStops) 

# Creat data frame from list names and computed elements

bsStpDt= data.frame(DistricToFullName(names(bsLst)), 
                    matrix(unlist(bsLst)), 
                    stringsAsFactors = FALSE) %>% 
    mutate_at(vars("matrix.unlist.bsLst.."),DataToNumeric)


colnames(bsStpDt)= c("District", 
                     "Transport") # name the columns

#==========================READING IN CRIME DATA================================
# There is problem with encoding in this file as special signs make the code
# not replicable on every computer when calling columns by their name
# (even if encoding is set to "UTF-8" ). Therefore, maching of columns 
# names by only couple of (first) latters in the name is required
# Columns of interst start with:  

crmCl= c("LOR", "Bezei", "Straft") # Vector with begining of column names 

crm = read.xlsx("SPL_BerlinDst_Data_Prep_1/Fallzahlen&HZ 2012-2017.xls",
                sheetName = "HZ_2017", encoding = "UTF-8",
                startRow = 3, as.data.frame = TRUE) %>%
    select(grep(paste(crmCl,collapse="|"), names(.))) %>%  # select columns 
    mutate_at(grep("Bezei", names(.)), as.character)


#========================PREPARING CRIME DATA FOR MERGING=======================

# Select data based on their "LOR Schlussel"[, 1] where districts are coded 
# with the last 4 numbers being "0000". Select only required data. Convert 
# names of the districts to full names, the rest of the data to numeric

crmDt = crm[grep("0000", crm[, 1]), ] %>%
    select(2:3) %>%
    mutate_at(grep("Bezei", names(.)), DistricToFullName) %>%  
    mutate_at(grep("Straft", names(.)), DataToNumeric)  


colnames(crmDt) = c("District", "Crime")  # name the columns


#======================= READING IN THE PARKING SPACES DATA=====================

# Writte in manualy numbers of parking spaces from mobility report. See:
# paset0("https://www.berlin.de/senuvk/verkehr/politik_planung/zahlen_",
#"fakten/download/Mobilitaet_dt_komplett.pdf")


rprtDt = c(26488, 4090, 26000, 20500, 2726, 7400, 7150)  # Avaiable data


#====================== PREPARING PARKING DATA FOR MERGING======================

# For the remaining districts the average of avaiable data has been used, not to
# impact the index calculation. Create a vector with observations for all the
# districts

prkNr = c(rprtDt, rep(mean(rprtDt), 12 - length(rprtDt)))         

prkDst = c("Mitte",   
           "Friedrichshain-Kreuzberg",
           "Pankow",
           "Charlottenburg-Wilmersdorf",
           "Spandau",
           "Steglitz-Zehlendorf",
           "Tempelhof-Schöneberg",
           "Neukolln",
           "Treptow-Köpenick",
           "Marzahn-Hellersdorf",
           "Lichtenberg",
           "Reinickendorf") # create vector with district names

prkDt = data.frame(prkDst, prkNr, 
                   stringsAsFactors = FALSE) %>%  # merge data frame
    mutate_at(vars(prkDst),DistricToFullName) %>%
    mutate_at(vars(prkNr),DataToNumeric)   # convert data to numeric 

colnames(prkDt) = c("District", "Parking")  # name columns

#========================READING IN TREES DATA==================================

# Data Source: paste0("https://de.statista.com/statistik/daten/studie/652680/",
#"umfrage/strassenbaeume-in-berlin-nach-bezirken/")
# Read in the trees data. 
# Ignore Warning: Warning message: In DataToNumeric(NA..1) : NAs introduced 
# by coercion

trNm = paste0("SPL_BerlinDst_Data_Prep_1/",
              "statistic_id652680_strassenbaeume-in-",
              "berlin-nach-bezirken-2017.xls")

tr = read.xlsx(trNm, sheetName = "Daten", as.data.frame = TRUE , 
               encoding = "UTF-8") %>%
    mutate_at(grep("Berlin", names(.)), as.character) %>%  # Con. Dist. Names 
    mutate_at(-grep("Berlin", names(.)), DataToNumeric)  # Con.other data to num.

#====================PREPARINF TREES DATA FOR MERGING===========================

trDst = tr[3:14, grep("Berlin", names(tr))] %>% 
    DistricToFullName(.) # select district names and mutate them  to full form

trKM = tr[3:14, 3] # select total number of trees per km of the road


trDt = data.frame(trDst,
                  trKM,
                  stringsAsFactors = FALSE) # merge data frame


colnames(trDt) = c("District", "Trees") # name columns

#===========READING IN  AND PREPARING FOR MERGING GREEN SPACE DATA =============

# Read in green space data
# Data source: Source of data : paste0("https://de.statista.com/statistik/",
# "daten/studie/652716/umfrage/oeffentliche-gruenflaechen-in-berlin-nach-",
# "bezirken/") 

# Again problem with encoding here. 
# Intrested in "Öffentliche.Grünflächen.in.ha" use "lich" to identify column


grSpNm = paste0("SPL_BerlinDst_Data_Prep_1/statistic_id652716_oeffentliche-",
                "gruenflaechen-in-berlin-nach-bezirken-2017.xlsx")

grSpDt = read.xlsx(grSpNm, sheetName = "Daten", startRow = 5, 
                   as.data.frame = TRUE, encoding = "UTF-8") %>%
    mutate_at(vars(grep("lich", names(.))),DataToNumeric) %>%
    mutate_at(vars(- grep("lich", names(.))), as.character) %>%
    mutate_at(vars(- grep("lich", names(.))), DistricToFullName)

colnames(grSpDt) = c("District", "GreenSp")

#================= MERGING DATA SETS INTO ONE DATA FRAME =======================


FnlDt = merge(wbsDt,spMbDt, by.y = "District") %>% 
    merge(.,clbsDt, by.y = "District") %>% 
    merge(.,crmDt,by.y = "District") %>%
    merge(.,prkDt,by.y = "District")%>%
    merge(.,bsStpDt,by.y = "District") %>%
    merge(.,trDt,by.y = "District") %>%
    merge(.,grSpDt,by.y = "District")  # Merge all the data sets by the district


write.csv2(FnlDt, "SPL_BerlinDst_Data_Prep_1/SPL_BerlinDst_Data_Prep_1.csv")

# Read in best with: 
# read.csv("SPL_BerlinDst_Data_Prep1/SPL_BerlinDst_Data_Prep_1.csv", 
# sep = ";", dec = ",", row.names = 1, stringsAsFactors = FALSE)

# ========================END OF THE SCRIPT=====================================
# ==============================================================================
```
