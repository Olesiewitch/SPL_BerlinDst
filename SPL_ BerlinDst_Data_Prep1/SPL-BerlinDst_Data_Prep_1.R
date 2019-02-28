
#=========================PREPARING THE ENVIROMENT==============================

setwd("~/SPL-Project-New/SPL_ BerlinDst_Data_Prep1")


packages = c("rvest","magrittr","readxl","rlang", "dplyr", "tidyr") 
                                                      # install and run packages
for (package in packages) {
  if(!require(package, character.only=TRUE))
                          {install.packages(package, character.only=TRUE)}
  library(package, character.only=TRUE)
}



#========ACCESING THE DATA FROM STATISTIK BERLIN BRANDENBURG WEBSITE============



ppltUrl = read_html("https://www.statistik-berlin-brandenburg.de/regionalstatistiken/r-gesamt_neu.asp?Ptyp=410&Sageb=13003&creg=BBB&anzwer=6")

szUrl = read_html("https://www.statistik-berlin-brandenburg.de/regionalstatistiken/r-gesamt_neu.asp?Ptyp=410&Sageb=33000&creg=BBB&anzwer=8")

srfUrl = read_html ("https://www.statistik-berlin-brandenburg.de/regionalstatistiken/r-gesamt_neu.asp?Ptyp=410&Sageb=33000&creg=BBB&anzwer=8")

hshlUrl =  read_html ("https://www.statistik-berlin-brandenburg.de/regionalstatistiken/r-gesamt_neu.asp?Ptyp=410&Sageb=12011&creg=BBB&anzwer=5")

stdtUrl = read_html("https://www.statistik-berlin-brandenburg.de/regionalstatistiken/r-gesamt_neu.asp?Ptyp=410&Sageb=21001&creg=BBB&anzwer=5&fbclid=IwAR0d7Ebm0uLtl5cc81O49tvFXeW0PRU_jEFh2qaCLWl1G2XSFv7mnb-SmwU")

immbUrl = read_html("https://www.statistik-berlin-brandenburg.de/regionalstatistiken/r-gesamt_neu.asp?Ptyp=410&Sageb=31000&creg=BBB&anzwer=0")

trsmUrl = read_html("https://www.statistik-berlin-brandenburg.de/regionalstatistiken/r-gesamt_neu.asp?Ptyp=410&Sageb=45005&creg=BBB&anzwer=7")

soclUrl = read_html("https://www.statistik-berlin-brandenburg.de/regionalstatistiken/r-gesamt_neu.asp?Ptyp=410&Sageb=22001&creg=BBB&anzwer=5")

chldUrl = read_html("https://www.statistik-berlin-brandenburg.de/regionalstatistiken/r-gesamt_neu.asp?Ptyp=410&Sageb=22005&creg=BBB&anzwer=9")

hndyUrl = read_html("https://www.statistik-berlin-brandenburg.de/regionalstatistiken/r-gesamt_neu.asp?Ptyp=410&Sageb=22007&creg=BBB&anzwer=10")

cmpUrl = read_html("https://www.statistik-berlin-brandenburg.de/regionalstatistiken/r-gesamt_neu.asp?Ptyp=410&Sageb=52001&creg=BBB&anzwer=5")

bnkrUrl = read_html("https://www.statistik-berlin-brandenburg.de/regionalstatistiken/r-gesamt_neu.asp?Ptyp=410&Sageb=52004&creg=BBB&anzwer=7") 

accdUrl = read_html("https://www.statistik-berlin-brandenburg.de/regionalstatistiken/r-gesamt_neu.asp?Ptyp=410&Sageb=46002&creg=BBB&anzwer=6")

allwUrl = read_html ("https://www.statistik-berlin-brandenburg.de/regionalstatistiken/r-gesamt_neu.asp?Ptyp=410&Sageb=22003&creg=BBB&anzwer=7")



#=======READING IN THE DATA FROM THE WEBSITE=============


pplt = html_table(html_nodes(ppltUrl, "table")[[1]], fill=TRUE, trim=TRUE)

sz = html_table(html_nodes(szUrl, "table")[[1]], fill=TRUE, trim=TRUE)

srf = html_table(html_nodes(srfUrl, "table")[[1]], fill=TRUE, trim=TRUE)

hshl = html_table(html_nodes(hshlUrl, "table")[[1]], fill=TRUE, trim=TRUE)

stdt = html_table(html_nodes(stdtUrl, "table")[[1]], fill=TRUE, trim=TRUE)

immb = html_table(html_nodes(immbUrl, "table")[[1]], fill=TRUE, trim=TRUE)

trsm = html_table(html_nodes(trsmUrl, "table")[[1]], fill=TRUE, trim=TRUE)

socl = html_table(html_nodes(soclUrl, "table")[[1]], fill=TRUE, trim=TRUE)

chld = html_table(html_nodes(chldUrl, "table")[[1]], fill=TRUE, trim=TRUE)

hndy = html_table(html_nodes(hndyUrl, "table")[[1]], fill=TRUE, trim=TRUE)

cmp = html_table(html_nodes(cmpUrl, "table")[[1]], fill=TRUE, trim=TRUE)

bnkr = html_table(html_nodes(bnkrUrl, "table")[[1]], fill=TRUE, trim=TRUE)

accd = html_table(html_nodes(accdUrl, "table")[[1]], fill=TRUE, trim=TRUE)

allw = html_table(html_nodes(allwUrl, "table")[[1]], fill=TRUE, trim=TRUE)



#=============READING IN THE DATA FROM THE FILES================================


sprtmb = read_excel("SB_B05-01-00_2018j01_BE.xls", sheet = "T9", 
                    range= "D4:L33", col_types = "numeric")

sprtcl = read_excel("SB_B05-01-00_2018j01_BE.xls", sheet = "G3", 
                    range = "J7:K19", col_types = c("text", "numeric"))


#### Dogs

### Green 







#======== DATA FRAME FROM STATISTIK WEBSITE FOR MERGING ========================


X1 = c(pplt[5:16,1])                               #chosing district name data 
X2 = c(pplt[5:16,2])                               #chosing population data 
X3 = c(sz[4:15,2])                                 #choising district size data 
X4 = c(srf[4:15,3])                                

#chosing residential and traffic surface data

X5 = c(srf[4:15,4])
X6 = c(pplt[5:16,3])
X7 = c(pplt[5:16,5])
X8 = c(hshl[4:15,2])
X9 = c(hshl[4:15,3])
X10 = c(stdt[4:15,2])
X11 = c(stdt[4:15,3])
X12 = c(stdt[4:15,4])
X13 = c(immb[4:15,2])
X14 = c(immb[4:15,3])
X15 = c(immb[4:15,4])
X16 = c(immb[4:15,5])
X17 = c(trsm[4:15,3])
X18 = c(trsm[4:15,4])
X19 = c(trsm[4:15,5])
X20 = c(socl[5:16,2])
X21 = c(chld[4:15,3])
X22 = c(chld[4:15,4])
X23 = c(chld[4:15,5])
X24 = c(hndy[4:15,2])
X25 = c(hndy[4:15,3])
X26 = c(cmp[4:15,2])
X27 = c(cmp[4:15,3])
X28 = c(bnkr[5:16,2])
X29 = c(accd[4:15,4])
X30 = c(allw[5:16,2])





#### Mergin the data 

webData = data.frame(X1,X2,X3,X4,X5, X6, X7,X8, X9, X10, X11, X12, X13, X14, 
                     X15, X16, X17, X18, X19, X20, X21, X22, X23, X24, X25,
                     X26, X27,X28, X29, X30) %>% 
                     separate(col=X1, into= c("X0", "X1"), sep =" ")

#Mergin all the data into one data frame, splitting the number and the name of 
#the district into seperate columns


colnames(webData) = c("Nr", "District", "Population", "District Size",
                    "Residential and traffic surface", "Agricultural surface",
                    "Employment", "Non-Professional Persons", 
                    "Private Hauseholds", "Persons/Household", "Schools", 
                    "Grades", "Pupils", "Residential buildings", "Flats",
                    "Total living space", "Living space per capita", 
                    "Hotel beds", "Tourist guests", "Overnight stays", 
                    "Social Help Recipients", "Children in daycare ", 
                    "% Children in daycare under 3 ", 
                    "% Children in daycare 3 - under 6", "Severely handicapped",
                    "Severely handicapped per 1000 inhabitants", "Companies",
                    "Taxable Revenues","Bankruptcies",
                    "Street traffic accidents /10K", 
                    "Housing allowance households") 

#================ PREPARING FILE DATA SETS FOR MERGING ========================


#Sports data preperation 

X31 = c(rowSums(sprtmb[18:29,1:3]))

#Adding up number 3 age groups to obtain number of  active sport club 
#members age 0-18

X32 = c(sprtmb$`61 und
mehr`[18:29])                                 #active sport club members age 61+

actData = data_frame(X31,X32)

colnames(actData)=c("Senior Members of Sport Clubs",
                       "Junior Members of Sport Clubs")
                     
clbsData = sprtcl

colnames(clbsData)=c("District", "Sports Club")



#================ MERGING DATA SETS INTO ONE DATA FRAME ========================  
 
data = data.frame(webData,actData)%>%merge.data.frame(data,clbsData,
                                                      by.y ="District") 

### adding the WebData and actData sets together as this same order of 
### districts was kept in the data sets



