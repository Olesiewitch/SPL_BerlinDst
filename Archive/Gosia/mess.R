#=========================PREPARING THE ENVIROMENT==============================

setwd("~/SPL-Project-New/SPL_ BerlinDst_Data_Prep1")

packages = c("rvest",
             "magrittr",
             "readxl",
             "rlang",
             "dplyr",
             "tidyr" ,
             "rgdal",
             "maptools",
             "xlsx")

for (package in packages) {
  # install and run packages
  if (!require(package, character.only = TRUE))
  {
    install.packages(package, character.only = TRUE)
  }
  library(package, character.only = TRUE)
}

#========ACCESING THE DATA FROM STATISTIK BERLIN BRANDENBURG WEBSITE============

#create a vector with names of matrixes to which the table with the data from
#respective URL will  be stored:


vrblnm = c(
  "pplt",
  "sz",
  "srf",
  "hshl",
  "stdt",
  "immb",
  "trsm",
  "socl",
  "chld",
  "hndy",
  "cmp",
  "cmp",
  "bnkr",
  "accd",
  "allw"
)

#Function input: URLs adresses
#Function output: list of data tables from the URL adresses

list = lapply(listURL, function (URL) {
  webpage = read_html(URL)         # read each ULR asdress as html vector
  
  table = (html_table(
    html_nodes(webpage, "table")[[1]],
    fill = TRUE,
    trim = TRUE
  ))
  
  
  #read in the data from each table under URL adress
})


# assign name to each table from the list and save it as a separate matrix

for (i in 1:length(list)) {
  assign(vrblnm[i], list[[i]])
}

#======== PREPARING DATA FRAME FROM STATISTIK WEBSITE FOR MERGING ==============

X1 = c(pplt[5:16,pplt])                                       #chose district name
X2 = c(pplt[5:16,2])                                    #chose population  size
X3 = c(sz[4:15,2])                                         #chose district size
X4 = c(srf[4:15,3])                      #chose residential and traffic surface
X5 = c(srf[4:15,4]) %>% replace(1, "0")             #chose agricultural surface
X6 = c(pplt[5:16, 3])                                          # chose employment
X7 = c(pplt[5:16, 5])                            # chose non-professional persons
X8 = c(hshl[4:15, 2])                           # chose  nr of private hauseholds
X9 = c(hshl[4:15, 3])                                   # chose persons/household
X10 = c(stdt[4:15, 2])                                      # chose nr of schools
X11 = c(stdt[4:15, 3])                                      # chose  nr of grades
X12 = c(stdt[4:15, 4])                                      # chose  nr of pupils
X13 = c(immb[4:15, 2])                       # chose  nr of residential buildings
X14 = c(immb[4:15, 3])                                       # chose  nr of flats
X15 = c(immb[4:15, 4])                                # chose  total living space
X16 = c(immb[4:15, 5])                            # chose living space per capita
X17 = c(trsm[4:15, 3])                                  # chose  nr of hotel beds
X18 = c(trsm[4:15, 4])                              # chose  nr of tourist guests
X19 = c(trsm[4:15, 5])                             # chose  nr of overnight stays
X20 = c(socl[5:16, 2])                      # chose  nr of social help recipients
X21 = c(chld[4:15, 3])                         # chose  nr of children in daycare
X22 = c(chld[4:15, 4])                   # chose % of children in daycare under 3
X23 = c(chld[4:15, 5])            # chose  nr of %Children in daycare 3 - under 6
X24 = c(hndy[4:15, 2])                        # chose  nr of severely handicapped
X25 = c(hndy[4:15, 3])                     # chose  nr of severely handicapped/1K
X26 = c(cmp[4:15, 2])                                    # chose  nr of companies
X27 = c(cmp[4:15, 3])                             # chose  nr of taxable revenues
X28 = c(bnkr[5:16, 2])                                # chose  nr of bankruptcies
X29 = c(accd[4:15, 4])               # chose  nr of street traffic accidents /10K
X30 = c(allw[5:16, 2])                # chose  nr of housing allowance households

#PART BELOW CAN BE CODED BETTER

#merge the numeric data together into one data frame

webnum = data.frame(
  X2,
  X3,
  X4,
  X5,
  X6,
  X7,
  X8,
  X9,
  X10,
  X11,
  X12,
  X13,
  X14,
  X15,
  X16,
  X17,
  X18,
  X19,
  X20,
  X21,
  X22,
  X23,
  X24,
  X25,
  X26,
  X27,
  X28,
  X29,
  X30
)

#merge all the data together and in numeric data replace "," by ".", remove
#white spaces and split the number and the name of the district in X1 into
#two seperate columns

webData = data.frame(X1, sapply(webnum, function(column) {
  commatodot = (gsub(",", ".", column))
  num = as.numeric(gsub("[[:space:]]", "", commatodot))
})) %>%
  separate(col = X1,
           into = c("X0", "X1"),
           sep = " ")


#name the columns of WebData data frame:

colnames(webData) = c(
  "Nr",
  "District",
  "Population",
  "District Size",
  "Residential and traffic surface",
  "Agricultural surface",
  "Employment",
  "Non-Professional Persons",
  "Private Hauseholds",
  "Persons/Household",
  "Schools",
  "Grades",
  "Pupils",
  "Residential buildings",
  "Flats",
  "Total living space",
  "Living space per capita",
  "Hotel beds",
  "Tourist guests",
  "Overnight stays",
  "Social Help Recipients",
  "Children in daycare ",
  "% Children in daycare under 3 ",
  "% Children in daycare 3 - under 6",
  "Severely handicapped",
  "Severely handicapped per 1000 inhabitants",
  "Companies",
  "Taxable Revenues",
  "Bankruptcies",
  "Street traffic accidents /10K",
  "Housing allowance households"
)


#================READING IN THE SPORT DATA FILES================================


# read in sport clubs memberships data

sprtmb = read_excel(
  "SB_B05-01-00_2018j01_BE.xls",
  sheet = "T9",
  range = "B4:L33",
  col_types = c("text",
                rep("numeric", 10))
)
#read in sports club number data

sprtcl = read_excel(
  "SB_B05-01-00_2018j01_BE.xls",
  sheet = "G3",
  range = "J7:K19",
  col_types = c("text", "numeric")
)


#=================== PREPARING SPORT DATA FOR MERGING ==========================

clbdst = c(sprtmb$X__1[18:29])                            #chose district names

X31 = c(rowSums(sprtmb[18:29, 3:5]))            #add up 3 age groups up to 6,7-14

# and 15-18 ) to obtain number of  active sport club members age 0-18

X32 = c(sprtmb$`61 und
        mehr`[18:29])                          # chose active sport club members age 61+


#create a data frame:

actData = data_frame(clbdst, X31, X32)

#name the columns:

colnames(actData) = c("District",
                      "Senior Members of Sport Clubs",
                      "Junior Members of Sport Clubs")

#create a data frame :

clbsData = data.frame(sprtcl) # chose nr of Sport clubs data

#name the columns:

colnames(clbsData) = c("District", "Sports Club")


#====================== READING IN BUS STOP DATA  ==============================


# read in district borders coordinates:

dstrbrd = getKMLcoordinates("bezirksgrenzen.kml")

#reading in bus stops data:

bstp = read.csv("stops_buses.csv")


#================== PREPARING BUS STOP DATA FOR MERGING ========================

## create a list of all the plygons assigned to corresponding districts

distlst = list(
  rein = dstrbrd [[1]],
  #chose polygon of Reinickendorf
  char = dstrbrd[[2]],
  #chose polygon of Charlottenburg-Wilmersdorf
  trep = dstrbrd[[3]] ,
  #chose polygon of Treptow-Köpenick
  
  pank = rbind(dstrbrd[[4]], dstrbrd[[5]], dstrbrd[[6]], dstrbrd[[7]],
               dstrbrd[[8]]),
  #chose polygons of Pankow
  neu = dstrbrd[[9]],
  #chose polygon of Neukolln
  lich = dstrbrd[[10]],
  #chose polygon of Lichtenberg
  marz = dstrbrd[[11]],
  #chose polygon of Marzahn-Hellersdorf
  span = dstrbrd[[12]],
  #chose polygon of Spandau
  steg = dstrbrd[[13]],
  #chose polygon of Steglitz-Zehlendorf
  mit = dstrbrd[[14]],
  #chose polygon of Mitte
  fri = dstrbrd[[15]],
  #chose polygon of Friedrichshain-Kreuzberg
  tem = dstrbrd[[16]]
)                      #chose polygon of Tempelhof-Schöneberg


#function input: polygon data for the district
# function output : number of bus stops in the district

list2 = lapply(distlst, function (district) {
  whichstops = point.in.polygon(bstp$stop_lon, bstp$stop_lat, district[, 1],
                                district[, 2]) # check if bus stops in the polygon
  nrstops = length(whichstops[whichstops != 0])
  
  #count how many bus stops are in the district (1 = in the polygon,
  #2 = on the border of the polygon, 0 = not in the polygon )
})

#create vector of district names

distnm = c(
  "rein",
  "char",
  "trep",
  "pank",
  "neu",
  "lich",
  "marz",
  "span",
  "steg",
  "mit",
  "fri",
  "tem"
)

#merge into a data frame

stpData = data.frame(distnm, matrix(unlist(list2)))

# name the columns

colnames(stpData) = c("District", "Bus Stop Number")

#==========================READING IN CRIME DATA================================

crm = read_excel(
  "Fallzahlen&HZ 2012-2017.xlsx",
  sheet = "HZ_2017",
  range = "B5:C155",
  col_types = c("text", "numeric")
)

#========================PREPARING CRIME DATA FOR MERGING=======================

#chose the total number of crime act per 1000 inhabitants

crmData = crm[c(1, 13, 23, 41, 60, 71, 81, 90, 102, 124, 135, 150), ]

# name the columns

colnames(crmData) = c("District", "Criminal Offences")


#======================= READING IN THE PARKING SPACES DATA=====================

# writte in manualy numbers of parking spaces from mobility report


rprt = c(26488, 4090, 26000, 20500, 2726, 7400, 7150)                   #avaiable data


#===================== PREPARING PARKING DATA FOR MERGING=======================

# for the remaining districts the average of avaiable data has been used not
# impact the index calculation

X33 = c(rprt, rep(mean(rprt), 12 - length(rprt)))           #create a vector with
#observations for all the districts


# create vector with district names

prkds = c(
  "Mitte",
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
  "Reinickendorf"
)

# merge data frame

prkData = data.frame(prkds, X33)

#name columns

colnames(prkData) = c("District", "Parking Spaces")
