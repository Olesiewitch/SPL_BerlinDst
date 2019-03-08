

  #=========================PREPARING THE ENVIROMENT==============================

  setwd("~/SPL-Project-New/SPL_ BerlinDst_Data_Prep1")


  pcks = c("rvest",
         "magrittr",
         "readxl",
         "rlang",
         "dplyr",
         "tidyr",
         "rgdal",
         "maptools",
         "xlsx", 
         "data.table")  # list pakages required 

  for (package in pcks){  # run loop for the listed packages
    
  if (!require(package, character.only = TRUE)) {   
    install.packages(package, character.only = TRUE)  # if required install package
  }
  library(package, character.only = TRUE) # read in all required packages
  }
  
  #========FUNCTIONS FOR CLEANING AND ARRANGING THE DATA========================

  GetDataUnderURL = function(URL){
      # Obtain the data from the table stored under URL
      # 
      # Args:
      #   URL: URL adress. Under the URL the table which contains required data 
      #   must be the first table [[1]] on the page. 
      # 
      # Returns: 
      # Matrix with the data stored in the first table under from the URL adress 
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
  
  
  #========ACCESING THE DATA FROM STATISTIK BERLIN BRANDENBURG WEBSITE==========

  # The data on Statistik Berlin Brandenburg Website are stored under several 
  # URL adresses. Creat a list with each URL as its element:

  lstUrl = list(pplt = paste0("https://www.statistik-berlin-brandenburg.de/re",
                              "gionalstatistiken/r-gesamt_neu.asp?Ptyp=410&Sa",
                              "geb=13003&creg=BBB&anzwer=6"),

  sz = paste0("https://www.statistik-berlin-brandenburg.de/regionalstatisti",
              "ken/r-gesamt_neu.asp?Ptyp=410&Sageb=33000&creg=BBB&anzwer=8"),

  srf = paste0("https://www.statistik-berlin-brandenburg.de/regionalstatisti",
               "ken/r-gesamt_neu.asp?Ptyp=410&Sageb=33000&creg=BBB&anzwer=8"),

  hshl = paste0("https://www.statistik-berlin-brandenburg.de/regionalstatisti",
                "ken/r-gesamt_neu.asp?Ptyp=410&Sageb=12011&creg=BBB&anzwer=5"),

  stdt = paste0("https://www.statistik-berlin-brandenburg.de/regionalstatisti",
                "ken/r-gesamt_neu.asp?Ptyp=410&Sageb=21001&creg=BBB&anzwer=5&",
                "fbclid=IwAR0d7Ebm0uLtl5cc81O49tvFXeW0PRU_jEFh2qaCLWl1G2XSFv7",
                "mnb-SmwU"),

  immb = paste0("https://www.statistik-berlin-brandenburg.de/regionalstatisti",
                "ken/r-gesamt_neu.asp?Ptyp=410&Sageb=31000&creg=BBB&anzwer=0"),

  trsm = paste0("https://www.statistik-berlin-brandenburg.de/regionalstatisti",
                "ken/r-gesamt_neu.asp?Ptyp=410&Sageb=45005&creg=BBB&anzwer=7"),

  socl = paste0("https://www.statistik-berlin-brandenburg.de/regionalstatisti",
                "ken/r-gesamt_neu.asp?Ptyp=410&Sageb=22001&creg=BBB&anzwer=5"),

  chld = paste0("https://www.statistik-berlin-brandenburg.de/regionalstatisti",
                "ken/r-gesamt_neu.asp?Ptyp=410&Sageb=22005&creg=BBB&anzwer=9"),
  
  hndy = paste0("https://www.statistik-berlin-brandenburg.de/regionalstatisti",
                "ken/r-gesamt_neu.asp?Ptyp=410&Sageb=22007&creg=BBB&an",
                "zwer=10"),

  cmp = paste0("https://www.statistik-berlin-brandenburg.de/regionalstatisti",
             "ken/r-gesamt_neu.asp?Ptyp=410&Sageb=52001&creg=BBB&anzwer=5"),

  bnkr = paste0("https://www.statistik-berlin-brandenburg.de/regionalstatisti",
              "ken/r-gesamt_neu.asp?Ptyp=410&Sageb=52004&creg=BBB&anzwer=7"),

  accd = paste0("https://www.statistik-berlin-brandenburg.de/regionalstatisti",
                "ken/r-gesamt_neu.asp?Ptyp=410&Sageb=46002&creg=BBB&anzwer=6"),
  
  allw = paste0("https://www.statistik-berlin-brandenburg.de/regionalstatisti",
                "ken/r-gesamt_neu.asp?Ptyp=410&Sageb=22003&creg=BBB&anzwer=7"))
  
  
  # Apply the DatafromURL function to all the previously listed URL adresses and 
  # split each data table from the tblLst into individual data frame
 
  tblLst = lapply(lstUrl, GetDataUnderURL) %>%
      list2env(envir = .GlobalEnv)
  
  
  #======== PREPARING DATA FRAME FROM STATISTIK WEBSITE FOR MERGING ============

  # The data tables aquired from the website are very poorly structured and  
  # columns are not directly linked with their names. Therefore hardcoding of  
  # numbers of rows and columns is required. 
  
  X1 = c(pplt[5:16, 1])  # chose district name data
  X2 = c(pplt[5:16, 2])  # chose population  size data
  X3 = c(sz[4:15, 2])  # chose district size data
  X4 = c(srf[4:15, 3])  # chose residential and traffic surface data
  X5 = c(srf[4:15, 4]) %>%  # chose agricultural surface data
      replace(1, "0")  # replace the first element "-" with 0
  X6 = c(pplt[5:16, 3])  # chose employment data
  X7 = c(pplt[5:16, 5])  # chose non-professional persons data
  X8 = c(hshl[4:15, 2])  # chose  nr of private hauseholds data
  X9 = c(hshl[4:15, 3])  # chose persons/household data
  X10 = c(stdt[4:15, 2])  # chose nr of schools data
  X11 = c(stdt[4:15, 3])   # chose  nr of grades data
  X12 = c(stdt[4:15, 4])  # chose  nr of pupils data
  X13 = c(immb[4:15, 2])  # chose  nr of residential buildings data
  X14 = c(immb[4:15, 3])  # chose  nr of flats
  X15 = c(immb[4:15, 4])  # chose  total living space
  X16 = c(immb[4:15, 5])  # chose living space per capita
  X17 = c(trsm[4:15, 3])  # chose  nr of hotel beds
  X18 = c(trsm[4:15, 4])  # chose  nr of tourist guests
  X19 = c(trsm[4:15, 5])  # chose  nr of overnight stays
  X20 = c(socl[5:16, 2])  # chose  nr of social help recipients
  X21 = c(chld[4:15, 3])  # chose  nr of children in daycare
  X22 = c(chld[4:15, 4])  # chose % of children in daycare under 3
  X23 = c(chld[4:15, 5])  # chose  nr of %Children in daycare 3 - under 6
  X24 = c(hndy[4:15, 2])  # chose  nr of severely handicapped
  X25 = c(hndy[4:15, 3])  # chose  nr of severely handicapped/1K
  X26 = c(cmp[4:15, 2])  # chose  nr of companies
  X27 = c(cmp[4:15, 3])  # chose  nr of taxable revenues
  X28 = c(bnkr[5:16, 2])  # chose  nr of bankruptcies
  X29 = c(accd[4:15, 4])  # chose  nr of street traffic accidents /10K
  X30 = c(allw[5:16, 2])  # chose  nr of housing allowance households
 
  
  # Merge the data vectors together into one data frame
  
  wbsDt = data.frame(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,
                       X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30)
  
 
  # Clean up data using DataTo Numeric and split the number and the name of the 
  # district in X1 into two seperate columns
  
  wbsDt = wbsDt %>%
     mutate_at(vars(X2:X30),DataToNumeric) %>% 
     separate(col = X1,into = c("X0", "X1"),sep = " ")
    
    
 
  # Name the columns of wbsDt
  colnames(wbsDt) = c("Nr",
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
                      "Housing allowance households")
  
  #================READING IN THE SPORT DATA FILES==============================
  
  
  # read in sport clubs memberships data and convert columns to numeric
  
  sprtMb = read_excel("SB_B05-01-00_2018j01_BE.xls", sheet = "T9", skip=3) %>%
      mutate_at(vars(`bis 6`:`61 und\nmehr`),DataToNumeric)
  
  # read in sport clubs numbers data and convert columns to numeric
  
  sprtCl = read_excel("SB_B05-01-00_2018j01_BE.xls", sheet = "G3") %>%
      mutate_at(vars(X__10),DataToNumeric)
  
  
  #=================== PREPARING SPORT DATA FOR MERGING ========================
  
  mbDst = c(sprtMb$X__2[18:29])  # chose district names
  
  #
  # Add up 3 age groups up to 6,7-14 and 15-18 to obtain number of  active 
  # sport club members age 0-18
  
  jnrMb = c(rowSums(sprtMb[18:29, 4:6]))  
  
  
  snrMb = c(sprtMb$`61 und\nmehr`[18:29])  # choose active sport club members age 61+
  
  spMbDt = data_frame(mbDst, jnrMb, snrMb,
                      stringsAsFactors = FALSE)  # create a data frame
  
  colnames(spMbDt) = c("District","Senior Members of Sport Clubs",
                   "Junior Members of Sport Clubs")  # name the columns
  
 
  clbDst = c(sprtCl$X__9[7:18])%>%
      as.character()  # chose sport club district name data
  clbNm = c(sprtCl$X__10[7:18])  # chose sport club number data
  
  clbsDt = data.frame(clbDst,clbNm, stringsAsFactors = FALSE)  # create a data frame
  colnames (clbsDt) = c("District", "Sports Club")  # name the column
  
  
  #====================== READING IN BUS STOP DATA  ============================
  
  
  # Read in district borders coordinates
  
  dstrBrd = getKMLcoordinates("bezirksgrenzen.kml")
  
  bsStp = read.csv("stops_buses.csv")  # Read in bus stops data
  
  
  #================== PREPARING BUS STOP DATA FOR MERGING ======================
  
  ## create a list of all the polygons assigned to corresponding districts
  
  dstLst = list( rein = dstrBrd [[1]],  # chose polygon of Reinickendorf
                 char = dstrBrd[[2]],  # chose polygon of Charlottenburg-Wilmersdorf
                 trep = dstrBrd[[3]],  # chose polygon of Treptow-Köpenic
                 pank = rbind(dstrBrd[[4]], dstrBrd[[5]], dstrBrd[[6]],
                              dstrBrd[[7]], dstrBrd[[8]]), # chose polygons of Pankow
                 #
                 neu = dstrBrd[[9]],  # chose polygon of Neukolln
                 lich = dstrBrd[[10]],  # chose polygon of Lichtenberg
                 marz = dstrBrd[[11]],  # chose polygon of Marzahn-Hellersdorf
                 spa = dstrBrd[[12]],  # chose polygon of Spandau
                 steg = dstrBrd[[13]],  # chose polygon of Steglitz-Zehlendorf
                 mit = dstrBrd[[14]],  # chose polygon of Mitte
                 fri = dstrBrd[[15]],  # chose polygon of Friedrichshain-Kreuzberg
                 tem = dstrBrd[[16]])  # chose polygon of Tempelhof-Schöneberg
  
  
 # Apply NrofBusStops function to all the districts: 
  
  bsLst = lapply(dstLst, NrofBusStops) 
      
  
      
  bsStpDt= data.frame(names(bsLst),matrix(unlist(bsLst)), # merge data frame
                      stringsAsFactors = FALSE) %>% 
      mutate_at(vars("matrix.unlist.bsLst.."),DataToNumeric) 

  colnames(bsStpDt)= c("District", "Bus Stop Number") # name the columns
  
  
  #==========================READING IN CRIME DATA==============================
  
  crm = read_excel( "Fallzahlen&HZ 2012-2017.xlsx",sheet = "HZ_2017",
    col_names = TRUE,skip = 3) %>% 
      select("LOR-Schlüssel (Bezirksregion)", "Bezeichnung (Bezirksregion)",
             "Straftaten \r\n-insgesamt-")
  
  
  #========================PREPARING CRIME DATA FOR MERGING=====================
  
  # Select data based on their "LOR Schlussel" where districts are coded with the
  # last 4 numbers being "0000". Select only required data.
  
  crmDt = crm[grep("0000", crm$`LOR-Schlüssel (Bezirksregion)`), ] %>%
      select("Bezeichnung (Bezirksregion)","Straftaten \r\n-insgesamt-") %>%
      mutate_at(vars("Straftaten \r\n-insgesamt-"),DataToNumeric)  #convert data to numeric 

  colnames(crmDt) = c("District", "Criminal Offences")  # name the columns
  
  
  #======================= READING IN THE PARKING SPACES DATA===================
  
  # Writte in manualy numbers of parking spaces from mobility report. See the 
  # protocol for more details  
  
  
  rprtDt = c(26488, 4090, 26000, 20500, 2726, 7400, 7150)  # avaiable data
  
  
  #===================== PREPARING PARKING DATA FOR MERGING=====================
  
  # For the remaining districts the average of avaiable data has been used not
  # impact the index calculation. Create a vector with observations for all the
  # districts
  
  prkNr = c(rprtDt, rep(mean(rprtDt), 12 - length(rprtDt)))         

  prkDst = c("Mitte",   # create vector with district names
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
             "Reinickendorf")  
 
  prkDt = data.frame(prkDst, prkNr) %>%  # merge data frame
      mutate_at(vars(prkNr),DataToNumeric)  # convert data to numeric 
  
  colnames(prkDt) = c("District", "Parking Spaces")  # name columns
  
  #======================READING IN TREES DATA==================================
  
  trNm = paste0("statistic_id652680_strassenbaeume-in-berlin-nach-", 
                         "bezirken-2017.xlsx")
  
  tr = read_excel(trNm, sheet = "Daten")
  
  #====================PREPARINF TREES DATA FOR MERGING===========================
  
  trDst = tr[3:14, "Straßenbäume in Berlin nach Bezirken 2017"]  # select district names data
  
  
  
  trKM = tr[3:14, "X__2"]  # select total number of trees per kilometer of the road
  
 
  
  trDt = data.frame(trDst, trKM) %>%  # merge data frame
      mutate_at(vars("X__2"), DataToNumeric)  # convert data to numeric 
      

  colnames(trDt) = c("District", 
                     "Total number of trees per km of the road") # name columns
  
  
  #======================READING IN GREEN SPACE DATA============================
  
  grSpNm = paste0("statistic_id652716_oeffentliche-gruenflaechen-in",
                 "-berlin-nach-bezirken-2017.xlsx")
  
  grSpDt = read_excel(grSpNm, sheet = "Daten", skip=4) %>%
      as.data.frame %>%   # save data as data frame
      mutate_at(vars("Öffentliche Grünflächen in ha"),
                DataToNumeric)  # convert data to numeric
      
 
  #====================PREPARINF GREEN SPACE DATA FOR MERGING===================
  
  colnames(grSpDt) = c("District", "Green Space in ha")
  
  
  #================ MERGING DATA SETS INTO ONE DATA FRAME ========================
  
  dtFls = list (wbsDt = wbsDt,  # create a list of data files for merging
                spMbDt = spMbDt, 
                clbsDt = clbsDt, 
                prkDt = prkDt, 
                crmDt = crmDt, 
                bsStpDt = bsStpDt, 
                trDt = trDt, 
                grSpDt = grSpDt)




  
  dtFlsArr = lapply(dtFls,DataByDistric) %>%   # arrange all the data files 
  list2env(envir = .GlobalEnv)  # save the arranged data set to Global Env. 
  

  FnlDt = merge(wbsDt,spMbDt, spMbDt, clbsDt, prkDt, crmDt, bsStpDt,
                trDt, grSpDt
                

      
 
  
  colnames(FnlDt) = c(colnames(wbsDt), 
                      colnames(spMbDt)[2:3],
                      colnames(clbsDt)[2],
                      ) 
  
  write.csv2(data1, "SPL-BerlinDst_Prep_1.csv")
  
  a = pmatch(c("mit", "neu", "ste"), clbsDt$District)
  
  
