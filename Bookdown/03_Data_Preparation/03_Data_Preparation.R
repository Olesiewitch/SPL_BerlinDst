#install and upload packages
packages = c("rvest", 
             "readxl",
             "magrittr",
             "dplyr")

for (package in packages) {
    #install packages if not installed yet
    if(!require(package, character.only = TRUE)){
        install.packages(package, character.only = TRUE)
    }
    #upload packages
    library(package, character.only = TRUE)
}


################Read helpful functions##########################################
source("./03_Data_Preparation/helpful_functions.R")

####################Upload the data#############################################


################################################################################
####################Street-Index MAtching Table#################################
################################################################################

# Upload street-index matching table from web 
# Upload list of districts in Berlin
dstr = read.csv2("03_Data_Preparation/Input/List_of_districts.csv", 
                 header = TRUE, 
                 sep=";", 
                 dec=",", 
                 stringsAsFactors = FALSE)

# Create table with data for the whole Berlin
StrMtch = data.frame()

for (i in 1:dim(dstr)[1]) {
    # Generate a link to data for all the districts and sub-districts
    link=paste0("https://berlin.kauperts.de/Bezirke/",
                dstr$District[i],
                "/Ortsteile/",
                dstr$Sub.district[i],
                "/Strassen")
    
    # Download the data from the web-page with generated link
    webpage = read_html(link)
    tbls = html_nodes(webpage, "table")
    tab = html_table(tbls)[[1]]
    
    # Add columns for district and sub-district
    tab$District = dstr$District[i]
    tab$SubDistrict = dstr$Sub.district[i]
    
    # Add created table to the table for the whole Berlin
    StrMtch = rbind(StrMtch, tab)
}

# Save resulting table to csv
write.csv2(StrMtch, 
           "./03_Data_Preparation/Output/Street_Index_Matching.csv", 
           row.names = FALSE)

################################################################################

# Street name formatting (in order to merge with air pollution data)
StrMtch$str = StrMtch$Straße %>%
    ReplaceUmlauts() %>%  # Replace umlauts and switch to lower case
    sub("str.$|str$|-strasse|strasse|-str.$", "", .)  # Delete street indicator

################################################################################
#############################Air Pollution Data#################################
################################################################################

# Read air-pollution data from excel
ap15 = read_excel("./03_Data_Preparation/Input/Air_Pollution_2015.xls", 
                  sheet = 1)

# Original names are too long and contain special symbols and spaces
mtch = read.csv2("./03_Data_Preparation/Input/matching.csv", 
                 sep = ";")  # Upload matching table for short names

names(ap15) = mtch$new[match(names(ap15), mtch$old)]  # Rename variables

################################################################################

ap15$Nr = as.numeric(ap15$Nr)  # Reformat steet section number to "numeric"

# Street name formatting (in order to merge with street-index matching table)
ap15$str = ap15$Street %>%
    ReplaceUmlauts() %>%  # Replace umlauts and switch to lower case
    sub("str.$|str$|-strasse|strasse|-str.$", "", .) %>%  # Delete street indicator
    sub("ak |as |ad ", "", .)  # Delete AK, AS, AD in the beginning

################################################################################
# Find PM10 and PM15 weighted averages for each street (by length of str.section)
Pltn = ap15 %>% 
    group_by(str) %>%
    summarize(L = sum(Length),
              MinPM10 = min(PM10_yearly), 
              MaxPM10 = max(PM10_yearly),
              AvgPM10 = weighted.mean(PM10_yearly, Length),
              MinPM25 = min(PM25_yearly), 
              MaxPM25 = max(PM25_yearly),
              AvgPM25 = weighted.mean(PM25_yearly, Length)) %>%
    arrange(str)

################################################################################
# Merge averaged air pollution tables with 
PltnM = merge(Pltn, StrMtch, by="str", all.x = TRUE, all.y = FALSE)

# Check how many streets were not merged
CountMissings(PltnM$PLZ)  # 51 streets were not in database for postal indexes

# Some streets are allocated to more than one district
# We assume here that such streets are equally devided among all districts 
# to which they belong

ap = PltnM %>%
    filter(!is.na(PLZ)) %>%  #filter NAs
    group_by(str) %>%
    mutate(DistrictLength = L/n()) %>% #appr. length of street in the district
    group_by(District) %>%
    summarize(PM10 = weighted.mean(AvgPM10, DistrictLength),  # av. PM10
              PM25 = weighted.mean(AvgPM25, DistrictLength)) %>%  # av. PM25
              
    arrange(desc(PM10))

# Save final result as csv
write.csv2(ap, 
           "./03_Data_preparation/Output/03_Data_preparation.csv", 
           row.names = FALSE)
