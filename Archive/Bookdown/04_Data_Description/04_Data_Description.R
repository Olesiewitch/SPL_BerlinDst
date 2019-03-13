#================ READING IN ALL PREPARED DATA SETS ============================

dt1 = read.csv2("./04_Data_Description/SPL_BerlinDst_Data_Prep_1.csv",
                sep = ";",
                dec = ",", 
                row.names = 1,
                stringsAsFactors = FALSE)

dt2 = read.csv2("./04_Data_Description/SPL_BerlinDst_Data_Prep_2.csv", 
                sep = ",",
                dec = ".",
                row.names = 1,
                stringsAsFactors = FALSE)

# naming = read.csv2("./naming.csv", sep = ";")
# names(dt2) = naming$new[match(names(dt2), naming$old)]  # Rename variables

dt3 = read.csv2("./04_Data_Description/SPL_BerlinDst_Data_Prep_3.csv", 
                sep = ";",
                dec = ",", 
                stringsAsFactors = FALSE)

#==================== MERGE THE DATA SETS ======================================
DistricToFullName = function (column){
    column[grepl("mit",  column, ignore.case = TRUE)] = "Mitte" 
    column[grepl("fri",  column, 
                 ignore.case = TRUE)] = "Friedrichshain-Kreuzberg"
    column[grepl("pank", column, ignore.case = TRUE)] = "Pankow"
    column[grepl("mar",  column, ignore.case = TRUE)] = "Marzahn-Hellersdorf" 
    column[grepl("char", column,
                 ignore.case = TRUE)] = "Charlottenburg-Wilmersdorf"
    column[grepl("spa",  column, ignore.case = TRUE)] = "Spandau" 
    column[grepl("ste",  column, ignore.case = TRUE)] = "Steglitz-Zehlendorf"  
    column[grepl("tem",  column, ignore.case = TRUE)] = "Tempelhof-Schoneberg" 
    column[grepl("trep", column, ignore.case = TRUE)] = "Treptow-Kopenick" 
    column[grepl("neu",  column, ignore.case = TRUE)] = "Neukolln" 
    column[grepl("lich", column, ignore.case = TRUE)] = "Lichtenberg" 
    column[grepl("rein", column, ignore.case = TRUE)] = "Reinickendorf"
    
    return(column)
}

dt2$District = DistricToFullName(dt2$District)
dt3$District = DistricToFullName(dt3$District)

lvbInDt = merge(dt1, dt2[,-1], by = "District") %>%
    merge(dt3, by = "District")

write.csv2(lvbInDt, "renamed.csv")

#==================== DATA ANALYSIS ============================================
install.packages("pastecs")
install.packages("ggridges")
install.packages("ggplot2")

# library
library(ggridges)
library(ggplot2)
library(pastecs)
library(reshape2)
library(dplyr)

stats = lvbInDt %>% 
    select(-c(District, Nr)) %>%
    stat.desc()  #aggregated statistics

#==================DATA VISUALIZATION===========================================
#normalized data
stnd = function(x) {
    if (is.numeric(x))
        (x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) - min(x, na.rm=TRUE))
    else x 
}

lvbInNrm = as.data.frame(lapply(lvbInDt, stnd))

#prepare data for ggplot
lvbInPl = lvbInDt %>% 
    select(-Nr) %>%
    melt()

# Set a unique color with fill, colour, and alpha
lvbInPl %>% 
    ggplot(aes(x = variable, y = value, fill = variable)) + 
    geom_boxplot(color="red", alpha=0.2)

# basic example
lvbInPl %>% 
    filter(!variable %in% c("Stays", "Revenue","Tourists", "Cycle", "Flats")) %>%
    ggplot(aes(x = value, y = variable, fill = value)) +
    geom_density_ridges() +
    theme_ridges() +
    theme(legend.position = "none")

lvbInPl %>% 
    filter(variable == "Revenue") %>%
    ggplot(aes(x = value, y = variable, fill = value)) +
    geom_density_ridges() +
    theme_ridges() +
    theme(legend.position = "none")



# Library
library(plotly)

# A basic boxplot
library(plotly)
p <- plot_ly(midwest, x = ~percollege, color = ~state, type = "box")
p



#====================CORRELATION MATRIX=========================================
# Libraries
library(ellipse)
library(RColorBrewer)

# Correlations of variables
dt = cor(lvbInDt[,3:dim(lvbInDt)[2]])

# Build a Pannel of 100 colors with Rcolor Brewer
my_colors <- brewer.pal(5, "Spectral")
my_colors=colorRampPalette(my_colors)(100)

# Order the correlation matrix
ord <- order(dt[1, ])
data_ord = dt[ord, ord]
plotcorr(data_ord , col=my_colors[data_ord*50+50] , mar=c(1,1,1,1)  )
