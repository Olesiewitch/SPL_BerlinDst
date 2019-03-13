#replace all the Umlauts by latin equivalents
ReplaceUmlauts = function(clmn){
    #Description: Replaces german special symbols and turns to lower case
    #Author: Aleksandra Kudaeva
    #Input:  column where you want to replace umlauts
    #Output: column without umlauts (lower case)
    
    clmn = tolower(clmn)  #all strings to lower case
    
    #check if at least one element of a vector has any umlauts in it
    #replaces umlauts until there are no one left
    while(any(grepl("ä|ö|ü|ß",clmn)) == TRUE) {
        clmn %<>% 
            sub("ä", "ae", .) %<>% 
            sub("ö", "oe", .) %<>% 
            sub("ü", "ue", .) %<>% 
            sub("ß", "ss", .)
    }
    return(clmn)
}

#count NAs
CountMissings=function(column){
    #Description: count NAs
    #Author: Aleksandra Kudaeva
    #Input:  column where you want to count missings
    #Output: number of missings
    sum(ifelse(is.na(column),1,0))
}

CallPack = function(pack){
  # Description: checks if the package from the list was installed,  
  # install new packages and load all the packages from the list
  # Author: Aleksandra Kudaeva
  # Args: vector with package names
  # Returns: -
  
  #create a list of packages that are not installed
  uninstalled = pack[!(pack %in% installed.packages()[,"Package"])]
  
  #create a list of packages that are installed
  installed   = pack[(pack %in% installed.packages()[,"Package"])]
  
  for (pack in uninstalled) {  #if not installed yet
    install.packages(pack, character.only = TRUE)  #install packages 
    library(pack, character.only = TRUE)  #call packages
  }
  for (pack in installed) {  #if already installed
    library(pack, character.only = TRUE)  #load packages
  }
}