#replace all the Umlauts by latin equivalents
ReplaceUmlauts = function(clmn){
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
    sum(ifelse(is.na(column),1,0))
}
