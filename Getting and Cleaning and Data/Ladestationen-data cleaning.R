library(readxl)
ladestationen = read_excel("ladestationen.xls")
Postleitzahlen = read_excel("ZuordnungderBezirkezuPostleitzahlen.xls")
View(Postleitzahlen)

Bezirk=Postleitzahlen$X__1[!is.na(Postleitzahlen$X__1)];Bezirk
Bezirk=Bezirk[2:length(Bezirk)];Bezirk
Bezirk[2]="Friedrichshain-Kreuzberg";Bezirk<- Bezirk[-3];Bezirk
Bezirk[4]="Charlottenburg-Wilmersdorf";Bezirk<- Bezirk[-5];Bezirk
Bezirk[7]="Tempelhof-Schöneberg";Bezirk<- Bezirk[-8];Bezirk

class(Bezirk)#charachter

#extract post code in column 'ladestationen$Adresse', where information is listed as 
#e.g. "Malteserstrasse 136<U+2013>138, 12249 Berlin", and create a new column with post code

adress=strsplit(ladestationen$Adresse,"[,]");adress
adress1=c()
for(i in 1:length(adress)) {
  adress1[i]<- adress[[i]][2] 
    }   ;  adress1                           
adress1<-as.character(adress1)
adress1<-strsplit(adress1," ")
adress11<-c()
for(i in 1:length(adress)) {
  adress11[i]<- adress1[[i]][2] 
}   ;  adress11 
#create a new dataframe with the name ladestationen1 with the new column of post code
ladestationen1=data.frame(ladestationen,adress11)
View(ladestationen1)

#extract the post code of each district from the original excel file 'ZuordnungderBezirkezuPostleitzahlen'

district01<-as.numeric(unlist(as.list(Postleitzahlen[6:8,3:12])))
district02<-as.numeric(unlist(as.list(Postleitzahlen[10:11,3:12])))
district03<-as.numeric(unlist(as.list(Postleitzahlen[13:15,3:12])))
district04<-as.numeric(unlist(as.list(Postleitzahlen[17:20,3:12])))
district05<-as.numeric(unlist(as.list(Postleitzahlen[22:23,3:12])))
district06<-as.numeric(unlist(as.list(Postleitzahlen[25:27,3:12])))
district07<-as.numeric(unlist(as.list(Postleitzahlen[29:32,3:12])))
district08<-as.numeric(unlist(as.list(Postleitzahlen[34:36,3:12])))
district09<-as.numeric(unlist(as.list(Postleitzahlen[38:39,3:12])))
district10<-as.numeric(unlist(as.list(Postleitzahlen[41:42,3:12])))
district11<-as.numeric(unlist(as.list(Postleitzahlen[44:45,3:12])))
district12<-as.numeric(unlist(as.list(Postleitzahlen[47:48,3:12])))

Postleitzhal<- as.data.frame(cbind(district01,district02,district03,district04,
                                   district05,district06,district07,district08,
                                   district09,district10,district11,district12))

#compare the post code of each ladestation, and assgin a district to each ladestation
dist_lade= rep(0,length(ladestationen1$adress11))
for(i in 1:length(ladestationen1$adress11)){
  for(j in 1:12){
    if(ladestationen1$adress11[i] %in% Postleitzhal[,j]){
      dist_lade[i]=j
    }
  }
}
sum(dist_lade!=0)

#count the number of ladestation in each district
count=rep(0,12)
for(i in 1:12){
  for(j in 1:length(dist_lade)){
    if(dist_lade[j]==i)
      count[i]=count[i]+1
  }
};count







