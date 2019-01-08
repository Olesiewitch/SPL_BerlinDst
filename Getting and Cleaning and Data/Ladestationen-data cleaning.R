setwd("/Users/qier/Downloads")
library(readxl)
ladestationen = read_excel("ladestationen.xls")
Postleitzahlen = read_excel("~/Downloads/ZuordnungderBezirkezuPostleitzahlen.xls")
View(Postleitzahlen)

Bezirk<-Postleitzahlen$X__1[!is.na(Postleitzahlen$X__1)];Bezirk
Bezirk<-Bezirk[2:length(Bezirk)];Bezirk
Bezirk[2]<-"Friedrichshain-Kreuzberg";Bezirk<- Bezirk[-3];Bezirk
Bezirk[4]<-"Charlottenburg";Bezirk<- Bezirk[-5];Bezirk
Bezirk[7]<-"Tempelhof-Schöneberg";Bezirk<- Bezirk[-8];Bezirk

class(Bezirk)#charachter

adress<-strsplit(ladestationen$Adresse,"[,]");adress
adress1<-c()
for(i in 1:length(adress)) {
  adress1[i]<- adress[[i]][2] 
    }   ;  adress1                           
adress1<-as.character(adress1)
adress1<-strsplit(adress1," ")
adress11<-c()
for(i in 1:length(adress)) {
  adress11[i]<- adress1[[i]][2] 
}   ;  adress11 

ladestationen1<-data.frame(ladestationen,adress11)
View(ladestationen1)

a1<-as.numeric(c(Postleitzahlen$X__2[6:8],Postleitzahlen$X__3[6:8],
     Postleitzahlen$X__4[6:8],Postleitzahlen$X__5[6:8],
     Postleitzahlen$X__6[6:8],Postleitzahlen$X__7[6:8],
     Postleitzahlen$X__8[6:7],Postleitzahlen$X__9[6:7],
     Postleitzahlen$X__10[6:7],Postleitzahlen$X__11[6:7]))
a2<-as.numeric(c(Postleitzahlen$X__2[10:11],Postleitzahlen$X__3[10:11],
                Postleitzahlen$X__4[10:11],Postleitzahlen$X__5[10:11],
                Postleitzahlen$X__6[10:11],Postleitzahlen$X__7[10],
                Postleitzahlen$X__8[10],Postleitzahlen$X__9[10],
                Postleitzahlen$X__10[10],Postleitzahlen$X__11[10]));b

a3<-as.numeric(unlist(as.list(Postleitzahlen[13:15,3:11])))
a4<-as.numeric(unlist(as.list(Postleitzahlen[17:20,3:11])))
a5<-as.numeric(unlist(as.list(Postleitzahlen[22:23,3:11])))
a6<-as.numeric(unlist(as.list(Postleitzahlen[25:27,3:11])))
a7<-as.numeric(unlist(as.list(Postleitzahlen[29:32,3:11])))
a8<-as.numeric(unlist(as.list(Postleitzahlen[34:36,3:11])))
a9<-as.numeric(unlist(as.list(Postleitzahlen[38:39,3:11])))
a10<-as.numeric(unlist(as.list(Postleitzahlen[41:42,3:11])))
a11<-as.numeric(unlist(as.list(Postleitzahlen[44:45,3:11])))
a12<-as.numeric(unlist(as.list(Postleitzahlen[47:48,3:11])))

Postleitzhal<- as.data.frame(cbind(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12))

count<-rep(0,12)

for(i in 1:length(ladestationen1$adress11)){
  if(ladestationen1$adress11[i] %in% Postleitzhal[,i]){
    count[i]<-count[i]+1
    }
};count 





