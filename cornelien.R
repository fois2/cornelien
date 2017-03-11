###################################################################
#########################CORNELIEN#################################
###################################################################

#Création de données fictives d'un restaurant
# Ouvert de 11h à 14h et 19 à 22h environ chiffre affaire moyen midi 20*10=200e 
# et soir 20*15=300e avec variation saisonnière et hebdomadaire
setwd("~/Dropbox/recherche/recherche/autres/cornelien")
require(xts)
library(stringr)
time_index <- seq(from = as.POSIXct("2015-01-01 12:00"), 
                  to = as.POSIXct("2017-03-01 24:00"), by = 60*60*12*2)
entreprise<-as.data.frame(time_index)
set.seed(1)
entreprise$jour<-weekdays(entreprise$time_index)
entreprise$mois<-months(entreprise$time_index)
entreprise$annee<-format(time_index, format = "%Y")
#faison
n_midi<-10*rnorm(length(time_index),20,2)
n_midi<-n_midi*rep(c(0.9,1.2,1.5,0,0.7,0.8,1.2),length(time_index)/7)*  
  #0.9 fois moins de chiffre d'affaire car 2015 commmence un jeudi...
  #je suppose premier jour time "lundi"
                                                        c(rep(c(
                                                        rep(0.7,30),#janvier
                                                        rep(0.8,30),
                                                        rep(0.9,30),
                                                        rep(1,30),
                                                        rep(1.2,30),
                                                        rep(1.5,30),
                                                        rep(1.2,30),
                                                        rep(0.6,30),
                                                        rep(1,30),
                                                        rep(0.9,30),
                                                        rep(0.8,30),
                                                        rep(0.9,30)),#décembre
                                                        length(time_index)/(365) )
                                                        )
entreprise$chiffre_affaires12 <- n_midi
entreprise<-subset(entreprise, entreprise$jour != "Dimanche")#j'enlève les journées non travaillées
entreprise$chiffre_affaires12  <- xts(entreprise$chiffre_affaires12 , order.by = entreprise$time_index)
write.table(entreprise,file="entreprise.csv",sep=";", row.names = F,col.names = F)
par(mfrow = c(2, 1))
plot(entreprise$chiffre_affaires12 )
dec<-decompose(ts(entreprise$chiffre_affaires12 ,frequency = 6))
plot(dec)
dec_trend<-decompose(ts(dec$trend,frequency = 30))
plot(dec_trend)
px<-as.numeric(dec_trend$seasonal)+as.numeric(dec$seasonal)+as.numeric(dec_trend$trend)
plot(entreprise$chiffre_affaires12,main="approximation par décomposition")
lines(px~entreprise$time_index,col="red")
#prévision avec lm voir partie correspondante plus loin

 ##météo
 #http://www.meteofrance.com/climat/meteo-date-passee?lieuId=593500&lieuType=VILLE_FRANCE&date=10-02-2016
 url<-as.data.frame(format(entreprise$time_index,format = "%d-%m-%Y"))#les URL sont différentiés par leur date  
 library(XML)
 extracteur <- function(URL){
     doc <- htmlParse((paste("http://www.meteofrance.com/climat/meteo-date-passee?lieuId=593500&lieuType=VILLE_FRANCE&date=",URL)))
     v1 <- xpathSApply(doc,"//div[@class= 'box-a clearfix']/ul",xmlValue)
  
   v2 <- xpathSApply(doc,  "//div[@class= 'echeances']",xmlValue)
   return(data.frame(v1, v2))
 }
 data <- c()
 error <- c()
 for (l in 1:(dim(url)[1])){
   tmp <- NULL
   try(tmp <- extracteur(url[l,1]))
       if (is.null(tmp)) {
         error <- c(error, l)
       } else {
         data <- rbind(data, tmp)
       }
 }
 write.table(data,file="data.txt",sep="", row.names = F,col.names = F)
 data<-read.table("data.txt", sep="",dec=".", na=" ",header=F)
 #creation fonction supprimant espace
 supprespace=function(x){
   a<-unlist(strsplit(x," "))
   paste(a[-which(a=="")],collapse=" ")
 }
 
 lille<-data.frame()
 meteolille<-data.frame()
 #la fonction gsub me permet de remplacer des caractères par d'autres
 for (i in 1:(dim(data)[1])) {
#### extraction donneées de la premiere colonne de data
   lille[i,1]<-    gsub("C","",
                 gsub("h","",
                 gsub("mm","",
                 gsub("Hauteur des précipitations : ", ";",
                 gsub("Durée d'ensoleillement de la journée : ", ";",
                 gsub("Température maximale de la journée :", ";" ,
                      gsub("°","",
                           gsub("Température minimale de la journée :","",data[i,1]))))))))
   lille[i,1]<-supprespace(as.character(lille[i,]))#suppression espace
   lille[i,1]<-gsub("\n","",lille[i,1])
   lille[i,1]<-gsub(" ","",lille[i,1])
   A<-str_locate_all(lille[i,1], ";")#je localise là où j'ai mis des ";"
   meteolille[i,1]<-url[i,1]
   meteolille[i,2]<-str_sub(lille[i,1], 1, (A[[1]][1,1])-1)#tout ce qui est avant le premier";"
   meteolille[i,3]<-str_sub(lille[i,1], (A[[1]][1,1])+1, (A[[1]][2,1])-1)
   meteolille[i,4]<-str_sub(lille[i,1], (A[[1]][2,1])+1, (A[[1]][3,1])-1)
   meteolille[i,5]<-str_sub(lille[i,1], (A[[1]][3,1])+1, -1)
   meteolille[i,5]<-gsub("NA","",meteolille[i,5])
#### extraction donneées de la seconde colonne de data
   apparition<-str_detect(data[i,2], c("10 h","13 h","16 h"))
   lille[i,2]<-   gsub("16 h", ";",
                                  gsub("13 h", ";",
                                      gsub("10 h", ";" 
                                              ,data[i,2])))
   lille[i,2]<-paste(lille[i,2],";")
   lille[i,2]<-supprespace(lille[i,2])#suppression espace
   lille[i,2]<-gsub("\n","",lille[i,2])
   A<-str_locate_all(lille[i,2], ";")#je localise là où j'ai mis des ";"
   Nombre.virgule<-cumsum(ifelse(apparition==F,0,1))
   meteolille[i,6]<-ifelse(1%in%which(apparition==T),str_sub(lille[i,2],  (A[[1]][1,1])+1, (A[[1]][2,1])-1),(meteolille[i,6]<-NA))#tout ce qui est après le premier";" et avant deuxieme
   meteolille[i,7]<-ifelse(Nombre.virgule[2]>Nombre.virgule[1],str_sub(lille[i,2], (A[[1]][Nombre.virgule[2],1])+1, (A[[1]][Nombre.virgule[3],1])-1),NA)
   meteolille[i,8]<-ifelse(Nombre.virgule[3]>Nombre.virgule[2],str_sub(lille[i,2], (A[[1]][Nombre.virgule[3],1])+1, -1),NA)
   meteolille[i,8]<-gsub(";","",meteolille[i,8])
    }
 colnames(meteolille)<-c("date","Temp.min","Temp.max","Duree.ensoleil","Hauteur.precip","10h","13h","16h")
 write.table(meteolille,file="meteolille.csv",sep=";",row.names = F,col.names = T)
 library(readr)
 meteolille <- read.csv("/Users/medjahedfouad/Dropbox/recherche/recherche/cornelien/cornelien/meteolille.csv", 
                          sep = ";")
#prévision lm
 plot(entreprise$chiffre_affaires12,main="approximation par décomposition")
 fusion<-cbind(entreprise,meteolille) 
 reg<-lm(chiffre_affaires12~jour+mois+annee+Temp.min+Temp.max+Duree.ensoleil+Hauteur.precip,data=fusion)
 lines(reg$fitted.values,col="red")
meteoprevu<-c(dim(url)[1])
demain<-as.POSIXlt(time_index[length(time_index)])# pour ajouter journée
demain$mday<-demain$mday+1
new<-as.data.frame(cbind(weekdays(demain),months(demain),format(demain, format = "%Y"),15,18,6,1))
colnames(new)<-c("jour","mois","annee","Temp.min","Temp.max","Duree.ensoleil","Hauteur.precip")
new$Temp.min<-as.numeric(new$Temp.min)
new$Temp.max<-as.numeric(new$Temp.max)
new$Duree.ensoleil<-as.numeric(new$Duree.ensoleil)
new$Hauteur.precip<-as.numeric(new$Hauteur.precip)
predict(reg,new)

 