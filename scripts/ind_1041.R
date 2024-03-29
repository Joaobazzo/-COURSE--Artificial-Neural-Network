#
# organiza dados
#
require(stringr)
setwd("/Users/Jo�o Pedro/Dropbox/PPGEA/2 TRIM/REDES NEURAIS/artigo/")
lista_ind <- list.files(path=paste0("raw/SIG/"),pattern = "*.csv")
ll <- 2
ind <- read.csv(paste0("raw/SIG/",lista_ind[ll]))
lista_ind[ll]
View(ind)
#
#
mydata <- data.frame(ind$Country.or.Area.Name,ind$Location,ind$Series.Code,
                     ind$X2010,ind$X2011,ind$X2012,ind$X2013,ind$X2014,
                     ind$X2015,ind$X2016)
mydata[is.na(mydata)] <- -999
colnames(mydata) <- c("pais","local","code",
                      2010:2016)
mydata <- mydata[which(mydata$local=="Total (national level)"),]
countries <- mydata$pais[duplicated(mydata$pais)==F]
totalf <- data.frame("pais"=c(),"ind"=c())
for(c in (1:length(countries))){
  total <- mydata[which(mydata$pais==countries[c]),]
  if(length(total$local)>2){
    total1 <- total[which(total$local=="Total (national level)"),]}else{
      total1 <- total[1,]
    }
  for(a in (dim(total1)[2]:4)){
    if(total1[a]==""){total1[a] <- -999}
    if(as.numeric(total1[a])>0){
      total2 <- data.frame(total1[1],total1[a])
      colnames(total2) <- c("pais","ind")
      break}}
  totalf <- rbind(totalf,total2)
}

totalf <- totalf[which(duplicated(totalf$pais)==F),]
View(totalf)

output <- write.csv(totalf,paste0("cut/",lista_ind[ll]),row.names = F)
