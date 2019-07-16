#
# organiza dados
#
require(stringr)
setwd("/Users/João Pedro/Dropbox/PPGEA/2 TRIM/REDES NEURAIS/artigo/")
lista_ind <- list.files(path=paste0("raw/SIG/"),pattern = "*.csv")
ind <- read.csv(paste0("raw/SIG/",lista_ind[3]))
View(ind)
#
#
mydata <- data.frame(ind$Country.or.Area.Name,ind$Location,ind$Series.Code,
                     ind$X1991,ind$X1992,ind$X1993,ind$X1994,
                     ind$X1995,ind$X1996,ind$X1997,ind$X1998,ind$X1999,
                     ind$X2000,ind$X2001,ind$X2002,ind$X2003,ind$X2004,
                     ind$X2005,ind$X2006,ind$X2007,ind$X2008,ind$X2009,
                     ind$X2010,ind$X2011,ind$X2012,ind$X2013,ind$X2014,
                     ind$X2015,ind$X2016)
mydata[is.na(mydata)] <- -999
colnames(mydata) <- c("pais","local","code",1991:2016)
mydata <- mydata[which(mydata$code=="VC_DSR_AFFCT"),]
countries <- mydata$pais[duplicated(mydata$pais)==F]
totalf <- data.frame("pais"=c(),"total"=c(),"ind"=c())
for(c in (1:length(countries))){
  total <- mydata[which(mydata$pais==countries[c]),]
  if(length(total$local)>2){
    total1 <- total[which(total$local=="Total (national level)"),]}else{
      total1 <- total[1,]
    }
  for(a in (dim(total1)[2]:4)){
    if(total1[a]==""){total1[a] <- -999}
    if(as.numeric(total1[a])>0){
      total2 <- data.frame(total1[1],total1[2],total1[a])
      colnames(total2) <- c("pais","total","ind")
      break}}
  totalf <- rbind(totalf,total2)
}
View(totalf)

output <- write.csv(totalf,"cut/11.5.1.csv",row.names = F)
