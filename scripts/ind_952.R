#
# organiza dados
#
require(stringr)
setwd("/Users/João Pedro/Dropbox/PPGEA/2 TRIM/REDES NEURAIS/artigo/")
lista_ind <- list.files(path=paste0("raw/SIG/"),pattern = "*.csv")
ll <- 25
ind <- read.csv(paste0("raw/SIG/",lista_ind[ll]),sep=",",dec = ",")
lista_ind[ll]
#
#
mydata <- data.frame(ind$Country.or.Area.Name,
                     as.character(ind$X2000),as.character(ind$X2001),
                     as.character(ind$X2002),as.character(ind$X2003),
                     as.character(ind$X2004),as.character(ind$X2005),
                     as.character(ind$X2006),as.character(ind$X2007),
                     as.character(ind$X2008),as.character(ind$X2009),
                     as.character(ind$X2010),as.character(ind$X2011),
                     as.character(ind$X2012),as.character(ind$X2013),
                     as.character(ind$X2014),as.character(ind$X2015))

mydata[is.na(mydata)] <- -999

#View(mydata)
#
mydata1 <- as.data.frame(matrix(0,nrow=dim(mydata)[1],ncol=dim(mydata)[2]))
for(i in (1:dim(mydata)[1])){
  for(j in (1:dim(mydata)[2])){
    mydata1[i,j] <- str_replace_all(as.vector(mydata[i,j]),",","")
  }
  mydata1[i,2:17] <- as.numeric(mydata1[i,2:17])
  mydata1[i,2:17][is.na(mydata1[i,2:17])] <- -999}
colnames(mydata1) <- c("pais",2000:2015)
mydata <- mydata1
View(mydata)
#
#

countries <- mydata$pais[duplicated(mydata$pais)==F]
totalf <- data.frame("pais"=c(),"ind"=c())
for(c in (1:length(countries))){
  total <- mydata[which(mydata$pais==countries[c]),]
  total1 <- total[1,]
  for(a in (dim(total1)[2]:4)){
    if(total1[a]==""){total1[a] <- -999}
    if(as.numeric(total1[a])>0.0){
      total2 <- data.frame(total1[1],total1[a])
      colnames(total2) <- c("pais","ind")
      break}}
  totalf <- rbind(totalf,total2)
}
totalf <- totalf[which(duplicated(totalf$pais)==F),]
View(totalf)

output <- write.csv(totalf,paste0("cut1/",lista_ind[ll]),row.names = F)
