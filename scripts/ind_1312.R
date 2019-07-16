#
# organiza dados
#
require(stringr)
setwd("/Users/João Pedro/Dropbox/PPGEA/2 TRIM/REDES NEURAIS/artigo/")
lista_ind <- list.files(path=paste0("raw/SIG/"),pattern = "*.csv")
ll <- 8
ind <- read.csv(paste0("raw/SIG/",lista_ind[ll]))
lista_ind[ll]
View(ind)
#
# mydata
#
mydata <- data.frame(ind$Country.or.Area.Name,ind$X2013,ind$X2014,
                     ind$X2015)
colnames(mydata) <- c("pais",2013:2015)
mydata[is.na(mydata)] <- -999
#
# converte 'mydata' para numero
#
mydata1 <- as.data.frame(matrix(0,nrow=dim(mydata)[1],ncol=dim(mydata)[2]))
for(i in (1:dim(mydata)[1])){
  for(j in (1:dim(mydata)[2])){
    mydata1[i,j] <- str_replace_all(as.vector(mydata[i,j]),",","")
  }
  mydata1[i,2:dim(mydata)[2]] <- as.numeric(mydata1[i,2:dim(mydata)[2]])
  mydata1[i,2:dim(mydata)[2]][is.na(mydata1[i,2:dim(mydata)[2]])] <- -999}
mydata <- mydata1
#
# checa paises duplicados
#
countries <- mydata$pais[duplicated(mydata$pais)==F]
totalf <- data.frame("pais"=c(),"ind"=c())
for(c in (1:length(countries))){
  total <- mydata[which(mydata$pais==countries[c]),]
  total1 <- total[1,]
  for(a in (dim(total1)[2]:2)){
    if(total1[a]==""){total1[a] <- -999}
    if(as.numeric(total1[a])>0){
      total2 <- data.frame(total1[1],total1[a])
      colnames(total2) <- c("pais","ind")
      break}}
  totalf <- rbind(totalf,total2)
}
totalf <- totalf[which(duplicated(totalf$pais)==F),]
View(totalf)
#
# exporta
#
output <- write.csv(totalf,"cut1/13.1.1.csv",row.names = F)
