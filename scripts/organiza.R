# -------------------
# organiza dados
# -------------------
require(stringr)
setwd("/Users/João Pedro/Dropbox/PPGEA/2 TRIM/REDES NEURAIS/artigo/")
lista_ind <- list.files(path=paste0("cut1/"),pattern = "*.csv")
lista_ind
# -------------------
# vetor de paises (total)
# -------------------
country <- as.character()
for(i in (1:length(lista_ind))){
  file <- read.csv(paste0("cut1/",lista_ind[i]))
  print(paste0(lista_ind[i]," i_",i," Dim_",dim(file)[2]))
  country <- append(country,as.character(file$pais))
  #print(paste0("dim_",dim(file)[2]))
}
country1 <- country[which(duplicated(country)==F)]
# -------------------
# varredura para juntar dados
# -------------------
file3 <- as.data.frame(as.vector(country1))
for(i in (1:length(lista_ind))){
  file1 <- read.csv(paste0("cut1/",lista_ind[i]))
  file2 <- c()
  dimf <- dim(file1)[2]
  for(c in (1:length(country1))){
    aux <- file1[,dimf][which(file1[,1]==country1[c])]
    if(length(aux)==0){aux <- -999}
    if(length(aux)>1){print(paste0("_",lista_ind[i],"_",which(file1[,1]==country1[c])))}
    file2 <- append(file2,aux)
    } # indice
  file3 <- cbind(file3,file2)
}
colnames(file3) <- c("nome",lista_ind)
# -------------------
# Exclui paises com falhas
# maiores que determinado criterio
# -------------------
file4 <- data.frame()
for(l in (1:dim(file3)[1])){
  num_falhas <- length(file3[l,][which(file3[l,]=="-999")])
  indice <- round(num_falhas/(dim(file3)[2]-1),2)
  if(indice < 0.35){file4 <- rbind(file4,file3[l,])}
}
# -------------------
# Exclui paises que nao tem 
# dados de HDI (Human Development Index)
# -------------------
hdi <- read.csv("raw/SIG/HDI.csv",sep=";",header = T,skip=1)
pais_hdi <- as.character(str_replace(hdi$Country," ",""))
file5 <- data.frame()
for(m in (1:dim(file4)[1])){
  if(file4$nome[m]%in%pais_hdi==TRUE){
    file5 <- rbind(file5,
                   file4[which(file4$nome==
                                 as.character(file4$nome[m])),])
  }
  #if(file4$nome[m]%in%pais_hdi==FALSE){
  #  print(as.character(file4$nome[m]))}
}
# -------------------
# Percentual de falhas por pais
# -------------------
name_saida <- paste0("cut1/falhas_paises.txt")
write(paste0("Lista Paises "),name_saida)
for(k in (1:dim(file5)[1])){
  num_falhas <- length(file5[k,][which(as.numeric(file5[k,])==-999)])
  write(paste0(file5[k,1]," > Falhas_",
               round(100*num_falhas/(dim(file5)[2]-1),2),"% | prop. ",
               num_falhas,"/",dim(file5)[2]-1),name_saida,append=T)
}
# -------------------
# Quantidade de falhas por indicador
# -------------------
name_falhas <- paste0("cut1/falhas_indicador.txt")
write(paste0("Lista Indicadores "),name_falhas)
for(k in (2:dim(file5)[2])){
  num_falhas <- length(file5[,k][which(as.numeric(file5[,k])==-999)])
  write(paste0("Indicador_",lista_ind[k-1],"___Falhas_",
               num_falhas,"/",length(file5[,k]),"   => ",
               round(100*num_falhas/length(file5[,k]),1),"%"),name_falhas,append=T)
}
# -------------------
# Varredura para completar com médias
# -------------------
file6 <- file5
for(j in (2:dim(file5)[2])){
  list_of <- which(file5[,j]==-999)
  list_on <- which(file5[,j]>=0)
  # novo
  file6[list_of,j] <- round(ave(file5[list_on,j])[1],2)
  file6[list_on,j] <- file5[list_on,j]
}
#View(file4)
write.table(file6,"cut1/input_som.txt",sep="\t",row.names = T,col.names = T)
View(file6)

