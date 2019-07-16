#
# plota IDH
#
require(stringr)
setwd("/Users/Joao Vieira/Dropbox/PPGEA/2 TRIM/REDES NEURAIS/")
source("artigo/scripts/mapa_md.R") #funcao 'mapa'
idh <- read.csv("artigo/raw/SIG/HDI.csv",skip=1,sep=";")
idh$Country <- as.character(str_replace(idh$Country," ",""))
idh_s <- data.frame("pais"=idh$Country,"idh"=idh$X2015/1000,"classe"=1)
#
# classifica
#
idh_s$classe[which(idh_s$idh>=0.8)] <- 1
idh_s$classe[-c(which(idh_s$idh<0.7),which(idh_s$idh>0.8))] <- 2
idh_s$classe[-c(which(idh_s$idh<0.55),which(idh_s$idh>=0.699))] <- 3
idh_s$classe[which(idh_s$idh<0.55)] <- 4
lista <- c("Muito alto","Alto","Médio","Baixo")
#lista <- lista[4:1]
#
# Mapa
#
fig_mapa <- paste0("artigo/simulacoes/sim_02/IDH.jpg")
salveplot <-jpeg(fig_mapa,
                 width = 20,
                 height=10,
                 pointsize=1.50,
                 res = 200,units = "cm")
mapa_idh(as.vector(idh_s$pais),as.vector(idh_s$classe),lista)
dev.off()