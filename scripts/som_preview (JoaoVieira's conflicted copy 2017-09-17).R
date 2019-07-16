#
# Rede de Kohonem
#
setwd("/Users/Joao Vieira/Dropbox/PPGEA/2 TRIM/REDES NEURAIS/")
source("ex3/JP_ex_03_mod.R") # funcao 'norma'
source("artigo/scripts/mapa_md.R") #funcao 'mapa'
require(SOMbrero)
#
# le arquivo
#
input1 <- read.table("artigo/cut1/input_som.txt",sep="\t",header=T)
input <- norma(input1[,2:dim(input1)[2]])
#
# inicializa SOM
#
dim <- c(1,4)
mygrid <- initGrid(dimension=dim, topo="square", dist.type="maximum")
ii <- initSOM(dimension=dim, topo=c("square"),
        radius.type=c("gaussian"),
        dist.type="euclidean",
        type="numeric", mode=c("online"),
        affectation=c("standard", "heskes"), maxit=500, nb.save=0,
        verbose=FALSE, proto0=NULL,
        init.proto="random",
        scaling="unitvar", eps0=1)
iris.som <- trainSOM(input,dimension=dim, topo=c("square"),
         radius.type=c("gaussian"),
         dist.type="euclidean",
         type="numeric", mode=c("online"),
         affectation=c("standard", "heskes"), maxit=500, nb.save=0,
         verbose=FALSE, proto0=NULL,
         init.proto="random",
         scaling="unitvar", eps0=1)
#
#
# =============================================
#
#


figura <- paste0("artigo/simulacoes/sim_01/rede_",dim[1],"x",dim[2],".jpg")
salveplot <-jpeg(figura,
                 width = 3000,
                 height=2000,
                 pointsize=2,
                 res = 1200,units = "px")
plot(iris.som, what="add", type="names", variable=input1[,1],
     scale=c(1.70,center=F,0.8))
dev.off()
#
# Mapa
#
fig_mapa <- paste0("artigo/simulacoes/sim_01/mapa_",dim[1],"x",dim[2],".jpg")
salveplot <-jpeg(fig_mapa,
                 width = 20,
                 height=10,
                 pointsize=1.0,
                 res = 1000,units = "cm")
mapa(input1[,1],iris.som$clustering)
dev.off()
#mapa("United States",1)
