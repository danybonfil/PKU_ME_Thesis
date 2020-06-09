##### Cognitive Games
rm(list=ls())

##### 1. List of files - Base_100 ##### 
library(tidyr)
library(dplyr)
library(reshape2)
# library(tidyverse)


setwd("C:/Users/danbo/Desktop/Original")

#Lista de archivos
list_of_files <- list.files("C:/Users/danbo/Desktop/Original",
                            recursive = FALSE,
                            pattern = "\\.txt$", 
                            full.names = FALSE)

#Nos quedamos con los nombres que impliquen un juego
list_of_games <- list_of_files[grepl("nback|stroop|iowa",list_of_files)]

#Ver cuales quedaron fuera
list_of_files[!grepl("nback|stroop|iowa",list_of_files)]


#Extraemos información de los nombres de los archivos de juegos
list_of_games <- data.frame("Nombre"=list_of_games,
                            stringsAsFactors=FALSE)


#Crear columna de orden de juegos
list_of_games$grupo <- NA
list_of_games$grupo[grepl("(nbackt\\.|stroopnp1\\.|iowagt2\\.)",list_of_games$Nombre)] <- "1_N-S-I"
list_of_games$grupo[grepl("(stroopnp\\.|iowagt1\\.|nbackt2\\.)",list_of_games$Nombre)] <- "2_S-I-N"
list_of_games$grupo[grepl("(iowagt\\.|nbackt1\\.|stroopnp2\\.)",list_of_games$Nombre)] <- "3_I-N-S"


list_of_games <- list_of_games %>% 
  separate("Nombre", 
           c("treatGame","fecha","data","playerID","txt"), 
           sep="\\.", remove=TRUE) %>% 
  select(-c(fecha,data,txt))


#Columna con información de caracteres antes del nombre del juego
list_of_games$treatment <- gsub("(nback|stroop|iowa).*","",
                                list_of_games$treatGame)
list_of_games$orden <- gsub(".*(nback|stroop|iowa)","",
                            list_of_games$treatGame)

list_of_games$game <- NA
list_of_games$game[grepl("nback",list_of_games$treatGame)] <- "nback"
list_of_games$game[grepl("stroop",list_of_games$treatGame)] <- "stroop"
list_of_games$game[grepl("iowa",list_of_games$treatGame)] <- "iowa"


###Leer csv para saber No Bonus
mturk <- read.csv("C:/Users/danbo/Desktop/Original/data.csv")
#Convert DF
mtdf <- as.data.frame(mturk)
#Quitar NAs solo quienes terminan
mtdfclean <- as.data.frame(mtdf[!is.na(mtdf$TIME_total),])
#Participant id treatment time
part <- select(mtdfclean, participant, random_treat.1, TIME_total)
#Separar participant s.--- para tener el id
names(part)[1] <- "playerID"
part$playerID <- gsub(".*s.","",
                      part$playerID)
part$playerID <- gsub(".txt","",
                      part$playerID)

part <- select(part, playerID, random_treat.1, TIME_total)

#Cambiar nombre a treatments == o %in% c(1,2,3) para mas de uno
part$random_treat.1[part$random_treat.1 %in% 1] <- "a1"
part$random_treat.1[part$random_treat.1 %in% 2] <- "a2"
part$random_treat.1[part$random_treat.1 %in% 3] <- "a3"
part$random_treat.1[part$random_treat.1 %in% 4] <- "a4"
part$random_treat.1[part$random_treat.1 %in% 5] <- "i1"
part$random_treat.1[part$random_treat.1 %in% 6] <- "i2"
part$random_treat.1[part$random_treat.1 %in% 7] <- "i3"
part$random_treat.1[part$random_treat.1 %in% 8] <- "d1"
part$random_treat.1[part$random_treat.1 %in% 9] <- "d2"
part$random_treat.1[part$random_treat.1 %in% 10] <- "d3"
part$random_treat.1[part$random_treat.1 %in% 11] <- "n1"
part$random_treat.1[part$random_treat.1 %in% 12] <- "n2"
part$random_treat.1[part$random_treat.1 %in% 13] <- "n3"
part$random_treat.1[part$random_treat.1 %in% 14] <- "n4"


##sigue hacer match de player id con treatment (merge da nuevo DF)
prueba <- merge(list_of_games, part)
#Select & rename
prueba <- select(prueba, playerID, grupo, game, random_treat.1, TIME_total)
prueba$treat <- prueba$random_treat.1
prueba$time <- prueba$TIME_total
#Base lista
base <- select(prueba, playerID, grupo, game, treat, time)

##


##Entrar para sacar resultados
#Inicializar i para probar manualmente
i<-1

#Creamos data.frame vacío
res <- data.frame("FILE"=NULL,"RES"=NULL, "DIF"=NULL, "4"=NULL, "5"=NULL,"game"=NULL,"itime"=NULL)

## Genérico para todos
for(i in 1:length(list_of_files)){
  list_of_files[i]
  #Leemos el i-ésimo archivo
  aux <- read.csv(list_of_files[i], sep=" ", header=FALSE)
  #Si el nombre del archivo contiene "nback"...
  if(grepl("nbackt",list_of_files[i])){
    #dos errores promedio
    res[i,1] <- list_of_files[i]
    res[i,2] <- aux[dim(aux)[1],1]
    res[i,3] <- aux[dim(aux)[1],2]
    res[i,5] <- mean(c(res[i,2],res[i,3]))/100
    res[i,4] <- 1-res[i,5]
    res[i,6] <- "nback"
    res[i,7] <- (sum(aux[1:dim(aux)[1],3],NA, na.rm = TRUE))/1000
  }
  if(grepl("stroopnp",list_of_files[i])){
    #difference and error perc
    res[i,1] <- list_of_files[i]
    res[i,2] <- aux[dim(aux)[1],1]
    res[i,3] <- aux[dim(aux)[1],2]
    res[i,4] <- res[i,3]/100
    res[i,5] <- NA
    res[i,6] <- "stroop"
    res[i,7] <- (sum(aux[1:dim(aux)[1],8],NA, na.rm = TRUE))/1000
  }
  if(grepl("iowagt",list_of_files[i])){
    #win/loss
    res[i,1] <- list_of_files[i]
    res[i,2] <- as.numeric(as.character(aux[dim(aux)[1],1]))
    res[i,3] <- aux[dim(aux)[1],2]
    res[i,4] <- (res[i,2]-2000)/2000
    res[i,5] <- NA
    res[i,6] <- "iowa"
    res[i,7] <- (sum(as.numeric(as.character(aux[1:dim(aux)[1],1])))-res[i,2])/1000
  }
}
#Todos los resultado
names(res) <- c("playerID","res", "dif","corerroi","percent","game","itime")
#Select & Merge by game
resultados <- select(res, playerID, corerroi, game, itime)  

resultados$playerID <- gsub(".*.data.","",
                            resultados$playerID)
resultados$playerID <- gsub(".txt","",
                            resultados$playerID)

completa <- merge(base, resultados)

base100 <- select(completa, playerID, grupo, treat, game, corerroi, itime, time)  

##### PRE POST Tasks - preposttask #####

library(plyr)
library(tidyr)
library(tidyverse)
library(dplyr)

setwd("C:/Users/danbo/Desktop//Original")

mturk <- read.csv("C:/Users/danbo/Desktop/Original/data.csv")
#Convert DF
mtdf <- as.data.frame(mturk)
#Quitar NAs solo quienes terminan
mtdfclean <- as.data.frame(mtdf[!is.na(mtdf$TIME_total),])
#Participant id treatment time
part <- select(mtdfclean, participant, random_treat.1, TIME_total)

#Separar Entry Survey
pretask <- mtdfclean[,0:17]
names(pretask)[1] <- "playerID"
pretask$playerID <- gsub(".*s.","",
                         pretask$playerID)
pretask$playerID <- gsub(".txt","",
                         pretask$playerID)
#Separar Exit Survey
#Se podrían haber sacado las dos secciones con c() mtdfclean[,0:17] +el resto
posttask <- mtdfclean[c(1,(length(mtdfclean)-14):length(mtdfclean))]
names(posttask)[1] <- "playerID"
posttask$playerID <- gsub(".*s.","",
                          pretask$playerID)
posttask$playerID <- gsub(".txt","",
                          pretask$playerID)
#mi codigo que fue mejorado arriba
#mtdfposttask <- mtdfclean[,c(1,(ncol(mtdfclean)-14):ncol(mtdfclean))]


##### Bonos y Pagos - base100.comp.pre.post #####

#Hacer un csv con catalogo de pagos, leer y hacer melt por tratamiento
#cambiar el nombre de las variables para que coincidan con base 100

#Incluir Delta pagos para regresion Harvard, y=corerroi; x=delta i3-i1, i3-i2
deltapagos1 <- read.csv("deltapagos1.csv",stringsAsFactors=FALSE)
deltapagos1.m <- melt(deltapagos1,"treat",
                      value.name="deltapago1") %>% 
  separate("variable", into=c("grupo","game"), sep="_")

deltapagos1.m$grupo[deltapagos1.m$grupo=="grupo1"] <- "1_N-S-I"
deltapagos1.m$grupo[deltapagos1.m$grupo=="grupo2"] <- "2_S-I-N"
deltapagos1.m$grupo[deltapagos1.m$grupo=="grupo3"] <- "3_I-N-S"

#Incluir Delta pagos para regresion Harvard, y=corerroi; x=delta i2-i1, i3-i2
deltapagos2 <- read.csv("deltapagos2.csv",stringsAsFactors=FALSE)
deltapagos2.m <- melt(deltapagos2,"treat",
                      value.name="deltapago2") %>% 
  separate("variable", into=c("grupo","game"), sep="_")

deltapagos2.m$grupo[deltapagos2.m$grupo=="grupo1"] <- "1_N-S-I"
deltapagos2.m$grupo[deltapagos2.m$grupo=="grupo2"] <- "2_S-I-N"
deltapagos2.m$grupo[deltapagos2.m$grupo=="grupo3"] <- "3_I-N-S"
#Delta pagos 3-2
deltapagos3 <- read.csv("deltapagos3.csv",stringsAsFactors=FALSE)
deltapagos3.m <- melt(deltapagos3,"treat",
                      value.name="deltapago3") %>% 
  separate("variable", into=c("grupo","game"), sep="_")

deltapagos3.m$grupo[deltapagos3.m$grupo=="grupo1"] <- "1_N-S-I"
deltapagos3.m$grupo[deltapagos3.m$grupo=="grupo2"] <- "2_S-I-N"
deltapagos3.m$grupo[deltapagos3.m$grupo=="grupo3"] <- "3_I-N-S"

deltapagos.m <- merge(deltapagos1.m,deltapagos2.m,all.x=TRUE,sort = FALSE)
deltapagos.m <- merge(deltapagos.m,deltapagos3.m,all.x=TRUE,sort = FALSE)

#Es mejor cambiar nombre de grupo
pagos <- read.csv("pagos.csv",stringsAsFactors=FALSE)

pagos.m <- melt(pagos,"treat",
                value.name="pago") %>% 
  separate("variable", into=c("grupo","game"), sep="_")

pagos.m$grupo[pagos.m$grupo=="grupo1"] <- "1_N-S-I"
pagos.m$grupo[pagos.m$grupo=="grupo2"] <- "2_S-I-N"
pagos.m$grupo[pagos.m$grupo=="grupo3"] <- "3_I-N-S"

### Prueba de pago por juego en orden de grupo
pagos.game <- as.data.frame(dcast(pagos.m,
                                  grupo+treat
                                  ~game, value.var="pago"))
names(pagos.game)[names(pagos.game) == "nback"] <- "nback.bonos"
names(pagos.game)[names(pagos.game) == "stroop"] <- "stroop.bonos"
names(pagos.game)[names(pagos.game) == "iowa"] <- "iowa.bonos"

#pagos.mg <- merge(pagos.m,pagos.game,all.x=TRUE,sort = FALSE)
pagos.m <- merge(pagos.m,deltapagos.m,all.x=TRUE,sort = FALSE)

base100.pagos <- merge(base100,pagos.m,all.x=TRUE)

### PARA NBACK 30-64% para que sea parecido, y es la media de accurr Jaeegg
#> count(base.pop$nback<=0.05) 30trials || count(base.pop$nback>=0.5) son 103
#x freq
#1 FALSE  717
#2  TRUE  287
#> count(base.pop$nback>=0.36) prueba con .64 que solo son 22 (perm 36 error)
#x freq
#1 FALSE  718
#2  TRUE  286
###PARA STROOP OK, 5% ERROR MAX = 3 trials de 10 train, neutral, nega
# aca 10, 15 y 15 ORIG eran 10 , 12 y 12 (Articulo ave error 6%)
#> count(base.pop$stroop<=0.05)
#x freq
#1 FALSE  404
#2  TRUE  600
###PARA IOWA esperabamos mejor performance las metricas son por cartas 100
# MEan bechara 94, 98 y 99 es de 28%, hay 4 estudios con (-) ave de 7.5
#> count(base.pop$iowa>=-0.05)
#x freq
#1 FALSE  954
#2  TRUE   50
#Original, Updated = N top 10, S (20,60) top 60, I top 20 
#> count(base.pop$iowa>=-0.05)+count(base.pop$stroop<=0.05)+count(base.pop$nback<=0.05)
#x freq
#1 0 2064
#2 3  933
#> count(base.pop$iowa>=-0.55)+count(base.pop$stroop<=0.05)+count(base.pop$nback>=0.5)
#x freq
#1 0 2095
#2 3  902
#para meter a todos en el excelent, cambiar iowa maximo 25%
#> Opción 1) Wins count(base.pop$iowa>=-0.55)+count(base.pop$stroop<=0.05)+count(base.pop$nback>=0.5)
#x freq
#1 0 2095
#2 3  902
#> count(base.pop$iowa>=-0.25)+count(base.pop$stroop<0.05)+count(base.pop$nback>=0.5)
#x freq
#1 0 2340
#2 3  657
#> Opción 2) count(base.pop$iowa>=-0.55)+count(base.pop$stroop<=0.05)+count(base.pop$nback>=0.27)
#x freq
#1 0 1796
#2 3 1201

#Los que son verdad = 1 Monto es Boolean
base100.pagos$monto <- NA
base100.pagos$monto[base100.pagos$game == "nback" &
                      base100.pagos$corerroi >=0.27] <- TRUE
base100.pagos$monto[base100.pagos$game == "stroop" &
                      base100.pagos$corerroi <=0.05] <- TRUE
base100.pagos$monto[base100.pagos$game == "iowa" &
                      base100.pagos$corerroi >=-0.55] <- TRUE

#Los que son falsos = 0
base100.pagos$monto[is.na(base100.pagos$monto)] <- 0

#Multiplica los true y da pago efectivo
base100.pagos$cash <- NA
base100.pagos$cash <- base100.pagos$monto * base100.pagos$pago

#Hasta aquí tienes info completa pero los cada jugador con 3 rows,
#Hacer unique y group by para resumir en un renglon todo
#Mergear uno por uno Primero deltas y luego cash
base100.delta1 <- unique(base100.pagos %>%
                           group_by(playerID) %>% 
                           transmute(delta1=sum(deltapago1)))

base100.delta2 <- unique(base100.pagos %>%
                           group_by(playerID) %>% 
                           transmute(delta2=sum(deltapago2)))

base100.delta3 <- unique(base100.pagos %>%
                           group_by(playerID) %>% 
                           transmute(delta3=sum(deltapago3)))

base100.delta <- merge(base100.delta1,base100.delta2)
base100.delta <- merge(base100.delta,base100.delta3)

#Cash
base100.pay <- unique(base100.pagos %>%
                        group_by(playerID) %>% 
                        transmute(pago=sum(pago)))

base100.cash <- unique (base100.pagos %>%
                          group_by(playerID) %>% 
                          transmute(cash=sum(cash)))

base100.cash <- merge(base100.cash,base100.pay)

base100.cash$fracmax <- NA
base100.cash$fracmax <- base100.cash$cash/base100.cash$pago

#AHora si mergea todo Deltas con Cash
base100.cash <- merge(base100.cash,base100.delta)

# Incluye unique ya que esta sumado unique(base100.cash$playerID)
#para quitar todos los repeditos con la misma suma.

#dcast saca una columna *game* y la cast=vierte en las columnas que la contienen
# que son Nback Stroop e Iowa y te da el valor cada uno de "corerroi"
base100_cast <- as.data.frame(dcast(base100.pagos, playerID+grupo+treat+time
                                    ~game, value.var="corerroi"))

base100_time <- as.data.frame(dcast(base100.pagos, playerID+grupo+treat+time
                                    ~game, value.var="itime"))

base100_time <- rename(base100_time,
                        c(nback = "t.nback", 
                          iowa = "t.iowa", stroop = "t.stroop"))

base100_cast <- merge(base100_cast,base100_time)
#Ordena
base100_cast <- select(base100_cast, playerID, grupo, treat, time,
                       nback, stroop, iowa, t.nback, t.stroop, t.iowa)

#Mergea pago en cash con la base casteada con playerID individual
base100.completa <- merge(base100_cast,base100.cash)
base100.completa <- merge(base100.completa,pagos.game,all.x=TRUE,sort = FALSE)

#Quitar los Deltas que no apliquen con misma técnica de bono
#Los que son verdad = 1 Monto es Boolean
#Grupo 1 NSI
base100.completa$deltabool1 <- NA
base100.completa$deltabool1[base100.completa$grupo == "1_N-S-I" &
                              base100.completa$stroop <=0.05] <- TRUE
base100.completa$deltabool2 <- NA
base100.completa$deltabool2[base100.completa$grupo == "1_N-S-I" &
                              base100.completa$stroop <=0.05] <- TRUE
base100.completa$deltabool3 <- NA
base100.completa$deltabool3[base100.completa$grupo == "1_N-S-I" &
                              base100.completa$iowa >=-0.55] <- TRUE
#Grupo 2 SIN
base100.completa$deltabool1[base100.completa$grupo == "2_S-I-N" &
                              base100.completa$iowa >=-0.55] <- TRUE
base100.completa$deltabool2[base100.completa$grupo == "2_S-I-N" &
                              base100.completa$iowa >=-0.55] <- TRUE
base100.completa$deltabool3[base100.completa$grupo == "2_S-I-N" &
                              base100.completa$nback >=0.27] <- TRUE

#Grupo 3 INS
base100.completa$deltabool1[base100.completa$grupo == "3_I-N-S" &
                              base100.completa$nback >=0.27] <- TRUE
base100.completa$deltabool2[base100.completa$grupo == "3_I-N-S" &
                              base100.completa$nback >=0.27] <- TRUE
base100.completa$deltabool3[base100.completa$grupo == "3_I-N-S" &
                              base100.completa$stroop <=0.05] <- TRUE

#Los que son falsos = 0
base100.completa$deltabool1[is.na(base100.completa$deltabool1)] <- 0
base100.completa$deltabool2[is.na(base100.completa$deltabool2)] <- 0
base100.completa$deltabool3[is.na(base100.completa$deltabool3)] <- 0

#Multiplica los true y da bool efectivo
base100.completa$deltav1 <- NA
base100.completa$deltav1 <- base100.completa$delta1*base100.completa$deltabool1
base100.completa$deltav2 <- NA
base100.completa$deltav2 <- base100.completa$delta2*base100.completa$deltabool2
base100.completa$deltav3 <- NA
base100.completa$deltav3 <- base100.completa$delta3*base100.completa$deltabool3

#Se resolvio con la columna de pago 
#Incluyo una nueva columna "Cash real" que solo dice el pago efectivo
#Para própositos explicativos del performance, a los no bonus se les
#asignará 3,6,9 o 12 en 111,222,333,444 para ver el perf de todo treat
#base100.completa$cashreal <- base100.completa$cash
# base100.completa$cashreal[base100.completa$treat=="n1"] <- 3
#base100.completa$cashreal[base100.completa$treat=="n2"] <- 6
#base100.completa$cashreal[base100.completa$treat=="n3"] <- 9
#base100.completa$cashreal[base100.completa$treat=="n4"] <- 12
#base100.completa$cash[is.na(base100.completa$cash)] <- 0

#PRE POST Tasks
#Para el merge tiene que coincidir al menos la variable de una columna,
#Y despues el contenido de las columnas también tiene que concidir
#Por eso hay que renombrar participant que es la  
# names(pretask)[1] <- "playerID"
# y ya con eso dps le quitas txt y s.
preposttask <- merge(pretask,posttask)
base100.comp.pre.post <- merge(base100.completa,preposttask)

#quitar tiempos mayores a 30 minutos (quita mayores a 30 / menores a 5)
base100.comp.pre.post <- base100.comp.pre.post %>% filter(time < 30)
base100.comp.pre.post <- base100.comp.pre.post %>% filter(time > 5)

saveRDS(base100.comp.pre.post, file = "base100.comp.pre.post.rds")

base100.cpp.sum <- (cbind(lapply(base100.comp.pre.post, summary)))


##### Cognitive Games
  ##### 2. List of files - Base_900 ##### 
        library(tidyr)
        library(dplyr)
        library(reshape2)
        # library(tidyverse)
        
        
        setwd("C:/Users/danbo/Desktop/OriginalL")
        
        #Lista de archivos
        list_of_files <- list.files("C:/Users/danbo/Desktop/OriginalL",
                                    recursive = FALSE,
                                    pattern = "\\.txt$", 
                                    full.names = FALSE)
        
        #Nos quedamos con los nombres que impliquen un juego
        list_of_games <- list_of_files[grepl("nback|stroop|iowa",list_of_files)]
        
        #Ver cuales quedaron fuera
        list_of_files[!grepl("nback|stroop|iowa",list_of_files)]
        
        
        #Extraemos información de los nombres de los archivos de juegos
        list_of_games <- data.frame("Nombre"=list_of_games,
                                    stringsAsFactors=FALSE)
        
        
        #Crear columna de orden de juegos
        list_of_games$grupo <- NA
        list_of_games$grupo[grepl("(nbackt\\.|stroopnp1\\.|iowagt2\\.)",list_of_games$Nombre)] <- "1_N-S-I"
        list_of_games$grupo[grepl("(stroopnp\\.|iowagt1\\.|nbackt2\\.)",list_of_games$Nombre)] <- "2_S-I-N"
        list_of_games$grupo[grepl("(iowagt\\.|nbackt1\\.|stroopnp2\\.)",list_of_games$Nombre)] <- "3_I-N-S"
        
        
        list_of_games <- list_of_games %>% 
          separate("Nombre", 
                   c("treatGame","fecha","data","playerID","txt"), 
                   sep="\\.", remove=TRUE) %>% 
          select(-c(fecha,data,txt))
        
        
        #Columna con información de caracteres antes del nombre del juego
        list_of_games$treatment <- gsub("(nback|stroop|iowa).*","",
                                        list_of_games$treatGame)
        list_of_games$orden <- gsub(".*(nback|stroop|iowa)","",
                                    list_of_games$treatGame)
        
        list_of_games$game <- NA
        list_of_games$game[grepl("nback",list_of_games$treatGame)] <- "nback"
        list_of_games$game[grepl("stroop",list_of_games$treatGame)] <- "stroop"
        list_of_games$game[grepl("iowa",list_of_games$treatGame)] <- "iowa"
        
        
        ###Leer csv para saber No Bonus
        mturk <- read.csv("C:/Users/danbo/Desktop/OriginalL/data.csv")
        #Convert DF
        mtdf <- as.data.frame(mturk)
        #Quitar NAs solo quienes terminan
        mtdfclean <- as.data.frame(mtdf[!is.na(mtdf$TIME_total),])
        
        #Quitar a los tramposos u otros que no califiquen
        mtdfclean <- mtdfclean %>% filter(!(participant %in% 
                                              c("s.5c732da1-436b-49d3-ab0a-23bfcd34edc8.txt",
                                                "s.ba9a24f6-439c-408c-8a09-4a1661e3604a.txt")))
        
        #Participant id treatment time
        part <- select(mtdfclean, participant, random_treat.1, TIME_total)
        #Separar participant s.--- para tener el id
        names(part)[1] <- "playerID"
        part$playerID <- gsub(".*s.","",
                              part$playerID)
        part$playerID <- gsub(".txt","",
                              part$playerID)
        
        part <- select(part, playerID, random_treat.1, TIME_total)
        
        #Cambiar nombre a treatments == o %in% c(1,2,3) para mas de uno
        part$random_treat.1[part$random_treat.1 %in% 1] <- "a1"
        part$random_treat.1[part$random_treat.1 %in% 2] <- "a2"
        part$random_treat.1[part$random_treat.1 %in% 3] <- "a3"
        part$random_treat.1[part$random_treat.1 %in% 4] <- "a4"
        part$random_treat.1[part$random_treat.1 %in% 5] <- "i1"
        part$random_treat.1[part$random_treat.1 %in% 6] <- "i2"
        part$random_treat.1[part$random_treat.1 %in% 7] <- "i3"
        part$random_treat.1[part$random_treat.1 %in% 8] <- "d1"
        part$random_treat.1[part$random_treat.1 %in% 9] <- "d2"
        part$random_treat.1[part$random_treat.1 %in% 10] <- "d3"
        part$random_treat.1[part$random_treat.1 %in% 11] <- "n1"
        part$random_treat.1[part$random_treat.1 %in% 12] <- "n2"
        part$random_treat.1[part$random_treat.1 %in% 13] <- "n3"
        part$random_treat.1[part$random_treat.1 %in% 14] <- "n4"
        
        
        ##sigue hacer match de player id con treatment (merge da nuevo DF)
        prueba <- merge(list_of_games, part)
        #Select & rename
        prueba <- select(prueba, playerID, grupo, game, random_treat.1, TIME_total)
        prueba$treat <- prueba$random_treat.1
        prueba$time <- prueba$TIME_total
        #Base lista
        base <- select(prueba, playerID, grupo, game, treat, time)
        
        ##
        
        
        
        ##Entrar para sacar resultados
        #Inicializar i para probar manualmente
        i<-1
        
        #Creamos data.frame vacío
        res <- data.frame("FILE"=NULL,"RES"=NULL, "DIF"=NULL, "4"=NULL, "5"=NULL,"game"=NULL,"itime"=NULL)
        
        ## Genérico para todos
        for(i in 1:length(list_of_files)){
          list_of_files[i]
          #Leemos el i-ésimo archivo
          aux <- read.csv(list_of_files[i], sep=" ", header=FALSE)
          #Si el nombre del archivo contiene "nback"...
          if(grepl("nbackt",list_of_files[i])){
            #dos errores promedio
            res[i,1] <- list_of_files[i]
            res[i,2] <- aux[dim(aux)[1],1]
            res[i,3] <- aux[dim(aux)[1],2]
            res[i,5] <- mean(c(res[i,2],res[i,3]))/100
            res[i,4] <- 1-res[i,5]
            res[i,6] <- "nback"
            res[i,7] <- (sum(aux[1:dim(aux)[1],3],NA, na.rm = TRUE))/1000
          }
          if(grepl("stroopnp",list_of_files[i])){
            #difference and error perc
            res[i,1] <- list_of_files[i]
            res[i,2] <- aux[dim(aux)[1],1]
            res[i,3] <- aux[dim(aux)[1],2]
            res[i,4] <- res[i,3]/100
            res[i,5] <- NA
            res[i,6] <- "stroop"
            res[i,7] <- (sum(aux[1:dim(aux)[1],8],NA, na.rm = TRUE))/1000
          }
          if(grepl("iowagt",list_of_files[i])){
            #win/loss
            res[i,1] <- list_of_files[i]
            res[i,2] <- as.numeric(as.character(aux[dim(aux)[1],1]))
            res[i,3] <- aux[dim(aux)[1],2]
            res[i,4] <- (res[i,2]-2000)/2000
            res[i,5] <- NA
            res[i,6] <- "iowa"
            res[i,7] <- (sum(as.numeric(as.character(aux[1:dim(aux)[1],1])))-res[i,2])/1000
          }
        }
        #Todos los resultado
        names(res) <- c("playerID","res", "dif","corerroi","percent","game","itime")
        #Select & Merge by game
        resultados <- select(res, playerID, corerroi, game, itime)  
        
        resultados$playerID <- gsub(".*.data.","",
                                    resultados$playerID)
        resultados$playerID <- gsub(".txt","",
                                    resultados$playerID)
        
        completa <- merge(base, resultados)
        
        base900 <- select(completa, playerID, grupo, treat, game, corerroi, itime, time)  
        
  ##### PRE POST Tasks - preposttask #####
        
        library(plyr)
        library(tidyr)
        library(tidyverse)
        library(dplyr)
        
        setwd("C:/Users/danbo/Desktop/OriginalL")
        
        mturk <- read.csv("C:/Users/danbo/Desktop/OriginalL/data.csv")
        #Convert DF
        mtdf <- as.data.frame(mturk)
        #Quitar NAs solo quienes terminan
        mtdfclean <- as.data.frame(mtdf[!is.na(mtdf$TIME_total),])
        #Participant id treatment time
        part <- select(mtdfclean, participant, random_treat.1, TIME_total)
        
        #Separar Entry Survey
        pretask <- mtdfclean[,0:17]
        names(pretask)[1] <- "playerID"
        pretask$playerID <- gsub(".*s.","",
                                 pretask$playerID)
        pretask$playerID <- gsub(".txt","",
                                 pretask$playerID)
        #Separar Exit Survey
        #Se podrían haber sacado las dos secciones con c() mtdfclean[,0:17] +el resto
        posttask <- mtdfclean[c(1,(length(mtdfclean)-14):length(mtdfclean))]
        names(posttask)[1] <- "playerID"
        posttask$playerID <- gsub(".*s.","",
                                  pretask$playerID)
        posttask$playerID <- gsub(".txt","",
                                  pretask$playerID)
        #mi codigo que fue mejorado arriba
        #mtdfposttask <- mtdfclean[,c(1,(ncol(mtdfclean)-14):ncol(mtdfclean))]
        
        
  ##### Bonos y Pagos - base900.comp.pre.post #####
        
        #Hacer un csv con catalogo de pagos, leer y hacer melt por tratamiento
        #cambiar el nombre de las variables para que coincidan con base 100
        
        #Incluir Delta pagos para regresion Harvard, y=corerroi; x=delta i3-i1, i3-i2
        deltapagos1 <- read.csv("deltapagos1.csv",stringsAsFactors=FALSE)
        deltapagos1.m <- melt(deltapagos1,"treat",
                              value.name="deltapago1") %>% 
          separate("variable", into=c("grupo","game"), sep="_")
        
        deltapagos1.m$grupo[deltapagos1.m$grupo=="grupo1"] <- "1_N-S-I"
        deltapagos1.m$grupo[deltapagos1.m$grupo=="grupo2"] <- "2_S-I-N"
        deltapagos1.m$grupo[deltapagos1.m$grupo=="grupo3"] <- "3_I-N-S"
        
        #Incluir Delta pagos para regresion Harvard, y=corerroi; x=delta i2-i1, i3-i2
        deltapagos2 <- read.csv("deltapagos2.csv",stringsAsFactors=FALSE)
        deltapagos2.m <- melt(deltapagos2,"treat",
                              value.name="deltapago2") %>% 
          separate("variable", into=c("grupo","game"), sep="_")
        
        deltapagos2.m$grupo[deltapagos2.m$grupo=="grupo1"] <- "1_N-S-I"
        deltapagos2.m$grupo[deltapagos2.m$grupo=="grupo2"] <- "2_S-I-N"
        deltapagos2.m$grupo[deltapagos2.m$grupo=="grupo3"] <- "3_I-N-S"
        #Delta pagos 3-2
        deltapagos3 <- read.csv("deltapagos3.csv",stringsAsFactors=FALSE)
        deltapagos3.m <- melt(deltapagos3,"treat",
                              value.name="deltapago3") %>% 
          separate("variable", into=c("grupo","game"), sep="_")
        
        deltapagos3.m$grupo[deltapagos3.m$grupo=="grupo1"] <- "1_N-S-I"
        deltapagos3.m$grupo[deltapagos3.m$grupo=="grupo2"] <- "2_S-I-N"
        deltapagos3.m$grupo[deltapagos3.m$grupo=="grupo3"] <- "3_I-N-S"
        
        deltapagos.m <- merge(deltapagos1.m,deltapagos2.m,all.x=TRUE,sort = FALSE)
        deltapagos.m <- merge(deltapagos.m,deltapagos3.m,all.x=TRUE,sort = FALSE)
        
        #Es mejor cambiar nombre de grupo
        pagos <- read.csv("pagos.csv",stringsAsFactors=FALSE)
        
        pagos.m <- melt(pagos,"treat",
                        value.name="pago") %>% 
          separate("variable", into=c("grupo","game"), sep="_")
        
        pagos.m$grupo[pagos.m$grupo=="grupo1"] <- "1_N-S-I"
        pagos.m$grupo[pagos.m$grupo=="grupo2"] <- "2_S-I-N"
        pagos.m$grupo[pagos.m$grupo=="grupo3"] <- "3_I-N-S"
        
        ### Prueba de pago por juego en orden de grupo
        pagos.game <- as.data.frame(dcast(pagos.m,
                                          grupo+treat
                                          ~game, value.var="pago"))
        names(pagos.game)[names(pagos.game) == "nback"] <- "nback.bonos"
        names(pagos.game)[names(pagos.game) == "stroop"] <- "stroop.bonos"
        names(pagos.game)[names(pagos.game) == "iowa"] <- "iowa.bonos"
        
        #pagos.mg <- merge(pagos.m,pagos.game,all.x=TRUE,sort = FALSE)
        pagos.m <- merge(pagos.m,deltapagos.m,all.x=TRUE,sort = FALSE)
        
        base900.pagos <- merge(base900,pagos.m,all.x=TRUE)
        
        #Los que son verdad = 1 Monto es Boolean
        base900.pagos$monto <- NA
        base900.pagos$monto[base900.pagos$game == "nback" &
                              base900.pagos$corerroi >=0.27] <- TRUE
        base900.pagos$monto[base900.pagos$game == "stroop" &
                              base900.pagos$corerroi <=0.05] <- TRUE
        base900.pagos$monto[base900.pagos$game == "iowa" &
                              base900.pagos$corerroi >=-0.55] <- TRUE
        
        #Los que son falsos = 0
        base900.pagos$monto[is.na(base900.pagos$monto)] <- 0
        
        #Multiplica los true y da pago efectivo
        base900.pagos$cash <- NA
        base900.pagos$cash <- base900.pagos$monto * base900.pagos$pago
        
        #Hasta aquí tienes info completa pero los cada jugador con 3 rows,
        #Hacer unique y group by para resumir en un renglon todo
        #Mergear uno por uno Primero deltas y luego cash
        base900.delta1 <- unique(base900.pagos %>%
                                   group_by(playerID) %>% 
                                   transmute(delta1=sum(deltapago1)))
        
        base900.delta2 <- unique(base900.pagos %>%
                                   group_by(playerID) %>% 
                                   transmute(delta2=sum(deltapago2)))
        
        base900.delta3 <- unique(base900.pagos %>%
                                   group_by(playerID) %>% 
                                   transmute(delta3=sum(deltapago3)))
        
        base900.delta <- merge(base900.delta1,base900.delta2)
        base900.delta <- merge(base900.delta,base900.delta3)
        
        #Cash
        base900.pay <- unique(base900.pagos %>%
                                group_by(playerID) %>% 
                                transmute(pago=sum(pago)))
        
        base900.cash <- unique(base900.pagos %>%
                                  group_by(playerID) %>% 
                                  transmute(cash=sum(cash)))
        
        base900.cash <- merge(base900.cash,base900.pay)
        
        base900.cash$fracmax <- NA
        base900.cash$fracmax <- base900.cash$cash/base900.cash$pago
        
        #AHora si mergea todo Deltas con Cash
        base900.cash <- merge(base900.cash,base900.delta)
        
        # Incluye unique ya que esta sumado unique(base900.cash$playerID)
        #para quitar todos los repeditos con la misma suma.
        
        #dcast saca una columna *game* y la cast=vierte en las columnas que la contienen
        # que son Nback Stroop e Iowa y te da el valor cada uno de "corerroi"
        base900_cast <- as.data.frame(dcast(base900.pagos, playerID+grupo+treat+time
                                            ~game, value.var="corerroi"))

        base900_time <- as.data.frame(dcast(base900.pagos, playerID+grupo+treat+time
                                            ~game, value.var="itime"))
        
        base900_time <- rename(base900_time,
                               c(nback = "t.nback", 
                                 iowa = "t.iowa", stroop = "t.stroop"))
        
        base900_cast <- merge(base900_cast,base900_time)
        #Ordena
        base900_cast <- select(base900_cast, playerID, grupo, treat, time,
                               nback, stroop, iowa, t.nback, t.stroop, t.iowa)
        
        
        base900.completa <- merge(base900_cast,base900.cash)
        base900.completa <- merge(base900.completa,pagos.game,all.x=TRUE,sort = FALSE)
        
        #Quitar los Deltas que no apliquen con misma técnica de bono
        #Los que son verdad = 1 Monto es Boolean
        #Grupo 1 NSI
        base900.completa$deltabool1 <- NA
        base900.completa$deltabool1[base900.completa$grupo == "1_N-S-I" &
                                      base900.completa$stroop <=0.05] <- TRUE
        base900.completa$deltabool2 <- NA
        base900.completa$deltabool2[base900.completa$grupo == "1_N-S-I" &
                                      base900.completa$stroop <=0.05] <- TRUE
        base900.completa$deltabool3 <- NA
        base900.completa$deltabool3[base900.completa$grupo == "1_N-S-I" &
                                      base900.completa$iowa >=-0.55] <- TRUE
        #Grupo 2 SIN
        base900.completa$deltabool1[base900.completa$grupo == "2_S-I-N" &
                                      base900.completa$iowa >=-0.55] <- TRUE
        base900.completa$deltabool2[base900.completa$grupo == "2_S-I-N" &
                                      base900.completa$iowa >=-0.55] <- TRUE
        base900.completa$deltabool3[base900.completa$grupo == "2_S-I-N" &
                                      base900.completa$nback >=0.27] <- TRUE
        
        #Grupo 3 INS
        base900.completa$deltabool1[base900.completa$grupo == "3_I-N-S" &
                                      base900.completa$nback >=0.27] <- TRUE
        base900.completa$deltabool2[base900.completa$grupo == "3_I-N-S" &
                                      base900.completa$nback >=0.27] <- TRUE
        base900.completa$deltabool3[base900.completa$grupo == "3_I-N-S" &
                                      base900.completa$stroop <=0.05] <- TRUE
        
        #Los que son falsos = 0
        base900.completa$deltabool1[is.na(base900.completa$deltabool1)] <- 0
        base900.completa$deltabool2[is.na(base900.completa$deltabool2)] <- 0
        base900.completa$deltabool3[is.na(base900.completa$deltabool3)] <- 0
        
        #Multiplica los true y da bool efectivo
        base900.completa$deltav1 <- NA
        base900.completa$deltav1 <- base900.completa$delta1*base900.completa$deltabool1
        base900.completa$deltav2 <- NA
        base900.completa$deltav2 <- base900.completa$delta2*base900.completa$deltabool2
        base900.completa$deltav3 <- NA
        base900.completa$deltav3 <- base900.completa$delta3*base900.completa$deltabool3
        
        #Se resolvio con la columna de pago 
        #Incluyo una nueva columna "Cash real" que solo dice el pago efectivo
        #Para própositos explicativos del performance, a los no bonus se les
        #asignará 3,6,9 o 12 en 111,222,333,444 para ver el perf de todo treat
        #base900.completa$cashreal <- base900.completa$cash
        # base900.completa$cashreal[base900.completa$treat=="n1"] <- 3
        #base900.completa$cashreal[base900.completa$treat=="n2"] <- 6
        #base900.completa$cashreal[base900.completa$treat=="n3"] <- 9
        #base900.completa$cashreal[base900.completa$treat=="n4"] <- 12
        #base900.completa$cash[is.na(base900.completa$cash)] <- 0
        
        #PRE POST Tasks
        #Para el merge tiene que coincidir al menos la variable de una columna,
        #Y despues el contenido de las columnas también tiene que concidir
        #Por eso hay que renombrar participant que es la  
        # names(pretask)[1] <- "playerID"
        # y ya con eso dps le quitas txt y s.
        preposttask <- merge(pretask,posttask)
        base900.comp.pre.post <- merge(base900.completa,preposttask)
        
        #quitar tiempos mayores a 30 minutos / menores 5
        base900.comp.pre.post <- base900.comp.pre.post %>% filter(time < 30)
        base900.comp.pre.post <- base900.comp.pre.post %>% filter(time > 5)
        
        saveRDS(base900.comp.pre.post, file = "base900.comp.pre.post.rds")
        
        base900.cpp.sum <- (cbind(lapply(base900.comp.pre.post, summary)))
        
        
        
        
    ##### 3. BASE.POP (Removes) #####
              #Library
              library(tidyr)
              library(dplyr)
              library(reshape2)
              library(ggplot2)
              library(scales)
              library(fabricatr)
              
              #Primero corre base.900 y luego Correr sin RM() en Base 100
              #Merge Both Bases
              base.pop.light <- rbind(base900.completa,base100.completa)
              #Quitar a los tramposos u otros que no califiquen
              base.pop.light <- base.pop.light %>% filter(!(playerID %in% 
                                                    c("5c732da1-436b-49d3-ab0a-23bfcd34edc8",
                                                      "ba9a24f6-439c-408c-8a09-4a1661e3604a")))
              #quitar tiempos mayores a 30 minutos (quita mayores a 30 / menores a 5)
              base.pop.light <- base.pop.light %>% filter(time < 30)
              base.pop.light <- base.pop.light %>% filter(time > 5)
              #quitar nas en perf
              base.pop.light <- as.data.frame(base.pop.light[!is.na(base.pop.light$nback),])
              base.pop.light <- as.data.frame(base.pop.light[!is.na(base.pop.light$stroop),])
              base.pop.light <- as.data.frame(base.pop.light[!is.na(base.pop.light$iowa),])
              
              #Base.pop Completa con Entry Exit Surveys
              base.pop <- rbind(base900.comp.pre.post,base100.comp.pre.post)
              
              base.pop<- base.pop%>% filter(!(playerID %in% 
                                                c("5c732da1-436b-49d3-ab0a-23bfcd34edc8",
                                                  "ba9a24f6-439c-408c-8a09-4a1661e3604a")))
              #quitar tiempos mayores a 30 minutos (quita mayores a 30 / menores a 5)
              base.pop<- base.pop%>% filter(time < 30)
              base.pop<- base.pop%>% filter(time > 5)
              
              #dummy i3 outliers 434583e6-54d0-439d-8a9e-f53b7f3603f7
              base.pop$t.iowa[259] <- 211.730
              base.pop$t.iowa[753] <- 259.220
                
              #quitar nas en perf
              base.pop <- as.data.frame(base.pop[!is.na(base.pop$nback),])
              base.pop <- as.data.frame(base.pop[!is.na(base.pop$stroop),])
              base.pop <- as.data.frame(base.pop[!is.na(base.pop$iowa),])
              
              #Remove NA y Revisar duplicados
              base.pop$playerID[base.pop$playerID
                                 %in% base.pop$playerID[duplicated(base.pop$playerID)]]
              
    ##### B.P.C - Dist y Cut-offs - Ordered Probit #####
              base.pop.nback <- as.data.frame(count(base.pop$nback))
              base.pop.nback <- base.pop.nback %>% mutate(cumsum = cumsum(freq))
              base.pop.nback$prop <- prop.table(base.pop.nback$freq) 
              base.pop.nback$cumperc <- cumsum((base.pop.nback$prop)*100)
              base.pop.nback$group <- 
                cut(base.pop.nback$cumperc,
                    breaks = seq(0,100,by=10), right = TRUE)
              
              base.pop.nback.q <- base.pop.nback %>% 
                group_by(group) %>%
                transmute(freq=sum(freq))
              base.pop.nback.q <- unique(base.pop.nback.q %>% 
                                         group_by(group))
              #Change name to later merge match
              base.pop.nback <- rename(base.pop.nback,
                                      c(x = "nback"))
              #Rename cut-offs (Levels of Factors are renamed w/revalue)
              base.pop.nback$cutoff.nback <- 
                revalue(base.pop.nback$group, c("(0,10]"="4Fair",
                                                "(10,20]"="4Fair",
                                                "(20,30]"="4Fair",
                                                "(30,40]"="4Fair",
                                                "(40,50]"="3Good",
                                                "(50,60]"="3Good",
                                                "(60,70]"="2Very Good",
                                                "(70,80]"="2Very Good",
                                                "(80,90]"="2Very Good",
                                                "(90,100]"="1Excellent"))
              
              base.pop.stroop <- as.data.frame(count(base.pop$stroop))
              base.pop.stroop <- base.pop.stroop %>% mutate(cumsum = cumsum(freq))
              base.pop.stroop$prop <- prop.table(base.pop.stroop$freq) 
              base.pop.stroop$cumperc <- cumsum((base.pop.stroop$prop)*100)
              base.pop.stroop$group <- 
                cut(base.pop.stroop$cumperc,
                    breaks = seq(0,100,by=10), right = TRUE)
              
              base.pop.stroop.q <- base.pop.stroop %>% 
                group_by(group) %>%
                transmute(freq=sum(freq))
              base.pop.stroop.q <- unique(base.pop.stroop.q %>% 
                                            group_by(group))
              #Change name to later merge match
              base.pop.stroop <- rename(base.pop.stroop,
                                       c(x = "stroop"))
              #Rename cut-offs (Levels of Factors are renamed w/revalue)
              base.pop.stroop$cutoff.stroop <- 
                revalue(base.pop.stroop$group, c("(0,10]"="1Excellent",
                                                 "(10,20]"="1Excellent",
                                                 "(20,30]"="1Excellent",
                                                 "(30,40]"="1Excellent",
                                                "(40,50]"="1Excellent",
                                                "(50,60]"="2Very Good",
                                                "(60,70]"="3Good",
                                                "(70,80]"="3Good",
                                                "(80,90]"="4Fair",
                                                "(90,100]"="4Fair"))
              
              base.pop.iowa <- as.data.frame(count(base.pop$iowa))
              base.pop.iowa <- base.pop.iowa %>% mutate(cumsum = cumsum(freq))
              base.pop.iowa$prop <- prop.table(base.pop.iowa$freq) 
              base.pop.iowa$cumperc <- cumsum((base.pop.iowa$prop)*100)
              base.pop.iowa$group <- 
                cut(base.pop.iowa$cumperc,
                    breaks = seq(0,100,by=10), right = TRUE)
              
              base.pop.iowa.q <- base.pop.iowa %>% 
                group_by(group) %>%
                transmute(freq=sum(freq))
              base.pop.iowa.q <- unique(base.pop.iowa.q %>% 
                                          group_by(group))
              
              #Change name to later merge match
              base.pop.iowa <- rename(base.pop.iowa,
                                      c(x = "iowa"))
              #Rename cut-offs (Levels of Factors are renamed w/revalue)
              base.pop.iowa$cutoff.iowa <- 
                revalue(base.pop.iowa$group, c("(0,10]"="4Fair",
                                               "(10,20]"="4Fair",
                                               "(20,30]"="4Fair",
                                               "(30,40]"="4Fair",
                                               "(40,50]"="3Good",
                                               "(50,60]"="3Good",
                                               "(60,70]"="3Good",
                                               "(70,80]"="3Good",
                                               "(80,90]"="2Very Good",
                                               "(90,100]"="1Excellent"))
              
              base.pop.fracmax <- as.data.frame(count(base.pop$fracmax))
              base.pop.fracmax <- base.pop.fracmax %>% mutate(cumsum = cumsum(freq))
              base.pop.fracmax$prop <- prop.table(base.pop.fracmax$freq) 
              base.pop.fracmax$cumperc <- cumsum((base.pop.fracmax$prop)*100)
              base.pop.fracmax$group <- 
                cut(base.pop.fracmax$cumperc,
                    breaks = seq(0,100,by=10), right = TRUE)
              
              base.pop.fracmax.q <- base.pop.fracmax %>% 
                group_by(group) %>%
                transmute(freq=sum(freq))
              base.pop.fracmax.q <- unique(base.pop.fracmax.q %>% 
                                             group_by(group))
              
              #Change name to later merge match
              base.pop.fracmax <- rename(base.pop.fracmax,
                                         c(x = "fracmax"))
              #Rename cut-offs (Levels of Factors are renamed w/revalue)
              base.pop.fracmax$cutoff.fracmax <- 
                revalue(base.pop.fracmax$group, c("(0,10]"="4Fair",
                                                  "(10,20]"="4Fair",
                                                  "(20,30]"="4Fair",
                                                  "(30,40]"="4Fair",
                                                  "(40,50]"="4Fair",
                                                  "(50,60]"="3Good",
                                                  "(60,70]"="3Good",
                                                  "(70,80]"="3Good",
                                                  "(80,90]"="2Very Good",
                                                  "(90,100]"="1Excellent"))
              
              #No need to melt, cada valor de nback ya tiene un cutoff
              nback.cutoff <- select(base.pop.nback, nback, cutoff.nback)
              #Tal vez mete un NaN porque no llega al 100%, solo quitarlo
              nback.cutoff <- as.data.frame(nback.cutoff[!is.na(nback.cutoff$nback),])
              stroop.cutoff <- select(base.pop.stroop, stroop, cutoff.stroop)
              iowa.cutoff <- select(base.pop.iowa, iowa, cutoff.iowa)
              fracmax.cutoff <- select(base.pop.fracmax, fracmax, cutoff.fracmax)
              #Merge base.pop.light con cutoffs de cada juego y fracmax
              prueba.cutoff <- merge(nback.cutoff,base.pop,all.x=TRUE,sort = FALSE)
              prueba.cutoff <- merge(stroop.cutoff,prueba.cutoff,all.x=TRUE,sort = FALSE)
              prueba.cutoff <- merge(iowa.cutoff,prueba.cutoff,all.x=TRUE,sort = FALSE)
              prueba.cutoff <- merge(fracmax.cutoff,prueba.cutoff,all.x=TRUE,sort = FALSE)
              
              base.pop.cutoff <- select(prueba.cutoff,playerID,grupo, treat, time, 
                                        fracmax, nback, stroop, iowa,
                                        t.nback, t.stroop, t.iowa,
                                        cutoff.fracmax, cutoff.nback, 
                                        cutoff.stroop, cutoff.iowa, 
                                        cash, pago, delta1, delta2, delta3, 
                                        iowa.bonos, nback.bonos, stroop.bonos, 
                                        deltabool1, deltabool2, deltabool3, 
                                        deltav1, deltav2, deltav3, 
                                        pre_task.1, age.1, education.1, income.1, 
                                        religion.1, mturkexp.1, mturkappr.1, mturkhits.1, 
                                        gender.1, OtherGender.1, location.1, OtherLocation.1, 
                                        ethnicity.1, OtherEthnicity.1, political.1, 
                                        be_aware.1, endquestion.1, secon2.1, subseq2.1, 
                                        enjoy.1, satisfied.1, difficulty.1, timing.1, 
                                        compajust.1, compensation.1, finalcomplete.1, 
                                        country, endcode, TIME_start, TIME_end, TIME_total
                                        )
              
              #Incluir Generic Groups
              base.pop.cutoff$treatgen <- NA
              base.pop.cutoff$treatgen <- base.pop.cutoff$treat
              base.pop.cutoff$treatmag <- NA
              base.pop.cutoff$treatmag <- base.pop.cutoff$treat
              base.pop.cutoff$treatpay <- NA
              base.pop.cutoff$treatpay <- base.pop.cutoff$treat
              
              #Incluir bono level y develop
              base.pop.cutoff$bonlev <- NA
              base.pop.cutoff$bonlev[base.pop.cutoff$treat=="a1"] <- 0
              base.pop.cutoff$bonlev[base.pop.cutoff$treat=="a2"] <- 1
              base.pop.cutoff$bonlev[base.pop.cutoff$treat=="a3"] <- 2
              base.pop.cutoff$bonlev[base.pop.cutoff$treat=="a4"] <- 3
              base.pop.cutoff$bonlev[base.pop.cutoff$treat=="d1"] <- 1
              base.pop.cutoff$bonlev[base.pop.cutoff$treat=="d2"] <- 2
              base.pop.cutoff$bonlev[base.pop.cutoff$treat=="d3"] <- 3
              base.pop.cutoff$bonlev[base.pop.cutoff$treat=="i1"] <- 1
              base.pop.cutoff$bonlev[base.pop.cutoff$treat=="i2"] <- 2
              base.pop.cutoff$bonlev[base.pop.cutoff$treat=="i3"] <- 3
              base.pop.cutoff$bonlev[base.pop.cutoff$treat=="n1"] <- 0
              base.pop.cutoff$bonlev[base.pop.cutoff$treat=="n2"] <- 1
              base.pop.cutoff$bonlev[base.pop.cutoff$treat=="n3"] <- 2
              base.pop.cutoff$bonlev[base.pop.cutoff$treat=="n4"] <- 3
              
              base.pop.cutoff$develop <- NA
              base.pop.cutoff$develop[base.pop.cutoff$location=="1"] <- 0
              base.pop.cutoff$develop[base.pop.cutoff$location=="2"] <- 1
              base.pop.cutoff$develop[base.pop.cutoff$location=="3"] <- 0
              base.pop.cutoff$develop[base.pop.cutoff$location=="4"] <- 1
              base.pop.cutoff$develop[base.pop.cutoff$location=="5"] <- 0
              
              
              base.pop.cutoff <- select(base.pop.cutoff,playerID,grupo, bonlev, 
                                        treat, treatgen, treatmag, treatpay,
                                        time, fracmax, nback, stroop, iowa,
                                        t.nback, t.stroop, t.iowa,
                                        cutoff.fracmax, cutoff.nback, 
                                        cutoff.stroop, cutoff.iowa, 
                                        cash, pago, delta1, delta2, delta3, 
                                        iowa.bonos, nback.bonos, stroop.bonos, 
                                        deltabool1, deltabool2, deltabool3, 
                                        deltav1, deltav2, deltav3, 
                                        pre_task.1, age.1, education.1, income.1, 
                                        religion.1, mturkexp.1, mturkappr.1, mturkhits.1, 
                                        gender.1, OtherGender.1, 
                                        develop, location.1, OtherLocation.1, 
                                        ethnicity.1, OtherEthnicity.1, political.1, 
                                        be_aware.1, endquestion.1, secon2.1, subseq2.1, 
                                        enjoy.1, satisfied.1, difficulty.1, timing.1, 
                                        compajust.1, compensation.1, finalcomplete.1, 
                                        country, endcode, TIME_start, TIME_end, TIME_total
              )
              
              #Rename generic Treatments
              base.pop.cutoff$treatgen[base.pop.cutoff$treatgen %in% "a1"] <- "abs"
              base.pop.cutoff$treatgen[base.pop.cutoff$treatgen %in% "a2"] <- "abs"
              base.pop.cutoff$treatgen[base.pop.cutoff$treatgen %in% "a3"] <- "abs"
              base.pop.cutoff$treatgen[base.pop.cutoff$treatgen %in% "a4"] <- "abs"
              base.pop.cutoff$treatgen[base.pop.cutoff$treatgen %in% "i1"] <- "inc"
              base.pop.cutoff$treatgen[base.pop.cutoff$treatgen %in% "i2"] <- "inc"
              base.pop.cutoff$treatgen[base.pop.cutoff$treatgen %in% "i3"] <- "inc"
              base.pop.cutoff$treatgen[base.pop.cutoff$treatgen %in% "d1"] <- "dec"
              base.pop.cutoff$treatgen[base.pop.cutoff$treatgen %in% "d2"] <- "dec"
              base.pop.cutoff$treatgen[base.pop.cutoff$treatgen %in% "d3"] <- "dec"
              base.pop.cutoff$treatgen[base.pop.cutoff$treatgen %in% "n1"] <- "nb"
              base.pop.cutoff$treatgen[base.pop.cutoff$treatgen %in% "n2"] <- "nb"
              base.pop.cutoff$treatgen[base.pop.cutoff$treatgen %in% "n3"] <- "nb"
              base.pop.cutoff$treatgen[base.pop.cutoff$treatgen %in% "n4"] <- "nb"
              
              #Incluir Magnitude Groups
              base.pop.cutoff$treatmag[base.pop.cutoff$treatmag %in% "a1"] <- "am"
              base.pop.cutoff$treatmag[base.pop.cutoff$treatmag %in% "a2"] <- "am"
              base.pop.cutoff$treatmag[base.pop.cutoff$treatmag %in% "a3"] <- "am"
              base.pop.cutoff$treatmag[base.pop.cutoff$treatmag %in% "a4"] <- "am"
              base.pop.cutoff$treatmag[base.pop.cutoff$treatmag %in% "i1"] <- "cm"
              base.pop.cutoff$treatmag[base.pop.cutoff$treatmag %in% "i2"] <- "cm"
              base.pop.cutoff$treatmag[base.pop.cutoff$treatmag %in% "i3"] <- "cm"
              base.pop.cutoff$treatmag[base.pop.cutoff$treatmag %in% "d1"] <- "cm"
              base.pop.cutoff$treatmag[base.pop.cutoff$treatmag %in% "d2"] <- "cm"
              base.pop.cutoff$treatmag[base.pop.cutoff$treatmag %in% "d3"] <- "cm"
              base.pop.cutoff$treatmag[base.pop.cutoff$treatmag %in% "n1"] <- "am"
              base.pop.cutoff$treatmag[base.pop.cutoff$treatmag %in% "n2"] <- "am"
              base.pop.cutoff$treatmag[base.pop.cutoff$treatmag %in% "n3"] <- "am"
              base.pop.cutoff$treatmag[base.pop.cutoff$treatmag %in% "n4"] <- "am"
              
              #Incluir Magnitude Groups
              base.pop.cutoff$treatpay[base.pop.cutoff$treatpay %in% "a1"] <- "1.¢6"
              base.pop.cutoff$treatpay[base.pop.cutoff$treatpay %in% "a2"] <- "2.¢9"
              base.pop.cutoff$treatpay[base.pop.cutoff$treatpay %in% "a3"] <- "3.¢12"
              base.pop.cutoff$treatpay[base.pop.cutoff$treatpay %in% "a4"] <- "4.¢15"
              base.pop.cutoff$treatpay[base.pop.cutoff$treatpay %in% "i1"] <- "2.¢9"
              base.pop.cutoff$treatpay[base.pop.cutoff$treatpay %in% "i2"] <- "3.¢12"
              base.pop.cutoff$treatpay[base.pop.cutoff$treatpay %in% "i3"] <- "4.¢15"
              base.pop.cutoff$treatpay[base.pop.cutoff$treatpay %in% "d1"] <- "2.¢9"
              base.pop.cutoff$treatpay[base.pop.cutoff$treatpay %in% "d2"] <- "3.¢12"
              base.pop.cutoff$treatpay[base.pop.cutoff$treatpay %in% "d3"] <- "4.¢15"
              base.pop.cutoff$treatpay[base.pop.cutoff$treatpay %in% "n1"] <- "1.¢6"
              base.pop.cutoff$treatpay[base.pop.cutoff$treatpay %in% "n2"] <- "2.¢9"
              base.pop.cutoff$treatpay[base.pop.cutoff$treatpay %in% "n3"] <- "3.¢12"
              base.pop.cutoff$treatpay[base.pop.cutoff$treatpay %in% "n4"] <- "4.¢15"

              
              
    ##### Dummies #####
              # real average age in subgroup (accuracy within 5 years)  y 70
              base.pop.cutoff$age[base.pop.cutoff$age.1=="1"] <- 21
              base.pop.cutoff$age[base.pop.cutoff$age.1=="2"] <- 21
              base.pop.cutoff$age[base.pop.cutoff$age.1=="3"] <- 30
              base.pop.cutoff$age[base.pop.cutoff$age.1=="4"] <- 40
              base.pop.cutoff$age[base.pop.cutoff$age.1=="5"] <- 50
              base.pop.cutoff$age[base.pop.cutoff$age.1=="6"] <- 60
              base.pop.cutoff$age[base.pop.cutoff$age.1=="7"] <- 70
              
              base.pop.cutoff$education[base.pop.cutoff$education.1=="1"] <- 0
              base.pop.cutoff$education[base.pop.cutoff$education.1=="2"] <- 1
              base.pop.cutoff$education[base.pop.cutoff$education.1=="3"] <- 2
              base.pop.cutoff$education[base.pop.cutoff$education.1=="4"] <- 3
              
              base.pop.cutoff$income[base.pop.cutoff$income.1=="1"] <- 0
              base.pop.cutoff$income[base.pop.cutoff$income.1=="2"] <- 1
              base.pop.cutoff$income[base.pop.cutoff$income.1=="3"] <- 2
              base.pop.cutoff$income[base.pop.cutoff$income.1=="4"] <- 3
              base.pop.cutoff$income[base.pop.cutoff$income.1=="5"] <- 4
              base.pop.cutoff$income[base.pop.cutoff$income.1=="6"] <- 5
              base.pop.cutoff$income[base.pop.cutoff$income.1=="7"] <- 6
              
              base.pop.cutoff$religion[base.pop.cutoff$religion.1=="1"] <- 2
              base.pop.cutoff$religion[base.pop.cutoff$religion.1=="2"] <- 1
              base.pop.cutoff$religion[base.pop.cutoff$religion.1=="3"] <- 0
              
              base.pop.cutoff$mturkexp[base.pop.cutoff$mturkexp.1=="1"] <- 0
              base.pop.cutoff$mturkexp[base.pop.cutoff$mturkexp.1=="2"] <- 1  
              base.pop.cutoff$mturkexp[base.pop.cutoff$mturkexp.1=="3"] <- 2  
              
              base.pop.cutoff$mturkappr[base.pop.cutoff$mturkappr.1=="1"] <- 0
              base.pop.cutoff$mturkappr[base.pop.cutoff$mturkappr.1=="2"] <- 1
              base.pop.cutoff$mturkappr[base.pop.cutoff$mturkappr.1=="3"] <- 2
              
              base.pop.cutoff$mturkhits[base.pop.cutoff$mturkhits.1=="1"] <- 0
              base.pop.cutoff$mturkhits[base.pop.cutoff$mturkhits.1=="2"] <- 1
              base.pop.cutoff$mturkhits[base.pop.cutoff$mturkhits.1=="3"] <- 2
              
              base.pop.cutoff$gender[base.pop.cutoff$gender.1=="1"] <- 0
              base.pop.cutoff$gender[base.pop.cutoff$gender.1=="2"] <- 1
              base.pop.cutoff$gender[base.pop.cutoff$gender.1=="3"] <- 0
              
              base.pop.cutoff$location[base.pop.cutoff$location.1=="1"] <- 0
              base.pop.cutoff$location[base.pop.cutoff$location.1=="2"] <- 1
              base.pop.cutoff$location[base.pop.cutoff$location.1=="3"] <- 2
              base.pop.cutoff$location[base.pop.cutoff$location.1=="4"] <- 3
              base.pop.cutoff$location[base.pop.cutoff$location.1=="5"] <- 4

              base.pop.cutoff$ethnicity[base.pop.cutoff$ethnicity.1=="1"] <- 0
              base.pop.cutoff$ethnicity[base.pop.cutoff$ethnicity.1=="2"] <- 1
              base.pop.cutoff$ethnicity[base.pop.cutoff$ethnicity.1=="3"] <- 2
              base.pop.cutoff$ethnicity[base.pop.cutoff$ethnicity.1=="4"] <- 3
              base.pop.cutoff$ethnicity[base.pop.cutoff$ethnicity.1=="5"] <- 4
              base.pop.cutoff$ethnicity[base.pop.cutoff$ethnicity.1=="6"] <- 5
              
              base.pop.cutoff$political[base.pop.cutoff$political.1=="1"] <- 0
              base.pop.cutoff$political[base.pop.cutoff$political.1=="2"] <- 2
              base.pop.cutoff$political[base.pop.cutoff$political.1=="3"] <- 1
              
              base.pop.cutoff$enjoy[base.pop.cutoff$enjoy.1=="1"] <- 0
              base.pop.cutoff$enjoy[base.pop.cutoff$enjoy.1=="2"] <- 1
              base.pop.cutoff$enjoy[base.pop.cutoff$enjoy.1=="3"] <- 2
              
              base.pop.cutoff$satisfied[base.pop.cutoff$satisfied.1=="1"] <- 0
              base.pop.cutoff$satisfied[base.pop.cutoff$satisfied.1=="2"] <- 1
              base.pop.cutoff$satisfied[base.pop.cutoff$satisfied.1=="3"] <- 2
              
              base.pop.cutoff$difficulty[base.pop.cutoff$difficulty.1=="1"] <- 0
              base.pop.cutoff$difficulty[base.pop.cutoff$difficulty.1=="2"] <- 1
              base.pop.cutoff$difficulty[base.pop.cutoff$difficulty.1=="3"] <- 2
              
              base.pop.cutoff$timing[base.pop.cutoff$timing.1=="1"] <- 0
              base.pop.cutoff$timing[base.pop.cutoff$timing.1=="2"] <- 1
              base.pop.cutoff$timing[base.pop.cutoff$timing.1=="3"] <- 2
              
              base.pop.cutoff$compajust[base.pop.cutoff$compajust.1=="1"] <- 2
              base.pop.cutoff$compajust[base.pop.cutoff$compajust.1=="2"] <- 1
              base.pop.cutoff$compajust[base.pop.cutoff$compajust.1=="3"] <- 0
              base.pop.cutoff$compajust[base.pop.cutoff$compajust.1=="4"] <- 3
              base.pop.cutoff$compajust[base.pop.cutoff$compajust.1=="5"] <- 4
              base.pop.cutoff$compajust[base.pop.cutoff$compajust.1=="6"] <- 5
              
              base.pop.cutoff$compensation[base.pop.cutoff$compensation.1=="1"] <- 0
              base.pop.cutoff$compensation[base.pop.cutoff$compensation.1=="2"] <- 1
              base.pop.cutoff$compensation[base.pop.cutoff$compensation.1=="3"] <- 2
              
    ##### Separate by Group, Treat & Cutoff #####
              
              base.pop.g1 <- subset(base.pop,
                                    grupo %in% c("1_N-S-I"))
              base.pop.g2 <- subset(base.pop,
                                    grupo %in% c("2_S-I-N"))
              base.pop.g3 <- subset(base.pop,
                                    grupo %in% c("3_I-N-S"))
              
              base.pop.cutoff.g1 <- subset(base.pop.cutoff,
                                    grupo %in% c("1_N-S-I"))
              base.pop.cutoff.g2 <- subset(base.pop.cutoff,
                                    grupo %in% c("2_S-I-N"))
              base.pop.cutoff.g3 <- subset(base.pop.cutoff,
                                    grupo %in% c("3_I-N-S"))
              
              base.pop.cutoff.abs <- subset(base.pop.cutoff,
                                           treatgen %in% c("abs"))
              base.pop.cutoff.dec <- subset(base.pop.cutoff,
                                           treatgen %in% c("dec"))
              base.pop.cutoff.inc <- subset(base.pop.cutoff,
                                           treatgen %in% c("inc"))
              base.pop.cutoff.nb <- subset(base.pop.cutoff,
                                           treatgen %in% c("nb"))
              
              base.pop.cutoff.fa <- subset(base.pop.cutoff,
                                            cutoff.fracmax %in% c("4Fair"))
              base.pop.cutoff.go <- subset(base.pop.cutoff,
                                            cutoff.fracmax %in% c("3Good"))
              base.pop.cutoff.vg <- subset(base.pop.cutoff,
                                            cutoff.fracmax %in% c("2Very Good"))
              base.pop.cutoff.ex <- subset(base.pop.cutoff,
                                           cutoff.fracmax %in% c("1Excellent"))
    
    ##### Group and CSV Generic Treat G1 #####
    base.pop.cutoff.g1.abs <- subset(base.pop.cutoff.g1,
                                  treatgen %in% c("abs"))
    base.pop.cutoff.g1.dec <- subset(base.pop.cutoff.g1,
                                  treatgen %in% c("dec"))
    base.pop.cutoff.g1.inc <- subset(base.pop.cutoff.g1,
                                  treatgen %in% c("inc"))
    base.pop.cutoff.g1.nb <- subset(base.pop.cutoff.g1,
                                 treatgen %in% c("nb"))
    
    write.csv(base.pop.cutoff.g1.abs,"C:/Users/danbo/Desktop/MturkR/BaseF/Gen/base.pop.cutoff.g1.abs.csv")
    write.csv(base.pop.cutoff.g1.dec,"C:/Users/danbo/Desktop/MturkR/BaseF/Gen/base.pop.cutoff.g1.dec.csv")
    write.csv(base.pop.cutoff.g1.inc,"C:/Users/danbo/Desktop/MturkR/BaseF/Gen/base.pop.cutoff.g1.inc.csv")
    write.csv(base.pop.cutoff.g1.nb,"C:/Users/danbo/Desktop/MturkR/BaseF/Gen/base.pop.cutoff.g1.nb.csv")
    
    ##### Group and CSV Generic Treat G2 #####
    base.pop.cutoff.g2.abs <- subset(base.pop.cutoff.g2,
                                  treatgen %in% c("abs"))
    base.pop.cutoff.g2.dec <- subset(base.pop.cutoff.g2,
                                  treatgen %in% c("dec"))
    base.pop.cutoff.g2.inc <- subset(base.pop.cutoff.g2,
                                  treatgen %in% c("inc"))
    base.pop.cutoff.g2.nb <- subset(base.pop.cutoff.g2,
                                 treatgen %in% c("nb"))
    
    write.csv(base.pop.cutoff.g2.abs,"C:/Users/danbo/Desktop/MturkR/BaseF/Gen/base.pop.cutoff.g2.abs.csv")
    write.csv(base.pop.cutoff.g2.dec,"C:/Users/danbo/Desktop/MturkR/BaseF/Gen/base.pop.cutoff.g2.dec.csv")
    write.csv(base.pop.cutoff.g2.inc,"C:/Users/danbo/Desktop/MturkR/BaseF/Gen/base.pop.cutoff.g2.inc.csv")
    write.csv(base.pop.cutoff.g2.nb,"C:/Users/danbo/Desktop/MturkR/BaseF/Gen/base.pop.cutoff.g2.nb.csv")
    
    ##### Group and CSV Generic Treat G3 #####
    base.pop.cutoff.g3.abs <- subset(base.pop.cutoff.g3,
                                  treatgen %in% c("abs"))
    base.pop.cutoff.g3.dec <- subset(base.pop.cutoff.g3,
                                  treatgen %in% c("dec"))
    base.pop.cutoff.g3.inc <- subset(base.pop.cutoff.g3,
                                  treatgen %in% c("inc"))
    base.pop.cutoff.g3.nb <- subset(base.pop.cutoff.g3,
                                 treatgen %in% c("nb"))
    
    write.csv(base.pop.cutoff.g3.abs,"C:/Users/danbo/Desktop/MturkR/BaseF/Gen/base.pop.cutoff.g3.abs.csv")
    write.csv(base.pop.cutoff.g3.dec,"C:/Users/danbo/Desktop/MturkR/BaseF/Gen/base.pop.cutoff.g3.dec.csv")
    write.csv(base.pop.cutoff.g3.inc,"C:/Users/danbo/Desktop/MturkR/BaseF/Gen/base.pop.cutoff.g3.inc.csv")
    write.csv(base.pop.cutoff.g3.nb,"C:/Users/danbo/Desktop/MturkR/BaseF/Gen/base.pop.cutoff.g3.nb.csv") 
    
    ##### Save Base.csv #####
              write.csv(base.pop,"C:/Users/danbo/Desktop/MturkR/BaseF/base.pop5.csv")
                write.csv(base.pop.g1,"C:/Users/danbo/Desktop/MturkR/BaseF/base.pop5.g1.csv")
                write.csv(base.pop.g2,"C:/Users/danbo/Desktop/MturkR/BaseF/base.pop5.g2.csv")
                write.csv(base.pop.g3,"C:/Users/danbo/Desktop/MturkR/BaseF/base.pop5.g3.csv")
              
                write.csv(base.pop.cutoff,"C:/Users/danbo/Desktop/MturkR/BaseF/base.pop.cutoff_EX.csv")
                write.csv(base.pop.cutoff.g1,"C:/Users/danbo/Desktop/MturkR/BaseF/base.pop.cutoff_EX.g1.csv")
                write.csv(base.pop.cutoff.g2,"C:/Users/danbo/Desktop/MturkR/BaseF/base.pop.cutoff_EX.g2.csv")
                write.csv(base.pop.cutoff.g3,"C:/Users/danbo/Desktop/MturkR/BaseF/base.pop.cutoff_EX.g3.csv")
                
                write.csv(base.pop.cutoff.abs,"C:/Users/danbo/Desktop/MturkR/BaseF/base.pop.cutoff_EX.abs.csv")
                write.csv(base.pop.cutoff.dec,"C:/Users/danbo/Desktop/MturkR/BaseF/base.pop.cutoff_EX.dec.csv")
                write.csv(base.pop.cutoff.inc,"C:/Users/danbo/Desktop/MturkR/BaseF/base.pop.cutoff_EX.inc.csv")
                write.csv(base.pop.cutoff.nb,"C:/Users/danbo/Desktop/MturkR/BaseF/base.pop.cutoff_EX.nb.csv")
                
                write.csv(base.pop.cutoff.fa,"C:/Users/danbo/Desktop/MturkR/BaseF/base.pop.cutoff_EX.fa.csv")
                write.csv(base.pop.cutoff.go,"C:/Users/danbo/Desktop/MturkR/BaseF/base.pop.cutoff_EX.go.csv")
                write.csv(base.pop.cutoff.vg,"C:/Users/danbo/Desktop/MturkR/BaseF/base.pop.cutoff_EX.vg.csv")
                write.csv(base.pop.cutoff.ex,"C:/Users/danbo/Desktop/MturkR/BaseF/base.pop.cutoff_EX.ex.csv")
                
                base.pop.cutoff$treat <- 
                  factor(base.pop.cutoff$treat)
                base.pop.cutoff$treatgen <- 
                  factor(base.pop.cutoff$treatgen)
                write.csv(base.pop.cutoff,"C:/Users/danbo/Desktop/MturkR/BaseF/base.pop.cutoff.factor.csv")
                
                ##### Distributions #####
                
                
        dist1 <- ggplot(base.pop.cutoff,
                       aes(x=fracmax, fill=treatmag)) + geom_density(alpha=.3)
                
        dist2 <- ggplot(base.pop.cutoff,
                       aes(x=fracmax, fill=treatgen)) + geom_density(alpha=.3)
        
        dist3 <- ggplot(base.pop.cutoff,
                        aes(x=nback, fill=treatmag)) + geom_density(alpha=.3)
        
        dist4 <- ggplot(base.pop.cutoff,
                        aes(x=nback, fill=treatgen)) + geom_density(alpha=.3)
        
        dist5 <- ggplot(base.pop.cutoff,
                        aes(x=stroop, fill=treatmag)) + geom_density(alpha=.3)
        
        dist6 <- ggplot(base.pop.cutoff,
                        aes(x=stroop, fill=treatgen)) + geom_density(alpha=.3)
        
        dist7 <- ggplot(base.pop.cutoff,
                        aes(x=iowa, fill=treatmag)) + geom_density(alpha=.3)
        
        dist8 <- ggplot(base.pop.cutoff,
                        aes(x=iowa, fill=treatgen)) + geom_density(alpha=.3)
        
        dist9 <- ggplot(base.pop.cutoff,
                        aes(x=time, fill=treatmag)) + geom_density(alpha=.3)
        
        dist10 <- ggplot(base.pop.cutoff,
                        aes(x=time, fill=treatgen)) + geom_density(alpha=.3)
        
        dist11 <- ggplot(base.pop.cutoff,
                        aes(x=t.nback, fill=treatmag)) + geom_density(alpha=.3)
        
        dist12 <- ggplot(base.pop.cutoff,
                        aes(x=t.nback, fill=treatgen)) + geom_density(alpha=.3)
        
        dist13 <- ggplot(base.pop.cutoff,
                        aes(x=t.stroop, fill=treatmag)) + geom_density(alpha=.3)
        
        dist14 <- ggplot(base.pop.cutoff,
                        aes(x=t.stroop, fill=treatgen)) + geom_density(alpha=.3)
        
        dist15 <- ggplot(base.pop.cutoff,
                        aes(x=t.iowa, fill=treatmag)) + geom_density(alpha=.3)
        
        dist16 <- ggplot(base.pop.cutoff,
                        aes(x=t.iowa, fill=treatgen)) + geom_density(alpha=.3)
        
        #SET WD and save all Graphs
        setwd("C:/Users/danbo/Desktop/MturkR/BaseF/GraphsBF/Dem")
        
        for(i in 1:16){
          
          ggsave(paste0("dist",i,".png"), plot = get(paste0("dist",i)))  
          
        }
                
                #
                ##### Correlation #####
                library(ggcorrplot)

                base.corr <- select(base.pop.cutoff,
                                    bonlev, fracmax, nback, stroop, iowa,
                                    time, t.nback, t.stroop, t.iowa,
                                    age.1,	education.1,	income.1,	religion.1,
                                    mturkexp.1,	mturkappr.1,	mturkhits.1,	
                                    gender.1,	location.1,	develop,	ethnicity.1,
                                    political.1,	enjoy.1,	satisfied.1,	difficulty.1,
                                    timing.1,	compajust.1,	compensation.1)
                
                corr <- round(cor(base.corr), 1)
                p.mat <- cor_pmat(base.corr)
                
                corr1 <- ggcorrplot(corr, hc.order = FALSE, 
                                    type = "lower", lab = TRUE, p.mat = p.mat,
                                    sig.level = .001,
                                    ggtheme = ggplot2::theme_dark(),)
                
                base.corr <- select(base.pop.cutoff,
                                    bonlev, fracmax, nback, stroop, iowa,
                                    time, t.nback, t.stroop, t.iowa,
                                    age.1,	gender.1, education.1,	income.1,	
                                    develop)
                
                corr <- round(cor(base.corr), 1)
                p.mat <- cor_pmat(base.corr)
                
                corr2 <- ggcorrplot(corr, hc.order = FALSE, 
                                    type = "lower", lab = TRUE, p.mat = p.mat,
                                    sig.level = .001,
                                    ggtheme = ggplot2::theme_dark(),)
                
                #SET WD and save all Graphs
                setwd("C:/Users/danbo/Desktop/MturkR/BaseF/GraphsBF")
                
                for(i in 1:2){
                  
                  ggsave(paste0("corr",i,".png"), plot = get(paste0("corr",i)))  
                  
                }

              
                ##### Mean SD SE #####
                library(plyr)
                #Grupo
                #Perf
                sgf <- ddply(base.pop.cutoff,
                               c("grupo"), summarise,
                               N    = length(fracmax),
                               mean = mean(fracmax),
                               sd   = sd(fracmax),
                               se   = sd / sqrt(N)
                )
                sgf <- rename(sgf,
                                  c(mean="Fracmax Mean",
                                    sd="Fracmax SD",
                                    se="Fracmax SE"))
                sgn <- ddply(base.pop.cutoff,
                                c("grupo"), summarise,
                                N    = length(nback),
                                mean = mean(nback),
                                sd   = sd(nback),
                                se   = sd / sqrt(N)
                )
                sgn <- rename(sgn,
                              c(mean="NBack Mean",
                                sd="NBack SD",
                                se="NBack SE"))
                sgs <- ddply(base.pop.cutoff,
                                c("grupo"), summarise,
                                N    = length(stroop),
                                mean = mean(stroop),
                                sd   = sd(stroop),
                                se   = sd / sqrt(N)
                )
                sgs <- rename(sgs,
                              c(mean="Stroop Mean",
                                sd="Stroop SD",
                                se="Stroop SE"))
                sgi <- ddply(base.pop.cutoff,
                                c("grupo"), summarise,
                                N    = length(iowa),
                                mean = mean(iowa),
                                sd   = sd(iowa),
                                se   = sd / sqrt(N)
                )
                sgi <- rename(sgi,
                              c(mean="Iowa Mean",
                                sd="Iowa SD",
                                se="Iowa SE"))
                sg <- merge(sgf,sgn)
                sg <- merge(sg,sgs)
                sg <- merge(sg,sgi)
                # TIME
                sgt <- ddply(base.pop.cutoff,
                             c("grupo"), summarise,
                             N    = length(time),
                             mean = mean(time),
                             sd   = sd(time),
                             se   = sd / sqrt(N)
                )
                sgt <- rename(sgt,
                              c(mean="Time Mean",
                                sd="Time SD",
                                se="Time SE"))
                sgnt <- ddply(base.pop.cutoff,
                             c("grupo"), summarise,
                             N    = length(t.nback),
                             mean = mean(t.nback),
                             sd   = sd(t.nback),
                             se   = sd / sqrt(N)
                )
                sgnt <- rename(sgnt,
                              c(mean="T.NBack Mean",
                                sd="T.NBack SD",
                                se="T.NBack SE"))
                sgst <- ddply(base.pop.cutoff,
                             c("grupo"), summarise,
                             N    = length(t.stroop),
                             mean = mean(t.stroop),
                             sd   = sd(t.stroop),
                             se   = sd / sqrt(N)
                )
                sgst <- rename(sgst,
                              c(mean="T.Stroop Mean",
                                sd="T.Stroop SD",
                                se="T.Stroop SE"))
                sgit <- ddply(base.pop.cutoff,
                             c("grupo"), summarise,
                             N    = length(t.iowa),
                             mean = mean(t.iowa),
                             sd   = sd(t.iowa),
                             se   = sd / sqrt(N)
                )
                sgit <- rename(sgit,
                              c(mean="T.Iowa Mean",
                                sd="T.Iowa SD",
                                se="T.Iowa SE"))
                sgti <- merge(sgt,sgnt)
                sgti <- merge(sgti,sgst)
                sgti <- merge(sgti,sgit)
                
                sumgrupo <- merge(sg,sgti)
                sumgrupo <- select(sumgrupo,grupo,N,
                                   `Fracmax Mean`,`NBack Mean`,`Stroop Mean`,`Iowa Mean`,
                                   `Time Mean`,`T.NBack Mean`,`T.Stroop Mean`,`T.Iowa Mean`,
                                   `Fracmax SD`,`NBack SD`,`Stroop SD`,`Iowa SD`,
                                   `Time SD`,`T.NBack SD`,`T.Stroop SD`,`T.Iowa SD`)
                #TreatMag
                #Perf
                sgf <- ddply(base.pop.cutoff,
                             c("treatmag"), summarise,
                             N    = length(fracmax),
                             mean = mean(fracmax),
                             sd   = sd(fracmax),
                             se   = sd / sqrt(N)
                )
                sgf <- rename(sgf,
                              c(mean="Fracmax Mean",
                                sd="Fracmax SD",
                                se="Fracmax SE"))
                sgn <- ddply(base.pop.cutoff,
                             c("treatmag"), summarise,
                             N    = length(nback),
                             mean = mean(nback),
                             sd   = sd(nback),
                             se   = sd / sqrt(N)
                )
                sgn <- rename(sgn,
                              c(mean="NBack Mean",
                                sd="NBack SD",
                                se="NBack SE"))
                sgs <- ddply(base.pop.cutoff,
                             c("treatmag"), summarise,
                             N    = length(stroop),
                             mean = mean(stroop),
                             sd   = sd(stroop),
                             se   = sd / sqrt(N)
                )
                sgs <- rename(sgs,
                              c(mean="Stroop Mean",
                                sd="Stroop SD",
                                se="Stroop SE"))
                sgi <- ddply(base.pop.cutoff,
                             c("treatmag"), summarise,
                             N    = length(iowa),
                             mean = mean(iowa),
                             sd   = sd(iowa),
                             se   = sd / sqrt(N)
                )
                sgi <- rename(sgi,
                              c(mean="Iowa Mean",
                                sd="Iowa SD",
                                se="Iowa SE"))
                sg <- merge(sgf,sgn)
                sg <- merge(sg,sgs)
                sg <- merge(sg,sgi)
                # TIME
                sgt <- ddply(base.pop.cutoff,
                             c("treatmag"), summarise,
                             N    = length(time),
                             mean = mean(time),
                             sd   = sd(time),
                             se   = sd / sqrt(N)
                )
                sgt <- rename(sgt,
                              c(mean="Time Mean",
                                sd="Time SD",
                                se="Time SE"))
                sgnt <- ddply(base.pop.cutoff,
                              c("treatmag"), summarise,
                              N    = length(t.nback),
                              mean = mean(t.nback),
                              sd   = sd(t.nback),
                              se   = sd / sqrt(N)
                )
                sgnt <- rename(sgnt,
                               c(mean="T.NBack Mean",
                                 sd="T.NBack SD",
                                 se="T.NBack SE"))
                sgst <- ddply(base.pop.cutoff,
                              c("treatmag"), summarise,
                              N    = length(t.stroop),
                              mean = mean(t.stroop),
                              sd   = sd(t.stroop),
                              se   = sd / sqrt(N)
                )
                sgst <- rename(sgst,
                               c(mean="T.Stroop Mean",
                                 sd="T.Stroop SD",
                                 se="T.Stroop SE"))
                sgit <- ddply(base.pop.cutoff,
                              c("treatmag"), summarise,
                              N    = length(t.iowa),
                              mean = mean(t.iowa),
                              sd   = sd(t.iowa),
                              se   = sd / sqrt(N)
                )
                sgit <- rename(sgit,
                               c(mean="T.Iowa Mean",
                                 sd="T.Iowa SD",
                                 se="T.Iowa SE"))
                sgti <- merge(sgt,sgnt)
                sgti <- merge(sgti,sgst)
                sgti <- merge(sgti,sgit)
                
                sumtreatmag <- merge(sg,sgti)
                sumtreatmag <- select(sumtreatmag,treatmag,N,
                                      `Fracmax Mean`,`NBack Mean`,`Stroop Mean`,`Iowa Mean`,
                                      `Time Mean`,`T.NBack Mean`,`T.Stroop Mean`,`T.Iowa Mean`,
                                      `Fracmax SD`,`NBack SD`,`Stroop SD`,`Iowa SD`,
                                      `Time SD`,`T.NBack SD`,`T.Stroop SD`,`T.Iowa SD`)
                #treatgen
                #Perf
                sgf <- ddply(base.pop.cutoff,
                             c("treatgen"), summarise,
                             N    = length(fracmax),
                             mean = mean(fracmax),
                             sd   = sd(fracmax),
                             se   = sd / sqrt(N)
                )
                sgf <- rename(sgf,
                              c(mean="Fracmax Mean",
                                sd="Fracmax SD",
                                se="Fracmax SE"))
                sgn <- ddply(base.pop.cutoff,
                             c("treatgen"), summarise,
                             N    = length(nback),
                             mean = mean(nback),
                             sd   = sd(nback),
                             se   = sd / sqrt(N)
                )
                sgn <- rename(sgn,
                              c(mean="NBack Mean",
                                sd="NBack SD",
                                se="NBack SE"))
                sgs <- ddply(base.pop.cutoff,
                             c("treatgen"), summarise,
                             N    = length(stroop),
                             mean = mean(stroop),
                             sd   = sd(stroop),
                             se   = sd / sqrt(N)
                )
                sgs <- rename(sgs,
                              c(mean="Stroop Mean",
                                sd="Stroop SD",
                                se="Stroop SE"))
                sgi <- ddply(base.pop.cutoff,
                             c("treatgen"), summarise,
                             N    = length(iowa),
                             mean = mean(iowa),
                             sd   = sd(iowa),
                             se   = sd / sqrt(N)
                )
                sgi <- rename(sgi,
                              c(mean="Iowa Mean",
                                sd="Iowa SD",
                                se="Iowa SE"))
                sg <- merge(sgf,sgn)
                sg <- merge(sg,sgs)
                sg <- merge(sg,sgi)
                # TIME
                sgt <- ddply(base.pop.cutoff,
                             c("treatgen"), summarise,
                             N    = length(time),
                             mean = mean(time),
                             sd   = sd(time),
                             se   = sd / sqrt(N)
                )
                sgt <- rename(sgt,
                              c(mean="Time Mean",
                                sd="Time SD",
                                se="Time SE"))
                sgnt <- ddply(base.pop.cutoff,
                              c("treatgen"), summarise,
                              N    = length(t.nback),
                              mean = mean(t.nback),
                              sd   = sd(t.nback),
                              se   = sd / sqrt(N)
                )
                sgnt <- rename(sgnt,
                               c(mean="T.NBack Mean",
                                 sd="T.NBack SD",
                                 se="T.NBack SE"))
                sgst <- ddply(base.pop.cutoff,
                              c("treatgen"), summarise,
                              N    = length(t.stroop),
                              mean = mean(t.stroop),
                              sd   = sd(t.stroop),
                              se   = sd / sqrt(N)
                )
                sgst <- rename(sgst,
                               c(mean="T.Stroop Mean",
                                 sd="T.Stroop SD",
                                 se="T.Stroop SE"))
                sgit <- ddply(base.pop.cutoff,
                              c("treatgen"), summarise,
                              N    = length(t.iowa),
                              mean = mean(t.iowa),
                              sd   = sd(t.iowa),
                              se   = sd / sqrt(N)
                )
                sgit <- rename(sgit,
                               c(mean="T.Iowa Mean",
                                 sd="T.Iowa SD",
                                 se="T.Iowa SE"))
                sgti <- merge(sgt,sgnt)
                sgti <- merge(sgti,sgst)
                sgti <- merge(sgti,sgit)
                
                sumtreatgen <- merge(sg,sgti)
                sumtreatgen <- select(sumtreatgen,treatgen,N,
                                      `Fracmax Mean`,`NBack Mean`,`Stroop Mean`,`Iowa Mean`,
                                      `Time Mean`,`T.NBack Mean`,`T.Stroop Mean`,`T.Iowa Mean`,
                                      `Fracmax SD`,`NBack SD`,`Stroop SD`,`Iowa SD`,
                                      `Time SD`,`T.NBack SD`,`T.Stroop SD`,`T.Iowa SD`)
                
                #treat 
                #Perf
                sgf <- ddply(base.pop.cutoff,
                             c("treat"), summarise,
                             N    = length(fracmax),
                             mean = mean(fracmax),
                             sd   = sd(fracmax),
                             se   = sd / sqrt(N)
                )
                sgf <- rename(sgf,
                              c(mean="Fracmax Mean",
                                sd="Fracmax SD",
                                se="Fracmax SE"))
                sgn <- ddply(base.pop.cutoff,
                             c("treat"), summarise,
                             N    = length(nback),
                             mean = mean(nback),
                             sd   = sd(nback),
                             se   = sd / sqrt(N)
                )
                sgn <- rename(sgn,
                              c(mean="NBack Mean",
                                sd="NBack SD",
                                se="NBack SE"))
                sgs <- ddply(base.pop.cutoff,
                             c("treat"), summarise,
                             N    = length(stroop),
                             mean = mean(stroop),
                             sd   = sd(stroop),
                             se   = sd / sqrt(N)
                )
                sgs <- rename(sgs,
                              c(mean="Stroop Mean",
                                sd="Stroop SD",
                                se="Stroop SE"))
                sgi <- ddply(base.pop.cutoff,
                             c("treat"), summarise,
                             N    = length(iowa),
                             mean = mean(iowa),
                             sd   = sd(iowa),
                             se   = sd / sqrt(N)
                )
                sgi <- rename(sgi,
                              c(mean="Iowa Mean",
                                sd="Iowa SD",
                                se="Iowa SE"))
                sg <- merge(sgf,sgn)
                sg <- merge(sg,sgs)
                sg <- merge(sg,sgi)
                # TIME
                sgt <- ddply(base.pop.cutoff,
                             c("treat"), summarise,
                             N    = length(time),
                             mean = mean(time),
                             sd   = sd(time),
                             se   = sd / sqrt(N)
                )
                sgt <- rename(sgt,
                              c(mean="Time Mean",
                                sd="Time SD",
                                se="Time SE"))
                sgnt <- ddply(base.pop.cutoff,
                              c("treat"), summarise,
                              N    = length(t.nback),
                              mean = mean(t.nback),
                              sd   = sd(t.nback),
                              se   = sd / sqrt(N)
                )
                sgnt <- rename(sgnt,
                               c(mean="T.NBack Mean",
                                 sd="T.NBack SD",
                                 se="T.NBack SE"))
                sgst <- ddply(base.pop.cutoff,
                              c("treat"), summarise,
                              N    = length(t.stroop),
                              mean = mean(t.stroop),
                              sd   = sd(t.stroop),
                              se   = sd / sqrt(N)
                )
                sgst <- rename(sgst,
                               c(mean="T.Stroop Mean",
                                 sd="T.Stroop SD",
                                 se="T.Stroop SE"))
                sgit <- ddply(base.pop.cutoff,
                              c("treat"), summarise,
                              N    = length(t.iowa),
                              mean = mean(t.iowa),
                              sd   = sd(t.iowa),
                              se   = sd / sqrt(N)
                )
                sgit <- rename(sgit,
                               c(mean="T.Iowa Mean",
                                 sd="T.Iowa SD",
                                 se="T.Iowa SE"))
                sgti <- merge(sgt,sgnt)
                sgti <- merge(sgti,sgst)
                sgti <- merge(sgti,sgit)
                
                sumtreat <- merge(sg,sgti)
                sumtreat <- select(sumtreat,treat,N,
                                      `Fracmax Mean`,`NBack Mean`,`Stroop Mean`,`Iowa Mean`,
                                      `Time Mean`,`T.NBack Mean`,`T.Stroop Mean`,`T.Iowa Mean`,
                                      `Fracmax SD`,`NBack SD`,`Stroop SD`,`Iowa SD`,
                                      `Time SD`,`T.NBack SD`,`T.Stroop SD`,`T.Iowa SD`)
                
                #cutoff.fracmax 
                #Perf
                sgf <- ddply(base.pop.cutoff,
                             c("cutoff.fracmax"), summarise,
                             N    = length(fracmax),
                             mean = mean(fracmax),
                             sd   = sd(fracmax),
                             se   = sd / sqrt(N)
                )
                sgf <- rename(sgf,
                              c(mean="Fracmax Mean",
                                sd="Fracmax SD",
                                se="Fracmax SE"))
                sgn <- ddply(base.pop.cutoff,
                             c("cutoff.fracmax"), summarise,
                             N    = length(nback),
                             mean = mean(nback),
                             sd   = sd(nback),
                             se   = sd / sqrt(N)
                )
                sgn <- rename(sgn,
                              c(mean="NBack Mean",
                                sd="NBack SD",
                                se="NBack SE"))
                sgs <- ddply(base.pop.cutoff,
                             c("cutoff.fracmax"), summarise,
                             N    = length(stroop),
                             mean = mean(stroop),
                             sd   = sd(stroop),
                             se   = sd / sqrt(N)
                )
                sgs <- rename(sgs,
                              c(mean="Stroop Mean",
                                sd="Stroop SD",
                                se="Stroop SE"))
                sgi <- ddply(base.pop.cutoff,
                             c("cutoff.fracmax"), summarise,
                             N    = length(iowa),
                             mean = mean(iowa),
                             sd   = sd(iowa),
                             se   = sd / sqrt(N)
                )
                sgi <- rename(sgi,
                              c(mean="Iowa Mean",
                                sd="Iowa SD",
                                se="Iowa SE"))
                sg <- merge(sgf,sgn)
                sg <- merge(sg,sgs)
                sg <- merge(sg,sgi)
                # TIME
                sgt <- ddply(base.pop.cutoff,
                             c("cutoff.fracmax"), summarise,
                             N    = length(time),
                             mean = mean(time),
                             sd   = sd(time),
                             se   = sd / sqrt(N)
                )
                sgt <- rename(sgt,
                              c(mean="Time Mean",
                                sd="Time SD",
                                se="Time SE"))
                sgnt <- ddply(base.pop.cutoff,
                              c("cutoff.fracmax"), summarise,
                              N    = length(t.nback),
                              mean = mean(t.nback),
                              sd   = sd(t.nback),
                              se   = sd / sqrt(N)
                )
                sgnt <- rename(sgnt,
                               c(mean="T.NBack Mean",
                                 sd="T.NBack SD",
                                 se="T.NBack SE"))
                sgst <- ddply(base.pop.cutoff,
                              c("cutoff.fracmax"), summarise,
                              N    = length(t.stroop),
                              mean = mean(t.stroop),
                              sd   = sd(t.stroop),
                              se   = sd / sqrt(N)
                )
                sgst <- rename(sgst,
                               c(mean="T.Stroop Mean",
                                 sd="T.Stroop SD",
                                 se="T.Stroop SE"))
                sgit <- ddply(base.pop.cutoff,
                              c("cutoff.fracmax"), summarise,
                              N    = length(t.iowa),
                              mean = mean(t.iowa),
                              sd   = sd(t.iowa),
                              se   = sd / sqrt(N)
                )
                sgit <- rename(sgit,
                               c(mean="T.Iowa Mean",
                                 sd="T.Iowa SD",
                                 se="T.Iowa SE"))
                sgti <- merge(sgt,sgnt)
                sgti <- merge(sgti,sgst)
                sgti <- merge(sgti,sgit)
                
                sumcutoff.fracmax <- merge(sg,sgti)
                sumcutoff.fracmax <- select(sumcutoff.fracmax,cutoff.fracmax,N,
                                   `Fracmax Mean`,`NBack Mean`,`Stroop Mean`,`Iowa Mean`,
                                   `Time Mean`,`T.NBack Mean`,`T.Stroop Mean`,`T.Iowa Mean`,
                                   `Fracmax SD`,`NBack SD`,`Stroop SD`,`Iowa SD`,
                                   `Time SD`,`T.NBack SD`,`T.Stroop SD`,`T.Iowa SD`)
                
                #cutoff.nback 
                #Perf
                sgf <- ddply(base.pop.cutoff,
                             c("cutoff.nback"), summarise,
                             N    = length(fracmax),
                             mean = mean(fracmax),
                             sd   = sd(fracmax),
                             se   = sd / sqrt(N)
                )
                sgf <- rename(sgf,
                              c(mean="Fracmax Mean",
                                sd="Fracmax SD",
                                se="Fracmax SE"))
                sgn <- ddply(base.pop.cutoff,
                             c("cutoff.nback"), summarise,
                             N    = length(nback),
                             mean = mean(nback),
                             sd   = sd(nback),
                             se   = sd / sqrt(N)
                )
                sgn <- rename(sgn,
                              c(mean="NBack Mean",
                                sd="NBack SD",
                                se="NBack SE"))
                sgs <- ddply(base.pop.cutoff,
                             c("cutoff.nback"), summarise,
                             N    = length(stroop),
                             mean = mean(stroop),
                             sd   = sd(stroop),
                             se   = sd / sqrt(N)
                )
                sgs <- rename(sgs,
                              c(mean="Stroop Mean",
                                sd="Stroop SD",
                                se="Stroop SE"))
                sgi <- ddply(base.pop.cutoff,
                             c("cutoff.nback"), summarise,
                             N    = length(iowa),
                             mean = mean(iowa),
                             sd   = sd(iowa),
                             se   = sd / sqrt(N)
                )
                sgi <- rename(sgi,
                              c(mean="Iowa Mean",
                                sd="Iowa SD",
                                se="Iowa SE"))
                sg <- merge(sgf,sgn)
                sg <- merge(sg,sgs)
                sg <- merge(sg,sgi)
                # TIME
                sgt <- ddply(base.pop.cutoff,
                             c("cutoff.nback"), summarise,
                             N    = length(time),
                             mean = mean(time),
                             sd   = sd(time),
                             se   = sd / sqrt(N)
                )
                sgt <- rename(sgt,
                              c(mean="Time Mean",
                                sd="Time SD",
                                se="Time SE"))
                sgnt <- ddply(base.pop.cutoff,
                              c("cutoff.nback"), summarise,
                              N    = length(t.nback),
                              mean = mean(t.nback),
                              sd   = sd(t.nback),
                              se   = sd / sqrt(N)
                )
                sgnt <- rename(sgnt,
                               c(mean="T.NBack Mean",
                                 sd="T.NBack SD",
                                 se="T.NBack SE"))
                sgst <- ddply(base.pop.cutoff,
                              c("cutoff.nback"), summarise,
                              N    = length(t.stroop),
                              mean = mean(t.stroop),
                              sd   = sd(t.stroop),
                              se   = sd / sqrt(N)
                )
                sgst <- rename(sgst,
                               c(mean="T.Stroop Mean",
                                 sd="T.Stroop SD",
                                 se="T.Stroop SE"))
                sgit <- ddply(base.pop.cutoff,
                              c("cutoff.nback"), summarise,
                              N    = length(t.iowa),
                              mean = mean(t.iowa),
                              sd   = sd(t.iowa),
                              se   = sd / sqrt(N)
                )
                sgit <- rename(sgit,
                               c(mean="T.Iowa Mean",
                                 sd="T.Iowa SD",
                                 se="T.Iowa SE"))
                sgti <- merge(sgt,sgnt)
                sgti <- merge(sgti,sgst)
                sgti <- merge(sgti,sgit)
                
                sumcutoff.nback <- merge(sg,sgti)
                sumcutoff.nback <- select(sumcutoff.nback,cutoff.nback,N,
                                            `Fracmax Mean`,`NBack Mean`,`Stroop Mean`,`Iowa Mean`,
                                            `Time Mean`,`T.NBack Mean`,`T.Stroop Mean`,`T.Iowa Mean`,
                                            `Fracmax SD`,`NBack SD`,`Stroop SD`,`Iowa SD`,
                                            `Time SD`,`T.NBack SD`,`T.Stroop SD`,`T.Iowa SD`)
                
                #cutoff.stroop 
                #Perf
                sgf <- ddply(base.pop.cutoff,
                             c("cutoff.stroop"), summarise,
                             N    = length(fracmax),
                             mean = mean(fracmax),
                             sd   = sd(fracmax),
                             se   = sd / sqrt(N)
                )
                sgf <- rename(sgf,
                              c(mean="Fracmax Mean",
                                sd="Fracmax SD",
                                se="Fracmax SE"))
                sgn <- ddply(base.pop.cutoff,
                             c("cutoff.stroop"), summarise,
                             N    = length(nback),
                             mean = mean(nback),
                             sd   = sd(nback),
                             se   = sd / sqrt(N)
                )
                sgn <- rename(sgn,
                              c(mean="NBack Mean",
                                sd="NBack SD",
                                se="NBack SE"))
                sgs <- ddply(base.pop.cutoff,
                             c("cutoff.stroop"), summarise,
                             N    = length(stroop),
                             mean = mean(stroop),
                             sd   = sd(stroop),
                             se   = sd / sqrt(N)
                )
                sgs <- rename(sgs,
                              c(mean="Stroop Mean",
                                sd="Stroop SD",
                                se="Stroop SE"))
                sgi <- ddply(base.pop.cutoff,
                             c("cutoff.stroop"), summarise,
                             N    = length(iowa),
                             mean = mean(iowa),
                             sd   = sd(iowa),
                             se   = sd / sqrt(N)
                )
                sgi <- rename(sgi,
                              c(mean="Iowa Mean",
                                sd="Iowa SD",
                                se="Iowa SE"))
                sg <- merge(sgf,sgn)
                sg <- merge(sg,sgs)
                sg <- merge(sg,sgi)
                # TIME
                sgt <- ddply(base.pop.cutoff,
                             c("cutoff.stroop"), summarise,
                             N    = length(time),
                             mean = mean(time),
                             sd   = sd(time),
                             se   = sd / sqrt(N)
                )
                sgt <- rename(sgt,
                              c(mean="Time Mean",
                                sd="Time SD",
                                se="Time SE"))
                sgnt <- ddply(base.pop.cutoff,
                              c("cutoff.stroop"), summarise,
                              N    = length(t.nback),
                              mean = mean(t.nback),
                              sd   = sd(t.nback),
                              se   = sd / sqrt(N)
                )
                sgnt <- rename(sgnt,
                               c(mean="T.NBack Mean",
                                 sd="T.NBack SD",
                                 se="T.NBack SE"))
                sgst <- ddply(base.pop.cutoff,
                              c("cutoff.stroop"), summarise,
                              N    = length(t.stroop),
                              mean = mean(t.stroop),
                              sd   = sd(t.stroop),
                              se   = sd / sqrt(N)
                )
                sgst <- rename(sgst,
                               c(mean="T.Stroop Mean",
                                 sd="T.Stroop SD",
                                 se="T.Stroop SE"))
                sgit <- ddply(base.pop.cutoff,
                              c("cutoff.stroop"), summarise,
                              N    = length(t.iowa),
                              mean = mean(t.iowa),
                              sd   = sd(t.iowa),
                              se   = sd / sqrt(N)
                )
                sgit <- rename(sgit,
                               c(mean="T.Iowa Mean",
                                 sd="T.Iowa SD",
                                 se="T.Iowa SE"))
                sgti <- merge(sgt,sgnt)
                sgti <- merge(sgti,sgst)
                sgti <- merge(sgti,sgit)
                
                sumcutoff.stroop <- merge(sg,sgti)
                sumcutoff.stroop <- select(sumcutoff.stroop,cutoff.stroop,N,
                                          `Fracmax Mean`,`NBack Mean`,`Stroop Mean`,`Iowa Mean`,
                                          `Time Mean`,`T.NBack Mean`,`T.Stroop Mean`,`T.Iowa Mean`,
                                          `Fracmax SD`,`NBack SD`,`Stroop SD`,`Iowa SD`,
                                          `Time SD`,`T.NBack SD`,`T.Stroop SD`,`T.Iowa SD`)
                
                #cutoff.iowa 
                #Perf
                sgf <- ddply(base.pop.cutoff,
                             c("cutoff.iowa"), summarise,
                             N    = length(fracmax),
                             mean = mean(fracmax),
                             sd   = sd(fracmax),
                             se   = sd / sqrt(N)
                )
                sgf <- rename(sgf,
                              c(mean="Fracmax Mean",
                                sd="Fracmax SD",
                                se="Fracmax SE"))
                sgn <- ddply(base.pop.cutoff,
                             c("cutoff.iowa"), summarise,
                             N    = length(nback),
                             mean = mean(nback),
                             sd   = sd(nback),
                             se   = sd / sqrt(N)
                )
                sgn <- rename(sgn,
                              c(mean="NBack Mean",
                                sd="NBack SD",
                                se="NBack SE"))
                sgs <- ddply(base.pop.cutoff,
                             c("cutoff.iowa"), summarise,
                             N    = length(stroop),
                             mean = mean(stroop),
                             sd   = sd(stroop),
                             se   = sd / sqrt(N)
                )
                sgs <- rename(sgs,
                              c(mean="Stroop Mean",
                                sd="Stroop SD",
                                se="Stroop SE"))
                sgi <- ddply(base.pop.cutoff,
                             c("cutoff.iowa"), summarise,
                             N    = length(iowa),
                             mean = mean(iowa),
                             sd   = sd(iowa),
                             se   = sd / sqrt(N)
                )
                sgi <- rename(sgi,
                              c(mean="Iowa Mean",
                                sd="Iowa SD",
                                se="Iowa SE"))
                sg <- merge(sgf,sgn)
                sg <- merge(sg,sgs)
                sg <- merge(sg,sgi)
                # TIME
                sgt <- ddply(base.pop.cutoff,
                             c("cutoff.iowa"), summarise,
                             N    = length(time),
                             mean = mean(time),
                             sd   = sd(time),
                             se   = sd / sqrt(N)
                )
                sgt <- rename(sgt,
                              c(mean="Time Mean",
                                sd="Time SD",
                                se="Time SE"))
                sgnt <- ddply(base.pop.cutoff,
                              c("cutoff.iowa"), summarise,
                              N    = length(t.nback),
                              mean = mean(t.nback),
                              sd   = sd(t.nback),
                              se   = sd / sqrt(N)
                )
                sgnt <- rename(sgnt,
                               c(mean="T.NBack Mean",
                                 sd="T.NBack SD",
                                 se="T.NBack SE"))
                sgst <- ddply(base.pop.cutoff,
                              c("cutoff.iowa"), summarise,
                              N    = length(t.stroop),
                              mean = mean(t.stroop),
                              sd   = sd(t.stroop),
                              se   = sd / sqrt(N)
                )
                sgst <- rename(sgst,
                               c(mean="T.Stroop Mean",
                                 sd="T.Stroop SD",
                                 se="T.Stroop SE"))
                sgit <- ddply(base.pop.cutoff,
                              c("cutoff.iowa"), summarise,
                              N    = length(t.iowa),
                              mean = mean(t.iowa),
                              sd   = sd(t.iowa),
                              se   = sd / sqrt(N)
                )
                sgit <- rename(sgit,
                               c(mean="T.Iowa Mean",
                                 sd="T.Iowa SD",
                                 se="T.Iowa SE"))
                sgti <- merge(sgt,sgnt)
                sgti <- merge(sgti,sgst)
                sgti <- merge(sgti,sgit)
                
                sumcutoff.iowa <- merge(sg,sgti)
                sumcutoff.iowa <- select(sumcutoff.iowa,cutoff.iowa,N,
                                           `Fracmax Mean`,`NBack Mean`,`Stroop Mean`,`Iowa Mean`,
                                           `Time Mean`,`T.NBack Mean`,`T.Stroop Mean`,`T.Iowa Mean`,
                                           `Fracmax SD`,`NBack SD`,`Stroop SD`,`Iowa SD`,
                                           `Time SD`,`T.NBack SD`,`T.Stroop SD`,`T.Iowa SD`)
                
                # sumcutoff.iowa sumcutoff.stroop sumcutoff.nback sumcutoff.fracmax 
                #sumtreat sumtreatgen sumtreatmag sumgrupo
                write.csv(sumgrupo,"C:/Users/danbo/Desktop/MturkR/BaseF/Means/sumgrupo.csv")
                write.csv(sumtreatmag,"C:/Users/danbo/Desktop/MturkR/BaseF/Means/sumtreatmag.csv")
                write.csv(sumtreatgen,"C:/Users/danbo/Desktop/MturkR/BaseF/Means/sumtreatgen.csv")
                write.csv(sumtreat,"C:/Users/danbo/Desktop/MturkR/BaseF/Means/sumtreat.csv")
                write.csv(sumcutoff.fracmax,"C:/Users/danbo/Desktop/MturkR/BaseF/Means/sumcutoff.fracmax.csv")
                write.csv(sumcutoff.nback,"C:/Users/danbo/Desktop/MturkR/BaseF/Means/sumcutoff.nback.csv")
                write.csv(sumcutoff.stroop,"C:/Users/danbo/Desktop/MturkR/BaseF/Means/sumcutoff.stroop.csv")
                write.csv(sumcutoff.iowa,"C:/Users/danbo/Desktop/MturkR/BaseF/Means/sumcutoff.iowa.csv")
                
                ##### Cutoffs Mean SD SE ####
                #cutoff.fracmax
                #Perf
                sgf <- ddply(base.pop.cutoff,
                             c("treatgen","cutoff.fracmax"), summarise,
                             N    = length(fracmax),
                             mean = mean(fracmax),
                             sd   = sd(fracmax),
                             se   = sd / sqrt(N)
                )
                sgf <- rename(sgf,
                              c(mean="Fracmax Mean",
                                sd="Fracmax SD",
                                se="Fracmax SE"))
                
                sgn <- ddply(base.pop.cutoff,
                             c("treatgen","cutoff.fracmax"), summarise,
                             N    = length(nback),
                             mean = mean(nback),
                             sd   = sd(nback),
                             se   = sd / sqrt(N)
                )
                sgn <- rename(sgn,
                              c(mean="NBack Mean",
                                sd="NBack SD",
                                se="NBack SE"))
                sgs <- ddply(base.pop.cutoff,
                             c("treatgen","cutoff.fracmax"), summarise,
                             N    = length(stroop),
                             mean = mean(stroop),
                             sd   = sd(stroop),
                             se   = sd / sqrt(N)
                )
                sgs <- rename(sgs,
                              c(mean="Stroop Mean",
                                sd="Stroop SD",
                                se="Stroop SE"))
                sgi <- ddply(base.pop.cutoff,
                             c("treatgen","cutoff.fracmax"), summarise,
                             N    = length(iowa),
                             mean = mean(iowa),
                             sd   = sd(iowa),
                             se   = sd / sqrt(N)
                )
                sgi <- rename(sgi,
                              c(mean="Iowa Mean",
                                sd="Iowa SD",
                                se="Iowa SE"))
                sg <- merge(sgf,sgn)
                sg <- merge(sg,sgs)
                sg <- merge(sg,sgi)
                # TIME
                sgt <- ddply(base.pop.cutoff,
                             c("treatgen","cutoff.fracmax"), summarise,
                             N    = length(time),
                             mean = mean(time),
                             sd   = sd(time),
                             se   = sd / sqrt(N)
                )
                sgt <- rename(sgt,
                              c(mean="Time Mean",
                                sd="Time SD",
                                se="Time SE"))
                sgnt <- ddply(base.pop.cutoff,
                              c("treatgen","cutoff.fracmax"), summarise,
                              N    = length(t.nback),
                              mean = mean(t.nback),
                              sd   = sd(t.nback),
                              se   = sd / sqrt(N)
                )
                sgnt <- rename(sgnt,
                               c(mean="T.NBack Mean",
                                 sd="T.NBack SD",
                                 se="T.NBack SE"))
                sgst <- ddply(base.pop.cutoff,
                              c("treatgen","cutoff.fracmax"), summarise,
                              N    = length(t.stroop),
                              mean = mean(t.stroop),
                              sd   = sd(t.stroop),
                              se   = sd / sqrt(N)
                )
                sgst <- rename(sgst,
                               c(mean="T.Stroop Mean",
                                 sd="T.Stroop SD",
                                 se="T.Stroop SE"))
                sgit <- ddply(base.pop.cutoff,
                              c("treatgen","cutoff.fracmax"), summarise,
                              N    = length(t.iowa),
                              mean = mean(t.iowa),
                              sd   = sd(t.iowa),
                              se   = sd / sqrt(N)
                )
                sgit <- rename(sgit,
                               c(mean="T.Iowa Mean",
                                 sd="T.Iowa SD",
                                 se="T.Iowa SE"))
                sgti <- merge(sgt,sgnt)
                sgti <- merge(sgti,sgst)
                sgti <- merge(sgti,sgit)
                
                sumgencutoff.fracmax <- merge(sg,sgti)
                sumgencutoff.fracmax <- select(sumgencutoff.fracmax,treatgen,cutoff.fracmax,N,
                                            `Fracmax Mean`,`NBack Mean`,`Stroop Mean`,`Iowa Mean`,
                                            `Time Mean`,`T.NBack Mean`,`T.Stroop Mean`,`T.Iowa Mean`,
                                            `Fracmax SD`,`NBack SD`,`Stroop SD`,`Iowa SD`,
                                            `Time SD`,`T.NBack SD`,`T.Stroop SD`,`T.Iowa SD`)
                
                sumgencutoff.fracmax <- sumgencutoff.fracmax[order(sumgencutoff.fracmax$cutoff.fracmax,decreasing=TRUE),]
                
                #Treatmag
                #Perf
                sgf <- ddply(base.pop.cutoff,
                             c("treatmag","cutoff.fracmax"), summarise,
                             N    = length(fracmax),
                             mean = mean(fracmax),
                             sd   = sd(fracmax),
                             se   = sd / sqrt(N)
                )
                sgf <- rename(sgf,
                              c(mean="Fracmax Mean",
                                sd="Fracmax SD",
                                se="Fracmax SE"))
                sgn <- ddply(base.pop.cutoff,
                             c("treatmag","cutoff.fracmax"), summarise,
                             N    = length(nback),
                             mean = mean(nback),
                             sd   = sd(nback),
                             se   = sd / sqrt(N)
                )
                sgn <- rename(sgn,
                              c(mean="NBack Mean",
                                sd="NBack SD",
                                se="NBack SE"))
                sgs <- ddply(base.pop.cutoff,
                             c("treatmag","cutoff.fracmax"), summarise,
                             N    = length(stroop),
                             mean = mean(stroop),
                             sd   = sd(stroop),
                             se   = sd / sqrt(N)
                )
                sgs <- rename(sgs,
                              c(mean="Stroop Mean",
                                sd="Stroop SD",
                                se="Stroop SE"))
                sgi <- ddply(base.pop.cutoff,
                             c("treatmag","cutoff.fracmax"), summarise,
                             N    = length(iowa),
                             mean = mean(iowa),
                             sd   = sd(iowa),
                             se   = sd / sqrt(N)
                )
                sgi <- rename(sgi,
                              c(mean="Iowa Mean",
                                sd="Iowa SD",
                                se="Iowa SE"))
                sg <- merge(sgf,sgn)
                sg <- merge(sg,sgs)
                sg <- merge(sg,sgi)
                # TIME
                sgt <- ddply(base.pop.cutoff,
                             c("treatmag","cutoff.fracmax"), summarise,
                             N    = length(time),
                             mean = mean(time),
                             sd   = sd(time),
                             se   = sd / sqrt(N)
                )
                sgt <- rename(sgt,
                              c(mean="Time Mean",
                                sd="Time SD",
                                se="Time SE"))
                sgnt <- ddply(base.pop.cutoff,
                              c("treatmag","cutoff.fracmax"), summarise,
                              N    = length(t.nback),
                              mean = mean(t.nback),
                              sd   = sd(t.nback),
                              se   = sd / sqrt(N)
                )
                sgnt <- rename(sgnt,
                               c(mean="T.NBack Mean",
                                 sd="T.NBack SD",
                                 se="T.NBack SE"))
                sgst <- ddply(base.pop.cutoff,
                              c("treatmag","cutoff.fracmax"), summarise,
                              N    = length(t.stroop),
                              mean = mean(t.stroop),
                              sd   = sd(t.stroop),
                              se   = sd / sqrt(N)
                )
                sgst <- rename(sgst,
                               c(mean="T.Stroop Mean",
                                 sd="T.Stroop SD",
                                 se="T.Stroop SE"))
                sgit <- ddply(base.pop.cutoff,
                              c("treatmag","cutoff.fracmax"), summarise,
                              N    = length(t.iowa),
                              mean = mean(t.iowa),
                              sd   = sd(t.iowa),
                              se   = sd / sqrt(N)
                )
                sgit <- rename(sgit,
                               c(mean="T.Iowa Mean",
                                 sd="T.Iowa SD",
                                 se="T.Iowa SE"))
                sgti <- merge(sgt,sgnt)
                sgti <- merge(sgti,sgst)
                sgti <- merge(sgti,sgit)
                
                summagcutoff.fracmax <- merge(sg,sgti)
                summagcutoff.fracmax <- select(summagcutoff.fracmax,treatmag,cutoff.fracmax,N,
                                          `Fracmax Mean`,`NBack Mean`,`Stroop Mean`,`Iowa Mean`,
                                          `Time Mean`,`T.NBack Mean`,`T.Stroop Mean`,`T.Iowa Mean`,
                                          `Fracmax SD`,`NBack SD`,`Stroop SD`,`Iowa SD`,
                                          `Time SD`,`T.NBack SD`,`T.Stroop SD`,`T.Iowa SD`)
                
                summagcutoff.fracmax <- summagcutoff.fracmax[order(summagcutoff.fracmax$cutoff.fracmax,decreasing=TRUE),]
                
                #indtreat
                sgf <- ddply(base.pop.cutoff,
                             c("treat","cutoff.fracmax"), summarise,
                             N    = length(fracmax),
                             mean = mean(fracmax),
                             sd   = sd(fracmax),
                             se   = sd / sqrt(N)
                )
                sgf <- rename(sgf,
                              c(mean="Fracmax Mean",
                                sd="Fracmax SD",
                                se="Fracmax SE"))
                
                sgn <- ddply(base.pop.cutoff,
                             c("treat","cutoff.fracmax"), summarise,
                             N    = length(nback),
                             mean = mean(nback),
                             sd   = sd(nback),
                             se   = sd / sqrt(N)
                )
                sgn <- rename(sgn,
                              c(mean="NBack Mean",
                                sd="NBack SD",
                                se="NBack SE"))
                sgs <- ddply(base.pop.cutoff,
                             c("treat","cutoff.fracmax"), summarise,
                             N    = length(stroop),
                             mean = mean(stroop),
                             sd   = sd(stroop),
                             se   = sd / sqrt(N)
                )
                sgs <- rename(sgs,
                              c(mean="Stroop Mean",
                                sd="Stroop SD",
                                se="Stroop SE"))
                sgi <- ddply(base.pop.cutoff,
                             c("treat","cutoff.fracmax"), summarise,
                             N    = length(iowa),
                             mean = mean(iowa),
                             sd   = sd(iowa),
                             se   = sd / sqrt(N)
                )
                sgi <- rename(sgi,
                              c(mean="Iowa Mean",
                                sd="Iowa SD",
                                se="Iowa SE"))
                sg <- merge(sgf,sgn)
                sg <- merge(sg,sgs)
                sg <- merge(sg,sgi)
                # TIME
                sgt <- ddply(base.pop.cutoff,
                             c("treat","cutoff.fracmax"), summarise,
                             N    = length(time),
                             mean = mean(time),
                             sd   = sd(time),
                             se   = sd / sqrt(N)
                )
                sgt <- rename(sgt,
                              c(mean="Time Mean",
                                sd="Time SD",
                                se="Time SE"))
                sgnt <- ddply(base.pop.cutoff,
                              c("treat","cutoff.fracmax"), summarise,
                              N    = length(t.nback),
                              mean = mean(t.nback),
                              sd   = sd(t.nback),
                              se   = sd / sqrt(N)
                )
                sgnt <- rename(sgnt,
                               c(mean="T.NBack Mean",
                                 sd="T.NBack SD",
                                 se="T.NBack SE"))
                sgst <- ddply(base.pop.cutoff,
                              c("treat","cutoff.fracmax"), summarise,
                              N    = length(t.stroop),
                              mean = mean(t.stroop),
                              sd   = sd(t.stroop),
                              se   = sd / sqrt(N)
                )
                sgst <- rename(sgst,
                               c(mean="T.Stroop Mean",
                                 sd="T.Stroop SD",
                                 se="T.Stroop SE"))
                sgit <- ddply(base.pop.cutoff,
                              c("treat","cutoff.fracmax"), summarise,
                              N    = length(t.iowa),
                              mean = mean(t.iowa),
                              sd   = sd(t.iowa),
                              se   = sd / sqrt(N)
                )
                sgit <- rename(sgit,
                               c(mean="T.Iowa Mean",
                                 sd="T.Iowa SD",
                                 se="T.Iowa SE"))
                sgti <- merge(sgt,sgnt)
                sgti <- merge(sgti,sgst)
                sgti <- merge(sgti,sgit)
                
                sumindcutoff.fracmax <- merge(sg,sgti)
                sumindcutoff.fracmax <- select(sumindcutoff.fracmax,treat,cutoff.fracmax,N,
                                               `Fracmax Mean`,`NBack Mean`,`Stroop Mean`,`Iowa Mean`,
                                               `Time Mean`,`T.NBack Mean`,`T.Stroop Mean`,`T.Iowa Mean`,
                                               `Fracmax SD`,`NBack SD`,`Stroop SD`,`Iowa SD`,
                                               `Time SD`,`T.NBack SD`,`T.Stroop SD`,`T.Iowa SD`)
                
                sumindcutoff.fracmax <- sumindcutoff.fracmax[order(sumindcutoff.fracmax$cutoff.fracmax,decreasing=TRUE),]
                
                #sumgencutoff.fracmax summagcutoff.fracmax sumindcutoff.fracmax 
                write.csv(sumgencutoff.fracmax,"C:/Users/danbo/Desktop/MturkR/BaseF/Means/sumgencutoff.fracmax.csv")
                write.csv(summagcutoff.fracmax,"C:/Users/danbo/Desktop/MturkR/BaseF/Means/summagcutoff.fracmax.csv")
                write.csv(sumindcutoff.fracmax,"C:/Users/danbo/Desktop/MturkR/BaseF/Means/sumindcutoff.fracmax.csv")
                
                #
                ##### 3.1 Regressions #####              
                setwd("C:/Users/danbo/Desktop/RTB")
                
                library(fastDummies)
                library(lmtest)
                library(sandwich)
                library(car)
                library(ggplot2)
                library(MASS)
                library(Hmisc)
                library(reshape2)
                library(stargazer)
                
                frlm <- lm(fracmax ~ treatpay, data=base.pop.cutoff)
                nrlm <- lm(nback ~ treatpay, data=base.pop.cutoff)
                srlm <- lm(stroop ~ treatpay, data=base.pop.cutoff)
                irlm <- lm(iowa ~ treatpay, data=base.pop.cutoff)
                stargazer(frlm, nrlm, srlm, irlm,
                          title="LR Results", align=TRUE, no.space=TRUE,
                          type = "html", out="paylr.doc")
                
                frlm <- lm(fracmax ~ treatpay, data=base.pop.cutoff)
                fcov <- vcovHC(frlm, type = "HC")
                frobust.se <- sqrt(diag(fcov))
                nrlm <- lm(nback ~ treatpay, data=base.pop.cutoff)
                ncov <- vcovHC(nrlm, type = "HC")
                nrobust.se <- sqrt(diag(ncov))
                srlm <- lm(stroop ~ treatpay, data=base.pop.cutoff)
                scov <- vcovHC(srlm, type = "HC")
                srobust.se <- sqrt(diag(scov))
                irlm <- lm(iowa ~ treatpay, data=base.pop.cutoff)
                icov <- vcovHC(irlm, type = "HC")
                irobust.se <- sqrt(diag(icov))
                stargazer(frlm, frlm,nrlm,nrlm,srlm,srlm,irlm,irlm,
                          se=list(NULL, frobust.se,
                                  NULL, nrobust.se,
                                  NULL, srobust.se,
                                  NULL, irobust.se),
                          title="LR Results", align=TRUE, no.space=TRUE,
                          type = "html", out="paylrSE.doc")
                
                cov <- vcovHC(reg.model, type = "HC")
                robust.se <- sqrt(diag(cov))
                stargazer(reg.model, reg.model, se=list(NULL, robust.se),
                          column.labels=c("default","robust"), align=TRUE)
                
                treatd <- dummy_cols(base.pop.cutoff$treat)
                fracmaxd <- base.pop.cutoff$fracmax
                fractre <- as.data.frame(cbind(fracmaxd,treatd))
                #Para modificar reference group, quitar dummy deseada. Else quita última
                regfractreat <- lm(fracmaxd ~ .data_a1 +
                                     .data_a2 + .data_a3 + .data_a4 +
                                     .data_d1 + .data_d2 + .data_d3 + 
                                     .data_i1 + .data_i2 + .data_i3 +
                                     .data_n1 + .data_n2 +
                                     .data_n3 + .data_n4, data=fractre)
                summary(regfractreat)
                #Reference Group es el primero por default
                #Usar relevel para desifnar reference group
                #base.pop.cutoff$treat <- relevel(base.pop.cutoff$treat, ref=n1)
                #No sirve relevel mientras quitar la que queramos de referencia arriba
                regprueba <- lm(fracmax ~ treatpay, data=base.pop.cutoff)
                summary(regprueba)
                
                #Graphs
                qqPlot(regprueba)
                residualPlots(regprueba)
                influencePlot(regprueba)
                influenceIndexPlot(regprueba)
                avPlots(regprueba)
                residualPlot(regprueba)
                
                #Robust SE
                #regress prestige education log2income women, robust (Stata)
                # R:
                # reg1$robse <- vcovHC(reg1, type="HC1")
                # coeftest(reg1,reg1$robse) 
                # Predicted Values:
                # prestige_hat <- fitted(reg1)
                # as.data.frame(prestige_hat)
                # prestige_resid <- residuals(reg1)
                # as.data.frame(prestige_resid)
                
                ##### 3.2 Ord Probit #####
                
                #Ordered Probit
                m <- polr(cutoff.fracmax ~ treat, 
                          data = base.pop.cutoff, Hess=TRUE,
                          method = c("probit"))
                ## view a summary of the model
                summary(m)
                
                #Ordered Probit
                fm <- polr(cutoff.fracmax ~ treatgen, 
                          data = base.pop.cutoff, Hess=TRUE,
                          method = c("probit"))
                fm.coef <- data.frame(coef(summary(fm)))
                fm.coef$pval = round((pnorm(abs(fm.coef$t.value),
                                            lower.tail = FALSE) * 2),2)
                #Ordered Probit
                nb <- polr(cutoff.nback ~ treatgen, 
                           data = base.pop.cutoff, Hess=TRUE,
                           method = c("probit"))
                nb.coef <- data.frame(coef(summary(nb)))
                nb.coef$pval = round((pnorm(abs(nb.coef$t.value),
                                            lower.tail = FALSE) * 2),2)
                #Ordered Probit
                st <- polr(cutoff.stroop ~ treatgen, 
                           data = base.pop.cutoff, Hess=TRUE,
                           method = c("probit"))
                st.coef <- data.frame(coef(summary(st)))
                st.coef$pval = round((pnorm(abs(st.coef$t.value),
                                            lower.tail = FALSE) * 2),2)
                #Ordered Probit
                io <- polr(cutoff.iowa ~ treatgen, 
                           data = base.pop.cutoff, Hess=TRUE,
                           method = c("probit"))
                io.coef <- data.frame(coef(summary(io)))
                io.coef$pval = round((pnorm(abs(io.coef$t.value),
                                            lower.tail = FALSE) * 2),2)
                library(stargazer)
                #stargazer(fm,nb,st,io, type="html", out="m1.htm")
                
                #fm.or=exp(coef(m1))
                #nb.or=exp(coef(m1))
                #st.or=exp(coef(m1))
                #io.or=exp(coef(m1))
                
                #stargazer(fm,nb,st,io, type="html", 
                 #         coef=list(fm.or),
                  #        coef=list(nb.or),
                   #       coef=list(st.or),
                    #      coef=list(io.or),
                     #     p.auto=FALSE, out="m1or.htm")
                
                
                ## view a summary of the model
                #summary(m)
                #(ctable <- coef(summary(m)))
                #p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
                #(ctable <- cbind(ctable, "p value" = p))
                #(ci <- confint(m))
                #confint.default(m)
                #exp(coef(m))
                #exp(cbind(OR = coef(m), ci))
                
                #Hay que hacerlos factores para poder cambiar la referencia
                base.pop.cutoff$treat <- 
                  factor(base.pop.cutoff$treat)
                base.pop.cutoff$treatgen <- 
                  factor(base.pop.cutoff$treatgen)
                base.pop.cutoff$treatgen <- 
                  relevel(base.pop.cutoff$treatgen, ref = "nb")
                #La mejor pero no exporta a tablas usar para margenes
                library(oglmx)
                # estimate standard ordered probit
                results.oprob<-oglmx(cutoff.fracmax ~ treatgen,
                                     data = base.pop.cutoff,
                                     link="probit",
                                     analhessian=TRUE,
                                     constantMEAN=FALSE,
                                     constantSD=FALSE,delta=0,
                                     threshparam=NULL,
                                     robust=TRUE)
                coef(results.oprob) # extract estimated coefficients
                summary(results.oprob)
                # calculate marginal effects at means
                margins.oglmx(results.oprob)
              
              
                
                ##### 3.3 T-Tests Individual #####
                library(tidyverse)
                library(rstatix)
                library(ggpubr)
                
                t.testf <- base.pop.cutoff %>%
                  t_test(fracmax ~ treat, detailed = TRUE) %>%
                  adjust_pvalue(method = "BH") %>%
                  add_significance()
                t.testf <- select(t.testf,group1,group2,n1,n2,statistic,p,estimate)
                t.testf <- rename(t.testf,
                                  c(statistic="Fracmax t-stat",
                                    p="Fracmax p-value",
                                    estimate="Fracmax Diff"))
                
                t.testn <- base.pop.cutoff %>%
                  t_test(nback ~ treat, detailed = TRUE) %>%
                  adjust_pvalue(method = "BH") %>%
                  add_significance()
                t.testn <- select(t.testn,group1,group2,n1,n2,statistic,p,estimate)
                t.testn <- rename(t.testn,
                                  c(statistic="NBack t-stat",
                                    p="NBack p-value",
                                    estimate="NBack Diff"))
                
                t.tests <- base.pop.cutoff %>%
                  t_test(stroop ~ treat, detailed = TRUE) %>%
                  adjust_pvalue(method = "BH") %>%
                  add_significance()
                t.tests <- select(t.tests,group1,group2,n1,n2,statistic,p,estimate)
                t.tests <- rename(t.tests,
                                  c(statistic="Stroop t-stat",
                                    p="Stroop p-value",
                                    estimate="Stroop Diff"))

                t.testi <- base.pop.cutoff %>%
                  t_test(iowa ~ treat, detailed = TRUE) %>%
                  adjust_pvalue(method = "BH") %>%
                  add_significance()
                t.testi <- select(t.testi,group1,group2,n1,n2,statistic,p,estimate)
                t.testi <- rename(t.testi,
                                  c(statistic="Iowa t-stat",
                                    p="Iowa p-value",
                                    estimate="Iowa Diff"))
                
                t.testtime <- base.pop.cutoff %>%
                  t_test(time ~ treat, detailed = TRUE) %>%
                  adjust_pvalue(method = "BH") %>%
                  add_significance()
                t.testtime <- select(t.testtime,group1,group2,n1,n2,statistic,p,estimate)
                t.testtime <- rename(t.testtime,
                                  c(statistic="Time t-stat",
                                    p="Time p-value",
                                    estimate="Time Diff"))
                
                t.testtn <- base.pop.cutoff %>%
                  t_test(t.nback ~ treat, detailed = TRUE) %>%
                  adjust_pvalue(method = "BH") %>%
                  add_significance()
                t.testtn <- select(t.testtn,group1,group2,n1,n2,statistic,p,estimate)
                t.testtn <- rename(t.testtn,
                                  c(statistic="T.NBack t-stat",
                                    p="T.NBack p-value",
                                    estimate="T.NBack Diff"))
                
                t.testts <- base.pop.cutoff %>%
                  t_test(t.stroop ~ treat, detailed = TRUE) %>%
                  adjust_pvalue(method = "BH") %>%
                  add_significance()
                t.testts <- select(t.testts,group1,group2,n1,n2,statistic,p,estimate)
                t.testts <- rename(t.testts,
                                  c(statistic="T.Stroop t-stat",
                                    p="T.Stroop p-value",
                                    estimate="T.Stroop Diff"))
                
                t.testti <- base.pop.cutoff %>%
                  t_test(t.iowa ~ treat, detailed = TRUE) %>%
                  adjust_pvalue(method = "BH") %>%
                  add_significance()
                t.testti <- select(t.testti,group1,group2,n1,n2,statistic,p,estimate)
                t.testti <- rename(t.testti,
                                  c(statistic="T.Iowa t-stat",
                                    p="T.Iowa p-value",
                                    estimate="T.Iowa Diff"))
                
                library(gtools)
                
                t1 <- merge(t.testf,t.testn,all.x=TRUE,sort = FALSE)
                t2 <- merge(t1,t.tests,all.x=TRUE,sort = FALSE)
                ttest.perf <- merge(t2,t.testi,all.x=TRUE,sort = FALSE)
                ttest.perf$fs <- NA
                ttest.perf$ns <- NA
                ttest.perf$ss <- NA
                ttest.perf$is <- NA
                ttest.perf$fs <- stars.pval(ttest.perf$`Fracmax p-value`)
                ttest.perf$ns <- stars.pval(ttest.perf$`NBack p-value`)
                ttest.perf$ss <- stars.pval(ttest.perf$`Stroop p-value`)
                ttest.perf$is <- stars.pval(ttest.perf$`Iowa p-value`)
                ttest.perf <- select(ttest.perf,
                                         group1,group2,n1,n2,
                                         `Fracmax Diff`,`Fracmax t-stat`,fs,
                                         `NBack Diff`,`NBack t-stat`,ns,
                                         `Stroop Diff`,`Stroop t-stat`,ss,
                                         `Iowa Diff`,`Iowa t-stat`,is,)
                #Remove NA si no tiene significancia en al menos 1 parametro
                ttest.perf[ttest.perf==" "]<-NA
                ttest.perf.sig <- as.data.frame(
                  ttest.perf[!is.na(ttest.perf$fs) |
                                   !is.na(ttest.perf$ns) |
                                   !is.na(ttest.perf$ss) |
                                   !is.na(ttest.perf$is),])
                
                tt1 <- merge(t.testtime,t.testtn,all.x=TRUE,sort = FALSE)
                tt2 <- merge(tt1,t.testts,all.x=TRUE,sort = FALSE)
                ttest.time <- merge(tt2,t.testti,all.x=TRUE,sort = FALSE)
                ttest.time$ts <- NA
                ttest.time$ns <- NA
                ttest.time$ss <- NA
                ttest.time$is <- NA
                ttest.time$ts <- stars.pval(ttest.time$`Time p-value`)
                ttest.time$ns <- stars.pval(ttest.time$`T.NBack p-value`)
                ttest.time$ss <- stars.pval(ttest.time$`T.Stroop p-value`)
                ttest.time$is <- stars.pval(ttest.time$`T.Iowa p-value`)
                ttest.time <- select(ttest.time,
                                         group1,group2,n1,n2,
                                         `Time Diff`,`Time t-stat`,ts,
                                         `T.NBack Diff`,`T.NBack t-stat`,ns,
                                         `T.Stroop Diff`,`T.Stroop t-stat`,ss,
                                         `T.Iowa Diff`,`T.Iowa t-stat`,is,)
                #Remove NA si no tiene significancia en al menos 1 parametro
                ttest.time[ttest.time==" "]<-NA
                ttest.time.sig <- as.data.frame(
                  ttest.time[!is.na(ttest.time$ts) |
                               !is.na(ttest.time$ns) |
                               !is.na(ttest.time$ss) |
                               !is.na(ttest.time$is),])
                
                write.csv(ttest.perf,"C:/Users/danbo/Desktop/MturkR/BaseF/ttest.perf.csv")
                write.csv(ttest.time,"C:/Users/danbo/Desktop/MturkR/BaseF/ttest.time.csv")
                write.csv(ttest.perf.sig,"C:/Users/danbo/Desktop/MturkR/BaseF/ttest.perf.sig.csv")
                write.csv(ttest.time.sig,"C:/Users/danbo/Desktop/MturkR/BaseF/ttest.time.sig.csv")
                
                ##### 3.3 T-Tests Individual - Anchoring Effect (A1-4 def) #####
               
                a1.t.testf <- base.pop.cutoff %>%
                  t_test(fracmax ~ treat, detailed = TRUE,
                         ref.group = "a1") %>%
                  adjust_pvalue(method = "BH") %>%
                  add_significance()
                a1.t.testf <- select(a1.t.testf,group1,group2,n1,n2,statistic,p,estimate)
                a1.t.testf <- rename(a1.t.testf,
                                  c(statistic="Fracmax t-stat",
                                    p="Fracmax p-value",
                                    estimate="Fracmax Diff"))
                
                n1.t.testf <- base.pop.cutoff %>%
                  t_test(fracmax ~ treat, detailed = TRUE,
                         ref.group = "n1") %>%
                  adjust_pvalue(method = "BH") %>%
                  add_significance()
                n1.t.testf <- select(n1.t.testf,group1,group2,n1,n2,statistic,p,estimate)
                n1.t.testf <- rename(n1.t.testf,
                                         c(statistic="Fracmax t-stat",
                                           p="Fracmax p-value",
                                           estimate="Fracmax Diff"))
                
                a1.t.testn <- base.pop.cutoff %>%
                  t_test(nback ~ treat, detailed = TRUE,
                         ref.group = "a1") %>%
                  adjust_pvalue(method = "BH") %>%
                  add_significance()
                a1.t.testn <- select(a1.t.testn,group1,group2,n1,n2,statistic,p,estimate)
                a1.t.testn <- rename(a1.t.testn,
                                  c(statistic="NBack t-stat",
                                    p="NBack p-value",
                                    estimate="NBack Diff"))
                
                n1.t.testn <- base.pop.cutoff %>%
                  t_test(nback ~ treat, detailed = TRUE,
                         ref.group = "n1") %>%
                  adjust_pvalue(method = "BH") %>%
                  add_significance()
                n1.t.testn <- select(n1.t.testn,group1,group2,n1,n2,statistic,p,estimate)
                n1.t.testn <- rename(n1.t.testn,
                                  c(statistic="NBack t-stat",
                                    p="NBack p-value",
                                    estimate="NBack Diff"))
                
                a1.t.tests <- base.pop.cutoff %>%
                  t_test(stroop ~ treat, detailed = TRUE,
                         ref.group = "a1") %>%
                  adjust_pvalue(method = "BH") %>%
                  add_significance()
                a1.t.tests <- select(a1.t.tests,group1,group2,n1,n2,statistic,p,estimate)
                a1.t.tests <- rename(a1.t.tests,
                                  c(statistic="Stroop t-stat",
                                    p="Stroop p-value",
                                    estimate="Stroop Diff"))
                
                n1.t.tests <- base.pop.cutoff %>%
                  t_test(stroop ~ treat, detailed = TRUE,
                         ref.group = "n1") %>%
                  adjust_pvalue(method = "BH") %>%
                  add_significance()
                n1.t.tests <- select(n1.t.tests,group1,group2,n1,n2,statistic,p,estimate)
                n1.t.tests <- rename(n1.t.tests,
                                  c(statistic="Stroop t-stat",
                                    p="Stroop p-value",
                                    estimate="Stroop Diff"))
                
                a1.t.testi <- base.pop.cutoff %>%
                  t_test(iowa ~ treat, detailed = TRUE,
                         ref.group = "a1") %>%
                  adjust_pvalue(method = "BH") %>%
                  add_significance()
                a1.t.testi <- select(a1.t.testi,group1,group2,n1,n2,statistic,p,estimate)
                a1.t.testi <- rename(a1.t.testi,
                                  c(statistic="Iowa t-stat",
                                    p="Iowa p-value",
                                    estimate="Iowa Diff"))
                
                n1.t.testi <- base.pop.cutoff %>%
                  t_test(iowa ~ treat, detailed = TRUE,
                         ref.group = "n1") %>%
                  adjust_pvalue(method = "BH") %>%
                  add_significance()
                n1.t.testi <- select(n1.t.testi,group1,group2,n1,n2,statistic,p,estimate)
                n1.t.testi <- rename(n1.t.testi,
                                  c(statistic="Iowa t-stat",
                                    p="Iowa p-value",
                                    estimate="Iowa Diff"))
                
                a1.t.testtime <- base.pop.cutoff %>%
                  t_test(time ~ treat, detailed = TRUE,
                         ref.group = "a1") %>%
                  adjust_pvalue(method = "BH") %>%
                  add_significance()
                a1.t.testtime <- select(a1.t.testtime,group1,group2,n1,n2,statistic,p,estimate)
                a1.t.testtime <- rename(a1.t.testtime,
                                     c(statistic="Time t-stat",
                                       p="Time p-value",
                                       estimate="Time Diff"))
                
                n1.t.testtime <- base.pop.cutoff %>%
                  t_test(time ~ treat, detailed = TRUE,
                         ref.group = "n1") %>%
                  adjust_pvalue(method = "BH") %>%
                  add_significance()
                n1.t.testtime <- select(n1.t.testtime,group1,group2,n1,n2,statistic,p,estimate)
                n1.t.testtime <- rename(n1.t.testtime,
                                     c(statistic="Time t-stat",
                                       p="Time p-value",
                                       estimate="Time Diff"))
                
                a1.t.testtn <- base.pop.cutoff %>%
                  t_test(t.nback ~ treat, detailed = TRUE,
                         ref.group = "a1") %>%
                  adjust_pvalue(method = "BH") %>%
                  add_significance()
                a1.t.testtn <- select(a1.t.testtn,group1,group2,n1,n2,statistic,p,estimate)
                a1.t.testtn <- rename(a1.t.testtn,
                                   c(statistic="T.NBack t-stat",
                                     p="T.NBack p-value",
                                     estimate="T.NBack Diff"))
                
                n1.t.testtn <- base.pop.cutoff %>%
                  t_test(t.nback ~ treat, detailed = TRUE,
                         ref.group = "n1") %>%
                  adjust_pvalue(method = "BH") %>%
                  add_significance()
                n1.t.testtn <- select(n1.t.testtn,group1,group2,n1,n2,statistic,p,estimate)
                n1.t.testtn <- rename(n1.t.testtn,
                                   c(statistic="T.NBack t-stat",
                                     p="T.NBack p-value",
                                     estimate="T.NBack Diff"))
                
                a1.t.testts <- base.pop.cutoff %>%
                  t_test(t.stroop ~ treat, detailed = TRUE,
                         ref.group = "a1") %>%
                  adjust_pvalue(method = "BH") %>%
                  add_significance()
                a1.t.testts <- select(a1.t.testts,group1,group2,n1,n2,statistic,p,estimate)
                a1.t.testts <- rename(a1.t.testts,
                                   c(statistic="T.Stroop t-stat",
                                     p="T.Stroop p-value",
                                     estimate="T.Stroop Diff"))
                
                n1.t.testts <- base.pop.cutoff %>%
                  t_test(t.stroop ~ treat, detailed = TRUE,
                         ref.group = "n1") %>%
                  adjust_pvalue(method = "BH") %>%
                  add_significance()
                n1.t.testts <- select(n1.t.testts,group1,group2,n1,n2,statistic,p,estimate)
                n1.t.testts <- rename(n1.t.testts,
                                   c(statistic="T.Stroop t-stat",
                                     p="T.Stroop p-value",
                                     estimate="T.Stroop Diff"))
                
                a1.t.testti <- base.pop.cutoff %>%
                  t_test(t.iowa ~ treat, detailed = TRUE,
                         ref.group = "a1") %>%
                  adjust_pvalue(method = "BH") %>%
                  add_significance()
                a1.t.testti <- select(a1.t.testti,group1,group2,n1,n2,statistic,p,estimate)
                a1.t.testti <- rename(a1.t.testti,
                                   c(statistic="T.Iowa t-stat",
                                     p="T.Iowa p-value",
                                     estimate="T.Iowa Diff"))
                
                n1.t.testti <- base.pop.cutoff %>%
                  t_test(t.iowa ~ treat, detailed = TRUE,
                         ref.group = "n1") %>%
                  adjust_pvalue(method = "BH") %>%
                  add_significance()
                n1.t.testti <- select(n1.t.testti,group1,group2,n1,n2,statistic,p,estimate)
                n1.t.testti <- rename(n1.t.testti,
                                   c(statistic="T.Iowa t-stat",
                                     p="T.Iowa p-value",
                                     estimate="T.Iowa Diff"))
                
                library(gtools)
                
                #perf
              
                a1.t1 <- merge(a1.t.testf,a1.t.testn,all.x=TRUE,sort = FALSE)
                a1.t2 <- merge(a1.t1,a1.t.tests,all.x=TRUE,sort = FALSE)
                a1.ttest.perf <- merge(a1.t2,a1.t.testi,all.x=TRUE,sort = FALSE)
                a1.ttest.perf$fs <- NA
                a1.ttest.perf$ns <- NA
                a1.ttest.perf$ss <- NA
                a1.ttest.perf$is <- NA
                a1.ttest.perf$fs <- stars.pval(a1.ttest.perf$`Fracmax p-value`)
                a1.ttest.perf$ns <- stars.pval(a1.ttest.perf$`NBack p-value`)
                a1.ttest.perf$ss <- stars.pval(a1.ttest.perf$`Stroop p-value`)
                a1.ttest.perf$is <- stars.pval(a1.ttest.perf$`Iowa p-value`)
                a1.ttest.perf <- select(a1.ttest.perf,
                                     group1,group2,n1,n2,
                                     `Fracmax Diff`,`Fracmax t-stat`,fs,
                                     `NBack Diff`,`NBack t-stat`,ns,
                                     `Stroop Diff`,`Stroop t-stat`,ss,
                                     `Iowa Diff`,`Iowa t-stat`,is,)
                #Remove NA si no tiene significancia en al menos 1 parametro
                a1.ttest.perf[a1.ttest.perf==" "]<-NA
                a1.ttest.perf.sig <- as.data.frame(
                  a1.ttest.perf[!is.na(a1.ttest.perf$fs) |
                               !is.na(a1.ttest.perf$ns) |
                               !is.na(a1.ttest.perf$ss) |
                               !is.na(a1.ttest.perf$is),])
                
                
                n1.t1 <- merge(n1.t.testf,n1.t.testn,all.x=TRUE,sort = FALSE)
                n1.t2 <- merge(n1.t1,n1.t.tests,all.x=TRUE,sort = FALSE)
                n1.ttest.perf <- merge(n1.t2,n1.t.testi,all.x=TRUE,sort = FALSE)
                n1.ttest.perf$fs <- NA
                n1.ttest.perf$ns <- NA
                n1.ttest.perf$ss <- NA
                n1.ttest.perf$is <- NA
                n1.ttest.perf$fs <- stars.pval(n1.ttest.perf$`Fracmax p-value`)
                n1.ttest.perf$ns <- stars.pval(n1.ttest.perf$`NBack p-value`)
                n1.ttest.perf$ss <- stars.pval(n1.ttest.perf$`Stroop p-value`)
                n1.ttest.perf$is <- stars.pval(n1.ttest.perf$`Iowa p-value`)
                n1.ttest.perf <- select(n1.ttest.perf,
                                        group1,group2,n1,n2,
                                        `Fracmax Diff`,`Fracmax t-stat`,fs,
                                        `NBack Diff`,`NBack t-stat`,ns,
                                        `Stroop Diff`,`Stroop t-stat`,ss,
                                        `Iowa Diff`,`Iowa t-stat`,is,)
                #Remove NA si no tiene significancia en al menos 1 parametro
                n1.ttest.perf[n1.ttest.perf==" "]<-NA
                n1.ttest.perf.sig <- as.data.frame(
                  n1.ttest.perf[!is.na(n1.ttest.perf$fs) |
                                  !is.na(n1.ttest.perf$ns) |
                                  !is.na(n1.ttest.perf$ss) |
                                  !is.na(n1.ttest.perf$is),])
                
                
                #time
                
                a1.tt1 <- merge(a1.t.testtime,a1.t.testtn,all.x=TRUE,sort = FALSE)
                a1.tt2 <- merge(a1.tt1,a1.t.testts,all.x=TRUE,sort = FALSE)
                a1.ttest.time <- merge(a1.tt2,a1.t.testti,all.x=TRUE,sort = FALSE)
                a1.ttest.time$ts <- NA
                a1.ttest.time$ns <- NA
                a1.ttest.time$ss <- NA
                a1.ttest.time$is <- NA
                a1.ttest.time$ts <- stars.pval(a1.ttest.time$`Time p-value`)
                a1.ttest.time$ns <- stars.pval(a1.ttest.time$`T.NBack p-value`)
                a1.ttest.time$ss <- stars.pval(a1.ttest.time$`T.Stroop p-value`)
                a1.ttest.time$is <- stars.pval(a1.ttest.time$`T.Iowa p-value`)
                a1.ttest.time <- select(a1.ttest.time,
                                     group1,group2,n1,n2,
                                     `Time Diff`,`Time t-stat`,ts,
                                     `T.NBack Diff`,`T.NBack t-stat`,ns,
                                     `T.Stroop Diff`,`T.Stroop t-stat`,ss,
                                     `T.Iowa Diff`,`T.Iowa t-stat`,is,)
                #Remove NA si no tiene significancia en al menos 1 parametro
                a1.ttest.time[a1.ttest.time==" "]<-NA
                a1.ttest.time.sig <- as.data.frame(
                  a1.ttest.time[!is.na(a1.ttest.time$ts) |
                               !is.na(a1.ttest.time$ns) |
                               !is.na(a1.ttest.time$ss) |
                               !is.na(a1.ttest.time$is),])
                
                n1.tt1 <- merge(n1.t.testtime,n1.t.testtn,all.x=TRUE,sort = FALSE)
                n1.tt2 <- merge(n1.tt1,n1.t.testts,all.x=TRUE,sort = FALSE)
                n1.ttest.time <- merge(n1.tt2,n1.t.testti,all.x=TRUE,sort = FALSE)
                n1.ttest.time$ts <- NA
                n1.ttest.time$ns <- NA
                n1.ttest.time$ss <- NA
                n1.ttest.time$is <- NA
                n1.ttest.time$ts <- stars.pval(n1.ttest.time$`Time p-value`)
                n1.ttest.time$ns <- stars.pval(n1.ttest.time$`T.NBack p-value`)
                n1.ttest.time$ss <- stars.pval(n1.ttest.time$`T.Stroop p-value`)
                n1.ttest.time$is <- stars.pval(n1.ttest.time$`T.Iowa p-value`)
                n1.ttest.time <- select(n1.ttest.time,
                                        group1,group2,n1,n2,
                                        `Time Diff`,`Time t-stat`,ts,
                                        `T.NBack Diff`,`T.NBack t-stat`,ns,
                                        `T.Stroop Diff`,`T.Stroop t-stat`,ss,
                                        `T.Iowa Diff`,`T.Iowa t-stat`,is,)
                #Remove NA si no tiene significancia en al menos 1 parametro
                n1.ttest.time[n1.ttest.time==" "]<-NA
                n1.ttest.time.sig <- as.data.frame(
                  n1.ttest.time[!is.na(n1.ttest.time$ts) |
                                  !is.na(n1.ttest.time$ns) |
                                  !is.na(n1.ttest.time$ss) |
                                  !is.na(n1.ttest.time$is),])
                
                write.csv(a1.ttest.perf,"C:/Users/danbo/Desktop/MturkR/BaseF/a1.ttest.perf.csv")
                write.csv(a1.ttest.time,"C:/Users/danbo/Desktop/MturkR/BaseF/a1.ttest.time.csv")
                write.csv(n1.ttest.perf,"C:/Users/danbo/Desktop/MturkR/BaseF/n1.ttest.perf.csv")
                write.csv(n1.ttest.time,"C:/Users/danbo/Desktop/MturkR/BaseF/n1.ttest.time.csv")
                
                write.csv(a1.ttest.perf.sig,"C:/Users/danbo/Desktop/MturkR/BaseF/a1.ttest.perf.sig.csv")
                write.csv(a1.ttest.time.sig,"C:/Users/danbo/Desktop/MturkR/BaseF/a1.ttest.time.sig.csv")
                write.csv(n1.ttest.perf.sig,"C:/Users/danbo/Desktop/MturkR/BaseF/n1.ttest.perf.sig.csv")
                write.csv(n1.ttest.time.sig,"C:/Users/danbo/Desktop/MturkR/BaseF/n1.ttest.time.sig.csv")
                
                ##### 3.3 T-Tests Individual - Anchoring Effect (N2 y N3) #####
                
                n2.t.testf <- base.pop.cutoff %>%
                  t_test(fracmax ~ treat, detailed = TRUE,
                         ref.group = "n2") %>%
                  adjust_pvalue(method = "BH") %>%
                  add_significance()
                n2.t.testf <- select(n2.t.testf,group1,group2,n1,n2,statistic,p,estimate)
                n2.t.testf <- rename(n2.t.testf,
                                     c(statistic="Fracmax t-stat",
                                       p="Fracmax p-value",
                                       estimate="Fracmax Diff"))
                
                n2.t.testn <- base.pop.cutoff %>%
                  t_test(nback ~ treat, detailed = TRUE,
                         ref.group = "n2") %>%
                  adjust_pvalue(method = "BH") %>%
                  add_significance()
                n2.t.testn <- select(n2.t.testn,group1,group2,n1,n2,statistic,p,estimate)
                n2.t.testn <- rename(n2.t.testn,
                                     c(statistic="NBack t-stat",
                                       p="NBack p-value",
                                       estimate="NBack Diff"))
                
                n2.t.tests <- base.pop.cutoff %>%
                  t_test(stroop ~ treat, detailed = TRUE,
                         ref.group = "n2") %>%
                  adjust_pvalue(method = "BH") %>%
                  add_significance()
                n2.t.tests <- select(n2.t.tests,group1,group2,n1,n2,statistic,p,estimate)
                n2.t.tests <- rename(n2.t.tests,
                                     c(statistic="Stroop t-stat",
                                       p="Stroop p-value",
                                       estimate="Stroop Diff"))
                
                n2.t.testi <- base.pop.cutoff %>%
                  t_test(iowa ~ treat, detailed = TRUE,
                         ref.group = "n2") %>%
                  adjust_pvalue(method = "BH") %>%
                  add_significance()
                n2.t.testi <- select(n2.t.testi,group1,group2,n1,n2,statistic,p,estimate)
                n2.t.testi <- rename(n2.t.testi,
                                     c(statistic="Iowa t-stat",
                                       p="Iowa p-value",
                                       estimate="Iowa Diff"))
                
                n2.t.testtime <- base.pop.cutoff %>%
                  t_test(time ~ treat, detailed = TRUE,
                         ref.group = "n2") %>%
                  adjust_pvalue(method = "BH") %>%
                  add_significance()
                n2.t.testtime <- select(n2.t.testtime,group1,group2,n1,n2,statistic,p,estimate)
                n2.t.testtime <- rename(n2.t.testtime,
                                        c(statistic="Time t-stat",
                                          p="Time p-value",
                                          estimate="Time Diff"))
                
                n2.t.testtn <- base.pop.cutoff %>%
                  t_test(t.nback ~ treat, detailed = TRUE,
                         ref.group = "n2") %>%
                  adjust_pvalue(method = "BH") %>%
                  add_significance()
                n2.t.testtn <- select(n2.t.testtn,group1,group2,n1,n2,statistic,p,estimate)
                n2.t.testtn <- rename(n2.t.testtn,
                                      c(statistic="T.NBack t-stat",
                                        p="T.NBack p-value",
                                        estimate="T.NBack Diff"))
                
                n2.t.testts <- base.pop.cutoff %>%
                  t_test(t.stroop ~ treat, detailed = TRUE,
                         ref.group = "n2") %>%
                  adjust_pvalue(method = "BH") %>%
                  add_significance()
                n2.t.testts <- select(n2.t.testts,group1,group2,n1,n2,statistic,p,estimate)
                n2.t.testts <- rename(n2.t.testts,
                                      c(statistic="T.Stroop t-stat",
                                        p="T.Stroop p-value",
                                        estimate="T.Stroop Diff"))
                
                n2.t.testti <- base.pop.cutoff %>%
                  t_test(t.iowa ~ treat, detailed = TRUE,
                         ref.group = "n2") %>%
                  adjust_pvalue(method = "BH") %>%
                  add_significance()
                n2.t.testti <- select(n2.t.testti,group1,group2,n1,n2,statistic,p,estimate)
                n2.t.testti <- rename(n2.t.testti,
                                      c(statistic="T.Iowa t-stat",
                                        p="T.Iowa p-value",
                                        estimate="T.Iowa Diff"))
                
                library(gtools)
                
                #perf
                
                n2.t1 <- merge(n2.t.testf,n2.t.testn,all.x=TRUE,sort = FALSE)
                n2.t2 <- merge(n2.t1,n2.t.tests,all.x=TRUE,sort = FALSE)
                n2.ttest.perf <- merge(n2.t2,n2.t.testi,all.x=TRUE,sort = FALSE)
                n2.ttest.perf$fs <- NA
                n2.ttest.perf$ns <- NA
                n2.ttest.perf$ss <- NA
                n2.ttest.perf$is <- NA
                n2.ttest.perf$fs <- stars.pval(n2.ttest.perf$`Fracmax p-value`)
                n2.ttest.perf$ns <- stars.pval(n2.ttest.perf$`NBack p-value`)
                n2.ttest.perf$ss <- stars.pval(n2.ttest.perf$`Stroop p-value`)
                n2.ttest.perf$is <- stars.pval(n2.ttest.perf$`Iowa p-value`)
                n2.ttest.perf <- select(n2.ttest.perf,
                                        group1,group2,n1,n2,
                                        `Fracmax Diff`,`Fracmax t-stat`,fs,
                                        `NBack Diff`,`NBack t-stat`,ns,
                                        `Stroop Diff`,`Stroop t-stat`,ss,
                                        `Iowa Diff`,`Iowa t-stat`,is,)
                #Remove NA si no tiene significancia en al menos 1 parametro
                n2.ttest.perf[n2.ttest.perf==" "]<-NA
                n2.ttest.perf.sig <- as.data.frame(
                  n2.ttest.perf[!is.na(n2.ttest.perf$fs) |
                                  !is.na(n2.ttest.perf$ns) |
                                  !is.na(n2.ttest.perf$ss) |
                                  !is.na(n2.ttest.perf$is),])
                
                #time
                
                n2.tt1 <- merge(n2.t.testtime,n2.t.testtn,all.x=TRUE,sort = FALSE)
                n2.tt2 <- merge(n2.tt1,n2.t.testts,all.x=TRUE,sort = FALSE)
                n2.ttest.time <- merge(n2.tt2,n2.t.testti,all.x=TRUE,sort = FALSE)
                n2.ttest.time$ts <- NA
                n2.ttest.time$ns <- NA
                n2.ttest.time$ss <- NA
                n2.ttest.time$is <- NA
                n2.ttest.time$ts <- stars.pval(n2.ttest.time$`Time p-value`)
                n2.ttest.time$ns <- stars.pval(n2.ttest.time$`T.NBack p-value`)
                n2.ttest.time$ss <- stars.pval(n2.ttest.time$`T.Stroop p-value`)
                n2.ttest.time$is <- stars.pval(n2.ttest.time$`T.Iowa p-value`)
                n2.ttest.time <- select(n2.ttest.time,
                                        group1,group2,n1,n2,
                                        `Time Diff`,`Time t-stat`,ts,
                                        `T.NBack Diff`,`T.NBack t-stat`,ns,
                                        `T.Stroop Diff`,`T.Stroop t-stat`,ss,
                                        `T.Iowa Diff`,`T.Iowa t-stat`,is,)
                #Remove NA si no tiene significancia en al menos 1 parametro
                n2.ttest.time[n2.ttest.time==" "]<-NA
                n2.ttest.time.sig <- as.data.frame(
                  n2.ttest.time[!is.na(n2.ttest.time$ts) |
                                  !is.na(n2.ttest.time$ns) |
                                  !is.na(n2.ttest.time$ss) |
                                  !is.na(n2.ttest.time$is),])
                
                write.csv(n2.ttest.perf,"C:/Users/danbo/Desktop/MturkR/BaseF/n2.ttest.perf.csv")
                write.csv(n2.ttest.time,"C:/Users/danbo/Desktop/MturkR/BaseF/n2.ttest.time.csv")
                
                write.csv(n2.ttest.perf.sig,"C:/Users/danbo/Desktop/MturkR/BaseF/n2.ttest.perf.sig.csv")
                write.csv(n2.ttest.time.sig,"C:/Users/danbo/Desktop/MturkR/BaseF/n2.ttest.time.sig.csv")
                
                ###
                n3.t.testf <- base.pop.cutoff %>%
                  t_test(fracmax ~ treat, detailed = TRUE,
                         ref.group = "n3") %>%
                  adjust_pvalue(method = "BH") %>%
                  add_significance()
                n3.t.testf <- select(n3.t.testf,group1,group2,n1,n2,statistic,p,estimate)
                n3.t.testf <- rename(n3.t.testf,
                                     c(statistic="Fracmax t-stat",
                                       p="Fracmax p-value",
                                       estimate="Fracmax Diff"))
                
                n3.t.testn <- base.pop.cutoff %>%
                  t_test(nback ~ treat, detailed = TRUE,
                         ref.group = "n3") %>%
                  adjust_pvalue(method = "BH") %>%
                  add_significance()
                n3.t.testn <- select(n3.t.testn,group1,group2,n1,n2,statistic,p,estimate)
                n3.t.testn <- rename(n3.t.testn,
                                     c(statistic="NBack t-stat",
                                       p="NBack p-value",
                                       estimate="NBack Diff"))
                
                n3.t.tests <- base.pop.cutoff %>%
                  t_test(stroop ~ treat, detailed = TRUE,
                         ref.group = "n3") %>%
                  adjust_pvalue(method = "BH") %>%
                  add_significance()
                n3.t.tests <- select(n3.t.tests,group1,group2,n1,n2,statistic,p,estimate)
                n3.t.tests <- rename(n3.t.tests,
                                     c(statistic="Stroop t-stat",
                                       p="Stroop p-value",
                                       estimate="Stroop Diff"))
                
                n3.t.testi <- base.pop.cutoff %>%
                  t_test(iowa ~ treat, detailed = TRUE,
                         ref.group = "n3") %>%
                  adjust_pvalue(method = "BH") %>%
                  add_significance()
                n3.t.testi <- select(n3.t.testi,group1,group2,n1,n2,statistic,p,estimate)
                n3.t.testi <- rename(n3.t.testi,
                                     c(statistic="Iowa t-stat",
                                       p="Iowa p-value",
                                       estimate="Iowa Diff"))
                
                n3.t.testtime <- base.pop.cutoff %>%
                  t_test(time ~ treat, detailed = TRUE,
                         ref.group = "n3") %>%
                  adjust_pvalue(method = "BH") %>%
                  add_significance()
                n3.t.testtime <- select(n3.t.testtime,group1,group2,n1,n2,statistic,p,estimate)
                n3.t.testtime <- rename(n3.t.testtime,
                                        c(statistic="Time t-stat",
                                          p="Time p-value",
                                          estimate="Time Diff"))
                
                n3.t.testtn <- base.pop.cutoff %>%
                  t_test(t.nback ~ treat, detailed = TRUE,
                         ref.group = "n3") %>%
                  adjust_pvalue(method = "BH") %>%
                  add_significance()
                n3.t.testtn <- select(n3.t.testtn,group1,group2,n1,n2,statistic,p,estimate)
                n3.t.testtn <- rename(n3.t.testtn,
                                      c(statistic="T.NBack t-stat",
                                        p="T.NBack p-value",
                                        estimate="T.NBack Diff"))
                
                n3.t.testts <- base.pop.cutoff %>%
                  t_test(t.stroop ~ treat, detailed = TRUE,
                         ref.group = "n3") %>%
                  adjust_pvalue(method = "BH") %>%
                  add_significance()
                n3.t.testts <- select(n3.t.testts,group1,group2,n1,n2,statistic,p,estimate)
                n3.t.testts <- rename(n3.t.testts,
                                      c(statistic="T.Stroop t-stat",
                                        p="T.Stroop p-value",
                                        estimate="T.Stroop Diff"))
                
                n3.t.testti <- base.pop.cutoff %>%
                  t_test(t.iowa ~ treat, detailed = TRUE,
                         ref.group = "n3") %>%
                  adjust_pvalue(method = "BH") %>%
                  add_significance()
                n3.t.testti <- select(n3.t.testti,group1,group2,n1,n2,statistic,p,estimate)
                n3.t.testti <- rename(n3.t.testti,
                                      c(statistic="T.Iowa t-stat",
                                        p="T.Iowa p-value",
                                        estimate="T.Iowa Diff"))
                
                library(gtools)
                
                #perf
                
                n3.t1 <- merge(n3.t.testf,n3.t.testn,all.x=TRUE,sort = FALSE)
                n3.t2 <- merge(n3.t1,n3.t.tests,all.x=TRUE,sort = FALSE)
                n3.ttest.perf <- merge(n3.t2,n3.t.testi,all.x=TRUE,sort = FALSE)
                n3.ttest.perf$fs <- NA
                n3.ttest.perf$ns <- NA
                n3.ttest.perf$ss <- NA
                n3.ttest.perf$is <- NA
                n3.ttest.perf$fs <- stars.pval(n3.ttest.perf$`Fracmax p-value`)
                n3.ttest.perf$ns <- stars.pval(n3.ttest.perf$`NBack p-value`)
                n3.ttest.perf$ss <- stars.pval(n3.ttest.perf$`Stroop p-value`)
                n3.ttest.perf$is <- stars.pval(n3.ttest.perf$`Iowa p-value`)
                n3.ttest.perf <- select(n3.ttest.perf,
                                        group1,group2,n1,n2,
                                        `Fracmax Diff`,`Fracmax t-stat`,fs,
                                        `NBack Diff`,`NBack t-stat`,ns,
                                        `Stroop Diff`,`Stroop t-stat`,ss,
                                        `Iowa Diff`,`Iowa t-stat`,is,)
                #Remove NA si no tiene significancia en al menos 1 parametro
                n3.ttest.perf[n3.ttest.perf==" "]<-NA
                n3.ttest.perf.sig <- as.data.frame(
                  n3.ttest.perf[!is.na(n3.ttest.perf$fs) |
                                  !is.na(n3.ttest.perf$ns) |
                                  !is.na(n3.ttest.perf$ss) |
                                  !is.na(n3.ttest.perf$is),])
                
                #time
                
                n3.tt1 <- merge(n3.t.testtime,n3.t.testtn,all.x=TRUE,sort = FALSE)
                n3.tt2 <- merge(n3.tt1,n3.t.testts,all.x=TRUE,sort = FALSE)
                n3.ttest.time <- merge(n3.tt2,n3.t.testti,all.x=TRUE,sort = FALSE)
                n3.ttest.time$ts <- NA
                n3.ttest.time$ns <- NA
                n3.ttest.time$ss <- NA
                n3.ttest.time$is <- NA
                n3.ttest.time$ts <- stars.pval(n3.ttest.time$`Time p-value`)
                n3.ttest.time$ns <- stars.pval(n3.ttest.time$`T.NBack p-value`)
                n3.ttest.time$ss <- stars.pval(n3.ttest.time$`T.Stroop p-value`)
                n3.ttest.time$is <- stars.pval(n3.ttest.time$`T.Iowa p-value`)
                n3.ttest.time <- select(n3.ttest.time,
                                        group1,group2,n1,n2,
                                        `Time Diff`,`Time t-stat`,ts,
                                        `T.NBack Diff`,`T.NBack t-stat`,ns,
                                        `T.Stroop Diff`,`T.Stroop t-stat`,ss,
                                        `T.Iowa Diff`,`T.Iowa t-stat`,is,)
                #Remove NA si no tiene significancia en al menos 1 parametro
                n3.ttest.time[n3.ttest.time==" "]<-NA
                n3.ttest.time.sig <- as.data.frame(
                  n3.ttest.time[!is.na(n3.ttest.time$ts) |
                                  !is.na(n3.ttest.time$ns) |
                                  !is.na(n3.ttest.time$ss) |
                                  !is.na(n3.ttest.time$is),])
                
                write.csv(n3.ttest.perf,"C:/Users/danbo/Desktop/MturkR/BaseF/n3.ttest.perf.csv")
                write.csv(n3.ttest.time,"C:/Users/danbo/Desktop/MturkR/BaseF/n3.ttest.time.csv")
                
                write.csv(n3.ttest.perf.sig,"C:/Users/danbo/Desktop/MturkR/BaseF/n3.ttest.perf.sig.csv")
                write.csv(n3.ttest.time.sig,"C:/Users/danbo/Desktop/MturkR/BaseF/n3.ttest.time.sig.csv")
                
                ##### 3.3 T-Tests Generico #####
                
                t.testf.gen <- base.pop.cutoff %>%
                  t_test(fracmax ~ treatgen, detailed = TRUE) %>%
                  adjust_pvalue(method = "BH") %>%
                  add_significance()
                t.testf.gen <- select(t.testf.gen,group1,group2,n1,n2,statistic,p,estimate)
                t.testf.gen <- rename(t.testf.gen,
                                  c(statistic="Fracmax t-stat",
                                    p="Fracmax p-value",
                                    estimate="Fracmax Diff"))
                
                t.testn.gen <- base.pop.cutoff %>%
                  t_test(nback ~ treatgen, detailed = TRUE) %>%
                  adjust_pvalue(method = "BH") %>%
                  add_significance()
                t.testn.gen <- select(t.testn.gen,group1,group2,n1,n2,statistic,p,estimate)
                t.testn.gen <- rename(t.testn.gen,
                                  c(statistic="NBack t-stat",
                                    p="NBack p-value",
                                    estimate="NBack Diff"))
                
                t.tests.gen <- base.pop.cutoff %>%
                  t_test(stroop ~ treatgen, detailed = TRUE) %>%
                  adjust_pvalue(method = "BH") %>%
                  add_significance()
                t.tests.gen <- select(t.tests.gen,group1,group2,n1,n2,statistic,p,estimate)
                t.tests.gen <- rename(t.tests.gen,
                                  c(statistic="Stroop t-stat",
                                    p="Stroop p-value",
                                    estimate="Stroop Diff"))
                
                t.testi.gen <- base.pop.cutoff %>%
                  t_test(iowa ~ treatgen, detailed = TRUE) %>%
                  adjust_pvalue(method = "BH") %>%
                  add_significance()
                t.testi.gen <- select(t.testi.gen,group1,group2,n1,n2,statistic,p,estimate)
                t.testi.gen <- rename(t.testi.gen,
                                  c(statistic="Iowa t-stat",
                                    p="Iowa p-value",
                                    estimate="Iowa Diff"))
                
                t.testtime.gen <- base.pop.cutoff %>%
                  t_test(time ~ treatgen, detailed = TRUE) %>%
                  adjust_pvalue(method = "BH") %>%
                  add_significance()
                t.testtime.gen <- select(t.testtime.gen,group1,group2,n1,n2,statistic,p,estimate)
                t.testtime.gen <- rename(t.testtime.gen,
                                     c(statistic="Time t-stat",
                                       p="Time p-value",
                                       estimate="Time Diff"))
                
                t.testtn.gen <- base.pop.cutoff %>%
                  t_test(t.nback ~ treatgen, detailed = TRUE) %>%
                  adjust_pvalue(method = "BH") %>%
                  add_significance()
                t.testtn.gen <- select(t.testtn.gen,group1,group2,n1,n2,statistic,p,estimate)
                t.testtn.gen <- rename(t.testtn.gen,
                                   c(statistic="T.NBack t-stat",
                                     p="T.NBack p-value",
                                     estimate="T.NBack Diff"))
                
                t.testts.gen <- base.pop.cutoff %>%
                  t_test(t.stroop ~ treatgen, detailed = TRUE) %>%
                  adjust_pvalue(method = "BH") %>%
                  add_significance()
                t.testts.gen <- select(t.testts.gen,group1,group2,n1,n2,statistic,p,estimate)
                t.testts.gen <- rename(t.testts.gen,
                                   c(statistic="T.Stroop t-stat",
                                     p="T.Stroop p-value",
                                     estimate="T.Stroop Diff"))
                
                t.testti.gen <- base.pop.cutoff %>%
                  t_test(t.iowa ~ treatgen, detailed = TRUE) %>%
                  adjust_pvalue(method = "BH") %>%
                  add_significance()
                t.testti.gen <- select(t.testti.gen,group1,group2,n1,n2,statistic,p,estimate)
                t.testti.gen <- rename(t.testti.gen,
                                   c(statistic="T.Iowa t-stat",
                                     p="T.Iowa p-value",
                                     estimate="T.Iowa Diff"))
                
                library(gtools)
                
                t1 <- merge(t.testf.gen,t.testn.gen,all.x=TRUE,sort = FALSE)
                t2 <- merge(t1,t.tests.gen,all.x=TRUE,sort = FALSE)
                ttest.perf.gen <- merge(t2,t.testi.gen,all.x=TRUE,sort = FALSE)
                ttest.perf.gen$fs <- NA
                ttest.perf.gen$ns <- NA
                ttest.perf.gen$ss <- NA
                ttest.perf.gen$is <- NA
                ttest.perf.gen$fs <- stars.pval(ttest.perf.gen$`Fracmax p-value`)
                ttest.perf.gen$ns <- stars.pval(ttest.perf.gen$`NBack p-value`)
                ttest.perf.gen$ss <- stars.pval(ttest.perf.gen$`Stroop p-value`)
                ttest.perf.gen$is <- stars.pval(ttest.perf.gen$`Iowa p-value`)
                ttest.perf.gen <- select(ttest.perf.gen,
                                         group1,group2,n1,n2,
                                         `Fracmax Diff`,`Fracmax t-stat`,fs,
                                         `NBack Diff`,`NBack t-stat`,ns,
                                         `Stroop Diff`,`Stroop t-stat`,ss,
                                         `Iowa Diff`,`Iowa t-stat`,is,)
                
                #Remove NA si no tiene significancia en al menos 1 parametro
                ttest.perf.gen[ttest.perf.gen==" "]<-NA
                ttest.perf.gen.sig <- as.data.frame(
                  ttest.perf.gen[!is.na(ttest.perf.gen$fs) |
                                   !is.na(ttest.perf.gen$ns) |
                                   !is.na(ttest.perf.gen$ss) |
                                   !is.na(ttest.perf.gen$is),])
                
                tt1 <- merge(t.testtime.gen,t.testtn.gen,all.x=TRUE,sort = FALSE)
                tt2 <- merge(tt1,t.testts.gen,all.x=TRUE,sort = FALSE)
                ttest.time.gen <- merge(tt2,t.testti.gen,all.x=TRUE,sort = FALSE)
                ttest.time.gen$ts <- NA
                ttest.time.gen$ns <- NA
                ttest.time.gen$ss <- NA
                ttest.time.gen$is <- NA
                ttest.time.gen$ts <- stars.pval(ttest.time.gen$`Time p-value`)
                ttest.time.gen$ns <- stars.pval(ttest.time.gen$`T.NBack p-value`)
                ttest.time.gen$ss <- stars.pval(ttest.time.gen$`T.Stroop p-value`)
                ttest.time.gen$is <- stars.pval(ttest.time.gen$`T.Iowa p-value`)
                ttest.time.gen <- select(ttest.time.gen,
                                         group1,group2,n1,n2,
                                         `Time Diff`,`Time t-stat`,ts,
                                         `T.NBack Diff`,`T.NBack t-stat`,ns,
                                         `T.Stroop Diff`,`T.Stroop t-stat`,ss,
                                         `T.Iowa Diff`,`T.Iowa t-stat`,is,)
                #Remove NA si no tiene significancia en al menos 1 parametro
                ttest.time.gen[ttest.time.gen==" "]<-NA
                ttest.time.gen.sig <- as.data.frame(
                  ttest.time.gen[!is.na(ttest.time.gen$ts) |
                                  !is.na(ttest.time.gen$ns) |
                                  !is.na(ttest.time.gen$ss) |
                                  !is.na(ttest.time.gen$is),])
                
                write.csv(ttest.perf.gen,"C:/Users/danbo/Desktop/MturkR/BaseF/ttest.perf.gen.csv")
                write.csv(ttest.time.gen,"C:/Users/danbo/Desktop/MturkR/BaseF/ttest.time.gen.csv")
                
                ##### 3.3 T-Tests Magnitude #####
                
                t.testf.mag <- base.pop.cutoff %>%
                  t_test(fracmax ~ treatmag, detailed = TRUE) %>%
                  adjust_pvalue(method = "BH") %>%
                  add_significance()
                t.testf.mag <- select(t.testf.mag,group1,group2,n1,n2,statistic,p,estimate)
                t.testf.mag <- rename(t.testf.mag,
                                      c(statistic="Fracmax t-stat",
                                        p="Fracmax p-value",
                                        estimate="Fracmax Diff"))
                
                t.testn.mag <- base.pop.cutoff %>%
                  t_test(nback ~ treatmag, detailed = TRUE) %>%
                  adjust_pvalue(method = "BH") %>%
                  add_significance()
                t.testn.mag <- select(t.testn.mag,group1,group2,n1,n2,statistic,p,estimate)
                t.testn.mag <- rename(t.testn.mag,
                                      c(statistic="NBack t-stat",
                                        p="NBack p-value",
                                        estimate="NBack Diff"))
                
                t.tests.mag <- base.pop.cutoff %>%
                  t_test(stroop ~ treatmag, detailed = TRUE) %>%
                  adjust_pvalue(method = "BH") %>%
                  add_significance()
                t.tests.mag <- select(t.tests.mag,group1,group2,n1,n2,statistic,p,estimate)
                t.tests.mag <- rename(t.tests.mag,
                                      c(statistic="Stroop t-stat",
                                        p="Stroop p-value",
                                        estimate="Stroop Diff"))
                
                t.testi.mag <- base.pop.cutoff %>%
                  t_test(iowa ~ treatmag, detailed = TRUE) %>%
                  adjust_pvalue(method = "BH") %>%
                  add_significance()
                t.testi.mag <- select(t.testi.mag,group1,group2,n1,n2,statistic,p,estimate)
                t.testi.mag <- rename(t.testi.mag,
                                      c(statistic="Iowa t-stat",
                                        p="Iowa p-value",
                                        estimate="Iowa Diff"))
                
                t.testtime.mag <- base.pop.cutoff %>%
                  t_test(time ~ treatmag, detailed = TRUE) %>%
                  adjust_pvalue(method = "BH") %>%
                  add_significance()
                t.testtime.mag <- select(t.testtime.mag,group1,group2,n1,n2,statistic,p,estimate)
                t.testtime.mag <- rename(t.testtime.mag,
                                         c(statistic="Time t-stat",
                                           p="Time p-value",
                                           estimate="Time Diff"))
                
                t.testtn.mag <- base.pop.cutoff %>%
                  t_test(t.nback ~ treatmag, detailed = TRUE) %>%
                  adjust_pvalue(method = "BH") %>%
                  add_significance()
                t.testtn.mag <- select(t.testtn.mag,group1,group2,n1,n2,statistic,p,estimate)
                t.testtn.mag <- rename(t.testtn.mag,
                                       c(statistic="T.NBack t-stat",
                                         p="T.NBack p-value",
                                         estimate="T.NBack Diff"))
                
                t.testts.mag <- base.pop.cutoff %>%
                  t_test(t.stroop ~ treatmag, detailed = TRUE) %>%
                  adjust_pvalue(method = "BH") %>%
                  add_significance()
                t.testts.mag <- select(t.testts.mag,group1,group2,n1,n2,statistic,p,estimate)
                t.testts.mag <- rename(t.testts.mag,
                                       c(statistic="T.Stroop t-stat",
                                         p="T.Stroop p-value",
                                         estimate="T.Stroop Diff"))
                
                t.testti.mag <- base.pop.cutoff %>%
                  t_test(t.iowa ~ treatmag, detailed = TRUE) %>%
                  adjust_pvalue(method = "BH") %>%
                  add_significance()
                t.testti.mag <- select(t.testti.mag,group1,group2,n1,n2,statistic,p,estimate)
                t.testti.mag <- rename(t.testti.mag,
                                       c(statistic="T.Iowa t-stat",
                                         p="T.Iowa p-value",
                                         estimate="T.Iowa Diff"))
                
                library(gtools)
                
                t1 <- merge(t.testf.mag,t.testn.mag,all.x=TRUE,sort = FALSE)
                t2 <- merge(t1,t.tests.mag,all.x=TRUE,sort = FALSE)
                ttest.perf.mag <- merge(t2,t.testi.mag,all.x=TRUE,sort = FALSE)
                ttest.perf.mag$fs <- NA
                ttest.perf.mag$ns <- NA
                ttest.perf.mag$ss <- NA
                ttest.perf.mag$is <- NA
                ttest.perf.mag$fs <- stars.pval(ttest.perf.mag$`Fracmax p-value`)
                ttest.perf.mag$ns <- stars.pval(ttest.perf.mag$`NBack p-value`)
                ttest.perf.mag$ss <- stars.pval(ttest.perf.mag$`Stroop p-value`)
                ttest.perf.mag$is <- stars.pval(ttest.perf.mag$`Iowa p-value`)
                ttest.perf.mag <- select(ttest.perf.mag,
                                         group1,group2,n1,n2,
                                         `Fracmax Diff`,`Fracmax t-stat`,fs,
                                         `NBack Diff`,`NBack t-stat`,ns,
                                         `Stroop Diff`,`Stroop t-stat`,ss,
                                         `Iowa Diff`,`Iowa t-stat`,is,)
                
                tt1 <- merge(t.testtime.mag,t.testtn.mag,all.x=TRUE,sort = FALSE)
                tt2 <- merge(tt1,t.testts.mag,all.x=TRUE,sort = FALSE)
                ttest.time.mag <- merge(tt2,t.testti.mag,all.x=TRUE,sort = FALSE)
                ttest.time.mag$ts <- NA
                ttest.time.mag$ns <- NA
                ttest.time.mag$ss <- NA
                ttest.time.mag$is <- NA
                ttest.time.mag$ts <- stars.pval(ttest.time.mag$`Time p-value`)
                ttest.time.mag$ns <- stars.pval(ttest.time.mag$`T.NBack p-value`)
                ttest.time.mag$ss <- stars.pval(ttest.time.mag$`T.Stroop p-value`)
                ttest.time.mag$is <- stars.pval(ttest.time.mag$`T.Iowa p-value`)
                ttest.time.mag <- select(ttest.time.mag,
                                         group1,group2,n1,n2,
                                         `Time Diff`,`Time t-stat`,ts,
                                         `T.NBack Diff`,`T.NBack t-stat`,ns,
                                         `T.Stroop Diff`,`T.Stroop t-stat`,ss,
                                         `T.Iowa Diff`,`T.Iowa t-stat`,is,)
                
                write.csv(ttest.perf.mag,"C:/Users/danbo/Desktop/MturkR/BaseF/ttest.perf.mag.csv")
                write.csv(ttest.time.mag,"C:/Users/danbo/Desktop/MturkR/BaseF/ttest.time.mag.csv")
                
      ##### Scaling and Z-scoring Games #####              
                library(scales)
                base.pop.std <- as.data.frame(base.pop[1:22],)
                base.pop.std$nback.std <- NA
                base.pop.std$nback.std <- rescale(base.pop.std$nback)
                base.pop.std$stroop.std <- NA
                base.pop.std$stroop.std <- rescale(base.pop.std$stroop)
                base.pop.std$iowa.std <- NA
                base.pop.std$iowa.std <- rescale(base.pop.std$iowa)
                
                base.pop.std$nback.z <- NA
                base.pop.std$nback.z <- scale(base.pop.std$nback.std,
                                              center = TRUE, scale = TRUE)
                base.pop.std$stroop.z <- NA
                base.pop.std$stroop.z <- scale(base.pop.std$stroop.std,
                                               center = TRUE, scale = TRUE)
                base.pop.std$iowa.z <- NA
                base.pop.std$iowa.z <- scale(base.pop.std$iowa.std,
                                             center = TRUE, scale = TRUE)
                
                ggplot(base.pop, aes(x=base.pop$nback)) +
                  geom_histogram(colour="black", fill="#00AFBB")
                ggplot(base.pop.std, aes(x=base.pop.std$nback.std)) +
                  geom_histogram(colour="black", fill="#00AFBB")
                ggplot(base.pop.std, aes(x=base.pop.std$nback.z)) +
                  geom_histogram(colour="black", fill="#00AFBB")
                
                ggplot(base.pop, aes(x=base.pop$stroop)) +
                  geom_histogram(colour="black", fill="#00AFBB")
                ggplot(base.pop.std, aes(x=base.pop.std$stroop.std)) +
                  geom_histogram(colour="black", fill="#00AFBB")
                ggplot(base.pop.std, aes(x=base.pop.std$stroop.z)) +
                  geom_histogram(colour="black", fill="#00AFBB")
                
                ggplot(base.pop, aes(x=base.pop$iowa)) +
                  geom_histogram(colour="black", fill="#00AFBB")
                ggplot(base.pop.std, aes(x=base.pop.std$iowa.std)) +
                  geom_histogram(colour="black", fill="#00AFBB")
                ggplot(base.pop.std, aes(x=base.pop.std$iowa.z)) +
                  geom_histogram(colour="black", fill="#00AFBB")
                
      ##### 4. GRAPHS #####
                
                    #Grupo Fracmax
                    myplot1 <-   ggplot(base.pop, aes(grupo, fracmax, colour = grupo)) +
                      geom_violin() + geom_point() +
                      stat_summary(fun=mean, geom="point", shape=10, 
                                   size=10, color="blue") +
                      labs(title = "Fraction of Maximum Payment all Treatments",
                           x = "Group",
                           y = "Fraction of Maximum Payment") +
                      scale_y_continuous(limits=c(0,1), labels = scales::percent)
                
                myplot2 <-   ggplot(base.pop, aes(treat, fracmax, colour = treat)) +
                  geom_violin() + geom_point() +
                  stat_summary(fun=mean, geom="point", shape=10, 
                               size=10, color="blue") +
                  labs(title = "Fraction of Maximum Payment all Treatments",
                       x = "Treatment",
                       y = "Fraction of Maximum Payment") +
                  scale_y_continuous(limits=c(0,1),
                                     labels = scales::percent) +
                  scale_color_manual(
                    values=c("#F8766D","#F8766D","#F8766D","#F8766D",
                             "#7CAE00","#7CAE00","#7CAE00",
                             "#00BFC4","#00BFC4","#00BFC4",
                             "#C77CFF","#C77CFF","#C77CFF","#C77CFF"))
                    
                    #Treat Fracmax
                  
      ##### Frac Treat Orden #####
                    
                  myplot3 <-   ggplot(subset(base.pop,
                                  treat %in% c("a1","a2","a3","a4")), 
                           mapping = aes(x = treat, y = fracmax, fill=grupo)) +
                      geom_violin(aes(color=grupo)) + facet_wrap(facets = vars(grupo)) +
                      stat_summary(fun=mean, geom="point", shape=10, 
                                   size=10, color="blue") +
                      labs(title = "Absolute Treatment - Fraction of Maximum Payment",
                           x = "Absolute Treatment",
                           y = "Fraction of Maximum Payment") +
                      scale_y_continuous(limits=c(0,1), labels = scales::percent)
                
                myplot4 <-   ggplot(subset(base.pop,
                                    treat %in% c("a1","a2","a3","a4")), 
                                    mapping = aes(x = treat, y = fracmax, color=treat)) +
                  geom_violin(aes(color=treat)) +
                  stat_summary(fun=mean, geom="point", shape=10, 
                               size=10, color="blue") +
                  labs(title = "Absolute Treatment - Fraction of Maximum Payment",
                       x = "Absolute Treatment",
                       y = "Fraction of Maximum Payment") +
                  scale_y_continuous(limits=c(0,1), labels = scales::percent)
                    
                  myplot5 <-   ggplot(subset(base.pop,
                                  treat %in% c("i1","i2","i3")), 
                           mapping = aes(x = treat, y = fracmax, fill=grupo)) +
                      geom_violin(aes(color=grupo)) + facet_wrap(facets = vars(grupo)) +
                      stat_summary(fun=mean, geom="point", shape=10, 
                                   size=10, color="blue") +
                      labs(title = "Increasing Treatment - Fraction of Maximum Payment",
                           x = "Increasing Treatment",
                           y = "Fraction of Maximum Payment") +
                      scale_y_continuous(limits=c(0,1), labels = scales::percent)
                  
                  myplot6 <-   ggplot(subset(base.pop,
                                  treat %in% c("i1","i2","i3")), 
                                  mapping = aes(x = treat, y = fracmax, color=treat)) +
                        geom_violin(aes(color=treat)) +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "Increasing Treatment - Fraction of Maximum Payment",
                         x = "Increasing Treatment",
                         y = "Fraction of Maximum Payment") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent)                 
                    
                  myplot7 <-   ggplot(subset(base.pop,
                                  treat %in% c("d1","d2","d3")), 
                           mapping = aes(x = treat, y = fracmax, fill=grupo)) +
                      geom_violin(aes(color=grupo)) + facet_wrap(facets = vars(grupo)) +
                      stat_summary(fun=mean, geom="point", shape=10, 
                                   size=10, color="blue") +
                      labs(title = "Decreasing Treatment - Fraction of Maximum Payment",
                           x = "Decreasing Treatment",
                           y = "Fraction of Maximum Payment") +
                      scale_y_continuous(limits=c(0,1), labels = scales::percent)
                  
                  myplot8 <-   ggplot(subset(base.pop,
                                             treat %in% c("d1","d2","d3")), 
                                      mapping = aes(x = treat, y = fracmax, color=treat)) +
                    geom_violin(aes(color=treat)) + 
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "Decreasing Treatment - Fraction of Maximum Payment",
                         x = "Decreasing Treatment",
                         y = "Fraction of Maximum Payment") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent)                  
                    
                  myplot9 <-   ggplot(subset(base.pop,
                                  treat %in% c("n1","n2","n3","n4")), 
                           mapping = aes(x = treat, y = fracmax, fill=grupo)) +
                      geom_violin(aes(color=grupo)) + facet_wrap(facets = vars(grupo)) +
                      stat_summary(fun=mean, geom="point", shape=10, 
                                   size=10, color="blue") +
                      labs(title = "No Bonus Treatment - Fraction of Maximum Payment",
                           x = "No Bonus Treatment",
                           y = "Fraction of Maximum Payment") +
                      scale_y_continuous(limits=c(0,1), labels = scales::percent)
                  
                  myplot10 <-   ggplot(subset(base.pop,
                                             treat %in% c("n1","n2","n3","n4")), 
                                      mapping = aes(x = treat, y = fracmax, color=treat)) +
                    geom_violin(aes(color=treat)) + 
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "No Bonus Treatment - Fraction of Maximum Payment",
                         x = "No Bonus Treatment",
                         y = "Fraction of Maximum Payment") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent)                  
                    
      ##### Frac Treat Comparables #####
                    
                  myplot11 <-   ggplot(subset(base.pop,
                                  treat %in% c("a1","n1")),
                           mapping = aes(x = treat, y = fracmax, fill=grupo)) +
                      geom_violin(aes(color=grupo)) + facet_wrap(facets = vars(grupo)) +
                      stat_summary(fun=mean, geom="point", shape=10, 
                                   size=10, color="blue") +
                      labs(title = "ABS1, NB1 - Fraction of Maximum Payment",
                           x = "ABS1, NB1",
                           y = "Fraction of Maximum Payment") +
                      scale_y_continuous(limits=c(0,1), labels = scales::percent)
                  
                  myplot12 <-   ggplot(subset(base.pop,
                                             treat %in% c("a1","n1")),
                                      mapping = aes(x = treat, y = fracmax, color=treat)) +
                    geom_violin(aes(color=treat)) + 
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "ABS1, NB1 - Fraction of Maximum Payment",
                         x = "ABS1, NB1",
                         y = "Fraction of Maximum Payment") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent)
                    
                  myplot13 <-   ggplot(subset(base.pop,
                                  treat %in% c("a2","i1","d1","n2")),
                           mapping = aes(x = treat, y = fracmax, fill=grupo)) +
                      geom_violin(aes(color=grupo)) + facet_wrap(facets = vars(grupo)) +
                      stat_summary(fun=mean, geom="point", shape=10, 
                                   size=10, color="blue") +
                      labs(title = "ABS2, DEC1, INC1, NB2 - Fraction of Maximum Payment",
                           x = "ABS2, DEC1, INC1, NB2",
                           y = "Fraction of Maximum Payment") +
                      scale_y_continuous(limits=c(0,1), labels = scales::percent)
                  
                  myplot14 <-   ggplot(subset(base.pop,
                                             treat %in% c("a2","i1","d1","n2")),
                                      mapping = aes(x = treat, y = fracmax, color=treat)) +
                    geom_violin(aes(color=treat)) +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "ABS2, DEC1, INC1, NB2 - Fraction of Maximum Payment",
                         x = "ABS2, DEC1, INC1, NB2",
                         y = "Fraction of Maximum Payment") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent)
                    
                  myplot15 <-   ggplot(subset(base.pop,
                                  treat %in% c("a3","i2","d2","n3")), 
                           mapping = aes(x = treat, y = fracmax, fill=grupo)) +
                      geom_violin(aes(color=grupo)) + facet_wrap(facets = vars(grupo)) +
                      stat_summary(fun=mean, geom="point", shape=10, 
                                   size=10, color="blue") +
                      labs(title = "ABS3, DEC2, INC2, NB3 - Fraction of Maximum Payment",
                           x = "ABS3, DEC2, INC2, NB3",
                           y = "Fraction of Maximum Payment") +
                      scale_y_continuous(limits=c(0,1), labels = scales::percent)
                  
                  myplot16 <-   ggplot(subset(base.pop,
                                             treat %in% c("a3","i2","d2","n3")), 
                                      mapping = aes(x = treat, y = fracmax, color=treat)) +
                    geom_violin(aes(color=treat)) + 
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "ABS3, DEC2, INC2, NB3 - Fraction of Maximum Payment",
                         x = "ABS3, DEC2, INC2, NB3",
                         y = "Fraction of Maximum Payment") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent)
                    
                  myplot17 <-   ggplot(subset(base.pop,
                                  treat %in% c("a4","i3","d3","n4")), 
                           mapping = aes(x = treat, y = fracmax, fill=grupo)) +
                      geom_violin(aes(color=grupo)) + facet_wrap(facets = vars(grupo)) +
                      stat_summary(fun=mean, geom="point", shape=10, 
                                   size=10, color="blue") +
                      labs(title = "ABS4, DEC3, INC3, NB4 - Fraction of Maximum Payment",
                           x = "ABS4, DEC3, INC3, NB4",
                           y = "Fraction of Maximum Payment") +
                      scale_y_continuous(limits=c(0,1), labels = scales::percent)
                  
                  myplot18 <-   ggplot(subset(base.pop,
                                             treat %in% c("a4","i3","d3","n4")), 
                                      mapping = aes(x = treat, y = fracmax, color=treat)) +
                    geom_violin(aes(color=treat)) +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "ABS4, DEC3, INC3, NB4 - Fraction of Maximum Payment",
                         x = "ABS4, DEC3, INC3, NB4",
                         y = "Fraction of Maximum Payment") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent)
                    
      ##### Frac Group Orden #####
                  myplot19 <-   ggplot(subset(base.pop,
                                  treat %in% c("a1","a2","a3","a4")),
                           aes(grupo, fracmax, colour = grupo)) +
                      geom_violin() + geom_point() +
                      stat_summary(fun=mean, geom="point", shape=10, 
                                   size=10, color="blue") +
                      labs(title = "Absolute Treatment - Fraction of Maximum Payment",
                           x = "Absolute Treatment per Group",
                           y = "Fraction of Maximum Payment") +
                      scale_y_continuous(limits=c(0,1), labels = scales::percent)
                    
                  myplot20 <-   ggplot(subset(base.pop,
                                  treat %in% c("i1","i2","i3")), 
                           aes(grupo, fracmax, colour = grupo)) +
                      geom_violin() + geom_point() +
                      stat_summary(fun=mean, geom="point", shape=10, 
                                   size=10, color="blue") +
                      labs(title = "Increasing Treatment - Fraction of Maximum Payment",
                           x = "Increasing Treatment per Group",
                           y = "Fraction of Maximum Payment") +
                      scale_y_continuous(limits=c(0,1), labels = scales::percent)
                    
                  myplot21 <-   ggplot(subset(base.pop,
                                  treat %in% c("d1","d2","d3")), 
                           aes(grupo, fracmax, colour = grupo)) +
                      geom_violin() + geom_point() +
                      stat_summary(fun=mean, geom="point", shape=10, 
                                   size=10, color="blue") +
                      labs(title = "Decreasing Treatment - Fraction of Maximum Payment",
                           x = "Decreasing Treatment per Group",
                           y = "Fraction of Maximum Payment") +
                      scale_y_continuous(limits=c(0,1), labels = scales::percent)
                    
                  myplot22 <-   ggplot(subset(base.pop,
                                  treat %in% c("n1","n2","n3","n4")), 
                           aes(grupo, fracmax, colour = grupo)) +
                      geom_violin() + geom_point() +
                      stat_summary(fun=mean, geom="point", shape=10, 
                                   size=10, color="blue") +
                      labs(title = "No Bonus Treatment - Fraction of Maximum Payment",
                           x = "NO Bonus Treatment per Group",
                           y = "Fraction of Maximum Payment") +
                      scale_y_continuous(limits=c(0,1), labels = scales::percent)
                    
                    
      ##### Frac Group Comparables #####
                    
                  myplot23 <-   ggplot(subset(base.pop,
                                  treat %in% c("a1","n1")),
                           aes(grupo, fracmax, colour = grupo)) +
                      geom_violin() + geom_point() +
                      stat_summary(fun=mean, geom="point", shape=10, 
                                   size=10, color="blue") +
                      labs(title = "ABS1, NB1 - Fraction of Maximum Payment",
                           x = "ABS1, NB1 per Group",
                           y = "Fraction of Maximum Payment") +
                      scale_y_continuous(limits=c(0,1), labels = scales::percent)
                    
                  myplot24 <-   ggplot(subset(base.pop,
                                  treat %in% c("a2","i1","d1","n2")),
                           aes(grupo, fracmax, colour = grupo)) +
                      geom_violin() + geom_point() +
                      stat_summary(fun=mean, geom="point", shape=10, 
                                   size=10, color="blue") +
                      labs(title = "ABS2, DEC1, INC1, NB2 - Fraction of Maximum Payment",
                           x = "ABS2, DEC1, INC1, NB2 per Group",
                           y = "Fraction of Maximum Payment") +
                      scale_y_continuous(limits=c(0,1), labels = scales::percent)
                    
                  myplot25 <-   ggplot(subset(base.pop,
                                  treat %in% c("a3","i2","d2","n3")), 
                           aes(grupo, fracmax, colour = grupo)) +
                      geom_violin() + geom_point() +
                      stat_summary(fun=mean, geom="point", shape=10, 
                                   size=10, color="blue") +
                      labs(title = "ABS3, DEC2, INC2, NB3 - Fraction of Maximum Payment",
                           x = "ABS3, DEC2, INC2, NB3 per Group",
                           y = "Fraction of Maximum Payment") +
                      scale_y_continuous(limits=c(0,1), labels = scales::percent)
                    
                  myplot26 <-   ggplot(subset(base.pop,
                                  treat %in% c("a4","i3","d3","n4")), 
                           aes(grupo, fracmax, colour = grupo)) +
                      geom_violin() + geom_point() +
                      stat_summary(fun=mean, geom="point", shape=10, 
                                   size=10, color="blue") +
                      labs(title = "ABS4, DEC3, INC3, NB4 - Fraction of Maximum Payment",
                           x = "ABS4, DEC3, INC3, NB4 per Group",
                           y = "Fraction of Maximum Payment") +
                      scale_y_continuous(limits=c(0,1), labels = scales::percent)
                    
                    
      ##### Game Perf Grupos #####
                  myplot27 <-   ggplot(base.pop, aes(grupo, nback, colour = grupo)) +
                      geom_violin() + geom_point() +
                      stat_summary(fun=mean, geom="point", shape=10, 
                                   size=10, color="blue") +
                      labs(title = "NBack Performance per Group",
                           x = "Group",
                           y = "Correct %") +
                      scale_y_continuous(limits=c(0,1), labels = scales::percent) 
                  
                  myplot28 <-   ggplot(base.pop, aes(treat, nback, colour = treat)) +
                    geom_violin() + geom_point() +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "NBack Performance per Group",
                         x = "Treatment",
                         y = "Correct %") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent) +
                    scale_color_manual(
                      values=c("#F8766D","#F8766D","#F8766D","#F8766D",
                               "#7CAE00","#7CAE00","#7CAE00",
                               "#00BFC4","#00BFC4","#00BFC4",
                               "#C77CFF","#C77CFF","#C77CFF","#C77CFF"))
                  
                  
                  myplot29 <-   ggplot(base.pop, aes(grupo, stroop, colour = grupo)) +
                      geom_violin() + geom_point() +
                      stat_summary(fun=mean, geom="point", shape=10, 
                                   size=10, color="blue") +
                      labs(title = "Stroop Performance per Group",
                           x = "Group",
                           y = "Incorrect %") +
                      scale_y_continuous(limits=c(0,1), labels = scales::percent) 

                  myplot30 <-   ggplot(base.pop, aes(treat, stroop, colour = treat)) +
                    geom_violin() + geom_point() +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "Stroop Performance per Group",
                         x = "Treatment",
                         y = "Incorrect %") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent) +
                    scale_color_manual(
                      values=c("#F8766D","#F8766D","#F8766D","#F8766D",
                               "#7CAE00","#7CAE00","#7CAE00",
                               "#00BFC4","#00BFC4","#00BFC4",
                               "#C77CFF","#C77CFF","#C77CFF","#C77CFF")) 
                  
                  myplot31 <-   ggplot(base.pop, aes(grupo, iowa, colour = grupo)) +
                      geom_violin() + geom_point() +
                      stat_summary(fun=mean, geom="point", shape=10, 
                                   size=10, color="blue") +
                      labs(title = "Iowa Performance per Group",
                           x = "Group",
                           y = "ROI %") +
                      scale_y_continuous(limits=c(-2.5,1), labels = scales::percent) 

                  myplot32 <-   ggplot(base.pop, aes(treat, iowa, colour = treat)) +
                    geom_violin() + geom_point() +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "Iowa Performance per Group",
                         x = "Treatment",
                         y = "ROI %") +
                    scale_y_continuous(limits=c(-2.5,.3), labels = scales::percent) +
                    scale_color_manual(
                      values=c("#F8766D","#F8766D","#F8766D","#F8766D",
                               "#7CAE00","#7CAE00","#7CAE00",
                               "#00BFC4","#00BFC4","#00BFC4",
                               "#C77CFF","#C77CFF","#C77CFF","#C77CFF")) 
                  
      ##### Game Perf Treat Orden - NBack #####
                  
                  myplot33 <-   ggplot(subset(base.pop,
                                              treat %in% c("a1","a2","a3","a4")), 
                                       mapping = aes(x = treat, y = nback, fill=grupo)) +
                    geom_violin(aes(color=grupo)) + facet_wrap(facets = vars(grupo)) +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "NBack Performance: Absolute Treatment",
                         x = "Absolute Treatment",
                         y = "Correct %") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent)

                  myplot34 <-   ggplot(subset(base.pop,
                                              treat %in% c("a1","a2","a3","a4")), 
                                       mapping = aes(x = treat, y = nback, colour=treat)) +
                    geom_violin() + 
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "NBack Performance: Absolute Treatment",
                         x = "Absolute Treatment",
                         y = "Correct %") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent)
                  
                  myplot35 <-   ggplot(subset(base.pop,
                                              treat %in% c("i1","i2","i3")), 
                                       mapping = aes(x = treat, y = nback, fill=grupo)) +
                    geom_violin(aes(color=grupo)) + facet_wrap(facets = vars(grupo)) +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "NBack Performance: Increasing Treatment",
                         x = "Increasing Treatment",
                         y = "Correct %") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent)

                  myplot36 <-   ggplot(subset(base.pop,
                                              treat %in% c("i1","i2","i3")), 
                                       mapping = aes(x = treat, y = nback, colour=treat)) +
                    geom_violin() + 
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "NBack Performance: Increasing Treatment",
                         x = "Increasing Treatment",
                         y = "Correct %") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent)
                  
                  myplot37 <-   ggplot(subset(base.pop,
                                              treat %in% c("d1","d2","d3")), 
                                       mapping = aes(x = treat, y = nback, fill=grupo)) +
                    geom_violin(aes(color=grupo)) + facet_wrap(facets = vars(grupo)) +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "NBack Performance: Decreasing Treatment",
                         x = "Decreasing Treatment",
                         y = "Correct %") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent)

                  myplot38 <-   ggplot(subset(base.pop,
                                              treat %in% c("d1","d2","d3")), 
                                       mapping = aes(x = treat, y = nback, colour=treat)) +
                    geom_violin() + 
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "NBack Performance: Decreasing Treatment",
                         x = "Decreasing Treatment",
                         y = "Correct %") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent)
                                    
                  myplot39 <-   ggplot(subset(base.pop,
                                              treat %in% c("n1","n2","n3","n4")), 
                                       mapping = aes(x = treat, y = nback, fill=grupo)) +
                    geom_violin(aes(color=grupo)) + facet_wrap(facets = vars(grupo)) +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "NBack Performance: No Bonus Treatment",
                         x = "No Bonus Treatment",
                         y = "Correct %") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent)
                  
                  myplot40 <-   ggplot(subset(base.pop,
                                              treat %in% c("n1","n2","n3","n4")), 
                                       mapping = aes(x = treat, y = nback, colour=treat)) +
                    geom_violin() +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "NBack Performance: No Bonus Treatment",
                         x = "No Bonus Treatment",
                         y = "Correct %") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent)
                  
      ##### Game Perf Treat Comparables - NBack #####
                  
                  myplot41 <-   ggplot(subset(base.pop,
                                             treat %in% c("a1","n1")),
                                      mapping = aes(x = treat, y = nback, fill=grupo)) +
                    geom_violin(aes(color=grupo)) + facet_wrap(facets = vars(grupo)) +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "NBack Performance: ABS1, NB1 per Group",
                         x = "ABS1, NB1 per Group",
                         y = "Correct %") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent)
                  
                  myplot42 <-   ggplot(subset(base.pop,
                                              treat %in% c("a1","n1")),
                                       mapping = aes(x = treat, y = nback, colour=treat)) +
                    geom_violin(aes(color=treat)) + 
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "NBack Performance: ABS1, NB1 per Group",
                         x = "ABS1, NB1 per Group",
                         y = "Correct %") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent)

                  myplot43 <-   ggplot(subset(base.pop,
                                              treat %in% c("a2","i1","d1","n2")),
                                       mapping = aes(x = treat, y = nback, fill=grupo)) +
                    geom_violin(aes(color=grupo)) + facet_wrap(facets = vars(grupo)) +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "NBack Performance: ABS2, DEC1, INC1, NB2 per Group",
                         x = "ABS2, DEC1, INC1, NB2 per Group",
                         y = "Correct %") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent)
                                    
                  myplot44 <-   ggplot(subset(base.pop,
                                             treat %in% c("a2","i1","d1","n2")),
                                       mapping = aes(x = treat, y = nback, colour=treat)) +
                    geom_violin(aes(color=treat)) + 
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "NBack Performance: ABS2, DEC1, INC1, NB2 per Group",
                         x = "ABS2, DEC1, INC1, NB2 per Group",
                         y = "Correct %") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent)
                  
                  myplot45 <-   ggplot(subset(base.pop,
                                             treat %in% c("a3","i2","d2","n3")), 
                                      mapping = aes(x = treat, y = nback, fill=grupo)) +
                    geom_violin(aes(color=grupo)) + facet_wrap(facets = vars(grupo)) +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "NBack Performance: ABS3, DEC2, INC2, NB3 per Group",
                         x = "ABS3, DEC2, INC2, NB3 per Group",
                         y = "Correct %") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent)
                  
                  myplot46 <-   ggplot(subset(base.pop,
                                              treat %in% c("a3","i2","d2","n3")), 
                                       mapping = aes(x = treat, y = nback, colour=treat)) +
                    geom_violin(aes(color=treat)) + 
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "NBack Performance: ABS3, DEC2, INC2, NB3 per Group",
                         x = "ABS3, DEC2, INC2, NB3 per Group",
                         y = "Correct %") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent)
                  
                  myplot47 <-   ggplot(subset(base.pop,
                                             treat %in% c("a4","i3","d3","n4")), 
                                      mapping = aes(x = treat, y = nback, fill=grupo)) +
                    geom_violin(aes(color=grupo)) + facet_wrap(facets = vars(grupo)) +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "NBack Performance: ABS4, DEC3, INC3, NB4 per Group",
                         x = "ABS4, DEC3, INC3, NB4 per Group",
                         y = "Correct %") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent)
                  
                  myplot48 <-   ggplot(subset(base.pop,
                                              treat %in% c("a4","i3","d3","n4")), 
                                       mapping = aes(x = treat, y = nback, colour=treat)) +
                    geom_violin(aes(color=treat)) + 
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "NBack Performance: ABS4, DEC3, INC3, NB4 per Group",
                         x = "ABS4, DEC3, INC3, NB4 per Group",
                         y = "Correct %") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent)
                  
      ##### Game Perf Group Orden - NBack ERRORName104NB4 #####
                  myplot49 <-   ggplot(subset(base.pop,
                                              treat %in% c("a1","a2","a3","a4")),
                                       aes(grupo, nback, colour = grupo)) +
                    geom_violin() + geom_point() +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "NBack Performance: Absolute Treatment",
                         x = "Absolute Treatment per Group",
                         y = "Correct %") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent)
                  
                  myplot50 <-   ggplot(subset(base.pop,
                                              treat %in% c("i1","i2","i3")), 
                                       aes(grupo, nback, colour = grupo)) +
                    geom_violin() + geom_point() +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = " NBack Performance: Increasing Treatment",
                         x = "Increasing Treatment per Group",
                         y = "Correct %") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent)
                  
                  myplot51 <-   ggplot(subset(base.pop,
                                              treat %in% c("d1","d2","d3")), 
                                       aes(grupo, nback, colour = grupo)) +
                    geom_violin() + geom_point() +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = " NBack Performance: Decreasing Treatment",
                         x = "Decreasing Treatment per Group",
                         y = "Correct %") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent)
                  
                  myplot104 <-   ggplot(subset(base.pop,
                                              treat %in% c("n1","n2","n3","n4")), 
                                       aes(grupo, nback, colour = grupo)) +
                    geom_violin() + geom_point() +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = " NBack Performance: No Bonus Treatment",
                         x = "NO Bonus Treatment per Group",
                         y = "Correct %") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent)
                  
                  
      ##### Game Perf Group Comparables - NBack #####
                  
                  myplot52 <-   ggplot(subset(base.pop,
                                              treat %in% c("a1","n1")),
                                       aes(grupo, nback, colour = grupo)) +
                    geom_violin() + geom_point() +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "NBack Performance: ABS1, NB1 per Group",
                         x = "ABS1, NB1 per Group",
                         y = "Correct %") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent)
                  
                  myplot53 <-   ggplot(subset(base.pop,
                                              treat %in% c("a2","i1","d1","n2")),
                                       aes(grupo, nback, colour = grupo)) +
                    geom_violin() + geom_point() +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "NBack Performance: ABS2, DEC1, INC1, NB2 per Group",
                         x = "ABS2, DEC1, INC1, NB2 per Group",
                         y = "Correct %") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent)
                  
                  myplot54 <-   ggplot(subset(base.pop,
                                              treat %in% c("a3","i2","d2","n3")), 
                                       aes(grupo, nback, colour = grupo)) +
                    geom_violin() + geom_point() +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "NBack Performance: ABS3, DEC2, INC2, NB3 per Group",
                         x = "ABS3, DEC2, INC2, NB3 per Group",
                         y = "Correct %") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent)
                  
                  myplot55 <-   ggplot(subset(base.pop,
                                              treat %in% c("a4","i3","d3","n4")), 
                                       aes(grupo, nback, colour = grupo)) +
                    geom_violin() + geom_point() +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "NBack Performance: ABS4, DEC3, INC3, NB4 per Group",
                         x = "ABS4, DEC3, INC3, NB4 per Group",
                         y = "Correct %") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent)
                  
                  
                  
      ##### Game Perf Treat Orden - Stroop #####
                  
                  myplot56 <-   ggplot(subset(base.pop,
                                              treat %in% c("a1","a2","a3","a4")), 
                                       mapping = aes(x = treat, y = stroop, fill=grupo)) +
                    geom_violin(aes(color=grupo)) + facet_wrap(facets = vars(grupo)) +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "Stroop Performance: Absolute Treatment",
                         x = "Absolute Treatment",
                         y = "Incorrect %") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent)
                  
                  myplot57 <-   ggplot(subset(base.pop,
                                              treat %in% c("a1","a2","a3","a4")), 
                                       mapping = aes(x = treat, y = stroop, colour=treat)) +
                    geom_violin() + 
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "Stroop Performance: Absolute Treatment",
                         x = "Absolute Treatment",
                         y = "Incorrect %") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent)
                  
                  myplot58 <-   ggplot(subset(base.pop,
                                              treat %in% c("i1","i2","i3")), 
                                       mapping = aes(x = treat, y = stroop, fill=grupo)) +
                    geom_violin(aes(color=grupo)) + facet_wrap(facets = vars(grupo)) +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "Stroop Performance: Increasing Treatment",
                         x = "Increasing Treatment",
                         y = "Incorrect %") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent)
                  
                  myplot59 <-   ggplot(subset(base.pop,
                                              treat %in% c("i1","i2","i3")), 
                                       mapping = aes(x = treat, y = stroop, colour=treat)) +
                    geom_violin() + 
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "Stroop Performance: Increasing Treatment",
                         x = "Increasing Treatment",
                         y = "Incorrect %") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent)
                  
                  myplot60 <-   ggplot(subset(base.pop,
                                              treat %in% c("d1","d2","d3")), 
                                       mapping = aes(x = treat, y = stroop, fill=grupo)) +
                    geom_violin(aes(color=grupo)) + facet_wrap(facets = vars(grupo)) +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "Stroop Performance: Decreasing Treatment",
                         x = "Decreasing Treatment",
                         y = "Incorrect %") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent)
                  
                  myplot61 <-   ggplot(subset(base.pop,
                                              treat %in% c("d1","d2","d3")), 
                                       mapping = aes(x = treat, y = stroop, colour=treat)) +
                    geom_violin() + 
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "Stroop Performance: Decreasing Treatment",
                         x = "Decreasing Treatment",
                         y = "Incorrect %") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent)
                  
                  myplot62 <-   ggplot(subset(base.pop,
                                              treat %in% c("n1","n2","n3","n4")), 
                                       mapping = aes(x = treat, y = stroop, fill=grupo)) +
                    geom_violin(aes(color=grupo)) + facet_wrap(facets = vars(grupo)) +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "Stroop Performance: No Bonus Treatment",
                         x = "No Bonus Treatment",
                         y = "Incorrect %") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent)
                  
                  myplot63 <-   ggplot(subset(base.pop,
                                              treat %in% c("n1","n2","n3","n4")), 
                                       mapping = aes(x = treat, y = stroop, colour=treat)) +
                    geom_violin() +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "Stroop Performance: No Bonus Treatment",
                         x = "No Bonus Treatment",
                         y = "Incorrect %") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent)
                  
      ##### Game Perf Treat Comparables - Stroop #####
                  
                  myplot64 <-   ggplot(subset(base.pop,
                                              treat %in% c("a1","n1")),
                                       mapping = aes(x = treat, y = stroop, fill=grupo)) +
                    geom_violin(aes(color=grupo)) + facet_wrap(facets = vars(grupo)) +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "Stroop Performance: ABS1, NB1 per Group",
                         x = "ABS1, NB1 per Group",
                         y = "Incorrect %") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent)
                  
                  myplot65 <-   ggplot(subset(base.pop,
                                              treat %in% c("a1","n1")),
                                       mapping = aes(x = treat, y = stroop, colour=treat)) +
                    geom_violin(aes(color=treat)) + 
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "Stroop Performance: ABS1, NB1 per Group",
                         x = "ABS1, NB1 per Group",
                         y = "Incorrect %") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent)
                  
                  myplot66 <-   ggplot(subset(base.pop,
                                              treat %in% c("a2","i1","d1","n2")),
                                       mapping = aes(x = treat, y = stroop, fill=grupo)) +
                    geom_violin(aes(color=grupo)) + facet_wrap(facets = vars(grupo)) +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "Stroop Performance: ABS2, DEC1, INC1, NB2 per Group",
                         x = "ABS2, DEC1, INC1, NB2 per Group",
                         y = "Incorrect %") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent)
                  
                  myplot67 <-   ggplot(subset(base.pop,
                                              treat %in% c("a2","i1","d1","n2")),
                                       mapping = aes(x = treat, y = stroop, colour=treat)) +
                    geom_violin(aes(color=treat)) + 
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "Stroop Performance: ABS2, DEC1, INC1, NB2 per Group",
                         x = "ABS2, DEC1, INC1, NB2 per Group",
                         y = "Incorrect %") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent)
                  
                  myplot68 <-   ggplot(subset(base.pop,
                                              treat %in% c("a3","i2","d2","n3")), 
                                       mapping = aes(x = treat, y = stroop, fill=grupo)) +
                    geom_violin(aes(color=grupo)) + facet_wrap(facets = vars(grupo)) +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "Stroop Performance: ABS3, DEC2, INC2, NB3 per Group",
                         x = "ABS3, DEC2, INC2, NB3 per Group",
                         y = "Incorrect %") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent)
                  
                  myplot69 <-   ggplot(subset(base.pop,
                                              treat %in% c("a3","i2","d2","n3")), 
                                       mapping = aes(x = treat, y = stroop, colour=treat)) +
                    geom_violin(aes(color=treat)) + 
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "Stroop Performance: ABS3, DEC2, INC2, NB3 per Group",
                         x = "ABS3, DEC2, INC2, NB3 per Group",
                         y = "Incorrect %") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent)
                  
                  myplot70 <-   ggplot(subset(base.pop,
                                              treat %in% c("a4","i3","d3","n4")), 
                                       mapping = aes(x = treat, y = stroop, fill=grupo)) +
                    geom_violin(aes(color=grupo)) + facet_wrap(facets = vars(grupo)) +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "Stroop Performance: ABS4, DEC3, INC3, NB4 per Group",
                         x = "ABS4, DEC3, INC3, NB4 per Group",
                         y = "Incorrect %") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent)
                  
                  myplot71 <-   ggplot(subset(base.pop,
                                              treat %in% c("a4","i3","d3","n4")), 
                                       mapping = aes(x = treat, y = stroop, colour=treat)) +
                    geom_violin(aes(color=treat)) + 
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "Stroop Performance: ABS4, DEC3, INC3, NB4 per Group",
                         x = "ABS4, DEC3, INC3, NB4 per Group",
                         y = "Incorrect %") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent)
                  
                  
      ##### Game Perf Group Orden - Stroop #####
                  myplot72 <-   ggplot(subset(base.pop,
                                              treat %in% c("a1","a2","a3","a4")),
                                       aes(grupo, stroop, colour = grupo)) +
                    geom_violin() + geom_point() +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "Stroop Performance: Absolute Treatment",
                         x = "Absolute Treatment per Group",
                         y = "Incorrect %") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent)
                  
                  myplot73 <-   ggplot(subset(base.pop,
                                              treat %in% c("i1","i2","i3")), 
                                       aes(grupo, stroop, colour = grupo)) +
                    geom_violin() + geom_point() +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = " Stroop Performance: Increasing Treatment",
                         x = "Increasing Treatment per Group",
                         y = "Incorrect %") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent)
                  
                  myplot74 <-   ggplot(subset(base.pop,
                                              treat %in% c("d1","d2","d3")), 
                                       aes(grupo, stroop, colour = grupo)) +
                    geom_violin() + geom_point() +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = " Stroop Performance: Decreasing Treatment",
                         x = "Decreasing Treatment per Group",
                         y = "Incorrect %") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent)
                  
                  myplot75 <-   ggplot(subset(base.pop,
                                              treat %in% c("n1","n2","n3","n4")), 
                                       aes(grupo, stroop, colour = grupo)) +
                    geom_violin() + geom_point() +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = " Stroop Performance: No Bonus Treatment",
                         x = "NO Bonus Treatment per Group",
                         y = "Incorrect %") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent)
                  
      ##### Game Perf Group Comparables - Stroop #####
                  
                  myplot76 <-   ggplot(subset(base.pop,
                                              treat %in% c("a1","n1")),
                                       aes(grupo, stroop, colour = grupo)) +
                    geom_violin() + geom_point() +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "Stroop Performance: ABS1, NB1 per Group",
                         x = "ABS1, NB1 per Group",
                         y = "Incorrect %") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent)
                  
                  myplot77 <-   ggplot(subset(base.pop,
                                              treat %in% c("a2","i1","d1","n2")),
                                       aes(grupo, stroop, colour = grupo)) +
                    geom_violin() + geom_point() +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "Stroop Performance: ABS2, DEC1, INC1, NB2 per Group",
                         x = "ABS2, DEC1, INC1, NB2 per Group",
                         y = "Incorrect %") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent)
                  
                  myplot78 <-   ggplot(subset(base.pop,
                                              treat %in% c("a3","i2","d2","n3")), 
                                       aes(grupo, stroop, colour = grupo)) +
                    geom_violin() + geom_point() +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "Stroop Performance: ABS3, DEC2, INC2, NB3 per Group",
                         x = "ABS3, DEC2, INC2, NB3 per Group",
                         y = "Incorrect %") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent)
                  
                  myplot79 <-   ggplot(subset(base.pop,
                                              treat %in% c("a4","i3","d3","n4")), 
                                       aes(grupo, stroop, colour = grupo)) +
                    geom_violin() + geom_point() +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "Stroop Performance: ABS4, DEC3, INC3, NB4 per Group",
                         x = "ABS4, DEC3, INC3, NB4 per Group",
                         y = "Incorrect %") +
                    scale_y_continuous(limits=c(0,1), labels = scales::percent)
                  
                  
      ##### Game Perf Treat Orden - Iowa #####
                  
                  myplot80 <-   ggplot(subset(base.pop,
                                              treat %in% c("a1","a2","a3","a4")), 
                                       mapping = aes(x = treat, y = iowa, fill=grupo)) +
                    geom_violin(aes(color=grupo)) + facet_wrap(facets = vars(grupo)) +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "Iowa Performance: Absolute Treatment",
                         x = "Absolute Treatment",
                         y = "ROI %") +
                    scale_y_continuous(limits=c(-2.5,0.3), labels = scales::percent)
                  
                  myplot81 <-   ggplot(subset(base.pop,
                                              treat %in% c("a1","a2","a3","a4")), 
                                       mapping = aes(x = treat, y = iowa, colour=treat)) +
                    geom_violin() + 
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "Iowa Performance: Absolute Treatment",
                         x = "Absolute Treatment",
                         y = "ROI %") +
                    scale_y_continuous(limits=c(-2.5,0.3), labels = scales::percent)
                  
                  myplot82 <-   ggplot(subset(base.pop,
                                              treat %in% c("i1","i2","i3")), 
                                       mapping = aes(x = treat, y = iowa, fill=grupo)) +
                    geom_violin(aes(color=grupo)) + facet_wrap(facets = vars(grupo)) +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "Iowa Performance: Increasing Treatment",
                         x = "Increasing Treatment",
                         y = "ROI %") +
                    scale_y_continuous(limits=c(-2.5,0.3), labels = scales::percent)
                  
                  myplot83 <-   ggplot(subset(base.pop,
                                              treat %in% c("i1","i2","i3")), 
                                       mapping = aes(x = treat, y = iowa, colour=treat)) +
                    geom_violin() + 
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "Iowa Performance: Increasing Treatment",
                         x = "Increasing Treatment",
                         y = "ROI %") +
                    scale_y_continuous(limits=c(-2.5,0.3), labels = scales::percent)
                  
                  myplot84 <-   ggplot(subset(base.pop,
                                              treat %in% c("d1","d2","d3")), 
                                       mapping = aes(x = treat, y = iowa, fill=grupo)) +
                    geom_violin(aes(color=grupo)) + facet_wrap(facets = vars(grupo)) +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "Iowa Performance: Decreasing Treatment",
                         x = "Decreasing Treatment",
                         y = "ROI %") +
                    scale_y_continuous(limits=c(-2.5,0.3), labels = scales::percent)
                  
                  myplot85 <-   ggplot(subset(base.pop,
                                              treat %in% c("d1","d2","d3")), 
                                       mapping = aes(x = treat, y = iowa, colour=treat)) +
                    geom_violin() + 
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "Iowa Performance: Decreasing Treatment",
                         x = "Decreasing Treatment",
                         y = "ROI %") +
                    scale_y_continuous(limits=c(-2.5,0.3), labels = scales::percent)
                  
                  myplot86 <-   ggplot(subset(base.pop,
                                              treat %in% c("n1","n2","n3","n4")), 
                                       mapping = aes(x = treat, y = iowa, fill=grupo)) +
                    geom_violin(aes(color=grupo)) + facet_wrap(facets = vars(grupo)) +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "Iowa Performance: No Bonus Treatment",
                         x = "No Bonus Treatment",
                         y = "ROI %") +
                    scale_y_continuous(limits=c(-2.5,0.3), labels = scales::percent)
                  
                  myplot87 <-   ggplot(subset(base.pop,
                                              treat %in% c("n1","n2","n3","n4")), 
                                       mapping = aes(x = treat, y = iowa, colour=treat)) +
                    geom_violin() +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "Iowa Performance: No Bonus Treatment",
                         x = "No Bonus Treatment",
                         y = "ROI %") +
                    scale_y_continuous(limits=c(-2.5,0.3), labels = scales::percent)
                  
      ##### Game Perf Treat Comparables - Iowa #####
                  
                  myplot88 <-   ggplot(subset(base.pop,
                                              treat %in% c("a1","n1")),
                                       mapping = aes(x = treat, y = iowa, fill=grupo)) +
                    geom_violin(aes(color=grupo)) + facet_wrap(facets = vars(grupo)) +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "Iowa Performance: ABS1, NB1 per Group",
                         x = "ABS1, NB1 per Group",
                         y = "ROI %") +
                    scale_y_continuous(limits=c(-2.5,0.3), labels = scales::percent)
                  
                  myplot89 <-   ggplot(subset(base.pop,
                                              treat %in% c("a1","n1")),
                                       mapping = aes(x = treat, y = iowa, colour=treat)) +
                    geom_violin(aes(color=treat)) + 
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "Iowa Performance: ABS1, NB1 per Group",
                         x = "ABS1, NB1 per Group",
                         y = "ROI %") +
                    scale_y_continuous(limits=c(-2.5,0.3), labels = scales::percent)
                  
                  myplot90 <-   ggplot(subset(base.pop,
                                              treat %in% c("a2","i1","d1","n2")),
                                       mapping = aes(x = treat, y = iowa, fill=grupo)) +
                    geom_violin(aes(color=grupo)) + facet_wrap(facets = vars(grupo)) +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "Iowa Performance: ABS2, DEC1, INC1, NB2 per Group",
                         x = "ABS2, DEC1, INC1, NB2 per Group",
                         y = "ROI %") +
                    scale_y_continuous(limits=c(-2.5,0.3), labels = scales::percent)
                  
                  myplot91 <-   ggplot(subset(base.pop,
                                              treat %in% c("a2","i1","d1","n2")),
                                       mapping = aes(x = treat, y = iowa, colour=treat)) +
                    geom_violin(aes(color=treat)) + 
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "Iowa Performance: ABS2, DEC1, INC1, NB2 per Group",
                         x = "ABS2, DEC1, INC1, NB2 per Group",
                         y = "ROI %") +
                    scale_y_continuous(limits=c(-2.5,0.3), labels = scales::percent)
                  
                  myplot92 <-   ggplot(subset(base.pop,
                                              treat %in% c("a3","i2","d2","n3")), 
                                       mapping = aes(x = treat, y = iowa, fill=grupo)) +
                    geom_violin(aes(color=grupo)) + facet_wrap(facets = vars(grupo)) +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "Iowa Performance: ABS3, DEC2, INC2, NB3 per Group",
                         x = "ABS3, DEC2, INC2, NB3 per Group",
                         y = "ROI %") +
                    scale_y_continuous(limits=c(-2.5,0.3), labels = scales::percent)
                  
                  myplot93 <-   ggplot(subset(base.pop,
                                              treat %in% c("a3","i2","d2","n3")), 
                                       mapping = aes(x = treat, y = iowa, colour=treat)) +
                    geom_violin(aes(color=treat)) + 
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "Iowa Performance: ABS3, DEC2, INC2, NB3 per Group",
                         x = "ABS3, DEC2, INC2, NB3 per Group",
                         y = "ROI %") +
                    scale_y_continuous(limits=c(-2.5,0.3), labels = scales::percent)
                  
                  myplot94 <-   ggplot(subset(base.pop,
                                              treat %in% c("a4","i3","d3","n4")), 
                                       mapping = aes(x = treat, y = iowa, fill=grupo)) +
                    geom_violin(aes(color=grupo)) + facet_wrap(facets = vars(grupo)) +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "Iowa Performance: ABS4, DEC3, INC3, NB4 per Group",
                         x = "ABS4, DEC3, INC3, NB4 per Group",
                         y = "ROI %") +
                    scale_y_continuous(limits=c(-2.5,0.3), labels = scales::percent)
                  
                  myplot95 <-   ggplot(subset(base.pop,
                                              treat %in% c("a4","i3","d3","n4")), 
                                       mapping = aes(x = treat, y = iowa, colour=treat)) +
                    geom_violin(aes(color=treat)) + 
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "Iowa Performance: ABS4, DEC3, INC3, NB4 per Group",
                         x = "ABS4, DEC3, INC3, NB4 per Group",
                         y = "ROI %") +
                    scale_y_continuous(limits=c(-2.5,0.3), labels = scales::percent)
                  
                  
      ##### Game Perf Group Orden - Iowa #####
                  myplot96 <-   ggplot(subset(base.pop,
                                              treat %in% c("a1","a2","a3","a4")),
                                       aes(grupo, iowa, colour = grupo)) +
                    geom_violin() + geom_point() +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "Iowa Performance: Absolute Treatment",
                         x = "Absolute Treatment per Group",
                         y = "ROI %") +
                    scale_y_continuous(limits=c(-2.5,1), labels = scales::percent)
                  
                  myplot97 <-   ggplot(subset(base.pop,
                                              treat %in% c("i1","i2","i3")), 
                                       aes(grupo, iowa, colour = grupo)) +
                    geom_violin() + geom_point() +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = " Iowa Performance: Increasing Treatment",
                         x = "Increasing Treatment per Group",
                         y = "ROI %") +
                    scale_y_continuous(limits=c(-2.5,1), labels = scales::percent)
                  
                  myplot98 <-   ggplot(subset(base.pop,
                                              treat %in% c("d1","d2","d3")), 
                                       aes(grupo, iowa, colour = grupo)) +
                    geom_violin() + geom_point() +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = " Iowa Performance: Decreasing Treatment",
                         x = "Decreasing Treatment per Group",
                         y = "ROI %") +
                    scale_y_continuous(limits=c(-2.5,1), labels = scales::percent)
                  
                  myplot99 <-   ggplot(subset(base.pop,
                                              treat %in% c("n1","n2","n3","n4")), 
                                       aes(grupo, iowa, colour = grupo)) +
                    geom_violin() + geom_point() +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = " Iowa Performance: No Bonus Treatment",
                         x = "NO Bonus Treatment per Group",
                         y = "ROI %") +
                    scale_y_continuous(limits=c(-2.5,1), labels = scales::percent)
                  
      ##### Game Perf Group Comparables - Iowa #####
                  
                  myplot100 <-   ggplot(subset(base.pop,
                                              treat %in% c("a1","n1")),
                                       aes(grupo, iowa, colour = grupo)) +
                    geom_violin() + geom_point() +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "Iowa Performance: ABS1, NB1 per Group",
                         x = "ABS1, NB1 per Group",
                         y = "ROI %") +
                    scale_y_continuous(limits=c(-2.5,1), labels = scales::percent)
                  
                  myplot101 <-   ggplot(subset(base.pop,
                                              treat %in% c("a2","i1","d1","n2")),
                                       aes(grupo, iowa, colour = grupo)) +
                    geom_violin() + geom_point() +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "Iowa Performance: ABS2, DEC1, INC1, NB2 per Group",
                         x = "ABS2, DEC1, INC1, NB2 per Group",
                         y = "ROI %") +
                    scale_y_continuous(limits=c(-2.5,1), labels = scales::percent)
                  
                  myplot102 <-   ggplot(subset(base.pop,
                                              treat %in% c("a3","i2","d2","n3")), 
                                       aes(grupo, iowa, colour = grupo)) +
                    geom_violin() + geom_point() +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "Iowa Performance: ABS3, DEC2, INC2, NB3 per Group",
                         x = "ABS3, DEC2, INC2, NB3 per Group",
                         y = "ROI %") +
                    scale_y_continuous(limits=c(-2.5,1), labels = scales::percent)
                  
                  myplot103 <-   ggplot(subset(base.pop,
                                              treat %in% c("a4","i3","d3","n4")), 
                                       aes(grupo, iowa, colour = grupo)) +
                    geom_violin() + geom_point() +
                    stat_summary(fun=mean, geom="point", shape=10, 
                                 size=10, color="blue") +
                    labs(title = "Iowa Performance: ABS4, DEC3, INC3, NB4 per Group",
                         x = "ABS4, DEC3, INC3, NB4 per Group",
                         y = "ROI %") +
                    scale_y_continuous(limits=c(-2.5,1), labels = scales::percent)
                  
                  
                  
                  
      ##### Save Graphs #####
                  
                  #SET WD and save all Graphs
                  setwd("C:/Users/danbo/Desktop/MturkR/BaseF/GraphsBF")
                  
                  for(i in 1:104){
                    
                    ggsave(paste0("myplot",i,".png"), plot = get(paste0("myplot",i)))  
                    
                  }
                  
                  #COn el GET busca el "objeto" que es una gráfica y la guarda como ""i.png
                  #Dead Code
                  #ggsave("myplot2.png", plot = myplot2)
                  #ggsave("myplot36.png", plot = myplot3)
                  
      ##### Renaming Variables - base.pop.vars #####
                  #Para tener PRE and POST answers con Nombres
                  library(reshape)
                  
                  base.pop.vars <- rename(base.pop.cutoff,
                                          c(age.1 = "age", education.1 = "education",
                                            income.1 = "income", religion.1 = "religion",
                                            mturkexp.1 = "mturkexp", mturkappr.1 = "mturkappr",
                                            mturkhits.1 = "mturkhits", gender.1 = "gender",
                                            location.1 = "location", ethnicity.1 = "ethnicity", 
                                            political.1 = "political", secon2.1 = "secondbonus",
                                            subseq2.1 = "subseq2bonus", enjoy.1 = "enjoy",
                                            satisfied.1 = "satisfied", difficulty.1 = "difficulty",
                                            timing.1 = "timing", compajust.1 = "compajust",
                                            compensation.1 = "compensation"))
                  
                  #PRETASK
                  base.pop.vars$age[base.pop.vars$age=="1"] <- "18 to 24"
                  base.pop.vars$age[base.pop.vars$age=="2"] <- "18 to 24"
                  base.pop.vars$age[base.pop.vars$age=="3"] <- "25 to 34"
                  base.pop.vars$age[base.pop.vars$age=="4"] <- "35 to 44"
                  base.pop.vars$age[base.pop.vars$age=="5"] <- "45 to 54"
                  base.pop.vars$age[base.pop.vars$age=="6"] <- "55 to 64"
                  base.pop.vars$age[base.pop.vars$age=="7"] <- "65 or older"
                  
                  base.pop.vars$education[base.pop.vars$education=="1"] <- "Less than a high school diploma"
                  base.pop.vars$education[base.pop.vars$education=="2"] <- "High school or equivalent"
                  base.pop.vars$education[base.pop.vars$education=="3"] <- "Bachelor's Degree (e.g. BA, BS)"
                  base.pop.vars$education[base.pop.vars$education=="4"] <- "Master's Degree or higher (e.g. MA, MS, PHD)"
                  
                  base.pop.vars$income[base.pop.vars$income=="1"] <- "Below $5K"
                  base.pop.vars$income[base.pop.vars$income=="2"] <- "$5K to $10K"
                  base.pop.vars$income[base.pop.vars$income=="3"] <- "$10K to $20K"
                  base.pop.vars$income[base.pop.vars$income=="4"] <- "$20K to $40K"
                  base.pop.vars$income[base.pop.vars$income=="5"] <- "$40K to $60K"
                  base.pop.vars$income[base.pop.vars$income=="6"] <- "$60K to $80K"
                  base.pop.vars$income[base.pop.vars$income=="7"] <- "Over $80K"
                  
                  base.pop.vars$religion[base.pop.vars$religion=="1"] <- "Yes, I practice (e.g. pray) every day"
                  base.pop.vars$religion[base.pop.vars$religion=="2"] <- "Yes, I practice (e.g. pray) at least once a week"
                  base.pop.vars$religion[base.pop.vars$religion=="3"] <- "No, I do not practice any religion"
                  
                  base.pop.vars$mturkexp[base.pop.vars$mturkexp=="1"] <- "0 to 6 months"
                  base.pop.vars$mturkexp[base.pop.vars$mturkexp=="2"] <- "7 to 12 months"  
                  base.pop.vars$mturkexp[base.pop.vars$mturkexp=="3"] <- "More than 12 months"  
                  
                  base.pop.vars$mturkappr[base.pop.vars$mturkappr=="1"] <- "Below 95%"
                  base.pop.vars$mturkappr[base.pop.vars$mturkappr=="2"] <- "95% to 98%"
                  base.pop.vars$mturkappr[base.pop.vars$mturkappr=="3"] <- "Above 98%"
                  
                  base.pop.vars$mturkhits[base.pop.vars$mturkhits=="1"] <- "0 to 999"
                  base.pop.vars$mturkhits[base.pop.vars$mturkhits=="2"] <- "1,000 to 4,999"
                  base.pop.vars$mturkhits[base.pop.vars$mturkhits=="3"] <- "Above 5,000"
                  
                  base.pop.vars$gender[base.pop.vars$gender=="1"] <- "Male"
                  base.pop.vars$gender[base.pop.vars$gender=="2"] <- "Female"
                  base.pop.vars$gender[base.pop.vars$gender=="3"] <- "Male"
                  
                  base.pop.vars$location[base.pop.vars$location=="1"] <- "Brazil"
                  base.pop.vars$location[base.pop.vars$location=="2"] <- "Europe"
                  base.pop.vars$location[base.pop.vars$location=="3"] <- "India"
                  base.pop.vars$location[base.pop.vars$location=="4"] <- "USA"
                  base.pop.vars$location[base.pop.vars$location=="5"] <- "Other"
                  #Others
                  base.pop.vars$location[base.pop.vars$OtherLocation.1=="Colombia"] <- "Brazil"
                  base.pop.vars$location[base.pop.vars$OtherLocation.1=="Mexico"] <- "Brazil"
                  base.pop.vars$location[base.pop.vars$OtherLocation.1=="Venezuela"] <- "Brazil"
                  base.pop.vars$location[base.pop.vars$OtherLocation.1=="austria"] <- "Europe"
                  base.pop.vars$location[base.pop.vars$OtherLocation.1=="britain"] <- "Europe"
                  base.pop.vars$location[base.pop.vars$OtherLocation.1=="England"] <- "Europe"
                  base.pop.vars$location[base.pop.vars$OtherLocation.1=="German"] <- "Europe"
                  base.pop.vars$location[base.pop.vars$OtherLocation.1=="Germany"] <- "Europe"
                  base.pop.vars$location[base.pop.vars$OtherLocation.1=="london"] <- "Europe"
                  base.pop.vars$location[base.pop.vars$OtherLocation.1=="Ireland"] <- "Europe"
                  base.pop.vars$location[base.pop.vars$OtherLocation.1=="IT"] <- "Europe"
                  base.pop.vars$location[base.pop.vars$OtherLocation.1=="italy"] <- "Europe"
                  base.pop.vars$location[base.pop.vars$OtherLocation.1=="Serbia"] <- "Europe"
                  base.pop.vars$location[base.pop.vars$OtherLocation.1=="Turkey"] <- "Europe"
                  base.pop.vars$location[base.pop.vars$OtherLocation.1=="UK"] <- "Europe"
                  base.pop.vars$location[base.pop.vars$OtherLocation.1=="United Kingdom"] <- "Europe"
                  base.pop.vars$location[base.pop.vars$OtherLocation.1=="australia"] <- "Europe"
                  base.pop.vars$location[base.pop.vars$OtherLocation.1=="Bangladesh"] <- "India"
                  base.pop.vars$location[base.pop.vars$OtherLocation.1=="Khanewal, Pakistan"] <- "India"
                  base.pop.vars$location[base.pop.vars$OtherLocation.1=="nepal"] <- "India"
                  base.pop.vars$location[base.pop.vars$OtherLocation.1=="India"] <- "India"
                  base.pop.vars$location[base.pop.vars$OtherLocation.1=="Can"] <- "USA"
                  base.pop.vars$location[base.pop.vars$OtherLocation.1=="Canada"] <- "USA"
                  base.pop.vars$location[base.pop.vars$OtherLocation.1=="New Jersey"] <- "USA"
                  base.pop.vars$location[base.pop.vars$OtherLocation.1=="China"] <- "Other"
                  base.pop.vars$location[base.pop.vars$OtherLocation.1=="Lesotho"] <- "Other"
                  base.pop.vars$location[base.pop.vars$OtherLocation.1=="Macedonia"] <- "Other"
                  base.pop.vars$location[base.pop.vars$OtherLocation.1=="Philippines"] <- "Other"
                  #Region
                  base.pop.vars$location[base.pop.vars$location=="Brazil"] <- "Latin-America"
                  base.pop.vars$location[base.pop.vars$location=="Europe"] <- "Europe"
                  base.pop.vars$location[base.pop.vars$location=="India"] <- "Asia"
                  base.pop.vars$location[base.pop.vars$location=="USA"] <- "North America"
                  base.pop.vars$location[base.pop.vars$location=="Other"] <- "Asia"
                  
                  #Un/Developed
                  base.pop.vars$develop <- NA
                  base.pop.vars$develop <- base.pop.vars$location
                  base.pop.vars$develop[base.pop.vars$location=="Brazil"] <- "Third World Country"
                  base.pop.vars$develop[base.pop.vars$location=="Europe"] <- "First World Country"
                  base.pop.vars$develop[base.pop.vars$location=="India"] <- "Third World Country"
                  base.pop.vars$develop[base.pop.vars$location=="USA"] <- "First World Country"
                  base.pop.vars$develop[base.pop.vars$location=="Other"] <- "Third World Country"
                  
                  
                  base.pop.vars$ethnicity[base.pop.vars$ethnicity=="1"] <- "White"
                  base.pop.vars$ethnicity[base.pop.vars$ethnicity=="2"] <- "Mixed"
                  base.pop.vars$ethnicity[base.pop.vars$ethnicity=="3"] <- "Asian or Asian British"
                  base.pop.vars$ethnicity[base.pop.vars$ethnicity=="4"] <- "Black or Black British"
                  base.pop.vars$ethnicity[base.pop.vars$ethnicity=="5"] <- "Chinese"
                  base.pop.vars$ethnicity[base.pop.vars$ethnicity=="6"] <- "Other ethnic group"
                  
                  
                  base.pop.vars$political[base.pop.vars$political=="1"] <- "Left (Democrat or equivalent)"
                  base.pop.vars$political[base.pop.vars$political=="2"] <- "Right (Republican or equivalent)"
                  base.pop.vars$political[base.pop.vars$political=="3"] <- "Center"
                  
                  base.pop.vars$secondbonus[base.pop.vars$secondbonus=="1"] <- "Lower than"
                  base.pop.vars$secondbonus[base.pop.vars$secondbonus=="2"] <- "Equal to"
                  base.pop.vars$secondbonus[base.pop.vars$secondbonus=="3"] <- "Higher than"
                  
                  base.pop.vars$enjoy[base.pop.vars$enjoy=="1"] <- "Less than expected"
                  base.pop.vars$enjoy[base.pop.vars$enjoy=="2"] <- "Appropriate"
                  base.pop.vars$enjoy[base.pop.vars$enjoy=="3"] <- "More than expected"
                  
                  base.pop.vars$satisfied[base.pop.vars$satisfied=="1"] <- "Less than expected"
                  base.pop.vars$satisfied[base.pop.vars$satisfied=="2"] <- "Appropriate"
                  base.pop.vars$satisfied[base.pop.vars$satisfied=="3"] <- "More than expected"
                  
                  base.pop.vars$difficulty[base.pop.vars$difficulty=="1"] <- "Lower than expected"
                  base.pop.vars$difficulty[base.pop.vars$difficulty=="2"] <- "Appropriate"
                  base.pop.vars$difficulty[base.pop.vars$difficulty=="3"] <- "Higher than expected"
                  
                  base.pop.vars$timing[base.pop.vars$timing=="1"] <- "Shorter than expected"
                  base.pop.vars$timing[base.pop.vars$timing=="2"] <- "Appropriate"
                  base.pop.vars$timing[base.pop.vars$timing=="3"] <- "Larger than expected"
                  
                  base.pop.vars$compajust[base.pop.vars$compajust=="1"] <- "(-$0.01)"
                  base.pop.vars$compajust[base.pop.vars$compajust=="2"] <- "(-$0.03)"
                  base.pop.vars$compajust[base.pop.vars$compajust=="3"] <- "(-$0.05)"
                  base.pop.vars$compajust[base.pop.vars$compajust=="4"] <- "(+$0.01)"
                  base.pop.vars$compajust[base.pop.vars$compajust=="5"] <- "(+$0.03)"
                  base.pop.vars$compajust[base.pop.vars$compajust=="6"] <- "(+$0.05)"
                  
                  base.pop.vars$compensation[base.pop.vars$compensation=="1"] <- "Lower than expected"
                  base.pop.vars$compensation[base.pop.vars$compensation=="2"] <- "Appropriate"
                  base.pop.vars$compensation[base.pop.vars$compensation=="3"] <- "Higher than expected"
                  
                  #Ordena
                  base.pop.vars <- select(base.pop.vars,playerID,grupo, treat, time, 
                                          fracmax, nback, stroop, iowa,
                                          t.nback, t.stroop, t.iowa,
                                          cutoff.fracmax, cutoff.nback, 
                                          cutoff.stroop, cutoff.iowa, 
                                          cash, pago, delta1, delta2, delta3, 
                                          iowa.bonos, nback.bonos, stroop.bonos, 
                                          deltabool1, deltabool2, deltabool3, 
                                          deltav1, deltav2, deltav3, 
                                          pre_task.1,
                                          age, education, income, religion,
                                          mturkexp, mturkappr, mturkhits,
                                          gender, OtherGender.1,
                                          location, OtherLocation.1,develop,
                                          ethnicity, OtherEthnicity.1,
                                          political, secondbonus, subseq2bonus,
                                          enjoy, satisfied, difficulty, timing,
                                          compajust, compensation,
                                          country, endcode,
                                          TIME_start, TIME_end
                  )
                  
                  base.pop.vars.sum <- (cbind(lapply(base.pop.vars, summary)))
                  
                  write.csv(base.pop.vars,"C:/Users/danbo/Desktop/MturkR/BaseF/base.pop.VARS.csv")
                  
                 
                  
                  
      ##### Dem Graphs #####
                  dem1 <-   ggplot(base.pop.vars, aes(x = as.factor(age),fill=gender)) +
                    geom_bar(aes(y = (..count..)/sum(..count..)),
                             alpha=0.5, position="dodge") +
                    geom_text(aes(y = ((..count..)/sum(..count..)), 
                                  label = scales::percent((..count..)/sum(..count..))), 
                              stat = "count", vjust = -0.001, size=3) +
                    scale_y_continuous(labels = percent) +
                    labs(title = "Age Distribution by Gender", 
                         y = "Percent (%)", x = "Age")
                  
                  dem2 <-   ggplot(base.pop.vars, aes(x = as.factor(age))) +
                    geom_bar(aes(y = (..count..)/sum(..count..)),
                             alpha=0.5, position="dodge",fill="#F8766D") +
                    geom_text(aes(y = ((..count..)/sum(..count..)), 
                                  label = scales::percent((..count..)/sum(..count..))), 
                              stat = "count", vjust = -0.001) +
                    scale_y_continuous(labels = percent) +
                    labs(title = "Age Distribution", 
                         y = "Percent (%)", x = "Age")
                  
                  dem3 <-   ggplot(base.pop.vars, aes(x = as.factor(time))) +
                    geom_bar(aes(y = (..count..)/sum(..count..)),
                             alpha=0.5, position="dodge",fill="#F8766D") +
                    geom_text(aes(y = ((..count..)/sum(..count..)), 
                                  label = scales::percent((..count..)/sum(..count..))), 
                              stat = "count", vjust = -0.001) +
                    scale_y_continuous(labels = percent) +
                    labs(title = "Total Time Distribution (Minutes)", 
                         y = "Percent (%)", x = "Minutes")
                  
                  dem4 <-   ggplot(base.pop.vars, aes(x = as.factor(gender),fill=gender)) +
                    geom_bar(aes(y = (..count..)/sum(..count..)),
                             alpha=0.5, position="dodge") +
                    geom_text(aes(y = ((..count..)/sum(..count..)), 
                                  label = scales::percent((..count..)/sum(..count..))), 
                              stat = "count", vjust = -0.001, size=3) +
                    scale_y_continuous(labels = percent) +
                    labs(title = "Gender by Region", 
                         y = "Percent (%)", x = "Gender") +
                    facet_grid(~location)
                  
                  dem5 <-   ggplot(base.pop.vars, aes(x = as.factor(gender),fill=gender)) +
                    geom_bar(aes(y = (..count..)/sum(..count..)),
                             alpha=0.5, position="dodge") +
                    geom_text(aes(y = ((..count..)/sum(..count..)), 
                                  label = scales::percent((..count..)/sum(..count..))), 
                              stat = "count", vjust = -0.001, size=3) +
                    scale_y_continuous(labels = percent) +
                    labs(title = "Gender by Countries' Development", 
                         y = "Percent (%)", x = "Gender") +
                    facet_grid(~develop)
                  
                  dem6 <-   ggplot(base.pop.vars, aes(x = as.factor(income),fill=income)) +
                    geom_bar(aes(y = (..count..)/sum(..count..)),
                             alpha=0.5, position="dodge") +
                    geom_text(aes(y = ((..count..)/sum(..count..)), 
                                  label = scales::percent((..count..)/sum(..count..))), 
                              stat = "count", vjust = -0.001,size=2) +
                    scale_y_continuous(labels = percent) +
                    labs(title = "Self-Proclaimed Income by Countries' Development", 
                         y = "Percent (%)", x = "USD") +
                    facet_grid(~develop)+ theme(
                      axis.text.x = element_blank(),
                      axis.ticks = element_blank())
                  
                  dem7 <-   ggplot(base.pop.vars, aes(x = as.factor(religion),fill=religion)) +
                    geom_bar(aes(y = (..count..)/sum(..count..)),
                             alpha=0.5, position="dodge") +
                    geom_text(aes(y = ((..count..)/sum(..count..)), 
                                  label = scales::percent((..count..)/sum(..count..))), 
                              stat = "count", vjust = -0.001,size=2) +
                    scale_y_continuous(labels = percent) +
                    labs(title = "Religion by Countries' Development", 
                         y = "Percent (%)", x = "Religion") +
                    facet_grid(~develop)+ theme(
                      axis.text.x = element_blank(),
                      axis.ticks = element_blank())
                  
                  dem8 <-   ggplot(base.pop.vars, aes(x = as.factor(education),fill=education)) +
                    geom_bar(aes(y = (..count..)/sum(..count..)),
                             alpha=0.5, position="dodge") +
                    geom_text(aes(y = ((..count..)/sum(..count..)), 
                                  label = scales::percent((..count..)/sum(..count..))), 
                              stat = "count", vjust = -0.001,size=2) +
                    scale_y_continuous(labels = percent) +
                    labs(title = "Education by Countries' Development", 
                         y = "Percent (%)", x = "Maximum level of Studies") +
                    facet_grid(~develop)+ theme(
                      axis.text.x = element_blank(),
                      axis.ticks = element_blank())
                  
                  dem9 <-   ggplot(base.pop.vars, aes(x = as.factor(political),fill=political)) +
                    geom_bar(aes(y = (..count..)/sum(..count..)),
                             alpha=0.5, position="dodge") +
                    geom_text(aes(y = ((..count..)/sum(..count..)), 
                                  label = scales::percent((..count..)/sum(..count..))), 
                              stat = "count", vjust = -0.001,size=2) +
                    scale_y_continuous(labels = percent) +
                    labs(title = "Political Orientation by Countries' Development", 
                         y = "Percent (%)", x = "Political Orientation") +
                    facet_grid(~develop)+ theme(
                      axis.text.x = element_blank(),
                      axis.ticks = element_blank())
                  
                  dem10 <-   ggplot(base.pop.vars, aes(x = as.factor(mturkexp),fill=mturkexp)) +
                    geom_bar(aes(y = (..count..)/sum(..count..)),
                             alpha=0.5, position="dodge") +
                    geom_text(aes(y = ((..count..)/sum(..count..)), 
                                  label = scales::percent((..count..)/sum(..count..))), 
                              stat = "count", vjust = -0.001,size=2) +
                    scale_y_continuous(labels = percent) +
                    labs(title = "MTurk Experience by Countries' Development", 
                         y = "Percent (%)", x = "MTurk Experience") +
                    facet_grid(~develop)+ theme(
                      axis.text.x = element_blank(),
                      axis.ticks = element_blank())
                  
                  dem11 <-   ggplot(base.pop.vars, aes(x = as.factor(mturkappr),fill=mturkappr)) +
                    geom_bar(aes(y = (..count..)/sum(..count..)),
                             alpha=0.5, position="dodge") +
                    geom_text(aes(y = ((..count..)/sum(..count..)), 
                                  label = scales::percent((..count..)/sum(..count..))), 
                              stat = "count", vjust = -0.001,size=2) +
                    scale_y_continuous(labels = percent) +
                    labs(title = "MTurk Approval Rate by Countries' Development", 
                         y = "Percent (%)", x = "MTurk Approval Rate") +
                    facet_grid(~develop)+ theme(
                      axis.text.x = element_blank(),
                      axis.ticks = element_blank())
                  
                  dem12 <-   ggplot(base.pop.vars, aes(x = as.factor(mturkhits),fill=mturkhits)) +
                    geom_bar(aes(y = (..count..)/sum(..count..)),
                             alpha=0.5, position="dodge") +
                    geom_text(aes(y = ((..count..)/sum(..count..)), 
                                  label = scales::percent((..count..)/sum(..count..))), 
                              stat = "count", vjust = -0.001,size=2) +
                    scale_y_continuous(labels = percent) +
                    labs(title = "MTurk HITs by Countries' Development", 
                         y = "Percent (%)", x = "Completed HITs in MTurk") +
                    facet_grid(~develop)+ theme(
                      axis.text.x = element_blank(),
                      axis.ticks = element_blank())
                  
                  dem13 <-   ggplot(base.pop.vars, aes(x = as.factor(ethnicity),fill=ethnicity)) +
                    geom_bar(aes(y = (..count..)/sum(..count..)),
                             alpha=0.5, position="dodge") +
                    geom_text(aes(y = ((..count..)/sum(..count..)), 
                                  label = scales::percent((..count..)/sum(..count..))), 
                              stat = "count", vjust = -0.001,size=2) +
                    scale_y_continuous(labels = percent) +
                    labs(title = "Ethnicity by Countries' Development", 
                         y = "Percent (%)", x = "Ethnicity") +
                    facet_grid(~develop)+ theme(
                      axis.text.x = element_blank(),
                      axis.ticks = element_blank())
                  
                  dem14 <-   ggplot(base.pop.vars, aes(x = as.factor(enjoy),fill=enjoy)) +
                    geom_bar(aes(y = (..count..)/sum(..count..)),
                             alpha=0.5, position="dodge") +
                    geom_text(aes(y = ((..count..)/sum(..count..)), 
                                  label = scales::percent((..count..)/sum(..count..))), 
                              stat = "count", vjust = -0.001,size=2) +
                    scale_y_continuous(labels = percent) +
                    labs(title = "Found Task Enjoyable by Countries' Development", 
                         y = "Percent (%)", x = "Enjoyment") +
                    facet_grid(~develop)+ theme(
                      axis.text.x = element_blank(),
                      axis.ticks = element_blank())
                  
                  dem15 <-   ggplot(base.pop.vars, aes(x = as.factor(satisfied),fill=satisfied)) +
                    geom_bar(aes(y = (..count..)/sum(..count..)),
                             alpha=0.5, position="dodge") +
                    geom_text(aes(y = ((..count..)/sum(..count..)), 
                                  label = scales::percent((..count..)/sum(..count..))), 
                              stat = "count", vjust = -0.001,size=2) +
                    scale_y_continuous(labels = percent) +
                    labs(title = "Satisfied with Results by Countries' Development", 
                         y = "Percent (%)", x = "Satisfaction") +
                    facet_grid(~develop)+ theme(
                      axis.text.x = element_blank(),
                      axis.ticks = element_blank())
                  
                  dem16 <-   ggplot(base.pop.vars, aes(x = as.factor(difficulty),fill=difficulty)) +
                    geom_bar(aes(y = (..count..)/sum(..count..)),
                             alpha=0.5, position="dodge") +
                    geom_text(aes(y = ((..count..)/sum(..count..)), 
                                  label = scales::percent((..count..)/sum(..count..))), 
                              stat = "count", vjust = -0.001,size=2) +
                    scale_y_continuous(labels = percent) +
                    labs(title = "Perception of Difficulty by Countries' Development", 
                         y = "Percent (%)", x = "Difficulty") +
                    facet_grid(~develop)+ theme(
                      axis.text.x = element_blank(),
                      axis.ticks = element_blank())
                  
                  dem17 <-   ggplot(base.pop.vars, aes(x = as.factor(timing),fill=timing)) +
                    geom_bar(aes(y = (..count..)/sum(..count..)),
                             alpha=0.5, position="dodge") +
                    geom_text(aes(y = ((..count..)/sum(..count..)), 
                                  label = scales::percent((..count..)/sum(..count..))), 
                              stat = "count", vjust = -0.001,size=2) +
                    scale_y_continuous(labels = percent) +
                    labs(title = "Perception of Allotted Time by Countries' Development", 
                         y = "Percent (%)", x = "Allotted Time") +
                    facet_grid(~develop)+ theme(
                      axis.text.x = element_blank(),
                      axis.ticks = element_blank())
                  
                  dem18 <-   ggplot(base.pop.vars, aes(x = as.factor(compensation),fill=compensation)) +
                    geom_bar(aes(y = (..count..)/sum(..count..)),
                             alpha=0.5, position="dodge") +
                    geom_text(aes(y = ((..count..)/sum(..count..)), 
                                  label = scales::percent((..count..)/sum(..count..))), 
                              stat = "count", vjust = -0.001,size=2) +
                    scale_y_continuous(labels = percent) +
                    labs(title = "Perception of Compensation by Countries' Development", 
                         y = "Percent (%)", x = "Compensation") +
                    facet_grid(~develop)+ theme(
                      axis.text.x = element_blank(),
                      axis.ticks = element_blank())
                  
                  dem19 <-   ggplot(base.pop.vars, aes(x = as.factor(compajust),fill=compajust)) +
                    geom_bar(aes(y = (..count..)/sum(..count..)),
                             alpha=0.5, position="dodge") +
                    geom_text(aes(y = ((..count..)/sum(..count..)), 
                                  label = scales::percent((..count..)/sum(..count..))), 
                              stat = "count", vjust = -0.001,size=2) +
                    scale_y_continuous(labels = percent) +
                    labs(title = "Suggested Adjustment in Compensation by Countries' Development", 
                         y = "Percent (%)", x = "Compensation Adjustment") +
                    facet_grid(~develop)+ theme(
                      axis.text.x = element_blank(),
                      axis.ticks = element_blank())
                  
                  dem20 <-   ggplot(base.pop.vars, aes(x = as.factor(develop),fill=develop)) +
                    geom_bar(aes(y = (..count..)/sum(..count..)),
                             alpha=0.5, position="dodge") +
                    geom_text(aes(y = ((..count..)/sum(..count..)), 
                                  label = scales::percent((..count..)/sum(..count..))), 
                              stat = "count", vjust = -0.001,size=2) +
                    scale_y_continuous(labels = percent) +
                    labs(title = "Participants by Countries' Development", 
                         y = "Percent (%)", x = "Country Development")
                  
                  dem21 <-   ggplot(base.pop.vars, aes(x = as.factor(location),fill=location)) +
                    geom_bar(aes(y = (..count..)/sum(..count..)),
                             alpha=0.5, position="dodge") +
                    geom_text(aes(y = ((..count..)/sum(..count..)), 
                                  label = scales::percent((..count..)/sum(..count..))), 
                              stat = "count", vjust = -0.001,size=2) +
                    scale_y_continuous(labels = percent) +
                    labs(title = "Participants by Region", 
                         y = "Percent (%)", x = "Region")
                  
                  dem22 <-   ggplot(base.pop.vars, aes(x = as.factor(time),fill=gender)) +
                    geom_bar(aes(y = (..count..)/sum(..count..)),
                             alpha=0.5, position="dodge") +
                    geom_text(aes(y = ((..count..)/sum(..count..)), 
                                  label = scales::percent((..count..)/sum(..count..))), 
                              stat = "count", vjust = -0.001, size=3) +
                    scale_y_continuous(labels = percent) +
                    labs(title = "Total Time Distribution by Gender", 
                         y = "Percent (%)", x = "Minutes")
                  
                  ##### Save Graphs #####
                  
                  #SET WD and save all Graphs
                  setwd("C:/Users/danbo/Desktop/MturkR/BaseF/GraphsBF")
                  
                  for(i in 1:22){
                    
                    ggsave(paste0("dem",i,".png"), plot = get(paste0("dem",i)))  
                    
                  }
                  
                  #COn el GET busca el "objeto" que es una gráfica y la guarda como ""i.png
                  #Dead Code
                  #ggsave("myplot2.png", plot = myplot2)
                  #ggsave("myplot36.png", plot = myplot3)
                  
                  ##### BY Region #####
                  dem5 <-   ggplot(base.pop.vars, aes(x = as.factor(income),fill=income)) +
                    geom_bar(aes(y = (..count..)/sum(..count..)),
                             alpha=0.5, position="dodge") +
                    geom_text(aes(y = ((..count..)/sum(..count..)), 
                                  label = scales::percent((..count..)/sum(..count..))), 
                              stat = "count", vjust = -0.001,size=2) +
                    scale_y_continuous(labels = percent) +
                    labs(title = "Self-Proclaimed Income by Region", 
                         y = "Percent (%)", x = "USD") +
                    facet_grid(~location)+ theme(
                      axis.text.x = element_blank(),
                      axis.ticks = element_blank())
                  
                  dem6 <-   ggplot(base.pop.vars, aes(x = as.factor(religion),fill=religion)) +
                    geom_bar(aes(y = (..count..)/sum(..count..)),
                             alpha=0.5, position="dodge") +
                    geom_text(aes(y = ((..count..)/sum(..count..)), 
                                  label = scales::percent((..count..)/sum(..count..))), 
                              stat = "count", vjust = -0.001,size=2) +
                    scale_y_continuous(labels = percent) +
                    labs(title = "Religion by Region", 
                         y = "Percent (%)", x = "Religion") +
                    facet_grid(~location)+ theme(
                      axis.text.x = element_blank(),
                      axis.ticks = element_blank())
                  
                  dem7 <-   ggplot(base.pop.vars, aes(x = as.factor(education),fill=education)) +
                    geom_bar(aes(y = (..count..)/sum(..count..)),
                             alpha=0.5, position="dodge") +
                    geom_text(aes(y = ((..count..)/sum(..count..)), 
                                  label = scales::percent((..count..)/sum(..count..))), 
                              stat = "count", vjust = -0.001,size=2) +
                    scale_y_continuous(labels = percent) +
                    labs(title = "Education by Region", 
                         y = "Percent (%)", x = "Level of Studies") +
                    facet_grid(~location)+ theme(
                      axis.text.x = element_blank(),
                      axis.ticks = element_blank())
                  
                  dem8 <-   ggplot(base.pop.vars, aes(x = as.factor(political),fill=political)) +
                    geom_bar(aes(y = (..count..)/sum(..count..)),
                             alpha=0.5, position="dodge") +
                    geom_text(aes(y = ((..count..)/sum(..count..)), 
                                  label = scales::percent((..count..)/sum(..count..))), 
                              stat = "count", vjust = -0.001,size=2) +
                    scale_y_continuous(labels = percent) +
                    labs(title = "Political Orientation by Region", 
                         y = "Percent (%)", x = "Political Orientation") +
                    facet_grid(~location)+ theme(
                      axis.text.x = element_blank(),
                      axis.ticks = element_blank())
                  
                  dem9 <-   ggplot(base.pop.vars, aes(x = as.factor(mturkexp),fill=mturkexp)) +
                    geom_bar(aes(y = (..count..)/sum(..count..)),
                             alpha=0.5, position="dodge") +
                    geom_text(aes(y = ((..count..)/sum(..count..)), 
                                  label = scales::percent((..count..)/sum(..count..))), 
                              stat = "count", vjust = -0.001,size=2) +
                    scale_y_continuous(labels = percent) +
                    labs(title = "MTurk Experience by Region", 
                         y = "Percent (%)", x = "MTurk Experience") +
                    facet_grid(~location)+ theme(
                      axis.text.x = element_blank(),
                      axis.ticks = element_blank())
                  
                  dem10 <-   ggplot(base.pop.vars, aes(x = as.factor(mturkappr),fill=mturkappr)) +
                    geom_bar(aes(y = (..count..)/sum(..count..)),
                             alpha=0.5, position="dodge") +
                    geom_text(aes(y = ((..count..)/sum(..count..)), 
                                  label = scales::percent((..count..)/sum(..count..))), 
                              stat = "count", vjust = -0.001,size=2) +
                    scale_y_continuous(labels = percent) +
                    labs(title = "MTurk Approval Rate by Region", 
                         y = "Percent (%)", x = "MTurk Approval Rate") +
                    facet_grid(~location)+ theme(
                      axis.text.x = element_blank(),
                      axis.ticks = element_blank())
                  
                  dem11 <-   ggplot(base.pop.vars, aes(x = as.factor(mturkhits),fill=mturkhits)) +
                    geom_bar(aes(y = (..count..)/sum(..count..)),
                             alpha=0.5, position="dodge") +
                    geom_text(aes(y = ((..count..)/sum(..count..)), 
                                  label = scales::percent((..count..)/sum(..count..))), 
                              stat = "count", vjust = -0.001,size=2) +
                    scale_y_continuous(labels = percent) +
                    labs(title = "MTurk HITs by Region", 
                         y = "Percent (%)", x = "Completed HITs in MTurk") +
                    facet_grid(~location)+ theme(
                      axis.text.x = element_blank(),
                      axis.ticks = element_blank())
                  
                  dem12 <-   ggplot(base.pop.vars, aes(x = as.factor(ethnicity),fill=ethnicity)) +
                    geom_bar(aes(y = (..count..)/sum(..count..)),
                             alpha=0.5, position="dodge") +
                    geom_text(aes(y = ((..count..)/sum(..count..)), 
                                  label = scales::percent((..count..)/sum(..count..))), 
                              stat = "count", vjust = -0.001,size=2) +
                    scale_y_continuous(labels = percent) +
                    labs(title = "Political Orientation by Region", 
                         y = "Percent (%)", x = "Political Orientation") +
                    facet_grid(~location)+ theme(
                      axis.text.x = element_blank(),
                      axis.ticks = element_blank())
                  
                  dem13 <-   ggplot(base.pop.vars, aes(x = as.factor(enjoy),fill=enjoy)) +
                    geom_bar(aes(y = (..count..)/sum(..count..)),
                             alpha=0.5, position="dodge") +
                    geom_text(aes(y = ((..count..)/sum(..count..)), 
                                  label = scales::percent((..count..)/sum(..count..))), 
                              stat = "count", vjust = -0.001,size=2) +
                    scale_y_continuous(labels = percent) +
                    labs(title = "Found Task Enjoyable", 
                         y = "Percent (%)", x = "Enjoyment") +
                    facet_grid(~location)+ theme(
                      axis.text.x = element_blank(),
                      axis.ticks = element_blank())
                  
                  dem14 <-   ggplot(base.pop.vars, aes(x = as.factor(satisfied),fill=satisfied)) +
                    geom_bar(aes(y = (..count..)/sum(..count..)),
                             alpha=0.5, position="dodge") +
                    geom_text(aes(y = ((..count..)/sum(..count..)), 
                                  label = scales::percent((..count..)/sum(..count..))), 
                              stat = "count", vjust = -0.001,size=2) +
                    scale_y_continuous(labels = percent) +
                    labs(title = "Satisfied with Results", 
                         y = "Percent (%)", x = "Satisfaction") +
                    facet_grid(~location)+ theme(
                      axis.text.x = element_blank(),
                      axis.ticks = element_blank())
                  
                  dem15 <-   ggplot(base.pop.vars, aes(x = as.factor(difficulty),fill=difficulty)) +
                    geom_bar(aes(y = (..count..)/sum(..count..)),
                             alpha=0.5, position="dodge") +
                    geom_text(aes(y = ((..count..)/sum(..count..)), 
                                  label = scales::percent((..count..)/sum(..count..))), 
                              stat = "count", vjust = -0.001,size=2) +
                    scale_y_continuous(labels = percent) +
                    labs(title = "Perception of Difficulty", 
                         y = "Percent (%)", x = "Difficulty") +
                    facet_grid(~location)+ theme(
                      axis.text.x = element_blank(),
                      axis.ticks = element_blank())
                  
                  dem16 <-   ggplot(base.pop.vars, aes(x = as.factor(timing),fill=timing)) +
                    geom_bar(aes(y = (..count..)/sum(..count..)),
                             alpha=0.5, position="dodge") +
                    geom_text(aes(y = ((..count..)/sum(..count..)), 
                                  label = scales::percent((..count..)/sum(..count..))), 
                              stat = "count", vjust = -0.001,size=2) +
                    scale_y_continuous(labels = percent) +
                    labs(title = "Perception of Allotted Time", 
                         y = "Percent (%)", x = "Allotted Time") +
                    facet_grid(~location)+ theme(
                      axis.text.x = element_blank(),
                      axis.ticks = element_blank())
                  
                  dem17 <-   ggplot(base.pop.vars, aes(x = as.factor(compensation),fill=compensation)) +
                    geom_bar(aes(y = (..count..)/sum(..count..)),
                             alpha=0.5, position="dodge") +
                    geom_text(aes(y = ((..count..)/sum(..count..)), 
                                  label = scales::percent((..count..)/sum(..count..))), 
                              stat = "count", vjust = -0.001,size=2) +
                    scale_y_continuous(labels = percent) +
                    labs(title = "Perception of Compensation", 
                         y = "Percent (%)", x = "Compensation") +
                    facet_grid(~location)+ theme(
                      axis.text.x = element_blank(),
                      axis.ticks = element_blank())
                  
                  dem18 <-   ggplot(base.pop.vars, aes(x = as.factor(compajust),fill=compajust)) +
                    geom_bar(aes(y = (..count..)/sum(..count..)),
                             alpha=0.5, position="dodge") +
                    geom_text(aes(y = ((..count..)/sum(..count..)), 
                                  label = scales::percent((..count..)/sum(..count..))), 
                              stat = "count", vjust = -0.001,size=2) +
                    scale_y_continuous(labels = percent) +
                    labs(title = "Suggested Adjustment in Compensation", 
                         y = "Percent (%)", x = "Compensation Adjustment") +
                    facet_grid(~location)+ theme(
                      axis.text.x = element_blank(),
                      axis.ticks = element_blank())