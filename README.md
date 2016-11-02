# MineR

Análisis de Dropout

Primero borrar todo el workspace para evitar errores

```R
rm( list = ls(all= TRUE))
```

Luego cargar todas las librerias necesarias para el análisis.
```R
library(plyr)
library(dplyr)
library(reshape2)
library(bit64)
library(data.table)
library(RColorBrewer)
library(viridis)
# install.packages("viridis")
# install.packages("ggthemes")
library(ggthemes)
```
Y crear las variables globales, en este caso producto

```R
Prod = "ARAHKOR SDH" 
```

La data a usar viene en "ventanas" de 24 meses, por tanto para no perder información histórica, hay que ir analizando cada archivo e ir uniéndolo al siguiente. Para cada uno, se definirá una métrica llamada _Score_, que se detallará más adelante. Este _Score_ toma en cuenta el periodo de tiempo, así que es necesario ir acumulando la data.

La siguiente función carga el histórico de Close-Up. Cada nombre está codificado de la forma AAAAMMDD_HISTORICO.CSV. La función recibe como parámetro el número de archivos históricos que va a unir. Cada archivo es una ventana móvil de 24 meses, partiendo desde NOV2014. Al estar ordenadas por orden cronológico el output sería el siguiente:

1. LoadCloseupHistoric(1) >> Información del primer archivo 20160601_HISTORICO.csv
2. LoadCloseupHistoric(2) >> Información de los archivos 20160601_HISTORICO.csv y 20160701_HISTORICO.csv
3. LoadCloseupHistoric(3) >> Información de los archivos 20160601_HISTORICO.csv, 20160701_HISTORICO.csv y 20160801_HISTORICO.csv.

```R
LoadCloseupHistoric <- function(nFiles) {
  
  
  # loads all historical close-up files with the name structure "yyyymm_HISTORICO.csv"
  # needs common libraries loaded 
  
  # input: 
  #   - dataList: list() to store rx data
  #   - DoctorsDataList: list() to store Dr data
  
  # In this case only DocData is needed
  # note: add better output
  
  rData = "C:/Stendhal/3.data_sources/historic"
  Prod = "ARAHKOR SDH" 
  calculateSpecialty = "no"
  setwd(rData)
  
  files <- list.files(pattern = "*_HISTORICO.csv")
  
  files <- files[c(1:nFiles)]
  
  sprintf("files to load:")
  files
  dataList = list()  
  DoctorsDataList = list()
  
  for (i in 1:length(files)) {
    mkt <- readLines(files[i])
    mkt[10]
    header <- mkt[10]
    header <- gsub(" ","",header)
    header <- gsub("%","",header)
    header <- gsub("\\.","",header)
    header <- gsub('\"',"",header)
    header <- strsplit(header,",")
    xx <- data.frame(names = unlist(header))
    varNames <- as.vector(xx$names[xx$names != ""])
    
    mkt2 <- fread(files[i])
    n <- dim(mkt2)[2]
    
    mkt3 <- fread(files[i],col.names = varNames[1:n])
    
    mkt3$Periodo <-  as.Date(gsub("MES MOV ","01/",mkt3$Periodo), format = "%d/%m/%y")
    
    # load specialty files for better description
    especialidades <- fread("especialidades.csv")
    mkt3 <- inner_join(mkt3,especialidades, by = c("Esp1" = "ESP1"))
    
    mkt3$MedicoUnico <- as.character(mkt3$MedicoUnico)
    
    # get last dates categ and market share
    categAndMs <- mkt3 %>%
      select(MedicoUnico,Periodo,MS,Categ,Periodo) %>%
      filter(!is.na(Categ))
    
    
    selector <- categAndMs[,c(1:2)] %>%
      group_by(MedicoUnico) %>%
      summarise(Periodo = max(Periodo))
    
    categAndMs <- 
      inner_join(categAndMs,selector,by = c("MedicoUnico" = "MedicoUnico","Periodo"="Periodo"))
    
    categAndMs <- select(categAndMs,-Periodo)
    
    DoctorsData <- 
      unique(select(mkt3,MedicoUnico,Nombre,Matricula,Especialidad,Representantes,Domicilio,CodPostal,Localidad,Region,Periodo,-Categ,-MS))
    
    DoctorsData <- inner_join(DoctorsData,categAndMs)
    
    product_rx <- select(mkt3,-Nombre,-Matricula,-Esp1,-Esp2,-Especialidad,-Representantes,-Mercado,-Domicilio,-CodPostal,-Localidad,-Region,-Domicilio2,-Categ,-MS)
    
    names(product_rx) <- gsub("Productos","P_",names(product_rx))
    names(product_rx) <- gsub("Rnk","R_",names(product_rx))
    names(product_rx) <- gsub("MS","M_",names(product_rx))
    
    # melts data using dplyr
    a_melt <- melt(
      data = product_rx,
      id = c(1:2),
      variable.name = "variable",
      value.name =  "value"
    )
    
    a_melt_split <- a_melt
    a_melt_split$atribute <- substring(a_melt$variable,1,1)
    a_melt_split$NroProducto<- substring(a_melt$variable,3)
    a_melt_split <- select(a_melt_split, -variable)
    
    #dim(unique(cast_test))
    
    casted_Arahkor <- dcast(
      a_melt_split, MedicoUnico + Periodo + NroProducto ~ atribute, value.var="value" )
    
    casted_Arahkor <- filter(casted_Arahkor,P!="")
    casted_Arahkor <- select(casted_Arahkor,-NroProducto)
    dataList[[i]] <- casted_Arahkor
    DoctorsDataList[[i]] <- DoctorsData
  }
  
  united <- dplyr::bind_rows(dataList)
  united <- unique(united)
  TimeLine <- arrange(data.frame(Periodo = unique(united$Periodo)),Periodo)
  
  uniDocData <- dplyr::bind_rows(DoctorsDataList)
  
  newerDate <- select(uniDocData,MedicoUnico,Periodo) %>%
    group_by(MedicoUnico) %>%
    summarise(Periodo = max(Periodo))
  
  uniDocData <- inner_join(uniDocData,newerDate,by = c("MedicoUnico" = "MedicoUnico","Periodo" = "Periodo"))
  uniDocData <- select(uniDocData,-Periodo)
  uniDocData$Nombre <- gsub("#","N",uniDocData$Nombre)
  #uniDocData$nombreOrd <- unlist(lapply(uniDocData$Nombre,CollapseAndSort))
  uniDocData$CodPostal <- as.character(uniDocData$CodPostal)
  
  united$M <- as.double(united$M)
  united$MedicoUnico <- as.character(united$MedicoUnico)
  
  # CloseUpData <- list()
  # CloseUpData[[1]] <- united
  # CloseUpData[[2]] <- uniDocData
  # CloseUpData[[3]] <- TimeLine
  # return(CloseUpData)
  
  GUnitedProd <<- united
  GUnitedDoc  <<- uniDocData
  GTimeLine   <<- TimeLine
} 
```

Ahora la siguiente función sirve para calcular el _Score_

```r
calculateScore <- function(id){
  plusFactor = 1
  minusFactor = -0.3
  
  # logistic parameters
  logistic_min_value = 1
  logistic_max_value = 25
  logistic_stepness = 1
  logistic_mid_value = dim(GTimeLine)[1] - 12
  
  medData <<- filter(GUnitedProd,MedicoUnico == id & P == Prod)
  dim(medData)[1]
  if (dim(medData)[1] != 0){
    dscore <- left_join(GTimeLine,medData,by='Periodo')
    dscore$MedicoUnico[is.na(dscore$MedicoUnico)] <- id
    dscore$Recency <- c(dim(dscore)[1]:1)
    dscore$M[is.na(dscore$M)] <- 0
    dscore$Step[!is.na(dscore$P)] <- plusFactor
    dscore$Step[is.na(dscore$P)] <- minusFactor
    dscore$WeightedStep <- dscore$M/100 + dscore$Step
    dscore$LogisticRecency <- 
      logistic_min_value + (logistic_max_value - logistic_min_value)/(logistic_min_value + exp(logistic_stepness*(dscore$Recency-logistic_mid_value)))
    dscore$RecencyWeightedLogistic <- dscore$WeightedStep*dscore$LogisticRecency
    dscore$Score <- cumsum(dscore$RecencyWeightedLogistic)
    final <- dim(dscore)[1]
    out <- data.frame(MedicoUnico = id, score = dscore$Score[final])
    ScoreMatrix <<- rbind(ScoreMatrix,out)
  }
}
```

Y esta es un wrapper para la función _Score_. Esta sólo permite abreviar muchas partes del código necesaria para ordenar y filtrar la data proveniente del histórico.

```r
scoreWrapper <- function(){
  
  casted_Arahkor <- filter(GUnitedProd,GUnitedProd$P == Prod)
  
  # how many data points there are
  dataPoints <- as.data.frame(table(casted_Arahkor$MedicoUnico))
  casted_Arahkor$M <- as.numeric(casted_Arahkor$M)
  
  
  filtered <- select(dataPoints,MedicoUnico = Var1, Freq)
  
  # Scoring function --------------------------
  
  ScoreMatrix <<- data.frame( MedicoUnico = character(), score = double())
  
  results <- lapply(filtered$MedicoUnico, calculateScore)
  rownames(ScoreMatrix) <- NULL
  return(ScoreMatrix)
}
```

Ahora, cargamos la data y calculamos el score, corriendo la ventana hasta AGO2016, siendo la última data disponible.
```r
for (i in c(1:3)) {
  LoadCloseupHistoric(i)
  assign(paste0("data",i),scoreWrapper())
}


data1$date <- 201606
data2$date <- 201607
data3$date <- 201608
total_data <- rbind(data1,data2,data3)
total_data_casted <- dcast(data=total_data,MedicoUnico ~ date, value.var = "score")
```



