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
