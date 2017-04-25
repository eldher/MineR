# Agrupamiento, headers, array, dcast y slices

```R
# Hypotehsis. Regional Category is lower or equal than National Category


# input files
# "C:/Stendhal/3.data_sources/closeup"
# 1. especialidades.csv
# 2. *_all_ara2.csv
# 3. *_all_snc.csv

# "C:/Stendhal/14.cardio_snc_nat"
# 1. "201610_armado_cardio.csv"
# 2. "201610_entry_market_kallion.csv"
# 3. "ARMADO CARDIO REGION.csv"
# 4. "KALLION REGIONAL.csv"
# 5. "201610_ims_datos_mercado.csv"
# 6. "correccion_especialidad.csv"
# 7. "ims_specialties.xlsx"

# output
# "C:/Stendhal/14.cardio_snc_nat"
# sf "final_algoritmo.csv" -> categorized doctors by cardio and snc 

# working @2016.10.17
# working @2016.10.21
# working @2016.11.07
# working @2016.12.22 Added IMS specialty matchs
# testing @2017.01.19 Final fixes added


rm( list = ls(all= TRUE))

library(plyr)
library(dplyr)
library(reshape2)
library(bit64)
library(data.table)
library(ggplot2)
library(ggbiplot)
library(lubridate)
library(RColorBrewer)
library(readxl)


CollapseAndSort <- function(texto){
  cadena <- gsub(" ","",texto)
  cadena <- paste(sort(unlist(strsplit(cadena,""))), collapse = "")
  return(cadena)
}

QuitarAcentos <- function(texto) {
  iconv(texto, to='ASCII//TRANSLIT')
}



currentDate = "02_17"
pastDate = "02_16"

workDir = "C:/Stendhal/14.cardio_snc_nat"
setwd(workDir)
especialidades <- fread("especialidades.csv")


files <- list("201704_closeup_cardio_nacional.xlsx",
              "201704_closeup_cardio_regional.xlsx")



Shape_CloseUp_Data <- function(files,currentDate,pastDate){
  
  fDataList <- list()
  
  for(j in 1:length(files)){
    
    iData <- read_excel(files[[j]])
    
    tData <- iData[-c(1:7),]
    
    array <- tData[1,]
    array[1,8] <- array[1,7]
    array[1,10] <- array[1,9]
    array[1,13] <- array[1,12]
    array[1,15] <- array[1,14]
    
    array[is.na(array)] <- ""
    tData[1,] <- array
    
    new_header = list()
    
    
    for (i in c(1:dim(tData)[2])) {
      if (tData[[i]][1] != ""){
        new_header[i] = paste0(as.character(tData[[i]][2]),"_",as.character(tData[[i]][1]))
      }
      else{
        new_header[i] = as.character(tData[[i]][2])
      }
      
    }
    
    new_header <- data.table(new_header)
    new_header$new_header <- gsub("\\.","_",new_header$new_header)
    new_header$new_header <- gsub(" ","_",new_header$new_header)
    new_header$new_header <- gsub("%","",new_header$new_header)
    new_header$new_header <- gsub("/","_",new_header$new_header)
    new_header$new_header <- gsub(paste0("_TAM_",currentDate),"_Current",new_header$new_header)
    new_header$new_header <- gsub(paste0("_TAM_",pastDate),"_Past",new_header$new_header)
    
    names(tData) <- as.matrix(new_header$new_header)
    tData <- tData[-c(1,2),]
    
    # to delete columns full with NA's
    tData <- tData[,colSums(is.na(tData))<nrow(tData)]
    
    
    # Category and MS Unification
    tData$Categ <- ifelse(is.na(tData$Categ_Current),tData$Categ_Past,tData$Categ_Current)
    tData$MS <- ifelse(is.na(tData$MS_Current),tData$MS_Past,tData$MS_Current)
    
    tData$R <- ifelse(is.na(tData$Rnk1_Current),tData$Rnk1_Past,tData$Rnk1_Current)
    tData$ProdMS <- ifelse(is.na(tData$MS1_Current),tData$MS1_Past,tData$MS1_Current)
    
    
    tData <- select(tData,
                    Medico_Unico, 
                    Nombre_Medico, 
                    Categ, 
                    Prod = Productos1, R, 
                    ProdMS,everything(), 
                    -Categ_Current,
                    -Categ_Past, 
                    -MS_Current, 
                    -MS_Past, -Mercado, -Rnk1_Current, -Rnk1_Past, -MS1_Current, -MS1_Past)
    
    
    
    tData$Nombre_Medico <- gsub("#","N",tData$Nombre_Medico)
    tData$nombreOrd <- unlist(lapply(tData$Nombre_Medico,CollapseAndSort))
    tData$Cod_Postal <- as.character(tData$Cod_Postal)
    tData <- left_join(tData,especialidades, by = c("Esp1" = "ESP1"))
    
    tData <- select(tData,
                    MedicoUnico = Medico_Unico, 
                    NombreMedico = Nombre_Medico, 
                    Especialidad, 
                    Categ, 
                    MS, 
                    CodPostal = Cod_Postal, 
                    Region, 
                    nombreOrd)
    
    tData <- unique(tData)
    tData$tag <- paste0("tabla",j)
    fDataList[[j]] <- tData
  }
  
  fData <- dplyr::bind_rows(fDataList)
  fData <- unique(fData)
  # 
  # fDataGrouped <- fData %>% 
  #   group_by(MedicoUnico,NombreMedico,Especialidad,CodPostal,Region,nombreOrd) %>%
  #   summarise(Categ = min(Categ), MS = max(MS))
  # 
  # fDataGrouped <- ungroup(fDataGrouped)
  # 
  # return(fDataGrouped)
  return(fData)
}


ff <- Shape_CloseUp_Data(files,currentDate,pastDate)

ff2 <- select(ff,MedicoUnico,NombreMedico,Categ,tag)

ff2$Categ <- as.numeric(ff2$Categ)

ff3 <- ff2 %>% group_by(MedicoUnico,NombreMedico,tag) %>%  slice(which.min(Categ))

ff3 <- dcast(ff3, MedicoUnico ~ tag, value.var = "Categ")


ff3$compa <- ifelse(ff3$tabla1 < ff3$tabla2,1,0)

sum(ff3$compa)
1254/36048


```R
