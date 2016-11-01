# This script does it all.

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

# output
# "C:/Stendhal/14.cardio_snc_nat"
# sf "final_algoritmo.csv" -> categorized doctors by cardio and snc

# working @2016.10.17
# working @2016.10.21

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



CollapseAndSort <- function(texto){
  cadena <- gsub(" ","",texto)
  cadena <- paste(sort(unlist(strsplit(cadena,""))), collapse = "")
  return(cadena)
}

QuitarAcentos <- function(texto) {
  iconv(texto, to='ASCII//TRANSLIT')
}

Load_CloseUp_ARA_Mono_Market <- function(FileDate){
  # loads  close-up Monthly All ARA II MAT files with the name structure "yyyymm_all_ara2.csv"
  # needs common libraries loaded 
  
  # input: 
  #   - dataList: list() to store rx data
  #   - DoctorsDataList: list() to store Dr data
  # output
  #   - mkt3: File with doctors data, category and MS
  #   - mkt4: File with Rx %MS data for calculation of product preference
  #   - MarketData: List with both Data Frames [[1]] for mkt3 and [[2]] for mkt4
  
  MarketData <- list()
  rData = "C:/Stendhal/3.data_sources/closeup"
  setwd(rData)
  especialidades <- fread("especialidades.csv")
  
  files <- list.files(pattern = "*_all_ara2.csv")
  
  mkt <- readLines(files[1])
  #mkt[10]
  header <- mkt[10]
  header <- gsub(" ","",header)
  header <- gsub("%","",header)
  header <- gsub("\\.","",header)
  header <- gsub('\"',"",header)
  header <- strsplit(header,",")
  xx <- data.frame(names = unlist(header))
  varNames <- as.vector(xx$names[xx$names != ""])
  
  mkt2 <- fread(files[1])
  n <- dim(mkt2)[2]
  
  mkt3 <- fread(files[1],col.names = varNames[1:n],colClasses = rep(c("character"),each=n))
  
  new_header = list()
  
  for (i in c(1:dim(mkt3)[2])) {
    new_header[i] = paste0(as.character(mkt3[[i]][9]),"_",as.character(mkt3[[i]][8]))
  }
  
  new_header <- gsub(" ","",new_header)
  new_header <- gsub("%","",new_header)
  new_header <- gsub("\\.","",new_header)
  new_header <- gsub('\"',"",new_header)
  new_header <- gsub("_","",new_header)
  new_header <- gsub("TAM08","",new_header)
  new_header <- gsub("/","_",new_header)
  new_header <- gsub("Rnk1","RK_Prod",new_header)
  new_header <- gsub("MS1","MS_Prod",new_header)
  new_header <- gsub("Productos1","producto",new_header)
  names(mkt3) <- new_header
  
  mkt3 <- mkt3[10:dim(mkt3)[1],]
  mkt3 <- left_join(mkt3,especialidades, by = c("Esp1" = "ESP1"))
  
  mkt3$Categ_16<- ifelse(mkt3$Categ_16 == "", mkt3$Categ_15, mkt3$Categ_16)
  # mkt3$Categ_16 <- gsub("1","Q1",mkt3$Categ_16)
  # mkt3$Categ_16 <- gsub("2","Q2",mkt3$Categ_16)
  # mkt3$Categ_16 <- gsub("3","Q3",mkt3$Categ_16)
  # mkt3$Categ_16 <- gsub("4","Q4",mkt3$Categ_16)
  # mkt3$Categ_16 <- gsub("5","Q5",mkt3$Categ_16)
  mkt3$NombreMedico <- gsub("#","N",mkt3$NombreMedico)
  mkt3$nombreOrd <- unlist(lapply(mkt3$NombreMedico,CollapseAndSort))
  mkt3$CodPostal <- as.character(mkt3$CodPostal)
  
  # sets dataframe for calculations of product preference by type and category
  mkt4 <- select(mkt3,MedicoUnico,NombreMedico,Matricula,Representantes,Categ_16,MS_16,producto,RK_Prod_16,MS_Prod_16,Especialidad,nombreOrd)
  mkt4 <- filter(mkt4,RK_Prod_16 != "")
  mkt4$RK_Prod_16 <- as.numeric(mkt4$RK_Prod_16)
  mkt4$MS_Prod_16 <- as.double(mkt4$MS_Prod_16)
  
  # loads product description ----------------------------------------
  productDescription <- read.csv("ara2_product_description.csv",stringsAsFactors = FALSE)
  productDescription <- select(productDescription,producto,tipo,molecula)
  mkt4 <- left_join(mkt4,productDescription)
  
  # sets dataframe for match with salesforce
  mkt3 <- unique(select(mkt3,MedicoUnico,NombreMedico,Matricula,Especialidad,CodPostal,Region,Categ_15,Categ_16,nombreOrd))
  
  MarketData[[1]] <- mkt3
  MarketData[[2]] <- mkt4
  
  return(MarketData)  
}

Load_CloseUp_SNC_Mono_Market <- function(FileDate){
  # loads  close-up Monthly All Epilepsy market MAT files with the name structure "yyyymm_all_snc.csv"
  # needs common libraries loaded 
  
  # input: 
  #   - FileDate: Date to look for in file to read
  # output
  #   - mkt3: File with doctors data, category and MS
  #   - mkt4: File with Rx %MS data for calculation of product preference
  #   - MarketData: List with both Data Frames [[1]] for mkt3 and [[2]] for mkt4
  
  # working on this
  
  rData = "C:/Stendhal/3.data_sources/closeup"
  setwd(rData)
  especialidades <- fread("especialidades.csv")
  
  files <- list.files(pattern = "*_all_snc.csv")
  
  mkt <- readLines(files[1])
  #mkt[10]
  header <- mkt[10]
  header <- gsub(" ","",header)
  header <- gsub("%","",header)
  header <- gsub("\\.","",header)
  header <- gsub('\"',"",header)
  header <- strsplit(header,",")
  xx <- data.frame(names = unlist(header))
  varNames <- as.vector(xx$names[xx$names != ""])
  n <- length(varNames)
  
  mkt2 <- fread(files[1],autostart=100L,header=FALSE, sep=",", col.names = varNames[1:n])
  
  new_header = list()
  
  for (i in c(1:dim(mkt2)[2])) {
    new_header[i] = paste0(as.character(mkt2[[i]][10]),"_",as.character(mkt2[[i]][9]))
  }
  
  new_header <- gsub(" ","",new_header)
  new_header <- gsub("%","",new_header)
  new_header <- gsub("\\.","",new_header)
  new_header <- gsub('\"',"",new_header)
  new_header <- gsub("_","",new_header)
  new_header <- gsub("TAM08","",new_header)
  new_header <- gsub("/","_",new_header)
  new_header <- gsub("Rnk1","RK_Prod",new_header)
  new_header <- gsub("MS1","MS_Prod",new_header)
  new_header <- gsub("Productos1","producto",new_header)
  names(mkt2) <- new_header
  
  mkt2 <- mkt2[11:dim(mkt2)[1],]
  mkt2 <- left_join(mkt2,especialidades, by = c("Esp1" = "ESP1"))

  mkt2$nombreOrd <- unlist(lapply(mkt2$NombreMedico,CollapseAndSort))
  # pedir a Nat que agregue el codpostal
  # mkt3$NombreMedico <- gsub("#","N",mkt3$NombreMedico)
  # mkt3$nombreOrd <- unlist(lapply(mkt3$NombreMedico,CollapseAndSort))
  # mkt3$CodPostal <- as.character(mkt3$CodPostal)
  
  # sets dataframe for calculations of product preference by type and category
  mkt4 <- select(mkt2,MedicoUnico,NombreMedico,Categ_16,MS_16,producto,RK_Prod_16,MS_Prod_16,Especialidad,nombreOrd)
  mkt4 <- filter(mkt4,RK_Prod_16 != "")
  mkt4$RK_Prod_16 <- as.numeric(mkt4$RK_Prod_16)
  mkt4$MS_Prod_16 <- as.double(mkt4$MS_Prod_16)
  
  # loads product description ----------------------------------------
  productDescription <- read.csv("product_description.csv",stringsAsFactors = FALSE)
  productDescription <- select(productDescription,producto,tipo,molecula)
  mkt4 <- left_join(mkt4,productDescription)
  
  # sets dataframe for match with salesforce
  #mkt3 <- unique(select(mkt3,MedicoUnico,NombreMedico,Matricula,Especialidad,Region,Categ_16,nombreOrd))
  
  #MarketData[[1]] <- mkt4
  #MarketData[[2]] <- mkt4
  
  return(mkt4)  
}

Load_CloseUp_One_Market <- function(FileName){
  # loads  close-up Monthly All ARA II MAT files with the name structure "yyyymm_all_ara2.csv"
  # needs common libraries loaded 
  
  # input: 
  #   - dataList: list() to store rx data
  #   - DoctorsDataList: list() to store Dr data
  # output
  #   - mkt3: File with doctors data, category and MS
  #   - mkt4: File with Rx %MS data for calculation of product preference
  #   - MarketData: List with both Data Frames [[1]] for mkt3 and [[2]] for mkt4
  
  MarketData <- list()
  rData = "C:/Stendhal/14.cardio_snc_nat"

  setwd(rData)
  especialidades <- fread("especialidades.csv")
  
  files <- list.files(pattern = FileName)
  
  mkt <- readLines(files[1])
  #mkt[10]
  header <- mkt[10]
  header <- gsub(" ","",header)
  header <- gsub("%","",header)
  header <- gsub("\\.","",header)
  header <- gsub('\"',"",header)
  header <- strsplit(header,",")
  xx <- data.frame(names = unlist(header))
  varNames <- as.vector(xx$names[xx$names != ""])
  
  mkt2 <- fread(files[1])
  n <- dim(mkt2)[2]
  
  mkt3 <- fread(files[1],col.names = varNames[1:n],colClasses = rep(c("character"),each=n))
  
  new_header = list()
  
  for (i in c(1:dim(mkt3)[2])) {
    new_header[i] = paste0(as.character(mkt3[[i]][9]),"_",as.character(mkt3[[i]][8]))
  }
  
  new_header <- gsub(" ","",new_header)
  new_header <- gsub("%","",new_header)
  new_header <- gsub("\\.","",new_header)
  new_header <- gsub('\"',"",new_header)
  new_header <- gsub("_","",new_header)
  new_header <- gsub("TAM08","",new_header)
  new_header <- gsub("/","_",new_header)
  new_header <- gsub("Rnk1","RK_Prod",new_header)
  new_header <- gsub("MS1","MS_Prod",new_header)
  new_header <- gsub("Productos1","producto",new_header)
  names(mkt3) <- new_header
  
  mkt3 <- mkt3[10:dim(mkt3)[1],]
  mkt3 <- left_join(mkt3,especialidades, by = c("Esp1" = "ESP1"))
  
  #mkt3$Categ_16<- ifelse(mkt3$Categ_16 == "", mkt3$Categ_15, mkt3$Categ_16)
  # mkt3$Categ_16 <- gsub("1","Q1",mkt3$Categ_16)
  # mkt3$Categ_16 <- gsub("2","Q2",mkt3$Categ_16)
  # mkt3$Categ_16 <- gsub("3","Q3",mkt3$Categ_16)
  # mkt3$Categ_16 <- gsub("4","Q4",mkt3$Categ_16)
  # mkt3$Categ_16 <- gsub("5","Q5",mkt3$Categ_16)
  mkt3$NombreMedico <- gsub("#","N",mkt3$NombreMedico)
  mkt3$nombreOrd <- unlist(lapply(mkt3$NombreMedico,CollapseAndSort))
  mkt3$CodPostal <- as.character(mkt3$CodPostal)
  
  # sets dataframe for calculations of product preference by type and category
  mkt4 <- select(mkt3,MedicoUnico,NombreMedico,Matricula,Representantes,Categ_16,MS_16,producto,RK_Prod_16,MS_Prod_16,Especialidad,nombreOrd)
  mkt4 <- filter(mkt4,RK_Prod_16 != "")
  mkt4$RK_Prod_16 <- as.numeric(mkt4$RK_Prod_16)
  mkt4$MS_Prod_16 <- as.double(mkt4$MS_Prod_16)
  
  # loads product description ----------------------------------------
  #productDescription <- read.csv("ara2_product_description.csv",stringsAsFactors = FALSE)
  #productDescription <- select(productDescription,producto,tipo,molecula)
  #mkt4 <- left_join(mkt4,productDescription)
  
  # sets dataframe for match with salesforce
  mkt3 <- unique(select(mkt3,MedicoUnico,NombreMedico,Matricula,Especialidad,CodPostal,Region,MS_16,Categ_16,nombreOrd))
  
  MarketData[[1]] <- mkt3
  MarketData[[2]] <- mkt4
  
  return(MarketData)  
}

Load_CloseUp_Regional_Market <- function(FileName){
  # loads Close-up Monthly One Market (Cardio or SNC) 
  # needs common libraries loaded 
  
  # input: 
  #   - FileName: File to read
  # output
  #   - mkt2: Doctor's name, code, specialty, quantile and %ms
  
  #FileName = "ARMADO CARDIO REGION.csv"
  rData = "C:/Stendhal/14.cardio_snc_nat"
  setwd(rData)
  especialidades <- fread("especialidades.csv")
  
  files <- list.files(pattern = FileName)
  
  mkt <- readLines(files[1])
  #mkt[10]
  header <- mkt[10]
  header <- gsub(" ","",header)
  header <- gsub("%","",header)
  header <- gsub("\\.","",header)
  header <- gsub('\"',"",header)
  header <- strsplit(header,",")
  xx <- data.frame(names = unlist(header))
  varNames <- as.vector(xx$names[xx$names != ""])
  n <- length(varNames)
  
  mkt2 <- fread(files[1],autostart=100L,header=FALSE, sep=",", col.names = varNames[1:n])
  
  new_header = list()
  
  for (i in c(1:dim(mkt2)[2])) {
    new_header[i] = paste0(as.character(mkt2[[i]][10]),"_",as.character(mkt2[[i]][9]))
  }
  
  new_header <- gsub(" ","",new_header)
  new_header <- gsub("%","",new_header)
  new_header <- gsub("\\.","",new_header)
  new_header <- gsub('\"',"",new_header)
  new_header <- gsub("_","",new_header)
  new_header <- gsub("TAM08","",new_header)
  new_header <- gsub("/","_",new_header)
  new_header <- gsub("Rnk1","RK_Prod",new_header)
  new_header <- gsub("MS1","MS_Prod",new_header)
  new_header <- gsub("Productos1","producto",new_header)
  names(mkt2) <- new_header
  
  mkt2 <- mkt2[11:dim(mkt2)[1],]
  mkt2 <- left_join(mkt2,especialidades, by = c("Esp1" = "ESP1"))
  mkt2 <- unique(select(mkt2,MedicoUnico,NombreMedico,Especialidad,MS_16,Categ_16))
  return(mkt2)
}
  
Load_Salesforce <- function(FileDate){
  # loads Salesforce doctors data form file with name structure "yyyymm_universo.csv"
  # needs common libraries loaded 
  
  # input: 
  #   - FileDate: date in format yyyymm to load
  # output
  #   - salesforce: data frame with loaded data and main fields set to preference
  
  # FileDate = "201610"
  rData = "C:/Stendhal/3.data_sources/salesforce"
  setwd(rData)
  salesforce <- read.csv(paste0(FileDate,"_universo.csv"),stringsAsFactors = FALSE)
  
  names(salesforce) <- unlist(tolower(lapply(names(salesforce),QuitarAcentos)))
  names(salesforce) <- gsub("\\.","_",names(salesforce))
  names(salesforce) <- gsub("__","_",names(salesforce))
  
  salesforce <- filter(salesforce,salesforce$contacto_stendhal_id_del_contacto_stendhal != "")
  #salesforce$medico_close_up_tam_quintil_ <- gsub("/NA","",salesforce$medico_close_up_tam_quintil_ )
  salesforce$codigo_postal <- as.character(salesforce$codigo_postal)
  salesforce$contacto_stendhal_nombre_del_contacto_stendhal <- gsub("\\.","",salesforce$contacto_stendhal_nombre_del_contacto_stendhal)
  salesforce$nombreOrd <- unlist(lapply(salesforce$contacto_stendhal_nombre_del_contacto_stendhal,CollapseAndSort))
  salesforce$numeracion_close_up[nchar(salesforce$numeracion_close_up) < 5] <- ""
  return(salesforce)
}

Load_Salesforce_Px_Cost_IMS <- function(FileName){
  # loads Salesforce doctors data form file with name structure "yyyymm_universo.csv"
  # needs common libraries loaded 
  
  # input: 
  #   - FileDate: date in format yyyymm to load
  # output
  #   - salesforce: data frame with loaded data and main fields set to preference
  
  # FileDate = "201610"
  # rData = "C:/Stendhal/15.cardio_snc_nat"
  # setwd(rData)
  pxCostData <- read.csv(FileName,stringsAsFactors = FALSE)
  
  names(pxCostData) <- unlist(tolower(lapply(names(pxCostData),QuitarAcentos)))
  names(pxCostData) <- gsub("\\.","_",names(pxCostData))
  names(pxCostData) <- gsub("__","_",names(pxCostData))

  
  valid_column_names <- make.names(names=names(pxCostData), unique=TRUE, allow_ = TRUE)
  names(pxCostData) <- valid_column_names
  
  pxCostData <- unique(select(pxCostData,contacto_stendhal_numeracion_salesforce,customer,cat_antihipert,gpa_antihipert,pac_antihipert,hon_antihipert,cat_epilep,gpa_epilep,pac_epilep,hon_epilep,brick))
  
  pxCC <- pxCostData %>% group_by(contacto_stendhal_numeracion_salesforce) %>% summarise_all(funs(max))
  
  return(pxCC)
}

GetMSByProductType <- function(mkt){
  # loads Close-Up Rx data from Ara_Mono_Market and calculates P() of B,BGx, Gx based on %ms
  # needs common libraries loaded 
  
  # input: 
  #   - mkt: rx file generated by ARA_Mono_Market
  # output
  #   - rxDistrib: Rx distribution P() by %ms 
  
  # FileDate = "201610"
  
  resumenByMed <- select(mkt,MedicoUnico,tipo,MS_Prod_16) %>% 
    group_by(MedicoUnico,tipo) %>% summarise(total = sum(MS_Prod_16))
  
  castedResumen <- dcast(
    resumenByMed, MedicoUnico ~ tipo, value.var="total" )
  
  rxDistrib <- unique(castedResumen)
  return(rxDistrib)
}

CorrectSpecialties <- function(sf){
  rData = "C:/Stendhal/14.cardio_snc_nat"
  setwd(rData)
  especialidades_corr <- fread("correccion_especialidad.csv")
  sf <- left_join(sf,especialidades_corr)
  sf$contacto_stendhal_especialidad <- sf$contacto_stendhal_especialidad_corrected
  sf <- select(sf,everything(),-contacto_stendhal_especialidad_corrected)
  return(sf)
}

# loading salesforce data -------------------------
salesforce <- Load_Salesforce("20161024")
salesforce <- CorrectSpecialties(salesforce)

# All Ara2 Market --------------------------------------
MarketData <- Load_CloseUp_ARA_Mono_Market("201608")
mkt3 <- MarketData[[1]]
mkt4 <- MarketData[[2]]

toJoin <- select(mkt3, nombreOrd, CodPostal, MedicoUnico_ARA11 = MedicoUnico)
salesforce <- left_join(salesforce,toJoin,by = c("nombreOrd" = "nombreOrd","codigo_postal" = "CodPostal"))

toJoin <-
  select(mkt3, nombreOrd,Region, MedicoUnico_ARA22 = MedicoUnico)
salesforce <- left_join(salesforce,toJoin,by = c("nombreOrd" = "nombreOrd","ciudad" = "Region"))

toJoin <-
  select(mkt3, nombreOrd,MedicoUnico_ARA33 = MedicoUnico)
salesforce <- left_join(salesforce,toJoin)

# hacer cruces con kallion all market -----
SNCData <- Load_CloseUp_SNC_Mono_Market("201610")
toJoin <- unique(select(SNCData,nombreOrd, MedicoUnico_SNC = MedicoUnico))
salesforce <- left_join(salesforce,toJoin)


salesforce$MedicoUnico <- coalesce(salesforce$MedicoUnico_ARA11,salesforce$MedicoUnico_ARA22,salesforce$MedicoUnico_ARA33,salesforce$MedicoUnico_SNC)
salesforce <- select(salesforce,everything(),-MedicoUnico_ARA11, -MedicoUnico_ARA22, -MedicoUnico_ARA33,-MedicoUnico_SNC)

# Other mkt3 data needed
toJoin <- 
  select(mkt3, MedicoUnico, Cat16_ARA2 = Categ_16,Especialidad_ARA2 = Especialidad)
salesforce <- left_join(salesforce,toJoin)

# Armardo Cardio -----------------------------------------
armado_cardio <- Load_CloseUp_One_Market("201610_armado_cardio.csv")[[1]]
toJoin <- select(armado_cardio,MedicoUnico, Cat16_Cardio = Categ_16, MS16_cardio = MS_16)
salesforce_Arm_Ent <- left_join(salesforce,toJoin)

# Entry Neuro  -----------------------------------------
entry_neuro   <- Load_CloseUp_One_Market("201610_entry_market_kallion.csv")[[1]]
toJoin <- select(entry_neuro, MedicoUnico, Cat16_SNC = Categ_16, MS16_SNC = MS_16)
salesforce_Arm_Ent <- left_join(salesforce_Arm_Ent,toJoin)

#  +70% Gx Cardio prescribers ----------------------------
msByType <- GetMSByProductType(mkt4)
msByType <- unique(filter(msByType,!is.na(MedicoUnico) & (gx > 70)))
toJoin <- select(msByType,MedicoUnico,gx_Cardio = gx)
salesforce_Arm_Ent <- left_join(salesforce_Arm_Ent,toJoin)

# +70% Gx SNC prescribers ----------------------------
SNCData <- Load_CloseUp_SNC_Mono_Market("201610")
SNCByType <- GetMSByProductType(SNCData)
SNCByType <- unique(filter(SNCByType,!is.na(MedicoUnico) & (gx > 70)))
toJoin <- select(SNCByType,MedicoUnico,gx_SNC = gx)
salesforce_Arm_Ent <- left_join(salesforce_Arm_Ent,toJoin)

# regional Category and Market Share Cardio ----------
regionalCardio <- Load_CloseUp_Regional_Market("ARMADO CARDIO REGION.csv")
toJoin <- regionalCardio %>% group_by(MedicoUnico) %>% summarise(Categ_16 = min(Categ_16), MS_16 = max(MS_16))
toJoin <- select(toJoin,MedicoUnico,Categ_Reg_Cardio = Categ_16 , MS_Reg_Cardio = MS_16)
salesforce_Arm_Ent <- left_join(salesforce_Arm_Ent,toJoin)

# regional Category and Market Share SNC -----------------
regionalSNC <- Load_CloseUp_Regional_Market("KALLION REGIONAL.csv")
toJoin <- regionalSNC %>% group_by(MedicoUnico) %>% summarise(Categ_16 = min(Categ_16), MS_16 = max(MS_16))
toJoin <- select(toJoin,MedicoUnico,Categ_Reg_SNC = Categ_16 , MS_Reg_SNC = MS_16)
salesforce_Arm_Ent <- left_join(salesforce_Arm_Ent,toJoin)

# merge closeup numbers ----------------------------------
salesforce_Arm_Ent$numeracion_close_up[salesforce_Arm_Ent$numeracion_close_up ==""] <- NA
salesforce_Arm_Ent$numeroCloseup <- coalesce(salesforce_Arm_Ent$numeracion_close_up,salesforce_Arm_Ent$MedicoUnico)

total_cruce <- length(unique(salesforce_Arm_Ent$numeroCloseup))
total_sf <- length(unique(salesforce_Arm_Ent$contacto_stendhal_numeracion_salesforce))
ratio <- total_cruce/total_sf

# load px and cost consulta ----------------------------
pxCostData <- Load_Salesforce_Px_Cost_IMS("201610_ims_datos_mercado.csv")
salesforce_Arm_Ent <- left_join(salesforce_Arm_Ent,pxCostData)

salesforce_Arm_Ent$numeracion_close_up <- salesforce_Arm_Ent$numeroCloseup
sf <- unique(select(salesforce_Arm_Ent,-numeroCloseup))



# Unir honorarios --------------------------------------
Honorarios <- function(x){
  weight <- 25
  if        ( x > 1500 )               { response <- "SUPER ALTOS"
  } else if ( x > 1000 & x <= 1500 )   { response <- "ALTOS"
  } else if ( x > 500  & x <= 1000 )   { response <- "MEDIOS"
  } else                               { response <- "BAJOS" }
  return(response)
}
sf$costo_consulta_lugar_visitado[is.na(sf$costo_consulta_lugar_visitado)] <- 0
sf$costo_consulta_unico <- sapply(as.numeric(sf$costo_consulta_lugar_visitado), Honorarios)
sf$hon_antihipert[sf$hon_antihipert == ""] <- NA
sf$hon_epilep[sf$hon_epilep  == ""] <- NA

sf$hon_antihipert <-  coalesce(sf$hon_antihipert,sf$costo_consulta_unico )
sf$hon_epilep <-  coalesce(sf$hon_epilep,sf$costo_consulta_unico )

# duplicate analysis ------------------------------------
# duplis <- select(salesforce_Arm_Ent,contacto_stendhal_numeracion_salesforce,datos_de_contacto_nombre_del_propietario) %>%
#   group_by(contacto_stendhal_numeracion_salesforce,datos_de_contacto_nombre_del_propietario) %>%
#   summarise(n=n())
# 
# duplis <- filter(as.data.table(table(salesforce_Arm_Ent$contacto_stendhal_numeracion_salesforce)),N>1)
# duplisAll <- filter(salesforce_Arm_Ent,contacto_stendhal_numeracion_salesforce %in% duplis$V1)

# results output common meds in bases  ----------------------------------------
common_doc <- inner_join(armado_cardio,entry_neuro,by=c("MedicoUnico"))



#write.csv(salesforce_Arm_Ent,"salesforce_Arm_Ent.csv",row.names = FALSE,na = "")
#write.csv(duplisAll,"duplis.csv")  
#write.csv(common_doc,"common_docs.csv",row.names = FALSE)

# Statistical analysis of algorithms variables
# boxplot(salesforce_Arm_Ent$px_tratados_con_ara_ii_monoterapia[salesforce_Arm_Ent$px_tratados_con_ara_ii_monoterapia < 100])
# px_ara2 <- as.data.table(table(salesforce_Arm_Ent$px_tratados_con_ara_ii_monoterapia))
# sum(px_ara2$N[px_ara2$V1 < 20])
# sum(px_ara2$N[px_ara2$V1 >= 20 & px_ara2$V1 <= 39 ])
# sum(px_ara2$N[px_ara2$V1 > 40])
# barplot(table(salesforce_Arm_Ent$px_tratados_con_ara_ii_monoterapia))



# Cardio Algorithm -----------------------------------------
# All preprocessing is needed at this point, in order to have a fully contained database 

# Minima categoria, nacional o regional --------------------
RegionalCategory <- function(x){
weight <- 25
  if ( x == 1 )       { response <- 5*weight/5
  } else if ( x == 2 ){ response <- 4*weight/5
  } else if ( x == 3 ){ response <- 3*weight/5
  } else if ( x == 4 ){ response <- 2*weight/5
  } else if ( x == 5 ){ response <- 1*weight/5
  } else              { response <- 0*weight/5}
  return(response)
}
sf$categRegional_p <- pmin(sf$Cat16_Cardio,sf$Categ_Reg_Cardio,na.rm = T)
sf$categRegional_p[is.na(sf$categRegional_p)] <- 6
sf$categRegional_p <- sapply(sf$categRegional_p, RegionalCategory)

# Es lider de Opinion ----------------------------------
Kol <- function(x){
  weight <- 10
  if        ( x == "Nacional" )       { response <- 5*weight/5
  } else if ( x == "Local/Regional" ) { response <- 4*weight/5
  } else if ( x == "No es líder" )    { response <- 0*weight/5
  } else { response <- 0}
  return(response)
}
sf$kol_p <- sapply(sf$el_medico_es_lider_de_opinion, Kol)

# Pacientes con ARA II ----------------------------------
PxAra2 <- function(x){
  weight <- 15
  if        ( x > 40 )            { response <- 5*weight/5
  } else if ( x >= 20 & x <= 39)  { response <- 4*weight/5
  } else if ( x > 0   & x < 20 )  { response <- 2*weight/5
  } else                          { response <- 0*weight/5}
  return(response)
}
sf$px_tratados_con_ara_ii_monoterapia[is.na(sf$px_tratados_con_ara_ii_monoterapia)] <- 0
sf$pxara2_p <- sapply(sf$px_tratados_con_ara_ii_monoterapia, PxAra2)

# GPA ------------------------------------------------------
GPA <- function(x){
  weight <- 20
  if        ( x >= 8 & x <= 12 ) { gpa_p = 5*weight/5
  } else if ( x >= 5 & x <= 7  ) { gpa_p = 4*weight/5
  } else if ( x >= 1 & x <= 4  ) { gpa_p = 3*weight/5
  } else                         { gpa_p = 0*weight/5}
  return(gpa_p)
}
sf$gpa_antihipert[is.na(sf$gpa_antihipert) | sf$gpa_antihipert ==""] <- 0
sf$gpa_p <- sapply(as.numeric(sf$gpa_antihipert), GPA)

# Especialidad ---------------------------------------------
Especialidad <- function(x){
  weight <- 20
  if ( x == "CARDIOLOGIA" )              { espec_p = 5*weight/5 
    } else if ( x == "MEDICINA INTERNA" ){ espec_p = 3*weight/5 
    } else if ( x == "MEDICINA GENERAL" ){ espec_p = 3*weight/5 
    } else if ( x == "ENDOCRINOLOGIA" )  { espec_p = 2*weight/5 
    } else                               { espec_p = 1*weight/5 }    
  return(espec_p)
}
sf$contacto_stendhal_especialidad [is.na(sf$contacto_stendhal_especialidad)] <- ""
sf$espec_p <- sapply(sf$contacto_stendhal_especialidad,Especialidad)

# Costo --------------------------------------------------- 
Costo <- function(x){
  weight <- 10
  if ( x == "SUPER ALTOS" )     { costo_p = 5*weight/5 
    } else if ( x == "ALTOS" )  { costo_p = 4*weight/5 
    } else if ( x == "MEDIOS" ) { costo_p = 3*weight/5 
    } else if ( x == "BAJOS" )  { costo_p = 2*weight/5 
    } else                      { costo_p = 2*weight/5  }
  return(costo_p)
}
sf$hon_antihipert[is.na(sf$hon_antihipert)] <- ""
sf$costo_p <- sapply(sf$hon_antihipert,Costo)

# Puntaje -------------------------------------------------
sf$puntaje <- sf$categRegional_p + sf$kol_p + sf$pxara2_p + sf$gpa_p + sf$espec_p + sf$costo_p

barplot(table(sf$puntaje))
Categoria <- function(x){
  if        ( x >= 75 & x <= 100 ) {Categoria <-1
  } else if ( x >= 50 & x <= 74  ) {Categoria <-2
  } else if ( x >= 30 & x <= 49 )  {Categoria <-3
  } else if ( x >= 15 & x <= 29  ) {Categoria <-4
  } else { Categoria <- 5 }
  return(Categoria)
}

sf$categoria_Algo_Cardio <- sapply(sf$puntaje,Categoria)

# Analisis de categorias Cardio ---------------------------------
sums <- as.data.table(table(sf$categoria_Algo_Cardio))

ggplot(sf,aes(x=factor(categoria_Algo_Cardio))) +
geom_bar(fill="#1C65BF", width=0.8) +
labs(title= "Categorias Algoritmo Cardio. Agosto 2016", x = "Categorias Algoritmo Cardio", y= "Cantidad") +
geom_text(aes(V1, N, label = N, fill = NULL), data = sums, vjust=-0.3, size = 5 ) +
theme_grey(base_size = 16) +
theme(axis.line=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(), legend.position="none")






sf <- unique(sf)
dat <- filter(sf,categoria_Algo_Cardio == 5) # Change category number and run again
barplot(table(dat$categoria))

# SNC Algorithm ----------------------------------------------
# All preprocessing is needed at this point, in order to have a fully contained database 

# Minima categoria, nacional o regional ----------------------
RegionalCategory <- function(x){
  weight <- 25
  if ( x == 1 )       { response <- 5*weight/5
  } else if ( x == 2 ){ response <- 4*weight/5
  } else if ( x == 3 ){ response <- 3*weight/5
  } else if ( x == 4 ){ response <- 2*weight/5
  } else if ( x == 5 ){ response <- 1*weight/5
  } else              { response <- 0*weight/5}
  return(response)
}
sf$categRegional_snc_p <- pmin(sf$Cat16_SNC,sf$Categ_Reg_SNC,na.rm = T)
sf$categRegional_snc_p[is.na(sf$categRegional_snc_p)] <- 6
sf$categRegional_snc_p <- sapply(sf$categRegional_snc_p, RegionalCategory)

#Es lider de Opinion --------------------------------------
Kol <- function(x){
  weight <- 10
  if        ( x == "Nacional" )       { response <- 5*weight/5
  } else if ( x == "Local/Regional" ) { response <- 4*weight/5
  } else if ( x == "No es líder" )    { response <- 0*weight/5
  } else { response <- 0}
  return(response)
}
sf$kol_snc_p <- sapply(sf$es_lider_de_opinion_snc, Kol)

# Pacientes con Epilepsia ----------------------------------
PxSNC <- function(x){
  weight <- 15
  if        ( x > 20 )            { response <- 5*weight/5
  } else if ( x >= 10 & x <= 20 ) { response <- 4*weight/5
  } else if ( x >= 5 & x <= 9 )   { response <- 3*weight/5
  } else if ( x < 0 & x <= 4 )    { response <- 2*weight/5
  } else                          { response <- 0*weight/5}
  return(response)
}
sf$px_tiene_al_mes_con_oxcarbazepina[is.na(sf$px_tiene_al_mes_con_oxcarbazepina)] <- 0
sf$pxsnc_p <- sapply(as.numeric(sf$px_tiene_al_mes_con_oxcarbazepina), PxSNC)

# GPA ------------------------------------------------------
GPA <- function(x){
  weight <- 20
  if        ( x >= 8 & x <= 12 ) { gpa_p = 5*weight/5
  } else if ( x >= 5 & x <= 7  ) { gpa_p = 4*weight/5
  } else if ( x >= 1 & x <= 4  ) {gpa_p =  3*weight/5
  } else                         { gpa_p = 0*weight/5}
  return(gpa_p)
}
sf$gpa_epilep[is.na(sf$gpa_epilep) | sf$gpa_epilep ==""] <- 0
sf$gpa_snc_p <- sapply(as.numeric(sf$gpa_epilep), GPA)

# Especialidad ---------------------------------------------
Especialidad <- function(x){
  weight <- 20
  if ( x == "NEUROLOGIA" | x == "NEUROCIRUGIA") { espec_p = 5*weight/5 
  } else if ( x == "PSIQUIATRIA" )       { espec_p = 3*weight/5 
  } else if ( x == "MEDICINA GENERAL" )  { espec_p = 3*weight/5 
  } else if ( x == "MEDICINA INTERNA" )  { espec_p = 2*weight/5 
  } else                                 { espec_p = 1*weight/5 }    
  return(espec_p)
}
# sf$contacto_stendhal_especialidad [is.na(sf$contacto_stendhal_especialidad)] <- ""
sf$espec_snc_p <- sapply(sf$contacto_stendhal_especialidad,Especialidad)

# Costo --------------------------------------------------- 
Costo <- function(x){
  weight <- 10
  if ( x == "SUPER ALTOS" )   { costo_p = 5*weight/5 
  } else if ( x == "ALTOS" )  { costo_p = 4*weight/5 
  } else if ( x == "MEDIOS" ) { costo_p = 3*weight/5 
  } else if ( x == "BAJOS" )  { costo_p = 2*weight/5 
  } else                      { costo_p = 1*weight/5 }
  return(costo_p)
}
sf$hon_epilep[is.na(sf$hon_epilep)] <- ""
sf$costo_snc_p <- sapply(sf$hon_epilep,Costo)

# Puntaje -------------------------------------------------
sf$puntaje_snc <- sf$categRegional_snc_p + sf$kol_snc_p + sf$pxsnc_p + sf$gpa_snc_p + sf$espec_snc_p + sf$costo_snc_p

barplot(table(sf$puntaje))
Categoria <- function(x){
  if        ( x >= 75 & x <= 100 ) {Categoria <-1
  } else if ( x >= 50 & x <= 74  ) {Categoria <-2
  } else if ( x >= 30 & x <= 49 )  {Categoria <-3
  } else if ( x >= 15 & x <= 29  ) {Categoria <-4
  } else { Categoria <- 5 }
  return(Categoria)
}
sf$categoria_Algo_SNC <- sapply(sf$puntaje_snc,Categoria)

# Analisis de categorias SNC ---------------------------------
sums <- as.data.table(table(sf$categoria_Algo_SNC))


ggplot(sf,aes(x=factor(categoria_Algo_SNC))) +
  geom_bar(fill="#1C65BF", width=0.8) +
  labs(title= "Categorias Algoritmo SNC Agosto 2016", x = "Categorias Algoritmo Cardio", y= "Cantidad") +
  geom_text(aes(V1, N, label = N, fill = NULL), data = sums, vjust=-0.3, size = 5 ) +
  theme_grey(base_size = 16) +
  theme(axis.line=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(), legend.position="none")



sf <- unique(sf)

# Acid test between former algorithm and new one  ----------------------------------------------


OldCateg <- filter(sf,toupper(categoria) %in% c("A","B","C","D","E"))

sums <- as.data.table(table(OldCateg$categoria_Algo_Cardio))

cbPalette <- c( "#0CAEFF" , "#1C65BF","#0D3D7C","#16284C")

ggplot(OldCateg, aes(factor(categoria_Algo_Cardio), fill=categoria)) + geom_bar() + labs(title = "Nueva Categorización Cardio vs Anterior") + scale_fill_manual(values=cbPalette) + scale_color_manual(values= cbPalette) + geom_text(aes(V1, N, label = N, fill = NULL), data = sums, vjust=-0.4, size = 5 )  + theme_grey(base_size = 15) + theme(axis.title.y=element_blank(),axis.title.x=element_blank())


# Experiment with faceted plot
# Create two columns with data and try plotting
# Difference between scoring in Cardio and SNC, it drives distribution
dataToFacet<- rbind(select(sf, puntos = puntaje) %>% mutate( type = "Cardio" ), select(sf, puntos = puntaje_snc) %>% mutate(type = "SNC" ))

ggplot(dataToFacet, aes(factor(puntos)))+geom_bar(fill="#1C65BF")+facet_wrap(~type, nrow=2,scales="free") + labs(title = "Puntajes Cardio y SNC", x="Puntaje" ) + theme(axis.title.y=element_blank())

# Write results -------------------------------------
write.csv(sf,"final_algoritmo.csv",row.names = FALSE,na = "")
getwd()

