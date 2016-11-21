
library(tidyr)
library(readxl)
library(reshape2)
library(forecast)
library(data.table)


LoadDataSet <- function(filename){

  data <- readxl::read_excel(filename, col_names = F)


  header = list()
  n = dim(data)[2]
  column_name = "" 
  
  for (j in c(1:n)){
    for (i in c(1:6)){
      if (!is.na(data[i,j])){
       column_name = paste0(column_name,"_",data[i,j])
      }
    }
    if (column_name != ""){
      print(column_name)
      header[j] = column_name
      column_name = ""
    }
    else{
      print("_NA_")
      header[j] = "_NA_"
    }
  }
  
  header <- gsub(" ","_",header)
  header <- gsub("_/_","_",header)  
  header <- gsub("000'","M",header)  
  header <- gsub("_\\(_","_",header)  
  header <- gsub("_\\(","_",header)  
  header <- gsub("_\\)_","_",header)  
  header <- gsub("_\\)","",header)  

  valid_column_names <- make.names(names=header, unique=TRUE, allow_ = TRUE)
  header <- valid_column_names
  header <- gsub("^(X_)","",header)  

  names(data) <- header

  # if col_names has a M, multiply by 1000, if has an MM multiply by millions 
  data <- data[-c(1:6),] 



  data$realdate <- paste(data$NA_, data$NA_.1,"01", sep = "-")
  Sys.setlocale("LC_TIME", "C") # put location in USA
  data$realdate <- as.Date(data$realdate,"%Y-%b-%d")
  data[,c(1:2)] <- NULL
  data <- select(data,realdate,everything())


  # only get columbs with PLUS_SALES
  data_serie <- data[,(grep("PLUS",names(data)))]
  data_serie <- cbind(data$realdate,data_serie)

  KeepCountryProduct <- function(x){
    # special case for MEX with channels
    if (unlist(strsplit(x,"_"))[1] == "MEX"){
      y <- paste(unlist(strsplit(x,"_"))[3],unlist(strsplit(x,"_"))[1],unlist(strsplit(x,"_"))[2],sep="_")
    }
    else {
      y <- paste(unlist(strsplit(x,"_"))[2],unlist(strsplit(x,"_"))[1],sep="_")
    }
    return(y)
  }

  names(data_serie) <- unlist(lapply(names(data_serie), KeepCountryProduct))

  data_serie <- rename(data_serie, fecha = `NA_data$realdate` )
  return(data_serie)
}


data1 <- LoadDataSet("C:/Stendhal/19.forecast/CONSOLIDADO_AMB_FMPR.xlsx")
data2 <- LoadDataSet("C:/Stendhal/19.forecast/CONSOLIDADO_ORQK_TYS.xlsx")
data2[,1] <- NULL;
data3 <- LoadDataSet("C:/Stendhal/19.forecast/CONSOLIDADO_VIR_TYSV2.xlsx")
data3[,1] <- NULL
  
total <- cbind(data1,data2,data3)

total <- total[,order(colnames(total))]

total <- filter(total, fecha <= "2016-09-01")

total$fecha <- NULL

serie.ts <- ts(total, frequency = 12, start = c(2011,01) ) 




plot(serie.ts[,1])
arima <- auto.arima(serie.ts[,1])

plot(fitted(arima))
lines(serie.ts[,1], col = "blue")
sqrt(mean(residuals(arima)^2))

# Holt Winters
Hwa <- HoltWinters(serie.ts[,1], seasonal = "additive")
plot(Hw)
sqrt(mean(residuals(Hwa)^2))

Hwm <- HoltWinters(serie.ts[,1], seasonal = "multiplicative")
plot(Hwm)
mean(residuals(Hwa)^2)

TB <- tbats(serie.ts[,1])
plot(fitted(TB))
lines(serie.ts[,1], col = "blue")
sqrt(mean(residuals(TB)^2))

Ets <- ets(serie.ts[,1])
plot(fitted(Ets))
lines(serie.ts[,1], col = "blue")
sqrt(mean(residuals(Ets)^2))



robust_Arima = function(serie.ts) {
  tryCatch({
    sqrt(mean(residuals(auto.arima(serie.ts))^2))
    },
    warning = function(w){
      print(paste("warning in arima"))
    },
    error = function(e) {
      print("error in arima")
      NaN
    }
  ) 
}

robust_HoltWinters_Additive = function(serie.ts) {
  tryCatch({
      sqrt(mean(residuals(HoltWinters(serie.ts, seasonal = "additive"))^2))
    },
    warning = function(w){
      print(paste("warning in Holt Winters Additive"))
    },
    error = function(e) {
      print("error in in Holt Winters Additive")
      NaN
    }
  ) 
}

robust_HoltWinters_Multiplicative = function(serie.ts) {
  tryCatch({
    sqrt(mean(residuals(HoltWinters(serie.ts, seasonal = "multiplicative"))^2))
  },
  warning = function(w){
    print(paste("warning in in Holt Winters Multiplicative"))
  },
  error = function(e) {
    print("error in in Holt Winters Multiplicative")
    NaN
  }
  ) 
}

robust_TBATS = function(serie.ts) {
  tryCatch({
    sqrt(mean(residuals(tbats(serie.ts))^2))
  },
  warning = function(w){
    print(paste("warning in TBATS"))
  },
  error = function(e) {
    print("error in TBATS")
    NaN
  }
  ) 
}

robust_ETS = function(serie.ts) {
  tryCatch({
    sqrt(mean(residuals(ets(serie.ts))^2))
  },
  warning = function(w){
    print(paste("warning in ETS"))
  },
  error = function(e) {
    print("error in ETS")
    NaN
  }
  ) 
}

result_table <- data.table( country_product = character(), Arima = double(), HoltWinters_A = double(), HoltWinters_M = double(), TBATS = double(), ETS = double())


for ( i in c(1:68)){

  # results[i,1] <- names(total)[i]
  # results[i,2] <<- robust_Arima(serie.ts[,i])
  # results[i,3] <<- robust_HoltWinters_Additive(serie.ts[,i])
  # results[i,4] <<- robust_HoltWinters_Multiplicative(serie.ts[,i])
  # results[i,5] <<- robust_TBATS(serie.ts[,i])
  # results[i,6] <<- robust_ETS
  results = list()
  results[1] <- names(total)[i]
  results[2] <- robust_Arima(serie.ts[,i])
  results[3] <- robust_HoltWinters_Additive(serie.ts[,i])
  results[4] <- robust_HoltWinters_Multiplicative(serie.ts[,i])
  results[5] <- robust_TBATS(serie.ts[,i])
  results[6] <- robust_ETS(serie.ts[,i])

  r <- data.frame(matrix(unlist(results),nrow = 1, ncol = 6, byrow = F))
  names(r) <- names(result_table)
  result_table <- rbind(result_table,r)

}

result_table2 <- result_table

result_melt <- melt(
  result_table2,
  id = 1,
  value.name = "values",
  variable.name = "variable"
)

result_melt <- unique(result_melt)


result_total <- select(result_melt,country_product,values) %>% group_by(country_product) %>% summarise( algo = min(values))
result_total <- inner_join(result_melt,result_total, by=c("country_product" = "country_product","values" = "algo"))

# get duplicates
filt <- result_total %>% group_by(country_product) %>% summarise(n = n())
result_total <- inner_join(result_total,filt)

result_total$variable <- as.character(result_total$variable)

result_total[which(result_total$n == 4),"variable"] <- "None" 

result_total <- unique(result_total)
result_total$n <- NULL


# data_melt <- melt(
#   data,
#   id = 1,
#   values.name = "values",
#   variables.name = "variables"
# )
# 
# data_melt$country <- strsplit(data_melt$variable, sep = "_")
# 
