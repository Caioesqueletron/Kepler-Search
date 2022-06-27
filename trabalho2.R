library(readr)
library(gridExtra)
library(dplyr)
data  = read.csv('cumulative.csv', stringsAsFactors = FALSE)

#1 - Imprime o atributo target
target <- data$koi_pdisposition
sample(data)
unique(data$koi_duration)

data.column.class = data$koi_pdisposition
data$rowid<- NULL
data$kepid<- NULL
data$kepler_name<- NULL
data$kepoi_name<-NULL
data$koi_teq_err1<-NULL
data$koi_teq_err2<-NULL


sapply(data, typeof)

summary(data)

# 4 - Exploração dos dados atraves de medidas de localidade

#função para coletar a moda de atributos nominais
getmode <- function(v) {
  uniqv <- unique(v)
 print( uniqv[which.max(tabulate(match(v, uniqv)))])

}

getLocationsMeasures <- function(value){
  print(value)
  
  mean <- mean(value, na.rm = TRUE)
  print(mean)
  median <- median(value, na.rm = TRUE)
  print(median)
}
frea

atributesMeanAndMedian <- data.frame(data$koi_score, 
                                   data$koi_period, 
                                   data$koi_time0bk,
                                   data$koi_impact,
                                   data$koi_duration,
                                   data$koi_depth,
                                   data$koi_prad,
                                   data$koi_teq,
                                   data$koi_insol,
                                   data$koi_model_snr,
                                   data$koi_slogg,
                                   data$koi_srad,
                                   data$koi_tce_plnt_num,
                                   data$ra,
                                   data$dec,
                                   data$koi_kepmag)
summary(atributesMeanAndMedian)


print(atributesMeanAndMedian)
#Medida de localidade do atributo koi_disposition
result <- getmode(data$koi_disposition)

#Medida de localidade do atributo koi_disposition
result <- getmode(data$koi_pdisposition)

#Medida de localidade do atributo koi_score
result <- getLocationsMeasures(data$koi_score)

#Medida de distribuição da koi_fpflag_nt
result <- getmode(data$koi_fpflag_nt)

#Medida de distribuição da koi_fpflag_ss
result <- getmode(data$koi_fpflag_ss)


#Item  7 - Separação de conjuntos de teste e treino
sample <- sample(c(rep(0, 0.8 * nrow(data)),  
                   rep(1, 0.2 * nrow(data))))
table(sample)
train <- data[sample == 0, ]
tabela <- table(train$koi_pdisposition)
print(tabela)
test  <- data[sample == 1, ]
tabela2 <- table(test$koi_pdisposition)
print(tabela2)