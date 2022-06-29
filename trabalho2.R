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




# 4 - Exploração dos dados atraves de medidas de localidade

#função para coletar a moda de atributos nominais
getmode <- function(v) {
  uniqv <- unique(v)
 print( uniqv[which.max(tabulate(match(v, uniqv)))])

}

getLocationsMeasures <- function(value){
  
 
    media <- mean(value, na.rm = TRUE)
    cat("\nMédia = ", media)
    mediana <- median(value, na.rm = TRUE)
    cat("\nMediana = ", mediana)
    maximum <- max(value, na.rm = TRUE)
    cat("\nValor Maximo =", maximum)
    minimum<- min(value, na.rm = TRUE)
    cat("\nValor minimo =",minimum)
    sortedData <- sort(value)
    quartis <- quantile(sortedData,type=4)
    cat("\nQuartis",quartis) 
  
 
}



getVariance <- function(value){
  
  variance <- var(value, na.rm=TRUE)
  cat("\nVariancia = ", variance)
  
  
}

getInterval <- function(value){
  interval <- (max(value, na.rm = TRUE) - min(value, na.rm = TRUE))
  cat("\nIntervalar = ", interval)
  
}

getDesvioPadrao <- function(value){
  desvio <- sd(value, na.rm = TRUE)
  cat("\nDesvio Padrão = ", desvio)
  
}

atributesForLocation <- data.frame(data$koi_score, 
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
atributesModeForLocation <- data.frame(data$koi_disposition, 
                                      data$koi_pdisposition, 
                                      data$koi_fpflag_nt,
                                      data$koi_fpflag_ss,
                                      data$koi_fpflag_co,
                                      data$koi_fpflag_ec)

#Media, mediana e quartis para os dados quantitativos
i<-0
for(row in atributesForLocation){
  cat("\n\nDados do", i )
  getLocationsMeasures(row)
  i<- i+1
  
}

#Moda dos atributos quantitativos
i<-0
for(row in atributesModeForLocation){
  cat("\n\nDados do", i )
  getmode(row)
  i<- i+1
  
}

#Printagem dos boxplots
boxplot(data$koi_score)
boxplot(data$koi_period)
boxplot(data$koi_time0bk)
boxplot(data$koi_impact)
boxplot(data$koi_duration)
boxplot(data$koi_depth)
boxplot(data$koi_prad)
boxplot(data$koi_teq)
boxplot(data$koi_insol)
boxplot()


######################## ---------------------------- ############################3

#Item 5 - Exploração das medidas de espalhamento
i<-0
for(row in atributesForLocation){
  cat("\n\nDados do", i )
  getInterval(row)
  i<- i+1
  
}

i<-0
for(row in atributesForLocation){
  cat("\n\nDados do", i )
  getVariance(row)
  i<- i+1
  
}
i<-0
for(row in atributesForLocation){
  cat("\n\nDados do", i )
  getDesvioPadrao(row)
  i<- i+1
  
}

#Item 6 - Medidas de dispersão

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