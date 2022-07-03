library(readr)
library(gridExtra)
library(dplyr)
library(ROSE)

#Inicio e alguns tratamentos
data  = read.csv('cumulative.csv', stringsAsFactors = FALSE)
i<- 1
while (i < 3000) {
  if(!is.na((data[i,5]))){
    
    if(data[i,5] == "FALSE POSITIVE"){
      print("passou")
      
      data<- data[-i,]
      i <- i + 1
    }
  }
  
} 



table(data$koi_disposition)

#Tratamento das colunas de erros

data$koi_time0bk_err1 <- data$koi_time0bk_err1 + data$koi_time0bk
data$koi_time0bk_err2 <- data$koi_time0bk_err2 + data$koi_time0bk
data$koi_time0bk <- NULL

data$koi_period_err1 <- data$koi_period_err1 + data$koi_period
data$koi_period_err2 <- data$koi_period_err2 + data$koi_period
data$koi_period <- NULL

data$koi_impact_err1 <- data$koi_impact_err1 + data$koi_impact
data$koi_impact_err2 <- data$koi_impact_err2 + data$koi_impact
data$koi_impact <- NULL

data$koi_duration_err1 <- data$koi_duration_err1 + data$koi_duration
data$koi_duration_err2 <- data$koi_duration_err2 + data$koi_duration
data$koi_duration <- NULL

data$koi_depth_err1 <- data$koi_depth_err1 + data$koi_depth
data$koi_depth_err2 <- data$koi_depth_err2+ data$koi_depth
data$koi_depth <- NULL

data$koi_insol_err1 <- data$koi_insol_err1 + data$koi_insol
data$koi_insol_err2 <- data$koi_insol_err2 + data$koi_insol
data$koi_insol <- NULL

data$koi_steff_err1 <- data$koi_steff_err1 + data$koi_steff
data$koi_steff_err2 <- data$koi_steff_err2 + data$koi_steff
data$koi_steff_err1 <- data$koi_steff_err1 + data$koi_steff

#1 - Imprime o atributo target
target <- data$koi_disposition
table(target)
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
boxplot(data$koi_score, ylab = "Disposition Score", main = "Disposition Score")
boxplot(data$koi_period, ylab= "koi_period", main="Period",outline=FALSE)
boxplot(data$koi_time0bk,outline=FALSE)
boxplot(data$koi_impact,outline=FALSE)
boxplot(data$koi_duration,outline=FALSE)
boxplot(data$koi_depth,outline=FALSE)
boxplot(data$koi_prad,outline=FALSE)
boxplot(data$koi_teq,outline=FALSE)
boxplot(data$koi_insol)


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
tabela2 <- table(test$koi_disposition)
print(tabela2)

#Item 8 - Eliminação de atributos não necessários
testMatriz = as.matrix.data.frame(train)
print(testMatriz)

#removendo atributos desnecesários do datset de treino
train$rowid<- NULL
train$kepid<- NULL
train$kepler_name<- NULL
train$kepoi_name<-NULL
train$koi_teq_err1<-NULL
train$koi_teq_err2<-NULL
print(train)
summary(train)

#removendo atributos desnecessários do dataset de teste
test$rowid<- NULL
test$kepid<- NULL
test$kepler_name<- NULL
test$kepoi_name<-NULL
test$koi_teq_err1<-NULL
test$koi_teq_err2<-NULL
nrow(test)
test[!duplicated(test),]
duplicated(test)
nrow(test)
#Item 9 - Eliminação de exemplos não necessários - estou na duvida de como fazer
nrow(train)
train[!duplicated(train),]
nrow(train)
unique(train)


summary(train)


#12 - Limpeza de dados
#Limpeza de dados treino e teste
for(i in 1:nrow(train)){
  if(is.na(train[i,3])){
    if(train[i,1] == "FALSE POSITIVE" && train[i,2] == "FALSE POSITIVE" ){
      print("passou")
      train[i,3] <- 0.000
    }
    
  }
}

#Verificando dados duplicados
print(train[duplicated(train),])
print(test[duplicated(test), ])



