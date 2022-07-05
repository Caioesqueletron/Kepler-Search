library(readr)
library(gridExtra)
library(dplyr)
library(skyscapeR)    

#Inicio e alguns tratamentos
data  = read.csv('cumulative.csv', stringsAsFactors = FALSE)

summary(data)

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
data$koi_steff<- NULL

data$koi_slogg_err1 <- data$koi_slogg_err1 + data$koi_slogg
data$koi_slogg_err2 <- data$koi_slogg_err2 + data$koi_slogg
data$koi_slogg <- NULL

data$koi_srad_err1 <- data$koi_srad_err1 + data$koi_srad
data$koi_srad_err2 <- data$koi_srad_err2 + data$koi_srad
data$koi_srad <- NULL

data$koi_prad_err1 <- data$koi_prad_err1 + data$koi_prad
data$koi_prad_err2 <- data$koi_prad_err2 + data$koi_prad

data$koi_prad <- NULL




summary(data)
#1 - Imprime o atributo target
target <- data$koi_pdisposition
table(target)
sample(data)
unique(data$koi_duration)


# 4 - Exploração dos dados atraves de medidas de localidade

#função para coletar a moda de atributos nominais
getmode <- function(v) {
  uniqv <- unique(v)
 print( uniqv[which.max(tabulate(match(v, uniqv)))])

}
#Função para calcular a média, mediana e quartis
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


#Função para calcular a variancia
getVariance <- function(value){
  
  variance <- var(value, na.rm=TRUE)
  cat("\nVariancia = ", variance)
  
  
}

#Função para calcular o intervalar
getInterval <- function(value){
  interval <- (max(value, na.rm = TRUE) - min(value, na.rm = TRUE))
  cat("\nIntervalar = ", interval)
  
}

#Função para calcular o desvio padrão
getDesvioPadrao <- function(value){
  desvio <- sd(value, na.rm = TRUE)
  cat("\nDesvio Padrão = ", desvio)
  
  
}

#Função para calcular a curtose
getCurtose <- function(value){
  curtose <- kurtosis(value,na.rm = TRUE)
  cat("\nCurtose = ", curtose)
  
}

#Função para calcular a obliquidade
getObliquidade <-function(value){
  obliquidade <- (value)
  cat("\nObliquidade =", oliquidade)
}

atributesForLocation <- data.frame(data$koi_score, 
                                   data$koi_period_err1,
                                   data$koi_period_err2, 
                                   data$koi_time0bk_err1,
                                   data$koi_time0bk_err2,
                                   data$koi_impact_err1,
                                   data$koi_impact_err2,
                                   data$koi_duration_err1,
                                   data$koi_duration_err2,
                                   data$koi_depth_err1,
                                   data$koi_depth_err2,
                                   data$koi_prad_err1,
                                   data$koi_prad_err2,
                                   data$koi_teq,
                                   data$koi_insol_err1,
                                   data$koi_insol_err2,
                                   data$koi_model_snr,
                                   data$koi_slogg_err1,
                                   data$koi_slogg_err2,
                                   data$koi_steff_err1,
                                   data$koi_steff_err2,
                                   data$koi_srad_err1,
                                   data$koi_srad_err2,
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
boxplot(data$koi_score, ylab = "Score", main = "Disposition Score")
boxplot(data$koi_period_err1, ylab= "dias", main="Periodo Orbtital[erro minimo]",outline=FALSE)
boxplot(data$koi_period_err2, ylab= "dias", main="Periodo Orbtital[erro maximo]",outline=FALSE)
boxplot(data$koi_time0bk_err1, ylab="BJD" ,main="Transit Epoch[erro minimo]",outline=TRUE)
boxplot(data$koi_time0bk_err2, ylab="BJD" ,main="Transit Epoch[erro maximo]",outline=TRUE)
boxplot(data$koi_impact_err1, main="Impact Parameter[erro minimo]" )
boxplot(data$koi_impact_err2, main="Impact Parameter[erro maximo]")
boxplot(data$koi_duration_err1,ylab="horas", main="Transit Duration[erro minimo]", outline=TRUE)
boxplot(data$koi_duration_err2,ylab="horas",main="Transit Duration[erro maximo]",outline=TRUE)
boxplot(data$koi_depth_err1,ylab="ppm", main="Transit Depth[erro minimo]",outline=FALSE)
boxplot(data$koi_depth_err2,ylab="ppm", main="Transit Dpeth[erro maximo]",outline=FALSE)
boxplot(data$koi_prad_err1, ylab="earth radii", main="Planetary Radius[erro minimo]",outline=FALSE)
boxplot(data$koi_prad_err2, ylab ="earth radii", main="Planetary Radius",outline=FALSE)
boxplot(data$koi_teq,outline=FALSE)
boxplot(data$koi_insol_err1, outline = FALSE)
boxplot(data$koi_insol_err2, outline = FALSE)
boxplot(data$koi_srad_err1, outline = FALSE)
boxplot(data$koi_srad_err2, outline = FALSE)
boxplot(data$koi_steff_err1, outline = FALSE)
boxplot(data$koi_steff_err2, outline = FALSE)
boxplot(data$koi_slogg_err1, outline = TRUE)
boxplot(data$koi_slogg_err2, outline = TRUE)
boxplot(data$ra, outline = TRUE)
boxplot(data$dec, outline = TRUE)
boxplot(data$koi_kepmag, outline = TRUE)


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
hist(data$koi_score, col="darkblue", border="black");
hist(data$koi_period_err1, col="darkblue", border="black");
hist(data$koi_period_err2, col="darkblue", border="black");
hist(data$koi_time0bk_err1, col="darkblue", border="black");
hist(data$koi_time0bk_err2, col="darkblue", border="black");
hist(data$koi_impact_err1, col="darkblue", border="black");
hist(data$koi_impact_err2, col="darkblue", border="black");
hist(data$koi_duration_err1, col="darkblue", border="black");
hist(data$koi_duration_err2, col="darkblue", border="black");
hist(data$koi_prad_err1, col="darkblue", border="black");
hist(data$koi_prad_err2, col="darkblue", border="black");
hist(data$koi_teq, col="darkblue", border="black");
hist(data$koi_insol_err1, col="darkblue", border="black", );
hist(data$koi_insol_err2, col="darkblue", border="black");
hist(data$koi_srad_err1, col="darkblue", border="black");
hist(data$koi_srad_err2, col="darkblue", border="black");
hist(data$koi_steff_err1, col="darkblue", border="black");
hist(data$koi_steff_err2, col="darkblue", border="black");
hist(data$koi_slogg_err1, col="darkblue", border="black");
hist(data$koi_slogg_err2, col="darkblue", border="black");
hist(data$ra, col="darkblue", border="black");
hist(data$dec, col="darkblue", border="black");
hist(data$koi_kepmag, col="darkblue", border="black");


i<-0
for(row in atributesForLocation){
  cat("\n\nDados do", i )
  getCurtose(row)
  i<- i+1
  
}

i<-0
for(row in atributesForLocation){
  cat("\n\nDados do", i )
  getObliquidade(row)
  i<- i+1
  
}


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

#removendo atributos desnecesários do datset de treino
train$rowid<- NULL
train$kepid<- NULL
train$kepler_name<- NULL
train$kepoi_name<-NULL
train$koi_teq_err1<-NULL
train$koi_teq_err2<-NULL
train$koi_tce_delivname<-NULL


summary(train)

#removendo atributos desnecessários do dataset de teste
test$rowid<- NULL
test$kepid<- NULL
test$kepler_name<- NULL
test$kepoi_name<-NULL
test$koi_teq_err1<-NULL
test$koi_teq_err2<-NULL
test$koi_tce_delivname<-NULL
nrow(test)
test[!duplicated(test),]
duplicated(test)
nrow(test)


#12 - Limpeza de dados
#Limpeza de dados treino e teste

#Eliminação de dados inconscistentes

for(i in 1:nrow(train)){
  if(is.na(train[i,2])){
    if(train[i,1] == "CONFIRMED" && train[i,2] == "FALSE POSITIVE" ){
      train[i,] <- NULL
    }
  }
}

for(i in 1:nrow(test)){
  if(is.na(test[i,2])){
    if(test[i,1] == "CONFIRMED" && test[i,2] == "FALSE POSITIVE" ){
      test[i,] <- NULL
    }
  }
}


#Preenchimento dos dados
summary(train$koi_score)
for(i in 1:nrow(train)){
  if(is.na(train[i,3])){
    if(train[i,1] == "FALSE POSITIVE" && train[i,2] == "FALSE POSITIVE" ){
      train[i,3] <- 0.000
    }
    else if(train[i,1] == "CONFIRMED" && train[i,2] == "CANDIDATE"){
      train[i,3] <- 1.000
      
    }
    
    else if(train[i,1] == "CANDIDATE" && train[i,2] == "CANDIDATE"){
      train[i,3] <- 0.800
      
    }
    
    else if(train[i,1] == "CONFIRMED" && train[i,2] == "FALSE POSITIVE"){
      train[i,3] <- 0.200
      
    }
    
    
    
  }
  
}


for(i in 1:nrow(test)){
  if(is.na(test[i,3])){
    if(test[i,1] == "FALSE POSITIVE" && test[i,2] == "FALSE POSITIVE" ){
      test[i,3] <- 0.000
    }
    else if(test[i,1] == "CONFIRMED" && test[i,2] == "CANDIDATE"){
      test[i,3] <- 1.000
      
    }
    
    else if(test[i,1] == "CANDIDATE" && test[i,2] == "CANDIDATE"){
      test[i,3] <- 0.8000
      
    }
    else if(test[i,1] == "CONFIRMED" && test[i,2] == "FALSE POSITIVE"){
      test[i,3] <- 0.200
      
    }
    
    
  }
 
}


#Verificando dados duplicados
print(train[duplicated(train),])
train <- train[!duplicated(train),]
test <- test[!duplicated(test), ]
print(train[duplicated(train),])

#Colocando a média nos valores que estão com NA(outra estratégia seria remove-los tambem visto que 
#não há uma ideia dos seus comportamentos exatamente)
for(j in 8:33){
  for(i in 1:nrow(train))
  if(is.na(train[i,j])){
    train[i,j] <- mean(train[,j],na.rm = TRUE)
  }
}

for(j in 8:33){
  for(i in 1:nrow(test))
    if(is.na(test[i,j])){
      test[i,j] <- mean(test[,j],na.rm = TRUE)
    }
}


summary(train)
summary(test)




#13 - Conversão de dados
#Conversão do atributo alvo de Simbolico para Númerico(Binário)
for(i in 1:nrow(train)){
  if(train[i,2] == "CANDIDATE"){
    train[i,2] = 1
  }else if (train[i,2] == "FALSE POSITIVE"){
    train[i,2] = 0
  }
}

for(i in 1:nrow(test)){
  if(test[i,2] == "CANDIDATE"){
    test[i,2] = 1
  }else if (test[i,2] == "FALSE POSITIVE"){
    test[i,2] = 0
  }
}

#Padronização dos dados que não formam uma distribuição normal
for(i in 1:nrow(train)){
  if(train[i,2] == "CANDIDATE"){
    train[i,2] = 1
  }else if (train[i,2] == "FALSE POSITIVE"){
    train[i,2] = 0
  }
}

#Padronização dos dados
#não há uma ideia dos seus comportamentos exatamente)
for(j in 8:30){
  for(i in 1:nrow(test)){
      test[i,j] <- (test[i,j] - mean(test[,j],na.rm = TRUE))/sd(test[,j], na.rm = TRUE)
    
  }
}

for(j in 8:30){
    for(i in 1:nrow(train)){
      train[i,j] <- (train[i,j] - mean(train[,j],na.rm = TRUE))/sd(train[,j], na.rm = TRUE)
      
    }
}
    
table(train$koi_pdisposition)
table(test$koi_pdisposition)

table(train)
#14 - Redução de dimensionalidade
mtcars.pca <- prcomp(train[,c(7)], center = TRUE,scale. = TRUE)
summary(mtcars.pca)