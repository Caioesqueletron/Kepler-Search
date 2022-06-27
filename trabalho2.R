library(readr)
library(gridExtra)
library(dplyr)
data  = read.csv('cumulative.csv', stringsAsFactors = FALSE)

#1target
target <- data$koi_pdisposition
sample(data)
unique(data$koi_duration)

data.column.class = data$koi_pdisposition
data$rowid<- NULL
data$kepid<- NULL
data$kepler_name<- NULL
data$koi_disposition<- NULL
data$koi_pdisposition<- NULL
data$kepoi_name<-NULL
data$koi_teq_err1<-NULL
data$koi_teq_err2<-NULL


sapply(data, typeof)

# 4 - Exploração dos dados atraves de medidas de localidade

#função para coletar a moda de atributos nominais
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]

}

#Medida de distriuição do atributo koi_disposition
modaKoi_disposition <- getmode(data$koi_disposition)
print(modaKoi_disposition)

#Medida de distriuição do atributo koi_disposition
modaKoi_pdisposition <- getmode(data$koi_pdisposition)
print(modaKoi_pdisposition)

#Medida de distribuição da koi_pdispodition
moda2 <- getmode(data$ko)


#Item  7 - Separação de conjuntos de teste e treino
sample <- sample(c(rep(0, 0.8 * nrow(data)),  # Create dummy for splitting
                   rep(1, 0.2 * nrow(data))))
table(sample)
train <- data[sample == 0, ]
tabela <- table(train$koi_pdisposition)
print(tabela)
test  <- data[sample == 1, ]
tabela2 <- table(test$koi_pdisposition)
print(tabela2)