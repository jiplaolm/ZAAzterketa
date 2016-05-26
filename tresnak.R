#library(dplyr)
library(reshape2)
library(irr)

# Shiny aplikazioak gordetzen dituen fitxategiak itzuli
defektuzkoFitxategiak <- function(dir="data") {
  return(list.files(dir,full.names = T))
}

## Fitxategi zerrenda bat emanda, datuak kargatzen ditu
fitxategiakIrakurri <- function(fitxategiak) {
  data.list <- lapply(fitxategiak, fitxategiaKargatu)
  data <- do.call("rbind",data.list)
  return(data)
}


## Egokitu beharko da fitxategiaren egitura aldatzen da eta!!!!!
## Fitxategi baten edukia fitratu
fitxategiaKargatu <- function(fitx,burua=F, bereiz="\t") {
  data <- read.csv(fitx, header=burua, sep="\t")
  # Gehitu erabiltzailearen identifikatzailea
  names(data) <- c("Galdera","Balioa")
  erab <- sub(".csv","",basename(fitx))
  data$Erab <- ifelse(grepl("irak",erab),erab,paste0("irak",erab))
  # Ordena aldatau
  data <- data[,c("Galdera","Erab","Balioa")]
  return(data)
}

# Dataset luze batetik, zabalera pasa
# Egokitu beharko dugu datu-egitura berrira
matrizeEgit <- function(data) {
  return (dcast(data,Galdera~Erab))
}


## Filtratu adostasunerako behar diren datuak 
filtratuBalorazioakAdostarunerako <- function(data) {
  emaitza <- apply(data,1, function(x) x[!is.na(x)])
  return(t(emaitza))
}

## Balorazioak matrize egitura batean izanda, adostasuna kalkulatzen du
adostasunak <- function(data) {
  data <- data[,-1]
  irakasleak <- colnames(data)
  probak <- combn(irakasleak,m=2, simplify=F)
  
  # Kalkulatu adostasunak
  emaitzak <- sapply(probak, function(x) kappa2(data[,c(x[1],x[2])], weight = "squared")$value)
  names(emaitzak) <- sapply(probak,function(x) paste(x[1],x[2],sep="-"))
  
  emaitzak <- as.matrix(emaitzak)
  colnames(emaitzak) <- c("Adostasuna")
  return(emaitzak)
}
