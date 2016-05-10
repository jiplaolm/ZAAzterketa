#library(dplyr)
library(reshape2)
library(irr)

fitxategiakIrakurri <- function(dir="data") {
  fitxategiak <- list.files(dir, full.names=T)
  data.list <- lapply(fitxategiak, fitxategiaKargatu)
  data <- do.call("rbind",data.list)
  return(data)
}

fitxategiaKargatu <- function(fitx,burua=F, bereiz="\t") {
  data <- read.csv(fitx, header=burua, sep="\t")
  # Gehitu erabiltzailearen identifikatzailea
  names(data) <- c("Galdera","Balioa")
  erab <- sub(".csv","",basename(fitx))
  data$Erab <- erab
  # Ordena aldatau
  data <- data[,c("Galdera","Erab","Balioa")]
  return(data)
}

matrizeEgit <- function(data) {
  return (dcast(data,Galdera~Erab))
}