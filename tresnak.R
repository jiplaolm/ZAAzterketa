library(dplyr)
library(tidyr)
library(irr)

# Shiny aplikazioak gordetzen dituen fitxategiak itzuli
defektuzkoFitxategiak <- function(dir="data") {
  return(list.files(dir,full.names = T))
}

## Fitxategi zerrenda bat emanda, datuak kargatzen ditu
fitxategiakIrakurri <- function(fitxategiak) {
  data.list <- lapply(fitxategiak, fitxategiaKargatu)
  data <- do.call("rbind",data.list)
  data <- data %>% mutate(
    Ariketa = as.numeric(Ariketa),
    Galdera = as.numeric(Galdera)
    )
  return(data)
}


## Egokitu beharko da fitxategiaren egitura aldatzen da eta!!!!!
## Fitxategi baten edukia fitratu
fitxategiaKargatu <- function(fitx,burua=T, bereiz="\t") {
  data <- read.csv(fitx, header=burua, sep="\t")
  garbituta <-data %>% select(Curso,starts_with("Q")) %>%
    extract(Curso,c("Irak"),"[[:alpha:]]+ [[:alpha:]]+ - [[:alpha:]]+ ([[:digit:]]+)")  %>% 
    mutate(Irak=paste0("Irak",Irak)) %>%
    gather(Gakoa,Balioa,-Irak) %>%
    extract(Gakoa,c("Ariketa","Mota","Galdera","Distraigarria"), "[[:alnum:]]+_ARIK_([[:digit:]]+)_([[:alpha:]]+)_([[:digit:]]+)[..]*(.*)")
  
  return(garbituta)
}

# Dataset luze batetik, zabalera pasa
# Egokitu beharko dugu datu-egitura berrira
matrizeEgit <- function(data) {
  emaitza <- data %>% spread(Irak, Balioa)
  return (emaitza)
}


# Aukeratu ebaluazioak dauzkaten zutabeak
aukeratuEbaluazioZutabeak <- function(data){
  return(select(data,starts_with("Irak")))
}


## Filtratu adostasunerako behar diren datuak 
filtratuBalorazioakAdostarunerako <- function(data) {
  emaitza <- apply(data,1, function(x) x[!is.na(x)])
  return(t(emaitza))
}

## Balorazioak matrize egitura batean izanda, adostasuna kalkulatzen du
adostasunak <- function(data) {
  data <- data[,-c(1:4)]
  irakasleak <- colnames(data)
  probak <- combn(irakasleak,m=2, simplify=F)
  
  # Kalkulatu adostasunak
  emaitzak <- sapply(probak, function(x) kappa2(data[,c(x[1],x[2])], weight = "squared")$value)
  names(emaitzak) <- sapply(probak,function(x) paste(x[1],x[2],sep="-"))
  
  emaitzak <- as.matrix(emaitzak)
  colnames(emaitzak) <- c("Adostasuna")
  return(emaitzak)
}
