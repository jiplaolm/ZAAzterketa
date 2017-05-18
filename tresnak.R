
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
## Egiteko - matrize batean neurri guztiak?
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

## Gwet-en neurria
gwetAdostasuna <- function(data) {
  return (gac(data, kat=4, weight = "quadratic")$est)
}


## KalkulatuGwetAdostasuna
kalkulatuGwetAdostasuna <- function(data) {
  data.mat <- matrizeEgit(data)
  return (gwetAdostasuna(filtratuBalorazioakAdostarunerako(aukeratuEbaluazioZutabeak(data.mat))))
}

kalkulatuAriketenGalderarenGwetAdostasuna <- function(data, galdera) {
  ados.datuak.list <- split(data, data$Ariketa)
  
  ## kalkutatu adostasunak
  emaitzak.list <- lapply(ados.datuak.list, kalkulatuGwetAdostasuna)
  ## Gorde emaitzak data.frame batean
  emaitzak.df <- do.call(rbind.data.frame, emaitzak.list)
  emaitzak.df$Ariketa <- row.names(emaitzak.df)
  colnames(emaitzak.df) <- c("Adostasuna", "Ariketa")
  emaitzak.df$Galdera <- galdera
  return (emaitzak.df %>% select(Ariketa,Galdera, Adostasuna))
}

kalkulatuAriketenGwetAdostasunak <-  function(data) {
  data.list <- split(data, data$Galdera)
  galderak <- names(data.list)
  emaitza <- lapply(galderak, function(x) kalkulatuAriketenGalderarenGwetAdostasuna(data.list[[x]] , x))
  emaitza.df <- do.call(rbind.data.frame, emaitza)
  emaitza.df$Galdera <- as.numeric(emaitza.df$Galdera)
  return(emaitza.df)

}

## Kalkulatu bat etorzea
kalkulatuAriketarenBatEtortzea <- function(data) {
  data.mat <- matrizeEgit(data)
  ebaluazio.datuak <- filtratuBalorazioakAdostarunerako(aukeratuEbaluazioZutabeak(data.mat))
  bateratzeak <-ebaluazio.datuak[,1]==ebaluazio.datuak[,2] 
  
  return (sum(bateratzeak)/length(bateratzeak))
}

kalkulatuAriketenGalderarenBatEtortzea <- function(data, galdera) {
  ados.datuak.list <- split(data, data$Ariketa)
  
  ## kalkutatu adostasunak
  emaitzak.list <- lapply(ados.datuak.list, kalkulatuAriketarenBatEtortzea)
  ## Gorde emaitzak data.frame batean
  emaitzak.df <- do.call(rbind.data.frame, emaitzak.list)
  emaitzak.df$Ariketa <- row.names(emaitzak.df)
  colnames(emaitzak.df) <- c("BatEtortzea", "Ariketa")
  emaitzak.df$Galdera <- galdera
  return (emaitzak.df %>% select(Ariketa,Galdera, BatEtortzea))
}

kalkulatuBatEtortzeak <-  function(data) {
  data.list <- split(data, data$Galdera)
  galderak <- names(data.list)
  emaitza <- lapply(galderak, function(x) kalkulatuAriketenGalderarenBatEtortzea(data.list[[x]] , x))
  emaitza.df <- do.call(rbind.data.frame, emaitza)
  emaitza.df$Galdera <- as.numeric(emaitza.df$Galdera)
  return(emaitza.df)
  
}
