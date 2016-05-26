## Aplikazioak erabiltzen dituen liburutegiak
library(shiny)
library(C3)
library(htmlwidgets)
library(shinyjs)
library(DT)
#options(shiny.maxRequestSize=5*1024^2) # Igotzeko tamaina igo behar izanez gero

# Beste funtzionalitatea kargatu
source("tresnak.R") # Data azterketarako, Agian modulu bihurtu eta mugitu menpekotasun hauek

# Garatutako Shiny moduluak ere kargatu
source("agreementModule.R")
source("arikAzterketaModule.R")