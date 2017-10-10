## Aplikazioak erabiltzen dituen liburutegiak
library(shiny)
library(C3)
library(htmlwidgets)
library(shinyjs)
library(DT)
library(plyr)
library(dplyr)
library(likert)
library(reshape2)
library(irr)
library(rel)

library(tidyr)


#options(shiny.maxRequestSize=5*1024^2) # Igotzeko tamaina igo behar izanez gero

# Beste funtzionalitatea kargatu
source("tresnak.R") # Data azterketarako, Agian modulu bihurtu eta mugitu menpekotasun hauek

# Garatutako Shiny moduluak ere kargatu
source("agreementModule.R")
source("arikAzterketaModule.R")
source("arikAzterketaViewModule.R")
source("taulaModule.R")
source("agreementViewModule.R")
source("arikGaldAzterketaModule.R")
source("exerciseAgreementAnalysisModule.R")
source("exerciseAgreementViewModule.R")
source("raterMonitoringViewModule.R")