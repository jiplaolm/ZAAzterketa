
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(C3)
source("tresnak.R")

data <- matrizeEgit(fitxategiakIrakurri())
adostasunak <- adostasunak(data)

shinyServer(function(input, output) {
  kappa.data <- reactive({ kappa2(data[, c(2,3)],weight="squared")})
  
#output$irr <- renderPrint({kappam.fleiss(data[, 2:ncol(data)],exact=F,detail=T)})
 output$irr <- renderPrint({kappa.data()})
 output$irrPlot <- renderC3Gauge({C3Gauge(kappa.data()$value)})
 output$irakArtean <- renderTable({adostasunak})

})
