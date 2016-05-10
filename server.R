
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
source("tresnak.R")

data <- matrizeEgit(fitxategiakIrakurri())
adostasunak <- adostasunak(data)

shinyServer(function(input, output) {

 output$irr <- renderText({kappam.fleiss(data[, 2:ncol(data)],exact=F,detail=T)$value})
 
 output$irakArtean <- renderTable({adostasunak})

})
