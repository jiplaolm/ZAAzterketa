
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(C3)
source("tresnak.R")
library(shinyjs)
library(DT)
#options(shiny.maxRequestSize=5*1024^2) # Igotzeko tamaina igo behar izanez gero



shinyServer(function(input, output) {
  fitxategiak <- reactive({
    if (input$defektuzkoak) {
      return(defektuzkoFitxategiak())
    }
    else {
      req(input$fitx)
      req(nrow(input$fitx)>1)
      return(input$fitx$datapath)
    }
  })
  
  data <- reactive({matrizeEgit(fitxategiakIrakurri(fitxategiak()))})
  adostasun <- reactive({adostasunak(data())})
  
  
  kappa.data <- reactive({ 
    matrix <- data()
    kappa2(matrix[, c(2,3)],weight="squared")})
  
#output$irr <- renderPrint({kappam.fleiss(data[, 2:ncol(data)],exact=F,detail=T)})
 output$irr <- renderPrint({kappa.data()})
 output$irrPlot <- renderC3Gauge({C3Gauge(kappa.data()$value)})
 output$irakArtean <- renderTable({adostasun()})
 
 output$datuTaula <- DT::renderDataTable({data()})
 
 # Panelak ezkutatzeko informazioa ez daukagunean
 observeEvent(input$defektuzkoak, {
   if (input$defektuzkoak){
     show("main")
   }
   else {
     files <- input$fitx
     if(is.null(files)||nrow(files)<2) {
       hide("main")}
     else{
       show("main")
       }
   }
 })
 
 observeEvent(input$fitx, {
   files <- input$fitx
   if(is.null(files)||nrow(files)<2) {
     hide("main")}
   else{
     show("main")
   }
 })

})
