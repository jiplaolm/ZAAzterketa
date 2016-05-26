
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

source("libs.R") # Interfaze grafikorak

shinyServer(function(input, output) {
  ## Datuak - reactive modukoak definitu grafikat etab. egunera daitezen
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

  
  ## Lotu interfazea datu azterketarekin
  callModule(agreementModule, "adostasuna", data)
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
