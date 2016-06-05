
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
  
  ## Datuak prestatu
  datuGuztiak <- reactive({fitxategiakIrakurri(fitxategiak())})
  
  oharrak <- reactive({
    select(filter(subset(datuGuztiak(),Galdera==4),!is.na(Balioa)),-Galdera,-Distraigarria)
    })
  
  data <- reactive({
    mutate(filter(datuGuztiak(),Galdera!=4), Balioa=as.numeric(Balioa))
  })
  dataTaula <- reactive({matrizeEgit(data())})
  
  dataAgreement <- reactive({
    datuak<-filter(data(),Ariketa <= 20)
    matrizeEgit(datuak)
  })
  
  dataAriketak <- reactive({filter(data(),Ariketa>20)})
  
  
  dataAriketaBakunak <- reactive({filter(dataAriketak(),Mota =="BAK")})
  
  dataAriketaAnitzak <- reactive({filter(dataAriketak(),Mota =="ANI")})
  
  ## Lotu interfazea datu azterketarekin
  callModule(agreementModule, "adostasuna", dataAgreement)
  callModule(arikAzterketaModule,"guztiak",dataAriketak)
  callModule(arikAzterketaModule,"bakunak",dataAriketaBakunak)
  callModule(arikAzterketaModule,"anitzak",dataAriketaAnitzak)
  callModule(taulaModule,"datuak",dataTaula)
  callModule(taulaModule,"oharrak",oharrak)
  
 
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
