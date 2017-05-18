
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

source("libs.R") # Interfaze grafikorak
theme_set(theme_bw(base_size=14)) # Theme zehaztu

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
  
  galdera.motak <- reactive({filter(data(), Galdera==1) %>% distinct(Ariketa, Mota)})
  
  adostasunDatuak <- reactive({
    ados.datuak <- filter(data(), Ariketa<=20, Galdera != 4)
    #
    kalkulatuAriketenGwetAdostasunak(ados.datuak)
    #ados.datuak
  })
  
  
  dataBatazbestekoa <- reactive({
    balorazioak <- filter(data(), Ariketa <=20, Galdera ==3)
    batazbestekoBalorazioak <- balorazioak %>% group_by(Ariketa) %>% summarise(Batazbestekoa =mean(Balioa))
    batazbestekoBalorazioak
  })
  
  dataDistrAdostasuna <- reactive({
   # 
    adostasun.info <- filter(adostasunDatuak(), Galdera == 3) %>% select(Ariketa, Adostasuna)
     
     ## Gehitu Galdera mota eta oharrak
     ariketa.motak <- filter(galdera.motak(),Ariketa <= 20)
     adostasun.info <- merge(adostasun.info, ariketa.motak,by= "Ariketa")
     
     ## Gehitu batazbesteko balorazioa
     batazbesteko.balorazioa <- dataBatazbestekoa()
     adostasun.info <- merge(adostasun.info, batazbesteko.balorazioa, by="Ariketa")
     
     # Oharrak izan dituztenak
     agr.oharrak <- filter(oharrak(),Ariketa <=20, !is.na(Balioa)) %>% group_by(Ariketa) %>% summarise(OharKopurua=n())

     adostasun.info<- merge(adostasun.info, agr.oharrak, by="Ariketa", all.x=T)
     adostasun.info$OharKopurua[is.na(adostasun.info$OharKopurua)] <- 0
     
     
     
     #
     adostasun.info
    #agr.oharrak
  })
  
  dataDistrAdostasunaAberastua <- reactive({
    list(data= dataDistrAdostasuna(), oharrak = filter(oharrak(), Ariketa<=20), balorazioak=filter(datuGuztiak(), Galdera==3, Ariketa<=20) %>% select(Irak, Ariketa, Mota, Balioa))
  })
  
  dataAriketaBakunak <- reactive({filter(dataAriketak(),Mota =="BAK")})
  
  dataAriketaAnitzak <- reactive({filter(dataAriketak(),Mota =="ANI")})
  
  
  
  irakasleenAgreementak <- reactive({
    irakArik <- filter(datuGuztiak(), Ariketa <=20, Galdera!=4) %>% distinct(Irak, Ariketa,Galdera)
    irakArikAdos <- merge(irakArik, adostasunDatuak(), by=c("Ariketa", "Galdera"))
    irakAdosMaila <- irakArikAdos %>% group_by(Irak, Galdera) %>% summarize(Adostasuna=mean(Adostasuna))
    irakAdosMaila
  })
  
  irakasleenDatuak <- reactive({
    irakasleen.balorazioak <- datuGuztiak() %>% filter(Galdera != 4)
    irakasleenDatuak <- merge(irakasleen.balorazioak, irakasleenAgreementak(), by=c("Irak","Galdera"))
    irakasleenDatuak
  })
  
  ## Lotu interfazea datu azterketarekin
  callModule(agreementModule, "adostasuna", dataAgreement)
  ## Hau agian mugituko dut
  callModule(exerciseAgreementViewModule, "hobetua", dataDistrAdostasunaAberastua)
  callModule(arikAzterketaModule,"guztiak",dataAriketak)
  callModule(arikAzterketaModule,"bakunak",dataAriketaBakunak)
  callModule(arikAzterketaModule,"anitzak",dataAriketaAnitzak)
  callModule(taulaModule,"datuak",dataTaula)
  callModule(taulaModule,"oharrak",oharrak)
  callModule(raterMonitoringViewModule,"irakasleak",irakasleenDatuak)
  
 
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
