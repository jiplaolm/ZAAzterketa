
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

source("libs.R") # Interfaze grafikorak
theme_set(theme_bw(base_size=14)) # Theme zehaztu

shinyServer(function(input, output) {
  
  # Datuak prest?
  fitxategiak.prest <- reactive({
    if (input$defektuzkoak) {
      return(T)
    }
    else {
      files <- input$fitx
      if(is.null(files)||nrow(files)<2) {
        return(F)
      }
      else{
        return(T)
      }
    }
  })
  
  ariketenInfo.prest <- reactive({
    if (input$defektuzkoInformazioa) {
      return(T)
    }
    else {
      files <- input$fitxInfo
      if(is.null(files)||nrow(files)!=1) {
        return(F)
      }
      else{
        return(T)
      }
    }
  })
  
  
  
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
  
  ## 
  ariketenInfoFitxategia <- reactive({
    if (input$defektuzkoInformazioa) {
      return (defektuzkoInfoFitxategiak())
    }
    else {
      req(input$fitxInfo)
      req(nrow(input$fitxInfo)==1)
      return(input$fitxInfo$datapath)
    }
    
  })
  
  ariketenInformazioa <- reactive({
    infoFitxategiakIrakurri(ariketenInfoFitxategia())
  })
  
  
  
  ## Datuak prestatu - Egokituta (Motak eta heuristikoa informazio fitxategitik jasota)
  datuGuztiak <- reactive({
  
    data <- fitxategiakIrakurri(fitxategiak())
    ## Ezabatu mota eta gehitu ariketen informaziotik jasotako mota eta erabilitako heuristikoa
    #
    data <- data %>% select(-Mota)
    data <- merge(data, ariketenInformazioa(), by="Ariketa")
    
    return(data)
  })
  

  
  
  oharrak <- reactive({
    data <- fitxategiakIrakurri(fitxategiak())
    ebaluazioDatuak <- select(subset(data, Galdera!=4),-Distraigarria, -Mota)
    galderaIzenak <- c('G1', 'G2', 'G3.1', 'G3.2', 'G3.3')
   
    ebaluazioDatuak$Galdera <- rep(galderaIzenak,nrow(ebaluazioDatuak)/length(galderaIzenak))
    ebaluazioak <- spread(ebaluazioDatuak,Galdera, Balioa)
    ebalOharrak <- select(filter(subset(datuGuztiak(),Galdera==4),!is.na(Balioa)),-Galdera,-Distraigarria)
    
    result <- merge(ebaluazioak, ebalOharrak, by=c('Irak', 'Ariketa'), all.y=T)
      return(result)
    })
  
  data <- reactive({
    mutate(filter(datuGuztiak(),Galdera!=4), Balioa=as.numeric(Balioa))
  })
  dataTaula <- reactive({matrizeEgit(data())})
  
  dataAgreement <- reactive({
    datuak<-filter(data(),Ariketa <= 20)
    matrizeEgit(datuak)
  })
  
  dataAgreementBakunak <- reactive({
    datuak<-filter(data(),Ariketa <= 20, Mota=="BAK")
    matrizeEgit(datuak)
  })
  
  dataAgreementAnitzak <- reactive({
    datuak<-filter(data(),Ariketa <= 20,Mota =="ANI")
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
  
  dataBatEtorri <- reactive( {
    ados.datuak <- filter(data(), Ariketa<=20, Galdera != 4)
    #
    kalkulatuBatEtortzeak(ados.datuak)
  })
  
  ariketenHeuristikoak <- reactive({
    select(data(), Ariketa, Heuristikoa)
  })
  
  adostasunAriketenHeuristikoak <- reactive({
    filter(ariketenHeuristikoak(), Ariketa<=20)
  })
  
  dataDistrAdostasuna <- reactive({
   # 
    adostasun.info <- filter(adostasunDatuak(), Galdera == 3) %>% select(Ariketa,Adostasuna)
    
    ## Gehitu bat etortzea
    bat.etortzea <- filter(dataBatEtorri(), Galdera==3)
    adostasun.info <- merge(adostasun.info,bat.etortzea, by="Ariketa")
     
     ## Gehitu Galdera mota eta oharrak
     ariketa.motak <- filter(galdera.motak(),Ariketa <= 20)
     adostasun.info <- merge(adostasun.info, ariketa.motak,by= "Ariketa")
     
     ## Gehitu batazbesteko balorazioa
     batazbesteko.balorazioa <- dataBatazbestekoa()
     adostasun.info <- merge(adostasun.info, batazbesteko.balorazioa, by="Ariketa")
     
     ## Gehitu heuristikoa
     heuristikoak <- adostasunAriketenHeuristikoak()
     adostasun.info <- merge(adostasun.info, heuristikoak, by="Ariketa")
     
     # Oharrak izan dituztenak
     agr.oharrak <- filter(oharrak(),Ariketa <=20, !is.na(Balioa)) %>% group_by(Ariketa) %>% summarise(OharKopurua=n())

     adostasun.info<- merge(adostasun.info, agr.oharrak, by="Ariketa", all.x=T)
     adostasun.info$OharKopurua[is.na(adostasun.info$OharKopurua)] <- 0
     
     
     
     #
     adostasun.info
    #agr.oharrak
  })
  
  dataDistrAdostasunaAberastua <- reactive({
    list(data= dataDistrAdostasuna(), oharrak = filter(oharrak(), Ariketa<=20), balorazioak=filter(datuGuztiak(), Galdera==3, Ariketa<=20) %>% select(Irak, Ariketa, Mota, Balioa, Heuristikoa))
  })
  
  dataDistrAdostasunaAberastuaBAK <- reactive({
    dataFil <- dataDistrAdostasuna() %>% filter(Mota=="BAK")
    list(data= dataFil, oharrak = filter(oharrak(), Ariketa<=20), balorazioak=filter(datuGuztiak(), Galdera==3, Ariketa<=20) %>% select(Irak, Ariketa, Mota, Balioa, Heuristikoa))
  })
  
  dataDistrAdostasunaAberastuaANI <- reactive({
    dataFil <- dataDistrAdostasuna() %>% filter(Mota=="ANI")
    list(data= dataFil, oharrak = filter(oharrak(), Ariketa<=20), balorazioak=filter(datuGuztiak(), Galdera==3, Ariketa<=20) %>% select(Irak, Ariketa, Mota, Balioa, Heuristikoa))
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
  callModule(agreementModule, "adostasunaBakunak", dataAgreementBakunak) 
  callModule(agreementModule, "adostasunaAnitzak", dataAgreementAnitzak)
  ## Hau agian mugituko dut
  callModule(exerciseAgreementAnalysisModule, "hobetua", dataDistrAdostasunaAberastua)
  callModule(exerciseAgreementAnalysisModule, "hobetuaBAK", dataDistrAdostasunaAberastuaBAK)
  callModule(exerciseAgreementAnalysisModule, "hobetuaANI", dataDistrAdostasunaAberastuaANI)
  callModule(arikAzterketaModule,"guztiak",dataAriketak)
  callModule(arikAzterketaModule,"bakunak",dataAriketaBakunak)
  callModule(arikAzterketaModule,"anitzak",dataAriketaAnitzak)
  callModule(taulaModule,"datuak",dataTaula)
  callModule(taulaModule,"oharrak",oharrak)
  callModule(raterMonitoringViewModule,"irakasleak",irakasleenDatuak)
  
 
 # Panelak ezkutatzeko informazioa ez daukagunean
 observeEvent(input$defektuzkoak, {
   if (fitxategiak.prest() & ariketenInfo.prest()){
     show("main")
   }
   else {
     hide("main")
   }
 })
 
 observeEvent(input$defektuzkoInformazioa, {
   if (fitxategiak.prest() & ariketenInfo.prest()){
     show("main")
   }
   else {
     hide("main")
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

 
 observeEvent(input$fitxInfo, {
   files <- input$fitxInfo
   if(is.null(files)||nrow(files)!=1) {
     hide("main")}
   else{
     show("main")
   }
 })
 
})
