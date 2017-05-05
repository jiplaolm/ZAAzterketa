# Agreement informazioa bistaratzeko modulua## Agreement-a erakusteko Shiny modulua

## Interfazeari lotutako kodea
agreementViewModuleUI <- function(id, testua) {
  ns <- NS(id)
  
  tagList(
    h3(testua),
    h4("Kappa"),
    fluidRow(
      column(4,C3GaugeOutput(ns("irrPlot"))),
      column(4,verbatimTextOutput(ns("irr"))),
      column(4,taulaModuleUI(ns("agrData")))
    ),
    h4("Adostasun portzentaia"),
    fluidRow(
      column(4,C3GaugeOutput(ns("percPlot"))),
      column(4,verbatimTextOutput(ns("perc"))))
  )
}

## Zerbitzariari lotutako kodea
agreementViewModule <- function(input, output, session, data) {
  ag.datuak <- reactive({
    mat <- data()
    filtratuBalorazioakAdostarunerako(aukeratuEbaluazioZutabeak(mat))
  })
  
  kappa.data <- reactive({ 
    kappa2(ag.datuak(),weight = "squared")
    })
  
  ados.portz <- reactive({
    erantzunak <- ag.datuak()
    erantzunak$ados <- erantzunak[,1]==erantzunak[,2]
    data.frame(Percentage=sum(erantzunak$ados)/length(erantzunak$ados))
  })
  
  output$irr <- renderPrint({kappa.data()})##
  output$perc <- renderPrint({ados.portz()})
  output$irrPlot <- renderC3Gauge({C3Gauge(round(kappa.data()$value,2))})
  output$percPlot <- renderC3Gauge({C3Gauge(round(ados.portz()$Percentage,2))})
  callModule(taulaModule,"agrData", ag.datuak)
}