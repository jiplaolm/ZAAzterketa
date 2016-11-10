# Agreement informazioa bistaratzeko modulua## Agreement-a erakusteko Shiny modulua

## Interfazeari lotutako kodea
agreementViewModuleUI <- function(id, testua) {
  ns <- NS(id)
  
  tagList(
    h3(testua),
    fluidRow(
      column(4,C3GaugeOutput(ns("irrPlot"))),
      column(4,verbatimTextOutput(ns("irr"))),
      column(4,taulaModuleUI(ns("agrData")))
    )
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
  
  output$irr <- renderPrint({kappa.data()})##
  output$irrPlot <- renderC3Gauge({C3Gauge(round(kappa.data()$value,2))})
  callModule(taulaModule,"agrData", ag.datuak)
}