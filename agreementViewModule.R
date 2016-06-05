# Agreement informazioa bistaratzeko modulua## Agreement-a erakusteko Shiny modulua

## Interfazeari lotutako kodea
agreementViewModuleUI <- function(id, testua) {
  ns <- NS(id)
  
  tagList(
    h3(testua),
    fluidRow(
      column(6,C3GaugeOutput(ns("irrPlot"))),
      column(6,verbatimTextOutput(ns("irr")))
    )
  )
}

## Zerbitzariari lotutako kodea
agreementViewModule <- function(input, output, session, data) {
  
  kappa.data <- reactive({ 
    mat <- data()

    ag.datuak <- filtratuBalorazioakAdostarunerako(aukeratuEbaluazioZutabeak(mat))
    kappa2(ag.datuak,weight = "squared")
    })
  
  output$irr <- renderPrint({kappa.data()})##
  output$irrPlot <- renderC3Gauge({C3Gauge(round(kappa.data()$value,2))})
}