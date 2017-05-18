# Agreement informazioa bistaratzeko modulua## Agreement-a erakusteko Shiny modulua

## Interfazeari lotutako kodea
agreementViewModuleUI <- function(id, testua) {
  ns <- NS(id)
  
  tagList(
    h3(testua),
    fluidRow(
      column(8, 
        h4("Kappa"),
        fluidRow(
          column(4,C3GaugeOutput(ns("irrPlot"))),
          column(4,verbatimTextOutput(ns("irr")))
        ),
        h4("Gwet's AC"),
        fluidRow(
          column(4,C3GaugeOutput(ns("gwetPlot"))),
          column(4,verbatimTextOutput(ns("gwetScore")))),
        h4("Adostasun portzentaia"),
        fluidRow(
          column(4,C3GaugeOutput(ns("percPlot"))),
          column(4,verbatimTextOutput(ns("perc")))),
        h4("Datuak")),
    
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
  
  gwet <- reactive({ 
    data.frame(Gwet=gac(ag.datuak(),kat=4,weight = "quadratic")$est)
  })
  
  ados.portz <- reactive({
    erantzunak <- ag.datuak()
    ados <- erantzunak[,1]==erantzunak[,2]
    data.frame(Percentage=sum(ados)/length(ados))
  })
  
  output$irr <- renderPrint({kappa.data()})##
  output$perc <- renderPrint({ados.portz()})
  output$gwetScore <- renderPrint({gwet()})
  output$irrPlot <- renderC3Gauge({C3Gauge(round(kappa.data()$value,2))})
  output$percPlot <- renderC3Gauge({C3Gauge(round(ados.portz()$Percentage,2))})
  output$gwetPlot <- renderC3Gauge({C3Gauge(round(gwet()$Gwet,2))})
  callModule(taulaModule,"agrData", ag.datuak)
}