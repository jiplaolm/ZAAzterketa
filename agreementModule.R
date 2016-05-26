## Agreement-a erakusteko Shiny modulua

## Interfazeari lotutako kodea
agreementModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h1("Adostasuna"),
    fluidRow(
      column(6,C3GaugeOutput(ns("irrPlot"))),
      column(6,verbatimTextOutput(ns("irr")))
    ),
    h1("Erabiltzaile artekoa"),
    tableOutput(ns("irakArtean"))
  )
}

## Zerbitzariari lotutako kodea
agreementModule <- function(input, output, session, data) {
  adostasun <- reactive({adostasunak(data())})
  
  
  kappa.data <- reactive({ 
    matrix <- data()
    kappa2(matrix[, c(2,3)],weight="squared")})
  
  #output$irr <- renderPrint({kappam.fleiss(data[, 2:ncol(data)],exact=F,detail=T)})
  output$irr <- renderPrint({kappa.data()})##
  output$irrPlot <- renderC3Gauge({C3Gauge(kappa.data()$value)})
  output$irakArtean <- renderTable({adostasun()})
}