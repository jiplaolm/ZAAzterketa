## Datuak taula formatuan adierazteko modulua

## Interfazeari lotutako kodea
taulaModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    DT::dataTableOutput(ns("datuTaula"))
  )
}

## Zerbitzariari lotutako kodea
taulaModule<- function(input, output, session, data) {
  output$datuTaula <- DT::renderDataTable({data()})
}