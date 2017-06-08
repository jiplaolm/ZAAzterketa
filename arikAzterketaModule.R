## Ariketen datu azterketarako modulua

## Interfazeari lotutako kodea
arikAzterketaModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h4("Momentuz Arik21tik aurrera bakarrik erakusten dira",style="color:red"),
    br(),
    arikAzterketaViewModuleUI(ns("guztiak"),"Guztiak kontuan hartuta"),
    br(),
    arikAzterketaViewModuleUI(ns("baseline"),"Baseline heuristikoarekin sortutakoak"),
    br(),
    arikAzterketaViewModuleUI(ns("mono"),"Monosemikoentzako heuristikoarekin sortutakoak"),
    br(),
    arikAzterketaViewModuleUI(ns("poli"),"Polisemikoentzako heuristikoarekin sortutakoak")
  )
}

## Zerbitzariari lotutako kodea
arikAzterketaModule<- function(input, output, session, data) {
  baseline <- reactive({
    filter(data(), Heuristikoa=="baseline")
  })
  mono <- reactive({
    filter(data(), Heuristikoa=="mono")
  })
  poly <- reactive({
    filter(data(), Heuristikoa=="poly")
  })
  callModule(arikAzterketaViewModule, "guztiak", data)
  callModule(arikAzterketaViewModule, "baseline", baseline)
  callModule(arikAzterketaViewModule, "mono", mono)
  callModule(arikAzterketaViewModule, "poli", poly)
}