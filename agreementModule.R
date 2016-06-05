## Agreement-a erakusteko Shiny modulua

## Interfazeari lotutako kodea
agreementModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    agreementViewModuleUI(ns("galdera1"),"Ariketak 'Izarrei begira' unitateko zientziako kontzeptu bat lantzeko balio du"),
    br(),
    agreementViewModuleUI(ns("galdera2"),"Aukera bakarra da argi eta garbi erantzun zuzena"),
    br(),
    agreementViewModuleUI(ns("galdera3"),"Distraigarriak bere funtzioa betetzen du. Distraigarria baliagarria da")
  )
}

## Zerbitzariari lotutako kodea
agreementModule <- function(input, output, session, data) {
  dataGald1 <- reactive({
    filter(data(), Galdera==1)
  })
  dataGald2 <- reactive({
    filter(data(), Galdera==2)
  })
  dataGald3 <- reactive({
    filter(data(), Galdera==3)
  })
  callModule(agreementViewModule, "galdera1", dataGald1)
  callModule(agreementViewModule, "galdera2", dataGald2)
  callModule(agreementViewModule, "galdera3", dataGald3)
}