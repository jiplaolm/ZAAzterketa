## Ariketen datu azterketarako modulua

## Interfazeari lotutako kodea
arikAzterketaModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h4("Momentuz irakasle Arik21tik aurrera bakarrik erakusten dira",style="color:red"),
    arikGaldAzterketaModuleUI(ns("galdera1"),"Ariketak 'Izarrei begira' unitateko zientziako kontzeptu bat lantzeko balio du"),
    br(),
    arikGaldAzterketaModuleUI(ns("galdera2"),"Aukera bakarra da argi eta garbi erantzun zuzena"),
    br(),
    arikGaldAzterketaModuleUI(ns("galdera3"),"Distraigarriak bere funtzioa betetzen du. Distraigarria baliagarria da")
  )
}

## Zerbitzariari lotutako kodea
arikAzterketaModule<- function(input, output, session, data) {
  dataGald1 <- reactive({
    filter(data(), Galdera==1)
  })
  dataGald2 <- reactive({
    filter(data(), Galdera==2)
  })
  dataGald3 <- reactive({
    filter(data(), Galdera==3)
  })
  callModule(arikGaldAzterketaModule, "galdera1", dataGald1)
  callModule(arikGaldAzterketaModule, "galdera2", dataGald2)
  callModule(arikGaldAzterketaModule, "galdera3", dataGald3)
}