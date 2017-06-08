# Ariketa mailan distraigarrien azterketa bistaratzeko modulua

## Interfazeari lotutako kodea
exerciseAgreementAnalysisModuleUI <- function(id, testua) {
  ns <- NS(id)
  
  tagList(
    h3(testua),
    br(),
    exerciseAgreementViewModuleUI(ns("Gwet"),"Gwet's AC erabiliz"),
    br(),
    exerciseAgreementViewModuleUI(ns("Bat"),"Balorazioen batetorzeak erabiliz"),
    br(),
    h3("Distraigarrien balorazioak"),
    fluidRow(
      plotOutput(ns("distPlot"))
    ),
    # fluidRow(
    #  column(12,verbatimTextOutput(ns("info")))
    #),
    br(),
    h3("Oharrak"),
    fluidRow(
      column(12, taulaModuleUI(ns("oharrakAdos")))
    )
  )
}

## Zerbitzariari lotutako kodea
exerciseAgreementAnalysisModule <- function(input, output, session, data) {
  # output$info <- renderPrint({data()})
  
  oharrak <- reactive({data()$oharrak})
  
  gwetData <- reactive({
    gwetInfo <- data()$data 
    gwetInfo
  })
  
  batData <- reactive({
    batInfo <- data()$data 
    batInfo <- batInfo %>% select(-Adostasuna) %>% mutate(Adostasuna=BatEtortzea)
    batInfo
  })
  
  # Erakutsi adostasun informazioa GWET
  callModule(exerciseAgreementViewModule, "Gwet", gwetData)
  
  # Erakutsi batetortze informazioa
  callModule(exerciseAgreementViewModule, "Bat", batData)
  
  # Erakutsi balorazioan informazioa
  output$distPlot <- renderPlot({
    plot.data <- data()$balorazioak
    plot.data$Ariketa <- as.numeric(plot.data$Ariketa)
    
    ggplot(plot.data, aes(x=as.factor(Ariketa), y=Balioa, color=Irak, shape=Mota)) + 
      #geom_point( size=5) + #aes(shape=as.factor(Ev)),
      xlab("Ariketa") + 
      ylab("Balorazioa")  + geom_jitter(width=.25, size=5, height = .25, alpha=.75)
  })
  
  # Erakutsi oharrak
  callModule(taulaModule,"oharrakAdos",oharrak)
}