# Ariketa mailan distraigarrien azterketa bistaratzeko modulua

## Interfazeari lotutako kodea
exerciseAgreementViewModuleUI <- function(id, testua) {
  ns <- NS(id)
  
  tagList(
    h3(testua),
    h4("Irakasleen arteko adostasuna ariketa bakoitzean (Distraigarrietan)"),
    fluidRow(
      plotOutput(ns("agrPlot"))
    ),
    h4("Distraigarrien balorazioak"),
    fluidRow(
      plotOutput(ns("distPlot"))
    ),
    #fluidRow(
    #  column(12,verbatimTextOutput(ns("info")))
    #),
    h4("Oharrak"),
    fluidRow(
      column(12, taulaModuleUI(ns("oharrakAdos")))
    )
  )
}

## Zerbitzariari lotutako kodea
exerciseAgreementViewModule <- function(input, output, session, data) {
  #output$info <- renderPrint({data()})
  
  oharrak <- reactive({data()$oharrak})
  
  # Erakutsi adostasun informazioa
  output$agrPlot <- renderPlot({
    plot.data <- data()$data
    plot.data$Ariketa <- as.numeric(plot.data$Ariketa)
    plot.data$baxua <- plot.data$Adostasuna<0.7 
    ggplot(plot.data, aes(x=as.factor(Ariketa), y=Adostasuna, color=Oharrak, shape=Mota)) + 
      geom_point( size=5) + #aes(shape=as.factor(Ev)),
      xlab("Ariketa") + 
      ylab("Ebaluatzaileen arteko adostasuna") +
      scale_color_manual(values=c("blue", "red"), labels=c("Ez", "Bai"))
  })
  
  # Erakutsi balorazioan informazioa
  output$distPlot <- renderPlot({
    plot.data <- data()$balorazioak
    plot.data$Ariketa <- as.numeric(plot.data$Ariketa)
 
    ggplot(plot.data, aes(x=as.factor(Ariketa), y=Balioa, color=Irak, shape=Mota)) + 
      geom_point( size=5) + #aes(shape=as.factor(Ev)),
      xlab("Ariketa") + 
      ylab("Balorazioa")  + geom_jitter()
  })
  
  # Erakutsi oharrak
  callModule(taulaModule,"oharrakAdos",oharrak)
}