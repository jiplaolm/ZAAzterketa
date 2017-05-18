# Ariketa mailan distraigarrien azterketa bistaratzeko modulua

## Interfazeari lotutako kodea
exerciseAgreementViewModuleUI <- function(id, testua) {
  ns <- NS(id)
  
  tagList(
    h3(testua),
    h4("Irakasleen arteko adostasuna ariketa bakoitzean (Distraigarrietan) - Ohar kopurua adierazita"),
    fluidRow(
      plotOutput(ns("agrPlot"))
    ),
    h4("Irakasleen arteko adostasuna ariketa bakoitzean (Distraigarrietan) - balorazioa adierazita"),
    fluidRow(
      plotOutput(ns("agrBalPlot"))
    ),
    h4("Distraigarrien balorazioak"),
    fluidRow(
      plotOutput(ns("distPlot"))
    ),
   # fluidRow(
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
 # output$info <- renderPrint({data()})
  
  oharrak <- reactive({data()$oharrak})
  
  # Erakutsi adostasun informazioa
  output$agrPlot <- renderPlot({
    plot.data <- data()$data
    plot.data$Ariketa <- as.numeric(plot.data$Ariketa)
    plot.data$baxua <- plot.data$Adostasuna<0.7 
    ggplot(plot.data, aes(x=as.factor(Ariketa), y=Adostasuna, color=as.factor(OharKopurua), shape=Mota)) + 
      geom_point( size=5) + #aes(shape=as.factor(Ev)),
      xlab("Ariketa") + 
      ylab("Ebaluatzaileen arteko adostasuna") +
      scale_color_manual(name="Ohar kopurua", values=c("0"="green", "1"="blue","2"= "red"))
  })
  
  # Erakutsi adostasun informazioa balorazioen arabera
  output$agrBalPlot <- renderPlot({
    plot.data <- data()$data
    plot.data$Ariketa <- as.numeric(plot.data$Ariketa)
    plot.data$baxua <- plot.data$Adostasuna<0.7 
    ggplot(plot.data, aes(x=as.factor(Ariketa), y=Adostasuna, color=Batazbestekoa, shape=Mota)) + 
      geom_point( size=5) + #aes(shape=as.factor(Ev)),
      xlab("Ariketa") + 
      ylab("Ebaluatzaileen arteko adostasuna") +
      scale_color_gradient()
  })
  
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