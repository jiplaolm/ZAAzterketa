# Ariketa mailan distraigarrien azterketa bistaratzeko modulua

## Interfazeari lotutako kodea
exerciseAgreementViewModuleUI <- function(id, testua) {
  ns <- NS(id)
  
  tagList(
    h3(testua),
    h4("Batazbesteko adostasun maila"),
    fluidRow(
      column(5, C3GaugeOutput(ns("avgAgr")))
    ),
    h4("Irakasleen arteko adostasuna ariketa bakoitzean (Distraigarrietan) - Ohar kopurua adierazita"),
    fluidRow(
      plotOutput(ns("agrPlot"))
    ),
    h4("Irakasleen arteko adostasuna ariketa bakoitzean (Distraigarrietan) - balorazioa adierazita"),
    fluidRow(
      plotOutput(ns("agrBalPlot"))
    ),
    h4("Irakasleen arteko adostasuna ariketa bakoitzean (Distraigarrietan) - erabilitako heuristikoa adierazita"),
    fluidRow(
      plotOutput(ns("agrHeuristPlot"))
    )
  )
}

## Zerbitzariari lotutako kodea
exerciseAgreementViewModule <- function(input, output, session, data) {

  batazbestekoa <- reactive({mean(data()$Adostasuna)})
  
  # Erakutsi batazbesteko adostasuna
  output$avgAgr <- renderC3Gauge({C3Gauge(round(batazbestekoa(),2))})
  
  # Erakutsi adostasun informazioa
  output$agrPlot <- renderPlot({
    plot.data <- data()
    plot.data$Ariketa <- as.numeric(plot.data$Ariketa)
    ggplot(plot.data, aes(x=as.factor(Ariketa), y=Adostasuna, color=as.factor(OharKopurua), shape=Mota)) + 
      geom_point( size=5) + #aes(shape=as.factor(Ev)),
      xlab("Ariketa") + 
      ylab("Ebaluatzaileen arteko adostasuna") +
      scale_color_manual(name="Ohar kopurua", values=c("0"="green", "1"="blue","2"= "red")) + 
      geom_hline(yintercept = batazbestekoa(), linetype="dashed", color="blue")
  })
  
  # Erakutsi adostasun informazioa balorazioen arabera
  output$agrBalPlot <- renderPlot({
    plot.data <- data()
    plot.data$Ariketa <- as.numeric(plot.data$Ariketa)
    ggplot(plot.data, aes(x=as.factor(Ariketa), y=Adostasuna, color=Batazbestekoa, shape=Mota)) + 
      geom_point( size=5) + #aes(shape=as.factor(Ev)),
      xlab("Ariketa") + 
      ylab("Ebaluatzaileen arteko adostasuna") +
      scale_color_gradient()+ 
      geom_hline(yintercept = batazbestekoa(), linetype="dashed", color="blue")
  })
  
  
  # Erakutsi adostasun informazioa balorazioen arabera
  output$agrHeuristPlot <- renderPlot({
    plot.data <- data()
    plot.data$Ariketa <- as.numeric(plot.data$Ariketa)
    ggplot(plot.data, aes(x=as.factor(Ariketa), y=Adostasuna, color=Heuristikoa, shape=Mota)) + 
      geom_point( size=5) + #aes(shape=as.factor(Ev)),
      xlab("Ariketa") + 
      ylab("Ebaluatzaileen arteko adostasuna") +
      scale_color_discrete()+ 
      geom_hline(yintercept = batazbestekoa(), linetype="dashed", color="blue")
  })
  
}