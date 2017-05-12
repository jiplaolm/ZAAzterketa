# Ariketa mailan distraigarrien azterketa bistaratzeko modulua

## Interfazeari lotutako kodea
exerciseAgreementViewModuleUI <- function(id, testua) {
  ns <- NS(id)
  
  tagList(
    h3(testua),
    fluidRow(
      column(8,plotOutput(ns("agrPlot"))),
      column(4,verbatimTextOutput(ns("info")))
    )
  )
}

## Zerbitzariari lotutako kodea
exerciseAgreementViewModule <- function(input, output, session, data) {
  output$info <- renderPrint({data()})
  output$agrPlot <- renderPlot({
    plot.data <- data()
    plot.data$Ariketa <- as.numeric(plot.data$Ariketa)
    plot.data$baxua <- plot.data$Adostasuna<0.7 
    ggplot(plot.data, aes(x=as.factor(Ariketa), y=Adostasuna, color=baxua)) + 
      geom_point( size=5) + #aes(shape=as.factor(Ev)),
      xlab("Ariketa") + 
      ylab("Ebaluatzaileen arteko adostasuna") +
      scale_color_manual(values=c("blue", "red"))
  })
}