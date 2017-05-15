# Ebaluatzaileen monitorizazio modulua

## Interfazeari lotutako kodea
raterMonitoringViewModuleUI <- function(id, testua) {
  ns <- NS(id)
  
  tagList(
    h3(testua),
    fluidRow(
      column(12, plotOutput(ns("raterPlot")))
    ),
    fluidRow(
      column(12, verbatimTextOutput(ns("testuan")))
    )
  )
}

## Zerbitzariari lotutako kodea
raterMonitoringViewModule <- function(input, output, session, data) {
  output$testuan <- renderPrint({data()})
  output$raterPlot <- renderPlot({
    plot.data <- data()
    
    ggplot(plot.data,aes(factor(Irak), y=as.numeric(Balioa), fill=Adostasuna)) + 
      xlab("Irakaslea") + 
      ylab("Balioa") + 
      geom_violin() + 
      geom_boxplot(width=0.1, fill="white") + 
      scale_fill_gradient() +
      facet_grid(~Galdera)
  })
}