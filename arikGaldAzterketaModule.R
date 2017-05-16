# Ariketa bakoitzeko galdera bati dagokion informazioa aurkezteko modulua

## Interfazeari lotutako kodea
arikGaldAzterketaModuleUI <- function(id, testua) {
  ns <- NS(id)
  
  tagList(
    h3(testua),
    verbatimTextOutput(ns("testua")),
    plotOutput(ns("plot"))
    
    # Besteak gehitu
  )
}

## Zerbitzariari lotutako kodea
arikGaldAzterketaModule<- function(input, output, session, data) {
  likert.data <- reactive({
    datuak <- data()
    datuak$Balioa <- factor(datuak$Balioa, levels = 1:4,
                            labels = c("Guztiz kontra",
                                       "Ez oso ados", 
                                       "Nahiko ados",
                                       "Guztiz ados"))
    likert(items=datuak[,6,drop=F])
  })
  output$testua <- renderPrint(likert.data())
  output$plot <- renderPlot({
    plot <- likert.bar.plot(likert.data(),plot.percents=T, legend="Erantzuna") + ylab("Portzentaia")
    LHS <- 2
    RHS <- 3
    if (sum(is.na(plot$layers[[LHS]]$data$Item))>0) plot$layers <- plot$layers[-LHS]
    if (sum(is.na(plot$layers[[RHS]]$data$Item))>0) plot$layers <- plot$layers[-RHS]
    plot
    })
}