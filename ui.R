

# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(C3)
library(htmlwidgets)
library(shinyjs)
library(DT)

shinyUI(fluidPage(useShinyjs(),
  # Application title
  titlePanel("Arikiturri: Zientzia ariketak"),
  sidebarLayout(sidebarPanel(## Erabiltzaileak erabili ditzaken aukerak
    helpText(
      "Aukeratu datu azterketarako erabili nahi dituzun fitxategiak"
    ),
    checkboxInput("defektuzkoak","Erabili gordetako data",T),
    conditionalPanel("!input.defektuzkoak",
                     fileInput("fitx", "Aukeratu aztertu nahi duzun data daukaten fitxategiak (gutxienez 2)",
                               multiple=T,
                               accept=c("text/csv","text/comma-separated-values",".csv"))
                     )
    ),
    mainPanel(div(id="main",
      tabsetPanel(
      # Datu azterketa erakusteko
      # Lehenengo panela, irakasleen arteko adostasun informazioa
      tabPanel("Adostasuna",h1("Adostasuna"),fluidRow(column(6,C3GaugeOutput("irrPlot")),column(6,verbatimTextOutput("irr"))), h1("Erabiltzaile artekoa"),tableOutput("irakArtean")),
      # Ariketen inguruko informazioa
      tabPanel("Ariketen informazioa"),
      # Algoritmoka
      tabPanel("Hitz bakunekoak"),
      tabPanel("Hitz anitzekoak"),
      tabPanel("Datuak", DT::dataTableOutput("datuTaula"))))
    ))
))
