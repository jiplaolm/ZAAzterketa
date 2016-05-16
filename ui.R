

# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(C3)
library(htmlwidgets)

shinyUI(fluidPage(
  # Application title
  titlePanel("Arikiturri: Zientzia ariketak"),
  sidebarLayout(sidebarPanel(## Erabiltzaileak erabili ditzaken aukerak
    helpText(
      "Aukerak hemen deskribatu"
    )),
    mainPanel(tabsetPanel(
      # Datu azterketa erakusteko
      # Lehenengo panela, irakasleen arteko adostasun informazioa
      tabPanel("Adostasuna",h1("Adostasuna"),fluidRow(column(6,C3GaugeOutput("irrPlot")),column(6,verbatimTextOutput("irr"))), h1("Erabiltzaile artekoa"),tableOutput("irakArtean")),
      # Ariketen inguruko informazioa
      tabPanel("Ariketen informazioa"),
      # Algoritmoka
      tabPanel("Algoritmo 1")
    )))
))
