 


# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
source("libs.R")


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
                     ),
      helpText("Aukeratu aztertutako ariketen informazioa jasotzen duen fitxategia"),
      checkboxInput("defektuzkoInformazioa", "Erabili gordetako datuak", T),
      conditionalPanel("!input.defektuzkoInformazioa",
                     fileInput("fitxInfo", "Aukeratu aztertutako informazioa jasotzen duen fitxategia",
                               multiple=F,
                               accept=c("text/csv","text/comma-separated-values",".csv"))
    )
      
    ),
    mainPanel(div(id="main",
      tabsetPanel(
      # Datu azterketa erakusteko
      # Lehenengo panela, irakasleen arteko adostasun informazioa
      tabPanel("Adostasuna",agreementModuleUI("adostasuna")),
      # irakasleen arteko adostasun informazioa bakunetan
      tabPanel("Adostasuna Bakunak",agreementModuleUI("adostasunaBakunak")), 
      # Lehenengo panela, irakasleen arteko adostasun anitzetan
      tabPanel("Adostasuna Anitzak",agreementModuleUI("adostasunaAnitzak")),
      # Ariketa mailako distraigarrien adostasuna
      tabPanel("Distraigarrien Adostasuna hobetua",exerciseAgreementAnalysisModuleUI("hobetua", "Distraigarrien informazioa hobetua")),
      # Ariketa mailako distraigarrien adostasuna - BAK
      tabPanel("Distraigarrien Adostasuna hobetua - BAK",exerciseAgreementAnalysisModuleUI("hobetuaBAK", "Distraigarrien informazioa hobetua - Bakunak")),
      # Ariketa mailako distraigarrien adostasuna - ANI
      tabPanel("Distraigarrien Adostasuna hobetua - ANI",exerciseAgreementAnalysisModuleUI("hobetuaANI", "Distraigarrien informazioa hobetua- Hitz Anitzak")),
      # Ariketen inguruko informazioa
      tabPanel("Ariketen informazioa", arikAzterketaModuleUI("guztiak")),
      # Ariketa motaka
      tabPanel("Hitz bakunekoak", arikAzterketaModuleUI("bakunak")),
      tabPanel("Hitz anitzekoak", arikAzterketaModuleUI("anitzak")),
      tabPanel("Datuak", taulaModuleUI("datuak")),
      tabPanel("Oharrak", taulaModuleUI("oharrak")),
      tabPanel("Ebaluatzaileak", raterMonitoringViewModuleUI("irakasleak","Irakasleen balorazioen azterketa"))
      )
    ))
)))
