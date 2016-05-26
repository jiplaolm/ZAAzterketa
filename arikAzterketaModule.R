## Ariketen datu azterketarako modulua

## Interfazeari lotutako kodea
arikAzterketaModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h1("Ariketak unitateko zientziako kontzeptu bat lantzeko balio du"),
    h1("Ariketak argi eta garbi erantzun zuzen bakarra du"),
    h1("Distraigarriak baliagarriak dira")
  )
}

## Zerbitzariari lotutako kodea
arikAzterketaModule<- function(input, output, session, data) {
 
}