
#' Madis
#'
#' is a launcher function
#'
#' @export
Madis <- function(wd='~/',lang=c('ch','eng')[1]) {
  #setwd(wd)
  #dir.create('app')
  sd=paste(path.package('madis'),'/app/',sep='')
  #file.copy(paste0(sd,'header.tex'),wd)
  readLines(paste0(sd,'appCN.R'))->app
  c(paste("wd=",paste('\"',wd,'\"',sep=''),sep=''),app,'shinyApp(ui = ui, server = server)')->appNew
  writeLines(appNew,paste0(sd,'app.R'))
  
  library(shiny)
  
  
  runApp(system.file("app", package = "madis"), launch.browser = TRUE)
}





