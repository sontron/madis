
#' Madis
#'
#' is a launcher function
#'
#' @export
Madis <- function(wd=paste0(getwd(),'/'),lang=c('ch','eng')[1],Encod='utf8') {
  #setwd(wd)
  #dir.create('app')
  sd=paste(path.package('madis'),'/app/',sep='')
  #file.copy(paste0(sd,'header.tex'),wd)
  readLines(paste0(sd,'appCN.R'),encoding = Encod)->app
  c(paste("wd=",paste('\"',wd,'\"',sep=''),sep=''),app)->appNew
  writeLines(appNew,paste0(sd,'app.R'))
  
  library(shiny)
  
  
  runApp(system.file("app", package = "madis"), launch.browser = TRUE)
}





