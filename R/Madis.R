
#' Madis
#'
#' is a launcher function
#'
#' @export
Madis <- function(wd=paste0(getwd(),'/'),lang=c('ch','en')[1],Encod='utf8') {
  #setwd(wd)
  #dir.create('app')
  sd=paste(path.package('madis'),'/app/',sep='')
  if(lang=='ch'){
    readLines(paste0(sd,'appCN.R'),encoding = Encod)->app
    c(paste("wd=",paste('\"',wd,'\"',sep=''),sep=''),app)->appNew
    writeLines(appNew,paste0(sd,'app.R'))
  } else {
    readLines(paste0(sd,'appEN.R'),encoding = Encod)->app
    c(paste("wd=",paste('\"',wd,'\"',sep=''),sep=''),app)->appNew
    writeLines(appNew,paste0(sd,'app.R'))
  }
  #file.copy(paste0(sd,'header.tex'),wd)
  
  
  library(shiny)
  
  
  runApp(system.file("app", package = "madis"), launch.browser = TRUE)
}





