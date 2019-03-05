library(ggplot2)
library(shinyAce)
library(shinythemes)
library(shinyWidgets)
library(stringi)
library(reshape2)
library(vcdExtra)
library(pander)
library(rmarkdown)
library(rms)
library(ggfortify)
library(party)
library(partykit)
library(rpart)
library(moonBook)
library(fBasics)
library(plotly)
library(prophet)
library(reshape2)
library(skimr)
library(madis)

options(shiny.maxRequestSize = 500*1024^2)

#rm(list=ls())
#gc()


as.numeric(Sys.time())->seed
sd=paste(path.package('madis'),'/app/',sep='')

ifelse(!dir.exists(wd),dir.create(wd),FALSE)
paste(wd,seed,sep='')->wd2
dir.create(wd2)
setwd(wd2)
file.copy(paste0(sd,'madisReportTemp.Rmd'),'madisReportTemp.Rmd')
file.copy(paste0(sd,'header.tex'),'header.tex')
#source(paste0(sd,'script.R'))

rm(list=c('seed','wd','wd2','sd'))
environment()->envMedstats
LstMedstats<-list()
LstMedstats$desc<-data.frame(xvars=NA,Digits=NA,dataName=NA,stringsAsFactors = F)
LstMedstats$hTest<-data.frame(xvars=NA,yvars=NA,alter=NA,paired=NA,nullHyp=NA,confLevel=NA,dataName=NA,stringsAsFactors = F)
LstMedstats$myGlm<-data.frame(Formula=NA,data=NA,weightsVar=NA,subset=NA,Family=NA,lower=NA)
LstMedstats$myTree<-data.frame(Formula=NA,data=NA,subset=NA,treeMethod=NA,Minsplit=NA,Minbucket=NA,Maxdepth=NA,CP=NA,Mincrit=NA)
LstMedstats$myCox<-data.frame(Formula=NA,data=NA,weightsVar=NA,subset=NA,strataVar=NA,lower=NA)
LstMedstats$myTable<-data.frame(Formula=NA,data=NA)
LstMedstats$myGplt<-data.frame(data=NA,x=NA,y=NA,size=NA,fill=NA,color=NA,shape=NA,alpha=NA,facetVar=NA,
                               geom=NA,smoothMethod=NA,barPos=NA,labx=NA,laby=NA,title=NA,Bins=NA,theme=NA,Width=NA,
                               Colour=NA,Fill=NA,Size=NA,Alpha=NA,Shape=NA)
LstMedstats$myProphet<-data.frame(data=NA,tsVar=NA, tsFormat=NA,measureVars=NA, groupVars = NA,Period = NA,FN=NA,
                                  Cap=NA,Floor=NA, Growth=NA,H=NA,yearlyS = NA,dailyS = NA,weeklyS = NA)
assign('LstMedstats',LstMedstats,env=envMedstats)


server<-function(input,output){
#appserverstart

  
  
  
  
  
  
  
  
#appserverstop
}


ui<-fluidPage(
  #shinythemes::themeSelector(),
  tags$head(
    tags$style(
      type="text/css", "
      #loadmessage {
      position: fixed;
      top: 0px;
      left: 0px;
      width: 100%;
      padding: 10px 0px 10px 0px;
      text-align: center;
      font-weight: bold;
      font-size: 100%;
      color: #000000;
      background-color: #CCFF66;
      z-index: 105;
      }
      ")
  ),
  conditionalPanel(condition="$('html').hasClass('shiny-busy')",tags$div("请耐心等待...",id="loadmessage")),
  
  
  
  
  navbarPage(
    'MADIS'
#appuistart
    
    
    





#appuistop
  )
)

shinyApp(ui = ui, server = server)
