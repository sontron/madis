#' fnPlotly
#' 
#' quick plotly based on ggplt2S. along with Filter param.
#' 
#' use ggplt2S params in ...
#' 
#' @examples 
#' 
#' fnPlotly(dt=mtcars,x='disp',y='mpg',geom=c('point','smooth'),Colour='red',alpha=.5,Filter=c('vs','gear','am'))
#' 
#' @export

fnPlotly<-function(dt,...,Filter=NULL){
  as.data.frame(dt)->dt
  if (interactive()) {
    library(shiny)
    library(DT)
    shinyApp(
      ui = fluidPage(
        sidebarLayout(
          sidebarPanel(
            panel(
              heading = 'Filters setting',
              uiOutput('more1')
            )
            
          ),
          mainPanel(
            panel(
              heading = 'Plotly result',
              plotlyOutput('plotly',height='900px')
            )
            
            
          )
        )
        
      ),
      server = function(input, output) {
        
        output$more1<-renderUI({
          list(
            if(is.null(Filter)){
              NULL
            } else {
              shinyFilter(dt,Filter)
            }
            
          )
          
        })
        
        output$plotly = renderPlotly({
          if(is.null(Filter)){
            dtt<-dt
          } else {
            
            indMat<-sapply(Filter,function(i){
              if(class(dt[,i])%in%c('character','factor')){
                dt[,i]%in%input[[i]]
              } else {
                dt[,i]>=input[[i]][1]&dt[,i]<=input[[i]][2]
                
              }
            })
            
            apply(indMat,1,all)->Ind
            dtt<-dt[Ind,]
            
            # dtt<-dt[dt[,slider]>=input$range[1]&dt[,slider]<=input$range[2],]
          }
          
          ggplt2S(data=dtt,...)$resPlotly
        })
      }
    )
  }
  
}



#' fnDT
#' 
#' render a DT in shiny
#' 
#' @examples 
#' fnDT(mtcars)
#' 
#' @export


fnDT<-function(dt){
  as.data.frame(dt)->dt
  if (interactive()) {
    library(shiny)
    library(DT)
    library(shinyWidgets)
    shinyApp(
      ui = fluidPage(
        sidebarLayout(
          sidebarPanel(
            panel(
              heading = 'vars to keep',
              uiOutput('more1')
            )
            
          ),
          mainPanel(
            panel(
              heading = 'DataTable Outputs',
              DTOutput('tbl')
            )
            
          )
        )
      ),
      server = function(input, output) {
        
        output$more1<-renderUI({
          list(
            pickerInput(inputId = 'varsKeep',
                        label = 'choose vars to show',
                        choices = names(dt),
                        selected = names(dt),
                        multiple = T)
          )
        })
        
        output$tbl = renderDT(
          datatable(dt[,input$varsKeep,drop=F]),
          server=T,filter='top'
        )
      }
    )
  }
}



#' shinyFilter
#' 
#' automaticly generate filter inputs
#' 
#' @export

shinyFilter<-function(dt,filter=Filter){
  renderUI({
    lapply(filter,function(i){
      if(class(dt[,i])%in%c('character','factor')){
        pickerInput(i,i,choices = unique(dt[,i]),selected = unique(dt[,i]),multiple = T)
      } else {
        numericRangeInput(i,i,value=c(min(dt[,i],na.rm=T),max(dt[,i],na.rm=T)))
      }
    })
  })
}
