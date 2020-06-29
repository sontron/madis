#' qPlotly
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

qGraph<-function(dt,...){
  require(stringi)
  require(plotly)
  require(ggplot2)
  require(shiny)
  require(shinyWidgets)
  require(showtext)
  as.data.frame(dt)->dt
  if (interactive()) {
    shinyApp(options=list(...),
             ui = fluidPage(
               sidebarLayout(
                 sidebarPanel(
                   panel(
                     heading = 'Filter setting',
                     uiOutput('more0_Filter'),
                     uiOutput('more1_Filter'),
                     status = 'primary'
                   ),
                   
                   panel(
                     heading = 'Plot settings',
                     uiOutput('more2_myGplt'),
                     status = 'primary'
                   ),
                   actionBttn('go_myGplt','confirm')
                 ),
                 mainPanel(
                   uiOutput('more3_myGplt'),
                   downloadButton('downloadGraph','download graph')
                 )
               )
               
             ),
             server = function(input, output) {
               
               
               
               output$more0_Filter<-renderUI({
                 list(
                   pickerInput('Filter','choose filter vars',choices = c('none'='',names(dt)),selected ='无',multiple=T )
                 )
                 
               })
               
               output$more1_Filter<-renderUI({
                 list(
                   if(length(setdiff(input$Filter,''))==0){
                     NULL
                   } else {
                     shinyFilter(dt,filter=setdiff(input$Filter,''))
                   }
                 )
               })
               
               
               output$more2_myGplt<-renderUI({
                 list(
                   panel(
                     flowLayout(
                       heading='set aes attributes',
                       pickerInput(
                         inputId='xvar_myGplt',
                         label='select x var',
                         choices = c('NULL'='NULL',names(dt)),
                         selected='NULL',
                         multiple = FALSE,
                         options = list(`actions-box` = FALSE)
                       ),
                       
                       pickerInput(
                         inputId='yvar_myGplt',
                         label='select y var',
                         choices = c('NULL'='NULL',names(dt)),
                         selected='NULL',
                         multiple = FALSE,
                         options = list(`actions-box` = FALSE)
                       ),
                       
                       pickerInput(
                         inputId='size_myGplt',
                         label='select size var',
                         choices = c('NULL'='NULL',names(dt)),
                         selected='NULL',
                         multiple = FALSE,
                         options = list(`actions-box` = FALSE)
                       ),
                       
                       pickerInput(
                         inputId='color_myGplt',
                         label='select colour var',
                         choices = c('NULL'='NULL',names(dt)),
                         selected='NULL',
                         multiple = FALSE,
                         options = list(`actions-box` = FALSE)
                       ),
                       
                       pickerInput(
                         inputId='fill_myGplt',
                         label='select fill var',
                         choices = c('NULL'='NULL',names(dt)),
                         selected='NULL',
                         multiple = FALSE,
                         options = list(`actions-box` = FALSE)
                       ),
                       
                       pickerInput(
                         inputId='shape_myGplt',
                         label='select shapevar',
                         choices = c('NULL'='NULL',names(dt)),
                         selected='NULL',
                         multiple = FALSE,
                         options = list(`actions-box` = FALSE)
                       ),
                       
                       pickerInput(
                         inputId='alpha_myGplt',
                         label='select alpha var',
                         choices = c('NULL'='NULL',names(dt)),
                         selected='NULL',
                         multiple = FALSE,
                         options = list(`actions-box` = FALSE)
                       )
                     )#,
                     # awesomeCheckbox('export_myGplt','将该结果输出报告',FALSE)
                   )
                 )
               })
               
               output$more3_myGplt<-renderUI({
                 list(
                   panel(
                     heading = 'graph results',
                     
                     tabsetPanel(
                       
                       tabPanel(
                         title='ggplot result',
                         plotOutput('ggplot_myGplt',height='700px'),
                         status='primary'
                       ),
                       tabPanel(
                         'plotly result',
                         plotlyOutput('plotly_myGplt',height='700px'),
                         status='primary'
                       )
                     ),
                     status='primary'
                   ),
                   panel(
                     heading='mor args',
                     
                     tabsetPanel(
                       tabPanel(
                         'set other attributes',
                         flowLayout(
                           pickerInput(
                             inputId='geom_myGplt',
                             label='select layer',
                             choices = c(
                               'box plot'='box',
                               'histogram'='hist',
                               'bar plot'='bar',
                               'line plot'='line',
                               'Jitter'='jitter',
                               'scatter plot'='point',
                               'smooth line'='smooth'
                             ),
                             selected='box',
                             multiple = TRUE,
                             options = list(`actions-box` = TRUE)
                           ),
                           
                           # conditionalPanel(
                           #   condition = "'smooth'%in%input['geom_myGplt']",
                           pickerInput(
                             inputId='smoothMethod_myGplt',
                             label='select smooth method',
                             choices = c(
                               'linear regression'='lm',
                               'GAM model'='gam',
                               'GLM model'='glm',
                               'loess'='loess'
                             ),
                             selected='lm',
                             multiple = FALSE,
                             options = list(`actions-box` = FALSE)
                           ),
                           # ),
                           
                           # conditionalPanel(
                           #   condition = "'bar'%in%input['geom_myGplt']",
                           pickerInput(
                             inputId='barPos_myGplt',
                             label='position of bar plot',
                             choices = c(
                               'Stack'='stack',
                               'Dodge'='dodge'
                             ),
                             selected='dodge',
                             multiple = FALSE,
                             options = list(`actions-box` = FALSE)
                           ),
                           # ),
                           
                           
                           pickerInput(
                             inputId='theme_myGplt',
                             label='set theme',
                             choices = c(
                               'Dark'='dark',
                               'Classic'='classic',
                               'Bw'='bw',
                               'Grey'='grey'
                             ),
                             selected='bw',
                             multiple = FALSE,
                             options = list(`actions-box` = FALSE)
                           ),
                           
                           pickerInput(
                             inputId='facetVar_myGplt',
                             label='select facet var',
                             choices = c('NULL'='NULL',names(dt)),
                             selected='NULL',
                             multiple = TRUE,
                             options = list(`actions-box` = TRUE)
                           ),
                           
                           textInputAddon(inputId='labx_myGplt','set x label',value='',placeholder = 'eg:x title for my graph',addon = icon('pencil')),
                           textInputAddon(inputId='laby_myGplt','set y label',value='',placeholder = 'eg:y title for my graph',addon = icon('pencil')),
                           textInputAddon(inputId='title_myGplt','set title',value='',placeholder = 'eg:my graph',addon = icon('pencil'))
                           
                         )
                       ),
                       tabPanel(
                         'set graph attributes',
                         flowLayout(
                           # conditionalPanel(
                           #   condition = "'hist'%in%input['geom_myGplt']",
                           numericInput(
                             inputId = 'Bins_myGplt',
                             label='bins',
                             min=1,
                             val=10,
                             step=1
                           ),
                           # ),
                           textInputAddon(
                             inputId='Colour_myGplt','set colour',value='NULL',placeholder = 'eg:red',addon = icon('pencil')
                           ),
                           textInputAddon(
                             inputId='Fill_myGplt','set fill',value='NULL',placeholder = 'eg:red',addon = icon('pencil')
                           ),
                           numericInput(
                             inputId = 'Size_myGplt',
                             label='set size',
                             min=1,
                             val='NULL',
                             step=1
                           ),
                           numericInput(
                             inputId = 'Alpha_myGplt',
                             label='set alpha',
                             min=0,
                             val='NULL',
                             step=1
                           ),
                           
                           numericInput(
                             inputId = 'Width_myGplt',
                             label='set width',
                             min=0.1,
                             val='NULL',
                             step=1
                           ),
                           
                           numericInput(
                             inputId = 'Shape_myGplt',
                             label='set shape',
                             min=1,
                             val='NULL',
                             step=1
                           )
                         )
                       )
                     ),
                     status='primary'
                   )
                   
                 )
               })
               
               
               res_myGplt<-reactive({
                 input$go_myGplt
                 req(input$go_myGplt)
                 if(length(setdiff(input$Filter,''))==0){
                   dat<-dt
                 } else {
                   
                   indMat<-sapply(setdiff(input$Filter,''),function(i){
                     if(class(dt[,i])%in%c('character','factor')){
                       dt[,i]%in%input[[i]]
                     } else {
                       dt[,i]>=input[[i]][1]&dt[,i]<=input[[i]][2]
                       
                     }
                   })
                   
                   apply(indMat,1,all)->Ind
                   dat<-dt[Ind,]
                   
                 }
                 ggplt2S(data=dat,
                         x=input$xvar_myGplt,
                         y=input$yvar_myGplt,
                         size=input$size_myGplt,
                         fill=input$fill_myGplt,
                         color=input$color_myGplt,
                         shape=input$shape_myGplt,
                         alpha=input$alpha_myGplt,
                         facetVar=input$facetVar_myGplt,
                         geom=input$geom_myGplt,
                         smoothMethod = input$smoothMethod_myGplt,
                         barPos=input$barPos_myGplt,
                         labx=input$labx_myGplt,
                         laby=input$laby_myGplt,
                         title=input$title_myGplt,
                         Bins=input$Bins_myGplt,
                         theme=input$theme_myGplt,
                         Width=input$Width_myGplt,
                         Colour=input$Colour_myGplt,  # newly added
                         Fill=input$Fill_myGplt,     
                         Size=input$Size_myGplt,
                         Alpha=input$Alpha_myGplt,
                         Shape=input$Shape_myGplt
                 )->res_myGplt
                 return(res_myGplt)
               })
               
               output$ggplot_myGplt<-renderPlot({
                 input$go_myGplt
                 isolate({
                   res_myGplt()->resmyGplt
                   resmyGplt$resGGplot
                 })
               })
               
               output$plotly_myGplt<-renderPlotly({
                 input$go_myGplt
                 isolate({
                   res_myGplt()->resmyGplt
                   resmyGplt$resPlotly
                 })
               })
               
               
               output$downloadGraph<-downloadHandler(
                 filename=function(){
                   paste('graph-',Sys.Date(),'.pdf',sep='')
                 },
                 content=function(file){
                   pdf(file)
                   showtext_begin()
                   plot(res_myGplt()$resGGplot)
                   showtext_end()
                   dev.off()
                   
                 }
                 
               )
               
               
               
             }
    )
  }
  
}



#' qDT
#' 
#' quick DT function 
#' 
#' render a DT in shiny
#' 
#' @examples 
#' fnDT(mtcars)
#' 
#' @export


qDT<-function(dt,...){
  require(DT)
  require(shiny)
  require(shinyWidgets)
  as.data.frame(dt)->dt
  if (interactive()) {
    shinyApp(options=list(...),
      ui = fluidPage(
        sidebarLayout(
          sidebarPanel(
            panel(
              heading = 'vars to keep',
              uiOutput('more1'),
              status='primary'
            )
            
          ),
          mainPanel(
            panel(
              heading = 'DataTable Outputs',
              DTOutput('tbl'),
              status='primary'
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
                        multiple = T,
                        options = list(`actions-box` = TRUE))
          )
        })
        
        output$tbl = renderDT(
          dt[,input$varsKeep,drop=F],
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

#' qTable
#' 
#' quick table functions
#' 
#' @export


qTable<-function(dt,...){
  require(DT)
  require(shiny)
  require(shinyWidgets)
  require(rhandsontable)
  as.data.frame(dt)->dt
  if (interactive()) {
    shinyApp(options=list(...),
             ui = fluidPage(
               sidebarLayout(
                 sidebarPanel(
                   uiOutput('more1_DT'),
                   actionBttn('go_DT','confirm')
                 ),
                 mainPanel(
                   panel(
                     heading='View head of the data',
                     rHandsontableOutput("dtHead"),
                     status='primary'
                     
                   ),
                   panel(
                     heading = 'set dataMnp args',
                     rHandsontableOutput("handsonTB"),
                     status='primary'
                   ),
                   panel(heading = 'results',
                         DT:::dataTableOutput('resMnp'),
                         status='primary'
                   ),
                   downloadButton('downloadData','download result')
                   
                 )
               )
             ),
             server = function(input, output) {
               
               output$more1_DT<-renderUI({
                 list(
                   panel(
                     heading = 'set no of args',
                     numericInput(
                       inputId = 'nrow_DT',
                       label = 'no of args',
                       value = 1,
                       min=1,
                       max=100
                     ),
                     status = 'primary'
                   )#,
                   # awesomeCheckbox('export_dataMnp','将该结果输出报告',FALSE)
                 )
               })
               
               
               output$dtHead<-renderRHandsontable({
                 rhandsontable(head(dt))
               })
               
               
               output$handsonTB<-renderRHandsontable({
                 rhandsontable(data.frame(
                   subset=rep('',input$nrow_DT),
                   newvars=rep('',input$nrow_DT),
                   newvarsformulas=rep('',input$nrow_DT),
                   newvarsby=rep('',input$nrow_DT),
                   indexnames=rep('',input$nrow_DT),
                   formulas=rep('',input$nrow_DT),
                   dimvars=rep('',input$nrow_DT),
                   dimnames=rep('',input$nrow_DT),
                   datevar=rep('',input$nrow_DT),
                   dtorders=rep('',input$nrow_DT),
                   margin=rep(0L,input$nrow_DT),  
                   revisedmargin=rep(0L,input$nrow_DT),  
                   revisednames=rep('',input$nrow_DT),  
                   revisedformulas=rep('',input$nrow_DT),  
                   ordervars=rep('',input$nrow_DT),   
                   orders=rep('',input$nrow_DT),   
                   digits=rep(0L,input$nrow_DT),      
                   tbvars=rep('',input$nrow_DT),         
                   hbvars=rep('',input$nrow_DT),          
                   colorder=rep('',input$nrow_DT)
                 ),readOnly=F)
               })
               
               dtCfg <- reactive({
                 hot_to_r(req(input$handsonTB))
               })
               
               res_DT<-eventReactive(input$go_DT,{
                 dtCfg()->cfg
                 cfg[cfg=='']<-NA
                 dataMnp(
                   data=dt,
                   subset=cfg$subset,
                   newVars=cfg$newvars,
                   newVarsFormulas=cfg$newvarsformulas,
                   newVarsBy=cfg$newvarsby,
                   indexNames=cfg$indexnames,
                   Formulas=cfg$formulas,
                   dimVars=cfg$dimvars,
                   dimNames=cfg$dimnames,
                   dateVar=cfg$datevar,
                   dtOrders=cfg$dtorders,
                   margin=cfg$margin,  
                   revisedMargin=cfg$revisedmargin,  
                   revisedNames=cfg$revisednames,  
                   revisedFormulas=cfg$revisedformulas,  
                   orderVars=cfg$ordervars,   
                   orders=cfg$orders,   
                   Digits=cfg$digits,      
                   tbVars=cfg$tbvars,         
                   hbVars=cfg$hbvars,          
                   colOrder=cfg$colorder
                   
                 )->res
                 return(res)
               })
               
               
               
               output$resMnp<-DT:::renderDataTable(
                 res_DT()$tabRes,server=T
               )
               
               
               
               output$downloadData<-downloadHandler(
                 filename=function(){
                   paste('tableRes-',Sys.Date(),'.csv',sep='')
                 },
                 content=function(file){
                   write.csv(res_DT()$tabRes,file,fileEncoding='GB18030')
                 }
               )
               
               
               
             }
    )
  }
}

