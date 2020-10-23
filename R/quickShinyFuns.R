#' qGraph
#' 
#' quick plotly based on ggplt2S. along with Filter param.
#' 
#' use ggplt2S params in ...
#' 
#' @export

qGraph<-function(dt,...){
  require(stringi)
  require(plotly)
  require(ggplot2)
  require(shiny)
  require(shinyWidgets)
  require(showtext)
  require(htmlwidgets)
  as.data.frame(dt)->dt
  if (interactive()) {
    shinyApp(options=list(...),
             ui = fluidPage(
               navbarPage(
                 title=div(icon("r-project"), strong("qGraph")),
                 tabPanel(
                   'ggplot',
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
                 tabPanel(
                   'prophet',
                   sidebarLayout(
                     sidebarPanel(
                       panel(
                         heading = 'Filter setting',
                         # status='primary',
                         uiOutput('more2_Filter'),
                         uiOutput('more3_Filter'),
                         status = 'primary'
                       ),
                       # flowLayout(
                       panel(
                         heading='set params',
                         status='primary',
                         
                         pickerInput(
                           inputId='tsvar_myProphet',
                           label='选择日期时间变量',
                           choices = c(names(dt)),
                           selected='NULL',
                           multiple = FALSE,
                           options = list(`actions-box` = FALSE)
                         ),
                         
                         pickerInput(
                           inputId='dateFormat_myProphet',
                           label='日期格式',
                           choices=c(
                             '年'='y',
                             '年月'='ym',
                             "年月日"="ymd",
                             '月日年'='mdy',
                             '日月年'='dmy'
                           ),
                           selected ='yyyymmdd',
                           multiple=FALSE,
                           options=list(`actions-box` = FALSE)
                         ),
                         
                         pickerInput(
                           inputId='timeFormat_myProphet',
                           label='时间格式',
                           choices=c(
                             '无'='',
                             '时'='H',
                             "时分"="HM",
                             '时分秒'='HMS'
                           ),
                           selected ='yyyymmdd',
                           multiple=FALSE,
                           options=list(`actions-box` = FALSE)
                         ),
                         
                         pickerInput(
                           inputId='measurevars_myProphet',
                           label='选择待分析变量',
                           choices = c(names(dt)),
                           selected='NULL',
                           multiple = TRUE,
                           options = list(`actions-box` = TRUE)
                         ),
                         
                         pickerInput(
                           inputId='groupvars_myProphet',
                           label='选择亚组分析变量',
                           choices = c('无'='1',names(dt)),
                           selected='1',
                           multiple = TRUE,
                           options = list(`actions-box` = TRUE)
                         ),
                         
                         pickerInput(
                           inputId='period_myProphet',
                           label='选择处理时间段',
                           choices = c(
                             '日'='days',
                             '周'='weeks',
                             '月'='months',
                             '季'='quarters',
                             '年'='years'
                             
                           ),
                           selected='months',
                           multiple = FALSE,
                           options = list(`actions-box` = FALSE)
                         ),
                         
                         
                         pickerInput(
                           inputId='growth_myProphet',
                           label='设定趋势类型',
                           choices = c(
                             '线性'='linear',
                             'Logistic'='logistic'
                           ),
                           selected='linear',
                           multiple = FALSE,
                           options = list(`actions-box` = FALSE)
                         ),
                         pickerInput(
                           inputId='FN_myProphet',
                           label='设定分析方法',
                           choices = c(
                             '求和'='function(x)sum(x,na.rm=T)',
                             '均值'='function(x)mean(x,na.rm=T)',
                             '中位数'='function(x)median(x,na.rm=T)',
                             '最大值'='function(x)max(x,na.rm=T)',
                             '最小值'='function(x)min(x,na.rm=T)',
                             '标准差'='function(x)sd(x,na.rm=T)'
                           ),
                           selected='function(x)sum(x,na.rm=T)',
                           multiple = FALSE,
                           options = list(`actions-box` = FALSE)
                         ),
                         awesomeCheckbox('dailyS_myProphet','是否分析日趋势',TRUE),
                         awesomeCheckbox('weeklyS_myProphet','是否分析周趋势',TRUE),
                         awesomeCheckbox('yearlyS_myProphet','是否分析年趋势',TRUE),
                         actionBttn('go_myProphet','confirm')
                       )
                       
                       # )
                       
                     ),
                     mainPanel(
                       uiOutput('more3_myProphet')#,
                       # downloadButton('downloadGraph','download graph')
                     )
                   )
                   
                   
                   
                   
                 )
               )
             ),
             server = function(input, output) {
               
               
               
               output$more0_Filter<-renderUI({
                 list(
                   pickerInput(
                     'Filter',
                     'choose filter vars',
                     choices = c('none'='',names(dt)),
                     selected ='无',
                     multiple=T,
                     options = list(`actions-box` = T))
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
                   paste('graph-',Sys.Date(),'.html',sep='')
                 },
                 content=function(file){
                   # html(file)
                   # showtext_begin()
                   saveWidget(as_widget(res_myGplt()$resPlotly),file,selfcontained = TRUE)
                   # showtext_end()
                   # dev.off()
                   
                 }
                 
               )
               
               
               output$more2_Filter<-renderUI({
                 list(
                   pickerInput('Filter2','choose filter vars',choices = c('none'='',names(dt)),selected ='无',multiple=T )
                 )
                 
               })
               
               output$more3_Filter<-renderUI({
                 list(
                   if(length(setdiff(input$Filter2,''))==0){
                     NULL
                   } else {
                     shinyFilter(dt,filter=setdiff(input$Filter2,''))
                   }
                 )
               })
               
               output$more3_myProphet<-renderUI({
                 list(
                   tabsetPanel(
                     tabPanel(
                       '历史数据结果',
                       tabsetPanel(
                         tabPanel(
                           '历史数据表格结果',
                           dataTableOutput(
                             'resTab_myProphet'
                           )
                         ),
                         tabPanel(
                           'ggplot结果',
                           plotOutput('ggplotHis_myProphet',height='700px'),
                           status='primary'
                         ),
                         tabPanel(
                           'plotly结果',
                           plotlyOutput('plotlyHis_myProphet',height='700px'),
                           status='primary'
                         )
                       )
                       
                       
                       
                     ),
                     tabPanel(
                       '预测结果',
                       tabsetPanel(
                         tabPanel(
                           '预测数据表格结果',
                           dataTableOutput(
                             'predTab_myProphet'
                           )
                         ),
                         tabPanel(
                           'ggplot结果',
                           plotOutput('ggplotPred_myProphet',height='700px'),
                           status='primary'
                         ),
                         tabPanel(
                           'plotly结果',
                           plotlyOutput('plotlyPred_myProphet',height='700px'),
                           status='primary'
                         )
                       )
                       
                       
                       
                     )
                   ),
                   # tabsetPanel(
                   tabPanel(
                     '调整可变属性',
                     flowLayout(
                       
                       numericInput(
                         inputId='cap_myProphet',
                         label='设定相对上限',
                         value=-1,
                         step=1
                       ),
                       
                       numericInput(
                         inputId='floor_myProphet',
                         label='设定相对下限',
                         value=-1,
                         step=1
                       ),
                       
                       numericInput(
                         inputId='H_myProphet',
                         label='设定预测时间长度',
                         value=10,
                         step=1
                       )
                     )
                   )
                   # )
                   
                 )
               })
               
               res_myProphet<-reactive({
                 input$go_myProphet
                 req(input$go_myProphet)
                 if(length(setdiff(input$Filter2,''))==0){
                   dat<-dt
                 } else {
                   
                   indMat<-sapply(setdiff(input$Filter2,''),function(i){
                     if(class(dt[,i])%in%c('character','factor')){
                       dt[,i]%in%input[[i]]
                     } else {
                       dt[,i]>=input[[i]][1]&dt[,i]<=input[[i]][2]
                       
                     }
                   })
                   
                   apply(indMat,1,all)->Ind
                   dat<-dt[Ind,]
                   
                 }
                 # isolate({
                 
                 prophetS(data=dat,
                          tsVar=input$tsvar_myProphet,
                          tsFormat = paste(input$dateFormat_myProphet,input$timeFormat_myProphet,sep=''),
                          measureVars=input$measurevars_myProphet,
                          groupVars = input$groupvars_myProphet,
                          Period = input$period_myProphet,
                          FN=input$FN_myProphet,
                          Cap=input$cap_myProphet,
                          Floor=input$floor_myProphet,
                          Growth=input$growth_myProphet,
                          H=input$H_myProphet,
                          yearlyS = input$yearlyS_myProphet,
                          dailyS = input$dailyS_myProphet,
                          weeklyS = input$weeklyS_myProphet
                          
                          
                 )->res_Prophet
                 return(res_Prophet)
               })
               
               
               
               output$resTab_myProphet<-renderDataTable({
                 input$go_myProphet
                 isolate({
                   res_myProphet()->resmyProphet
                   resmyProphet$tabRes
                 })
               })
               
               output$ggplotHis_myProphet<-renderPlot({
                 input$go_myProphet
                 isolate({
                   res_myProphet()->resmyProphet
                   resmyProphet$graphRes
                 })
               })
               
               output$plotlyHis_myProphet<-renderPlotly({
                 input$go_myProphet
                 isolate({
                   res_myProphet()->resmyProphet
                   resmyProphet$graphRes
                 })
               })
               
               
               output$predTab_myProphet<-renderDataTable({
                 input$go_myProphet
                 isolate({
                   res_myProphet()->resmyProphet
                   resmyProphet$tabPred
                 })
               })
               
               output$ggplotPred_myProphet<-renderPlot({
                 input$go_myProphet
                 isolate({
                   res_myProphet()->resmyProphet
                   resmyProphet$graphPredict
                 })
               })
               
               output$plotlyPred_myProphet<-renderPlotly({
                 input$go_myProphet
                 isolate({
                   res_myProphet()->resmyProphet
                   resmyProphet$graphPredict
                 })
               })
               
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
        pickerInput(i,i,choices = unique(dt[,i]),selected = unique(dt[,i]),multiple = T,options = list(`actions-box` = T))
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




#' qTS
#' 
#' quick TS funs. same as time series in Madis
#' 
#' 
#' @export

qTS<-function(dt,...){
  require(stringi)
  require(plotly)
  require(ggplot2)
  require(shiny)
  require(shinyWidgets)
  require(showtext)
  require(htmlwidgets)
  require(lubridate)
  require(prophet)
  as.data.frame(dt)->dt
  if (interactive()) {
    shinyApp(options=list(...),
             ui = fluidPage(
               sidebarLayout(
                 sidebarPanel(
                   panel(heading='set params',
                         status='primary',
                         flowLayout(
                           heading='选择时间序列分析各参数',
                           pickerInput(
                             inputId='tsvar_myProphet',
                             label='选择日期时间变量',
                             choices = c(names(dt)),
                             selected='NULL',
                             multiple = FALSE,
                             options = list(`actions-box` = FALSE)
                           ),
                           
                           pickerInput(
                             inputId='dateFormat_myProphet',
                             label='日期格式',
                             choices=c(
                               '年'='y',
                               '年月'='ym',
                               "年月日"="ymd",
                               '月日年'='mdy',
                               '日月年'='dmy'
                             ),
                             selected ='yyyymmdd',
                             multiple=FALSE,
                             options=list(`actions-box` = FALSE)
                           ),
                           
                           pickerInput(
                             inputId='timeFormat_myProphet',
                             label='时间格式',
                             choices=c(
                               '无'='',
                               '时'='H',
                               "时分"="HM",
                               '时分秒'='HMS'
                             ),
                             selected ='yyyymmdd',
                             multiple=FALSE,
                             options=list(`actions-box` = FALSE)
                           ),
                           
                           pickerInput(
                             inputId='measurevars_myProphet',
                             label='选择待分析变量',
                             choices = c(names(dt)),
                             selected='NULL',
                             multiple = TRUE,
                             options = list(`actions-box` = TRUE)
                           ),
                           
                           pickerInput(
                             inputId='groupvars_myProphet',
                             label='选择亚组分析变量',
                             choices = c('无'='1',names(dt)),
                             selected='1',
                             multiple = TRUE,
                             options = list(`actions-box` = TRUE)
                           ),
                           
                           pickerInput(
                             inputId='period_myProphet',
                             label='选择处理时间段',
                             choices = c(
                               '日'='days',
                               '周'='weeks',
                               '月'='months',
                               '季'='quarters',
                               '年'='years'
                               
                             ),
                             selected='months',
                             multiple = FALSE,
                             options = list(`actions-box` = FALSE)
                           ),
                           
                           
                           pickerInput(
                             inputId='growth_myProphet',
                             label='设定趋势类型',
                             choices = c(
                               '线性'='linear',
                               'Logistic'='logistic'
                             ),
                             selected='linear',
                             multiple = FALSE,
                             options = list(`actions-box` = FALSE)
                           ),
                           pickerInput(
                             inputId='FN_myProphet',
                             label='设定分析方法',
                             choices = c(
                               '求和'='function(x)sum(x,na.rm=T)',
                               '均值'='function(x)mean(x,na.rm=T)',
                               '中位数'='function(x)median(x,na.rm=T)',
                               '最大值'='function(x)max(x,na.rm=T)',
                               '最小值'='function(x)min(x,na.rm=T)',
                               '标准差'='function(x)sd(x,na.rm=T)'
                             ),
                             selected='function(x)sum(x,na.rm=T)',
                             multiple = FALSE,
                             options = list(`actions-box` = FALSE)
                           ),
                           awesomeCheckbox('dailyS_myProphet','是否分析日趋势',TRUE),
                           awesomeCheckbox('weeklyS_myProphet','是否分析周趋势',TRUE),
                           awesomeCheckbox('yearlyS_myProphet','是否分析年趋势',TRUE)
                         ),
                         actionBttn('go_myProphet','confirm')
                   )
                   
                 ),
                 mainPanel(
                   uiOutput('more3_myProphet')#,
                   # downloadButton('downloadGraph','download graph')
                 )
               )
               
             ),
             server = function(input, output) {
               
               output$more3_myProphet<-renderUI({
                 list(
                   tabsetPanel(
                     tabPanel(
                       '历史数据结果',
                       tabsetPanel(
                         tabPanel(
                           '历史数据表格结果',
                           dataTableOutput(
                             'resTab_myProphet'
                           )
                         ),
                         tabPanel(
                           'ggplot结果',
                           plotOutput('ggplotHis_myProphet',height='700px'),
                           status='primary'
                         ),
                         tabPanel(
                           'plotly结果',
                           plotlyOutput('plotlyHis_myProphet',height='700px'),
                           status='primary'
                         )
                       )
                       
                       
                       
                     ),
                     tabPanel(
                       '预测结果',
                       tabsetPanel(
                         tabPanel(
                           '预测数据表格结果',
                           dataTableOutput(
                             'predTab_myProphet'
                           )
                         ),
                         tabPanel(
                           'ggplot结果',
                           plotOutput('ggplotPred_myProphet',height='700px'),
                           status='primary'
                         ),
                         tabPanel(
                           'plotly结果',
                           plotlyOutput('plotlyPred_myProphet',height='700px'),
                           status='primary'
                         )
                       )
                       
                       
                       
                     )
                   ),
                   # tabsetPanel(
                   tabPanel(
                     '调整可变属性',
                     flowLayout(
                       
                       numericInput(
                         inputId='cap_myProphet',
                         label='设定相对上限',
                         value=-1,
                         step=1
                       ),
                       
                       numericInput(
                         inputId='floor_myProphet',
                         label='设定相对下限',
                         value=-1,
                         step=1
                       ),
                       
                       numericInput(
                         inputId='H_myProphet',
                         label='设定预测时间长度',
                         value=10,
                         step=1
                       )
                     )
                   )
                   # )
                   
                 )
               })
               
               res_myProphet<-reactive({
                 input$go_myProphet
                 req(input$go_myProphet)
                 # isolate({
                 
                 prophetS(data=dt,
                          tsVar=input$tsvar_myProphet,
                          tsFormat = paste(input$dateFormat_myProphet,input$timeFormat_myProphet,sep=''),
                          measureVars=input$measurevars_myProphet,
                          groupVars = input$groupvars_myProphet,
                          Period = input$period_myProphet,
                          FN=input$FN_myProphet,
                          Cap=input$cap_myProphet,
                          Floor=input$floor_myProphet,
                          Growth=input$growth_myProphet,
                          H=input$H_myProphet,
                          yearlyS = input$yearlyS_myProphet,
                          dailyS = input$dailyS_myProphet,
                          weeklyS = input$weeklyS_myProphet
                          
                          
                 )->res_Prophet
                 return(res_Prophet)
               })
               
               
               
               output$resTab_myProphet<-renderDataTable({
                 input$go_myProphet
                 isolate({
                   res_myProphet()->resmyProphet
                   resmyProphet$tabRes
                 })
               })
               
               output$ggplotHis_myProphet<-renderPlot({
                 input$go_myProphet
                 isolate({
                   res_myProphet()->resmyProphet
                   resmyProphet$graphRes
                 })
               })
               
               output$plotlyHis_myProphet<-renderPlotly({
                 input$go_myProphet
                 isolate({
                   res_myProphet()->resmyProphet
                   resmyProphet$graphRes
                 })
               })
               
               
               output$predTab_myProphet<-renderDataTable({
                 input$go_myProphet
                 isolate({
                   res_myProphet()->resmyProphet
                   resmyProphet$tabPred
                 })
               })
               
               output$ggplotPred_myProphet<-renderPlot({
                 input$go_myProphet
                 isolate({
                   res_myProphet()->resmyProphet
                   resmyProphet$graphPredict
                 })
               })
               
               output$plotlyPred_myProphet<-renderPlotly({
                 input$go_myProphet
                 isolate({
                   res_myProphet()->resmyProphet
                   resmyProphet$graphPredict
                 })
               })
               
               
               # output$downloadGraph<-downloadHandler(
               #   filename=function(){
               #     paste('graph-',Sys.Date(),'.html',sep='')
               #   },
               #   content=function(file){
               #     saveWidget(as_widget(res_myGplt()$resPlotly),file,selfcontained = TRUE)
               #     
               #   }
               #   
               # )
               
               
               
             }
    )
  }
  
}
