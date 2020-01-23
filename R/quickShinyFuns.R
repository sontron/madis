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

qGraph<-function(dt){
  require(stringi)
  require(plotly)
  require(ggplot2)
  require(shiny)
  require(shinyWidgets)
  as.data.frame(dt)->dt
  if (interactive()) {
    library(shiny)
    library(DT)
    shinyApp(
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
            actionBttn('go_myGplt','确定')
          ),
          mainPanel(
            uiOutput('more3_myGplt')
          )
        )
        
      ),
      server = function(input, output) {
        
        
        
        output$more0_Filter<-renderUI({
          list(
            pickerInput('Filter','choose filter vars',choices = c('无',names(dt)),selected ='无',multiple=T )
          )
          
        })
        
        output$more1_Filter<-renderUI({
          list(
            if(length(setdiff(input$Filter,'无'))==0){
              NULL
            } else {
              shinyFilter(dt,filter=setdiff(input$Filter,'无'))
            }
          )
        })
        
        
        output$more2_myGplt<-renderUI({
          list(
            panel(
              flowLayout(
                heading='选择作图各属性参数(aes)',
                pickerInput(
                  inputId='xvar_myGplt',
                  label='选择x轴变量',
                  choices = c('无'='NULL',names(dt)),
                  selected='NULL',
                  multiple = FALSE,
                  options = list(`actions-box` = FALSE)
                ),
                
                pickerInput(
                  inputId='yvar_myGplt',
                  label='选择y轴变量',
                  choices = c('无'='NULL',names(dt)),
                  selected='NULL',
                  multiple = FALSE,
                  options = list(`actions-box` = FALSE)
                ),
                
                pickerInput(
                  inputId='size_myGplt',
                  label='设定点或线的大小',
                  choices = c('无'='NULL',names(dt)),
                  selected='NULL',
                  multiple = FALSE,
                  options = list(`actions-box` = FALSE)
                ),
                
                pickerInput(
                  inputId='color_myGplt',
                  label='设定点线颜色',
                  choices = c('无'='NULL',names(dt)),
                  selected='NULL',
                  multiple = FALSE,
                  options = list(`actions-box` = FALSE)
                ),
                
                pickerInput(
                  inputId='fill_myGplt',
                  label='设定面的填充',
                  choices = c('无'='NULL',names(dt)),
                  selected='NULL',
                  multiple = FALSE,
                  options = list(`actions-box` = FALSE)
                ),
                
                pickerInput(
                  inputId='shape_myGplt',
                  label='设定形状',
                  choices = c('无'='NULL',names(dt)),
                  selected='NULL',
                  multiple = FALSE,
                  options = list(`actions-box` = FALSE)
                ),
                
                pickerInput(
                  inputId='alpha_myGplt',
                  label='设定透明度',
                  choices = c('无'='NULL',names(dt)),
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
            tabsetPanel(
              tabPanel(
                title='ggplot结果',
                plotOutput('ggplot_myGplt',height='700px'),
                status='primary'
              ),
              tabPanel(
                'plotly结果',
                plotlyOutput('plotly_myGplt',height='700px'),
                status='primary'
              )
            ),
            tabsetPanel(
              tabPanel(
                '调整可变属性',
                flowLayout(
                  pickerInput(
                    inputId='geom_myGplt',
                    label='选择图层',
                    choices = c(
                      '箱图'='box',
                      '直方图'='hist',
                      '条图'='bar',
                      '线图'='line',
                      'Jitter图'='jitter',
                      '散点图'='point',
                      '平滑曲线'='smooth'
                    ),
                    selected='box',
                    multiple = TRUE,
                    options = list(`actions-box` = TRUE)
                  ),
                  
                  # conditionalPanel(
                  #   condition = "'smooth'%in%input['geom_myGplt']",
                  pickerInput(
                    inputId='smoothMethod_myGplt',
                    label='选择平滑曲线函数',
                    choices = c(
                      '线性回归'='lm',
                      'GAM模型'='gam',
                      'GLM模型'='glm',
                      '局部回归'='loess'
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
                    label='条图呈现方式',
                    choices = c(
                      '堆叠'='stack',
                      'Dodge'='dodge'
                    ),
                    selected='dodge',
                    multiple = FALSE,
                    options = list(`actions-box` = FALSE)
                  ),
                  # ),
                  
                  
                  pickerInput(
                    inputId='theme_myGplt',
                    label='主题配色',
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
                    label='选择分层作图变量',
                    choices = c('无'='NULL',names(dt)),
                    selected='NULL',
                    multiple = TRUE,
                    options = list(`actions-box` = TRUE)
                  ),
                  
                  textInputAddon(inputId='labx_myGplt','设定x轴标题',value='',placeholder = 'eg:x title for my graph',addon = icon('pencil')),
                  textInputAddon(inputId='laby_myGplt','设定y轴标题',value='',placeholder = 'eg:y title for my graph',addon = icon('pencil')),
                  textInputAddon(inputId='title_myGplt','设定图标题',value='',placeholder = 'eg:my graph',addon = icon('pencil'))
                  
                )
              ),
              tabPanel(
                '设定固定属性',
                flowLayout(
                  # conditionalPanel(
                  #   condition = "'hist'%in%input['geom_myGplt']",
                  numericInput(
                    inputId = 'Bins_myGplt',
                    label='直方图宽度',
                    min=1,
                    val=10,
                    step=1
                  ),
                  # ),
                  textInputAddon(
                    inputId='Colour_myGplt','设定点及线的整体颜色',value='NULL',placeholder = 'eg:red',addon = icon('pencil')
                  ),
                  textInputAddon(
                    inputId='Fill_myGplt','设定面及区域的整体颜色',value='NULL',placeholder = 'eg:red',addon = icon('pencil')
                  ),
                  numericInput(
                    inputId = 'Size_myGplt',
                    label='设定点的大小',
                    min=1,
                    val='NULL',
                    step=1
                  ),
                  numericInput(
                    inputId = 'Alpha_myGplt',
                    label='设置透明度',
                    min=0,
                    val='NULL',
                    step=1
                  ),
                  
                  numericInput(
                    inputId = 'Width_myGplt',
                    label='条图及箱图宽度',
                    min=0.1,
                    val='NULL',
                    step=1
                  ),
                  
                  numericInput(
                    inputId = 'Shape_myGplt',
                    label='点的形状设定',
                    min=1,
                    val='NULL',
                    step=1
                  )
                )
              )
            )
            
          )
        })
        
        
        res_myGplt<-reactive({
          input$go_myGplt
          req(input$go_myGplt)
          if(length(setdiff(input$Filter,'无'))==0){
            dat<-dt
          } else {
            
            indMat<-sapply(setdiff(input$Filter,'无'),function(i){
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


qDT<-function(dt){
  require(DT)
  require(shiny)
  require(shinyWidgets)
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

#' qTable
#' 
#' quick table functions
#' 
#' @export


qTable<-function(dt){
  require(DT)
  require(shiny)
  require(shinyWidgets)
  require(rhandsontable)
  as.data.frame(dt)->dt
  if (interactive()) {
    library(shiny)
    library(DT)
    library(shinyWidgets)
    shinyApp(
      ui = fluidPage(
        sidebarLayout(
          sidebarPanel(
            uiOutput('more1_DT'),
            actionBttn('go_DT','确定')
          ),
          mainPanel(
            panel(
              heading = '设置表格参数',
              rHandsontableOutput("handsonTB")
            ),
            panel(heading = '表格结果',
                  DT:::dataTableOutput('resMnp')
            )
            
            
          )
        )
      ),
      server = function(input, output) {
        
        output$more1_DT<-renderUI({
          list(
            panel(
              heading = '设置参数个数',
              numericInput(
                inputId = 'nrow_DT',
                label = '设定配置参数个数',
                value = 1,
                min=1,
                max=100
              )
            ),
            awesomeCheckbox('export_dataMnp','将该结果输出报告',FALSE)
          )
        })
        
        
        
        output$handsonTB<-renderRHandsontable({
          rhandsontable(data.frame(
            子集=rep('',input$nrow_DT),
            新变量名称=rep('',input$nrow_DT),
            新变量计算公式=rep('',input$nrow_DT),
            新变量维度汇总=rep('',input$nrow_DT),
            指标名称=rep('',input$nrow_DT),
            指标公式=rep('',input$nrow_DT),
            维度变量=rep('',input$nrow_DT),
            维度名称=rep('',input$nrow_DT),
            日期变量=rep('',input$nrow_DT),
            日期格式=rep('',input$nrow_DT),
            边际汇总=rep(0L,input$nrow_DT),  
            调整边际汇总=rep(0L,input$nrow_DT),  
            调整结果名称=rep('',input$nrow_DT),  
            调整公式=rep('',input$nrow_DT),  
            行排序变量=rep('',input$nrow_DT),   
            行排序顺序=rep('',input$nrow_DT),   
            小数点=rep(0L,input$nrow_DT),      
            同比指标=rep('',input$nrow_DT),         
            环比指标=rep('',input$nrow_DT),          
            列排序=rep('',input$nrow_DT)
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
            subset=cfg$子集,
            newVars=cfg$新变量名称,
            newVarsFormulas=cfg$新变量计算公式,
            newVarsBy=cfg$新变量维度汇总,
            indexNames=cfg$指标名称,
            Formulas=cfg$指标公式,
            dimVars=cfg$维度变量,
            dimNames=cfg$维度名称,
            dateVar=cfg$日期变量,
            dtOrders=cfg$日期格式,
            margin=cfg$边际汇总,  
            revisedMargin=cfg$调整边际汇总,  
            revisedNames=cfg$调整结果名称,  
            revisedFormulas=cfg$调整公式,  
            orderVars=cfg$行排序变量,   
            orders=cfg$行排序顺序,   
            Digits=cfg$小数点,      
            tbVars=cfg$同比指标,         
            hbVars=cfg$环比指标,          
            colOrder=cfg$列排序
            
          )->res
          return(res)
        })
        
        
        
        output$resMnp<-DT:::renderDataTable(
          res_DT()$tabRes,server=T
        )
        
        
        
      }
    )
  }
}

