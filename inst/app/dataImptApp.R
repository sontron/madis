

server<-function(input,output){
#serverstart
  ###### 数据导入功能(data_Impt) ######
  data_dataImpt<-reactive({
    
    # input$go_dataImpt
    # req(input$go_dataImpt)
    
    
    if(is.null(input$file_dataImpt)) {
      Data<-read.csv(text=input$text_dataImpt,sep="\t",na.strings=input$nastr_dataImpt,stringsAsFactors = input$strAsFac_dataImpt,header=input$header_dataImpt,fileEncoding = input$encod_dataImpt)} else {
        inFile<-input$file_dataImpt
        if(input$argsMore_dataImpt=='') {
          Data<-read.table(file=inFile$datapath,na.strings=input$nastr_dataImpt,stringsAsFactors = input$strAsFac_dataImpt,header=input$header_dataImpt,fileEncoding = input$encod_dataImpt,sep=input$sep_dataImpt)
        } else {
          textfun_dataImpt<-paste("read.table(",paste("file=inFile$datapath","header=input$header_dataImpt","na.strings=input$nastr_dataImpt","stringsAsFactors = input$strAsFac_dataImpt","sep=input$sep_dataImpt","fileEncoding=input$encod_dataImpt",input$argsMore_dataImpt,sep=','),")",sep='')
          eval(parse(text=textfun_dataImpt))->Data
        }
      }
    return(Data)
  })
  
  
  output$args_dataImpt<-renderUI({
    list(
      panel(
        heading='数据读取参数设定',
        flowLayout(
          pickerInput(
            inputId='sep_dataImpt',
            label='文本分隔符',
            choices=c(
              '逗号分隔'=',',
              '制表分隔符'='\t',
              '空格分隔'=''
            ),
            selected=',',
            multiple = FALSE,
            options = list(`actions-box` = FALSE)
          ),
          pickerInput(
            inputId='nastr_dataImpt',
            label='缺失值类型',
            choices=c(
              '空白'='',
              '空格'=' ',
              'NA'='NA',
              '.'='.'
            ),
            selected='NA',
            multiple = TRUE,
            options = list(`actions-box` = TRUE)
          ),
          pickerInput(
            inputId='encod_dataImpt',
            label='文件编码格式',
            choices=c(
              'UTF8编码'='UTF8',
              'GB18030编码'='GB18030'
            ),
            selected='GB18030',
            multiple = FALSE,
            options = list(`actions-box` = FALSE)
          ),
          awesomeCheckbox('header_dataImpt','数据第一行为变量名',TRUE),
          awesomeCheckbox('strAsFac_dataImpt','是否将字符串转换成因子',FALSE)
        ),
        textInputAddon(inputId = "argsMore_dataImpt", label = "更多参数设定", placeholder = "eg:nrows=10",value='',addon = icon("pencil")),
        helpText('在更多参数设置一栏，可以自定义参数，在此是read.table函数的参数，若无则留空，多个参数设定，用","隔开')
      )
      
    )
  })
  
  
  output$more1_dataImpt<-renderUI({
    list(
      panel(
        heading='变量筛选及数据名设定',
        pickerInput(
          inputId = "varsKeep_dataImpt",
          label = "选定需要保留的变量",
          choices = names(data_dataImpt()),
          selected =names(data_dataImpt()),
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ),
        textInputAddon(inputId = "dataName_dataImpt", label = "输入保存对象的名称", placeholder = "eg:mydata",value='data1',addon = icon("pencil"))
      )
      
    )
  })
  
  assign_dataImpt<-observeEvent(input$go_dataImpt,{
    isolate({
      data_dataImpt()->dat
      dat[,input$varsKeep_dataImpt]->dat
      assign(input$dataName_dataImpt,dat,envMedstats)
    })
    
  })
  
  
  
  output$varClass_dataImpt<-renderPrint({
    input$go_dataImpt
    req(input$go_dataImpt)
    isolate({
      cat('当前数据共：',nrow(data_dataImpt()),'观测(行)','\n')
      cat('当前数据共：',ncol(data_dataImpt()),'变量(列)','\n')
      cat('数据各变量类型如下：','\n')
      sapply(data_dataImpt(),class)
    })
  })
  
  
  output$head_dataImpt<-renderPrint({
    input$go_dataImpt
    req(input$go_dataImpt)
    isolate({
      # get(input$dataName_dataImpt,envir=envMedstats)->DatSel_dataImpt
      # head(DatSel_dataImpt,n=max(nrow(DatSel_dataImpt),10))
      skim(data_dataImpt())
    })
  })
  
#serverstop
} 
 



ui<-fluidPage(
  navbarPage(
#uistart

  tabPanel(
    '导入本地数据',
    sidebarLayout(
      position='left',
      sidebarPanel(
        panel(
          heading='导入数据',
          fileInput(
            'file_dataImpt', 
            '点击上传数据',
            accept = c(
              '.csv',
              '.tsv',
              '.txt'
            )
          ),
          helpText('注意：数据需为txt或csv格式文件，或复制表格数据(excel，csv等文件)到下面的窗口中'),
          aceEditor("text_dataImpt", value="Sex\tEffect\nM\tNo\nW\tNo\nW\tNo\nM\tNo\nM\tYes\nM\tYes\nM\tYes\nM\tNo\nW\tYes", mode="r", theme="chrome",height="150px")
          
        ),
        
        uiOutput('args_dataImpt'),
        uiOutput('more1_dataImpt'),
        
        actionBttn('go_dataImpt','确认加载！')
      ),
      
      mainPanel(
        panel(
          heading='原始数据变量描述',
          verbatimTextOutput('varClass_dataImpt'),
          status='primary'
        ),
        #hr(),
        panel(
          heading='载入数据查看',
          verbatimTextOutput('head_dataImpt'),
          status='primary'
        )
        
      )
      
    )
  )
  
#uistop
)
)

shinyApp(ui=ui,server=server)

