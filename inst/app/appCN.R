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
library(rio)
library(rhandsontable)
library(ROCR)



options(shiny.maxRequestSize = 50000*1024^2)



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
environment()->envMadis
LstMadis<-list()
LstMadis$desc<-data.frame(xvars=NA,Digits=NA,dataName=NA,stringsAsFactors = F)
LstMadis$hTest<-data.frame(xvars=NA,yvars=NA,alter=NA,paired=NA,nullHyp=NA,confLevel=NA,dataName=NA,stringsAsFactors = F)
LstMadis$myGlm<-data.frame(Formula=NA,data=NA,weightsVar=NA,subset=NA,Family=NA,lower=NA)
LstMadis$myTree<-data.frame(Formula=NA,data=NA,subset=NA,treeMethod=NA,Minsplit=NA,Minbucket=NA,Maxdepth=NA,CP=NA,Mincrit=NA)
LstMadis$myCox<-data.frame(Formula=NA,data=NA,weightsVar=NA,subset=NA,strataVar=NA,lower=NA)
LstMadis$myTable<-data.frame(Formula=NA,data=NA)
LstMadis$myGplt<-data.frame(data=NA,x=NA,y=NA,size=NA,fill=NA,color=NA,shape=NA,alpha=NA,facetVar=NA,
                               geom=NA,smoothMethod=NA,barPos=NA,labx=NA,laby=NA,title=NA,Bins=NA,theme=NA,Width=NA,
                               Colour=NA,Fill=NA,Size=NA,Alpha=NA,Shape=NA)
LstMadis$myProphet<-data.frame(data=NA,tsVar=NA, tsFormat=NA,measureVars=NA, groupVars = NA,Period = NA,FN=NA,
                                  Cap=NA,Floor=NA, Growth=NA,H=NA,yearlyS = NA,dailyS = NA,weeklyS = NA)
LstMadis$myLme<-data.frame(formulaFixed=NA,formulaRandom=NA,Method=NA,data=NA,subset=NA)
LstMadis$Kmeans<-data.frame(data=NA,vars=NA,infgr=NA,supgr=NA,Centers=NA,Criterion=NA,Iter=NA,
                            iterMax=NA,Algorithm=NA,subset=NA,clusterName=NA,seed=NA,addVar=NA)
LstMadis$pca<-data.frame(data=NA,vars=NA,nfcts=NA,Rotate=NA,Scores=NA,subset=NA,pcaVarName=NA,addVar=NA)
LstMadis$fa<-data.frame(data=NA,vars=NA,nfcts=2,Rotate=NA,Scores=NA,FM=NA,subset=NA,faVarName=NA,addVar=NA)
LstMadis$dataMnp<-data.frame(data=NA,subset=NA,newVars=NA,newVarsFormulas=NA,newVarsBy=NA,indexNames=NA,Formulas=NA,
                             dimVars=NA,dimNames=NA,dateVar=NA,dtOrders=NA,margin=NA, revisedMargin=NA,revisedNames=NA,  
                             revisedFormulas=NA,orderVars=NA,orders=NA,Digits=NA,tbVars=NA,hbVars=NA,colOrder=NA)


assign('LstMadis',LstMadis,env=envMadis)




server<-function(input,output,session){
  
  ###### 所有有可能引起数据变化的input ######
  change_data<-reactive({
    input$go_dataImpt
    input$go_varName
    input$go_varMnp
    input$go_varClass
    input$go_reshape
    input$go_unique
    input$go_dataMerge
    input$go_naImpute
    input$go_dataFilter
    input$go_kmeans
    input$go_pca
    input$go_fa
    input$go_match
  })
  
  
  ###### 数据导入功能(data_Impt) ######
  data_dataImpt<-reactive({
    
    # input$go_dataImpt
    # req(input$go_dataImpt)
    
    
    if(is.null(input$file_dataImpt)) {
      Data<-read.table(text=input$text_dataImpt,
                     sep="\t",
                     na.strings=input$nastr_dataImpt,
                     stringsAsFactors = input$strAsFac_dataImpt,
                     header=input$header_dataImpt,
                     fileEncoding = input$encod_dataImpt)
      # Data<-import(input$text_dataImpt)
      } else {
        inFile<-input$file_dataImpt
        if(input$argsMore_dataImpt=='') {
          
          # tt <- readLines(inFile$datapath)
          # tt <- gsub("([^\\]|^)'","\\1\"",tt)
          # tt <- gsub("\\\\","\\",tt)
          # zz <- textConnection(tt)
          
          Data<-read.table(inFile$datapath,
                           na.strings=input$nastr_dataImpt,
                           stringsAsFactors = input$strAsFac_dataImpt,
                           header=input$header_dataImpt,
                           fileEncoding = input$encod_dataImpt,
                           sep=input$sep_dataImpt)
          # Data<-import(inFile$datapath)
           # close(zz)
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
          # pickerInput(
          #   inputId='encod_dataImpt',
          #   label='文件编码格式',
          #   choices=c(
          #     'UTF8编码'='UTF8',
          #     'GB18030编码'='GB18030'
          #   ),
          #   selected='GB18030',
          #   multiple = FALSE,
          #   options = list(`actions-box` = FALSE)
          # ),
          textInputAddon(inputId = 'encod_dataImpt',label = '文件编码格式',value = 'gb18030',placeholder = 'eg:utf8',addon = icon("pencil")),
          awesomeCheckbox('header_dataImpt','数据包含变量名',TRUE),
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
      assign(input$dataName_dataImpt,dat,envMadis)
      LstMadis$Data[[input$dataName_dataImpt]]<-dat
      assign('LstMadis',LstMadis,envir=envMadis)
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
      # get(input$dataName_dataImpt,envir=envMadis)->DatSel_dataImpt
      # head(DatSel_dataImpt,n=max(nrow(DatSel_dataImpt),10))
      skim(data_dataImpt())
      
    })
  })
  
  
  
  
  
  
  
  ###### 变量名修改功能(var_Name) ######
  
  output$more1_varName<-renderUI({
    
    change_data()
    #?#
    
    list(
      panel(heading='选择数据集',
            pickerInput(
              inputId = "dataSel_varName",
              label = "选择数据集",
              choices = ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))],
              selected =ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))][1],
              multiple = FALSE,
              options = list(`actions-box` = FALSE)
            )
      )
      
    )
  })
  
  data_varName<-reactive({
    change_data()
    get(input$dataSel_varName,envMadis)->dat_varName
    return(dat_varName)
  })
  
  
  output$more2_varName<-renderUI({
    
    list(
      panel(heading='变量名修改',
            pickerInput(
              inputId = "var_varName",
              label = "选择需要修改的变量名",
              choices = names(data_varName()),
              selected =names(data_varName())[1],
              multiple = FALSE,
              options = list(`actions-box` = FALSE)
            ),
            #selectInput('var_varName','选择需要修改的变量名',names(data_varName())),
            textInputAddon(inputId = "new_varName", label = "设定新的变量名", placeholder = "eg:new_var1",value='',addon = icon("pencil"))
            
      )
    )
  })
  
  rename_varName<-reactive({
    input$go_varName           
    req(input$go_varName)
    isolate({
      data_varName()->dat
      ifelse(input$new_varName=='',input$var_varName,input$new_varName)->newName
      names(dat)[which(names(dat)==input$var_varName)]<-newName
      assign(input$dataSel_varName,dat,envMadis)
      LstMadis$Data[[input$dataSel_varName]]<-dat
      assign('LstMadis',LstMadis,envir=envMadis)
      return(dat)
    })
  })
  
  observeEvent(input$go_varName,{  #### newly added for update picker input values.
    updatePickerInput(session,inputId = 'var_varName',choices = names(rename_varName()))
  })
  
  output$summary_varName<-renderPrint({
    print(summary(rename_varName()))
    print(rename_varName())
    
  })
  
  
  
  
  ###### 生成新变量(varMnp) ######
  output$more1_varMnp<-renderUI({

    change_data()
    list(
      panel(
        heading='选择数据集',
        pickerInput(
          inputId = "dataSel_varMnp",
          label = "选择数据集",
          choices = ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))],
          selected =ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))][1],
          multiple = FALSE,
          options = list(`actions-box` = FALSE)
        )
        #selectInput('dataSel_varMnp','选择数据集',ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui'))])
      )
    )
  })
  
  data_varMnp<-reactive({
    change_data()
    
    get(input$dataSel_varMnp,envMadis)->data_varMnp
    return(data_varMnp)
    
  })
  
  output$more2_varMnp<-renderUI({
    data_varMnp()->dat
    list(
      panel(
        heading='选择操作类型',
        pickerInput(
          inputId='type_varMnp',
          label='创建变量的方式',
          choices=c(
            '基于原始数据的变量'='dep',
            '不基于原始数据的变量'='noDep'
          ),
          selected ='dep',
          multiple=FALSE,
          options= list(`actions-box` = FALSE)
        )
      ),
      conditionalPanel(
        condition="input['type_varMnp']=='dep'",
        panel(
          heading='选择处理的变量',
          pickerInput(
            inputId = "varsSel_varMnp",
            label = "选定进行操作的变量",
            choices = names(dat),
            #selected =names(dat)[1],
            multiple = TRUE,
            options = list(`actions-box` = TRUE)
          )
        ),
        panel(
          heading='设定处理方法或函数',
          awesomeCheckbox('usemyFun_varMnp','自定义函数?',FALSE),
          conditionalPanel(
            condition = "!input['usemyFun_varMnp']",
            pickerInput(
              inputId='method_varMnp',
              label='处理方法',
              choices=c(
                '无处理'='',
                '求和{Sum}'='sum',
                '均值{Mean}'='mean',
                '标准差{SD}'='sd',
                '方差{Var}'='var',
                '最小值{Min}'='min',
                '最大值{Max}'='max',
                '中位数{Median}'='median',
                '绝对值{ABS}'='abs',
                '对数转换{Log}'='log',
                '指数转换{Exp}'='exp',
                '正弦转换{Sin}'='sin',
                '余弦转换{Cos}'='cos',
                '字符串查找{Detect}'='detect',
                '字符串提取{Extract}'='extract',
                '字符串替换{Replace}'='replace',
                '字符串补齐{Pad}'='strpad',
                '字符串截取{Sub}'='substr',
                '字符串切割{Split}'='split',
                '字符串合并{Paste}'='paste',
                '合法值判定{LegalSet}'='legalSet',
                '重编码{Recode}'='reCode'
              ),
              selected ='',
              multiple=FALSE,
              options = list(`actions-box` = FALSE)
            ),
            panel(
              heading='设定各个函数的参数',
              conditionalPanel(
                condition="input['method_varMnp']=='detect'||input['method_varMnp']=='replace'||input['method_varMnp']=='split'",
                textInputAddon('pattern_varMnp',label='模式(pattern)',value='',addon=icon('pencil')),
                awesomeCheckbox('regex_varMnp','是否为正则表达式',FALSE)
              ),
              conditionalPanel(
                condition="input['method_varMnp']=='replace'",
                pickerInput(
                  inputId='mode_replace',
                  label='字符串位置',
                  choices = c(
                    '第一个'='first',
                    '最后一个'='last',
                    '所有'='all'
                  ),
                  selected ='first',
                  multiple=FALSE,
                  options = list(`actions-box` = FALSE)
                ),
                textInputAddon('replacement_replace',label='替换的字符串(replacement)',value='',addon=icon('pencil'))
              ),
              conditionalPanel(
                condition = "input['method_varMnp']=='strpad'",
                numericInput('width_strpad','补齐的长度',min=0,max=Inf,value=10),
                textInputAddon(input='pad_strpad','需要填补的字符串',value='',placeholder = 'eg:0',addon = icon('pencil')),
                pickerInput(
                  inputId='side_strpad',
                  label='补齐方式',
                  choices = c(
                    '左侧补齐'='left',
                    '右侧补齐'='right',
                    '双侧补齐'='both'
                  ),
                  selected ='left',
                  multiple=FALSE,
                  options = list(`actions-box` = FALSE)
                )
              ),
              conditionalPanel(
                condition="input['method_varMnp']=='split'",
                numericInput('indexSplit_varMnp','选择分隔之后的第n个',value=1,min=1,max=100,step=1)
              ),
              conditionalPanel(
                condition="input['method_varMnp']=='substr'",
                numericInput('from_substr','起始位置',value=1,min=1,max=1000,step=1),
                numericInput('to_substr','结束位置',value=1,min=1,max=1000,step=1)
              ),
              
              conditionalPanel(
                condition = "input['method_varMnp']=='extract'",
                pickerInput(
                  inputId='mode_extract',
                  label='抽取的方式',
                  choices = c(
                    '第一个'='first',
                    '最后一个'='last',
                    '所有'='all'
                  ),
                  selected ='first',
                  multiple=FALSE,
                  options = list(`actions-box` = FALSE)
                ),
                textInputAddon(inputId='pattern_extract','输入抽取的字符串',value='',placeholder = 'eg:female',addon = icon('pencil')),
                awesomeCheckbox('regex_extract','正则表达式？',FALSE)
              ),
              conditionalPanel(
                condition="input['method_varMnp']=='paste'",
                textInputAddon('pasteSep_varMnp',label='拼接的字符或符号',value='',addon=icon('pencil'))
              ),
              conditionalPanel(
                condition="input['method_varMnp']=='legalSet'",
                pickerInput(
                  inputId='legalType_varMnp',
                  label='设置方法类别',
                  choices=c(
                    '区间范围'='ranges',
                    '元素'='elements',
                    '子字符串'='substrs'
                  ),
                  selected ='ranges',
                  multiple=FALSE,
                  options = list(`actions-box` = FALSE)
                ),
                textInputAddon(inputId='legalSet_varMnp',label='设定合法值范围',value='',addon=icon('pencil')),
                helpText('合法值范围可以取多个，用";"隔开'),
                awesomeCheckbox('regexLegal_varMnp','是否为正则表达式',FALSE)
              ),
              conditionalPanel(
                condition="input['method_varMnp']=='reCode'",
                pickerInput(
                  inputId='reCodeType_varMnp',
                  label='设置方法类别',
                  choices=c(
                    '区间范围'='ranges',
                    '元素'='elements',
                    '子字符串'='substrs'
                  ),
                  selected ='ranges',
                  multiple=FALSE,
                  options = list(`actions-box` = FALSE)
                ),
                textInputAddon(inputId='reGroup_varMnp',label='设定分组',value='',addon=icon('pencil')),
                textInputAddon(inputId='reGroupLabel_varMnp',label='设定分组标签',value='',addon=icon('pencil')),
                textInputAddon(inputId='reGroupOther_varMnp',label='设定其他组(未定义组)分组标签',value='Others',addon=icon('pencil'))
              )
            )
          ),
          conditionalPanel(
            condition = "input['usemyFun_varMnp']",
            textInputAddon(inputId = "fun_varMnp", label = "输入自定义的函数", placeholder = "eg:myfun",value='',addon = icon("pencil")),
            helpText('此处提供自定义函数，函数写法和R中自定义函数一致，如:funtion(x)sd(x)/mean(x)')
          )
        )
      ),
      
      conditionalPanel(
        condition="input['type_varMnp']=='noDep'",
        panel(
          heading='生成新变量的代码',
          textInputAddon('creatVar_varMnp',label='输入生成的新变量的代码',value='',addon=icon('pencil'))
        )
      ),
      
      panel(
        heading='设定新变量名',
        textInputAddon(inputId='varNewName_varMnp',label='输入新变量的名称',placeholder='eg:newVar1',value='',addon=icon('pencil')),
        helpText('变量名如果留空，则默认将原始变量名增加"_new"作为新变量名称，若原始变量数量大于1，则默认用第一个变量名')
      ),
      panel(
        heading='保存数据集',
        textInputAddon(inputId='dataName_varMnp',label='保存的数据名称',value='',placeholder = 'eg:data_newVarMnp',addon=icon('pencil'))
      )
    )
  })
  
  
  res_varMnp<-reactive({
    input$go_varMnp
    req(input$go_varMnp)
    isolate({
      data_varMnp()->dat
      
      if(input$type_varMnp=='dep'){
        if(input$method_varMnp==''&input$fun_varMnp==''){
          dat<-dat
        } else {
          if(!input$usemyFun_varMnp){
            if(input$method_varMnp%in%c('detect','replace','substr','split','paste','strpad','extract')) {
              if(input$method_varMnp=='detect'){
                if(!input$regex_varMnp){
                  resVarMnp<-Apply(dat,vars=input$varsSel_varMnp,MARGIN=1,FUN=function(x)stri_detect_fixed(x,input$pattern_varMnp))
                } else {
                  resVarMnp<-Apply(dat,vars=input$varsSel_varMnp,MARGIN=1,FUN=function(x)stri_detect_regex(x,input$pattern_varMnp))
                }
                
              }
              
              if(input$method_varMnp=='replace'){
                if(!input$regex_varMnp){
                  resVarMnp<-Apply(dat,vars=input$varsSel_varMnp,MARGIN=1,FUN=function(x)stri_replace(x,fixed=input$pattern_varMnp,replacement=input$replacement_replace,mode=input$mode_replace))
                } else {
                  resVarMnp<-Apply(dat,vars=input$varsSel_varMnp,MARGIN=1,FUN=function(x)stri_replace(x,regex=input$pattern_varMnp,replacement=input$replacement_replace,mode=input$mode_replace))
                  
                }
                
              }
              
              if(input$method_varMnp=='strpad'){
                resVarMnp<-Apply(dat,vars=input$varsSel_varMnp,MARGIN=1,FUN=function(x)stri_pad(x,width=input$width_strpad,pad=input$pad_strpad,side=input$side_strpad))
              }
              
              if(input$method_varMnp=='substr'){
                resVarMnp<-Apply(dat,vars=input$varsSel_varMnp,MARGIN=1,FUN=function(x)stri_sub(x,from=input$from_substr,to=input$to_substr))
              }
              
              if(input$method_varMnp=='extract'){
                if(!input$regex_extract){
                  resVarMnp<-Apply(dat,vars=input$varsSel_varMnp,MARGIN=1,FUN=function(x)stri_extract(x,fixed=input$pattern_extract,mode=input$mode_extract)[[1]][1])
                } else {
                  resVarMnp<-Apply(dat,vars=input$varsSel_varMnp,MARGIN=1,FUN=function(x)stri_extract(x,regex=input$pattern_extract,mode=input$mode_extract)[[1]][1])
                }
                
              }
              
              if(input$method_varMnp=='split'){
                if(!input$regex_varMnp){
                  resVarMnp<-Apply(dat,vars=input$varsSel_varMnp,MARGIN=1,FUN=function(x)stri_split_fixed(x,input$pattern_varMnp)[[1]][input$indexSplit_varMnp])
                } else {
                  resVarMnp<-Apply(dat,vars=input$varsSel_varMnp,MARGIN=1,FUN=function(x)stri_split_regex(x,input$pattern_varMnp)[[1]][input$indexSplit_varMnp])
                }
              }
              
              if(input$method_varMnp=='paste'){
                resVarMnp<-Apply(dat,vars=input$varsSel_varMnp,MARGIN=1,FUN=function(x)stri_c(x,collapse=input$pasteSep_varMnp)[input$indexSplit_varMnp])
              }
              
            } 
            if(input$method_varMnp%in%c('sum','mean','sd','var','min','max','median','max','abs','log','exp','sin','cos')){
              resVarMnp<-Apply(dat,vars=input$varsSel_varMnp,MARGIN=1,FUN=input$method_varMnp)
            }
            if(input$method_varMnp%in%c('legalSet','reCode')){
              ifelse(class(dat[,input$varsSel_varMnp])%in%c('numeric','integer'),'numeric',
                     ifelse(class(dat[,input$varsSel_varMnp])%in%c('character','factor'),'character',
                            ifelse(class(dat[,input$varsSel_varMnp])=='Date','datetime','unknown')))->mode
              
              if(input$method_varMnp=='legalSet'){
                method=input$legalType_varMnp
                Legal=as.vector(unlist(stri_split_fixed(input$legalSet_varMnp,';')))
                type=ifelse(input$regexLegal_varMnp,'regex','fixed')
                resVarMnpLogi<-Apply(dat,vars=input$varsSel_varMnp,MARGIN=1,FUN=function(x)validateVec(x,L=Legal,method=method,mode=mode,type=type))
                if(length(dim(resVarMnpLogi))==0){
                  resVarMnp<-ifelse(resVarMnpLogi,dat[,input$varsSel_varMnp],NA)
                } else {
                  resVarMnp<-matrix(nc=ncol(resVarMnpLogi),nr=nrow(resVarMnpLogi))
                  for(i in 1:length(input$varsSel_varMnp)){
                    resVarMnp[i,]<-ifelse(resVarMnpLogi[i,],dat[,input$varsSel_varMnp[i]],NA)
                  }
                }
                
                
              }
              
              if(input$method_varMnp=='reCode'){
                method=input$reCodeType_varMnp
                as.list(unlist(strsplit(input$reGroup_varMnp,';',fixed=T)))->groups
                if(mode!='numeric') {
                  for(i in 1:length(groups)){
                    as.vector(unlist(strsplit(groups[[i]],',',fixed=T)))->groups[[i]]
                  }
                }
                as.vector(unlist(strsplit(input$reGroupLabel_varMnp,';',fixed=T)))->labels
                resVarMnp<-Apply(dat,vars=input$varsSel_varMnp,MARGIN=1,FUN=function(x)recode(x,groups=groups,Labels=labels,method=method,mode=mode,na.val=input$reGroupOther_varMnp))
                
              }
            }
            
          } else {
            resVarMnp<-Apply(dat,vars=input$varsSel_varMnp,MARGIN=1,FUN=eval(parse(text=input$fun_varMnp)))
          }
          namOrig<-input$varsSel_varMnp
          if(is.null(namOrig)){
            dat
          } else {
            if(length(dim(resVarMnp))==0){
              if(input$varNewName_varMnp=='') {
                dat[,paste0(paste(namOrig,collapse='_'),'_new')]<-resVarMnp
              } else {
                dat[,paste(paste(namOrig,collapse='_'),input$varNewName_varMnp,sep='_')]<-resVarMnp
              }
            } else {
              if(input$varNewName_varMnp=='') {
                dat[,paste0(paste(namOrig,collapse='_'),'_new')]<-(resVarMnp)
              } else {
                dat[,paste(paste(namOrig,collapse='_'),input$varNewName_varMnp,sep='_')]<-(resVarMnp)
              }
            }
            
          }
        }
      }
      
      if(input$type_varMnp=='noDep'){
        if(input$creatVar_varMnp==''){
          dat<-dat
        } else {
          if(input$varNewName_varMnp==''){
            dat[,'newVarCreat']<-eval(parse(text=input$creatVar_varMnp))
          } else {
            dat[,input$varNewName_varMnp]<-eval(parse(text=input$creatVar_varMnp))
          }
        }
        
      }
      
      
    })
    if(input$dataName_varMnp==''){
      assign(input$dataSel_varMnp,dat,env=envMadis)
      LstMadis$Data[[input$dataSel_varMnp]]<-dat
    } else {
      assign(input$dataName_varMnp,dat,env=envMadis)
      LstMadis$Data[[input$dataName_varMnp]]<-dat
    }
    
    assign('LstMadis',LstMadis,envir=envMadis)
    
    #assign(input$dataSel_varMnp,dat,envMadis)
    return(dat)
  })
  
  observeEvent(input$go_varMnp,{  #### newly added for update picker input values.
    updatePickerInput(session,inputId = 'varsSel_varMnp',choices = names(res_varMnp()))
    # if(input$dataName_varMnp==''){
    #   NULL
    # } else {
    #   updatePickerInput(session,inputId = 'datSel_varMnp',choices = ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))])
    # }
    
  })
  
  output$summary_varMnp<-renderPrint({
    res_varMnp()->dt
    tryCatch(print(pander(head(dt))),error=function(e)print(head(dt)))
    #print(summary(dt))
    
  })
  
  
  
  
  
  
  ###### 变量类型转换(varClass) ######
  
  output$more1_varClass<-renderUI({
    
    change_data()
    #?#
    list(
      panel(
        heading='选择数据集',
        pickerInput(
          inputId = "dataSel_varClass",
          label = "选择数据集",
          choices = ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))],
          selected =ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))][1],
          multiple = FALSE,
          options = list(`actions-box` = FALSE)
        )
        #selectInput('dataSel_varClass','选择数据集',ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))])
      )
    )
  })
  
  data_varClass<-reactive({
    
    change_data()
    #?#
    get(input$dataSel_varClass,envMadis)->data_varClass
    return(data_varClass)
  })
  
  output$more2_varClass<-renderUI({
    list(
      panel(
        heading='变量类型转换',
        awesomeCheckbox('auto_varClass','是否进行自动判断？',TRUE),
        conditionalPanel(
          condition="input['auto_varClass']",
          numericInput('lengthTab','唯一元素数目',min=1,max=1000,value=10),
          numericInput('threshold','阈值',min=0,max=1,value=0.8)
          
        ),
        conditionalPanel(
          condition="!input['auto_varClass']",
          pickerInput(
            inputId='varsNum_varClass',
            label='转换为数值型变量',
            choices=names(data_varClass()),
            #selected =names(data_varClass())[1],
            multiple=TRUE,
            options = list(`actions-box` = TRUE)
          ),
          pickerInput(
            inputId='varsChar_varClass',
            label='转换为字符型变量',
            choices=names(data_varClass()),
            #selected =names(data_varClass())[1],
            multiple=TRUE,
            options = list(`actions-box` = TRUE)
          ),
          panel(
            heading='日期时间转换设置',
            pickerInput(
              inputId='varsDate_varClass',
              label='转换为日期型变量',
              choices=names(data_varClass()),
              #selected =names(data_varClass())[1],
              multiple=TRUE,
              options = list(`actions-box` = TRUE)
            ),
            pickerInput(
              inputId='dateFormat',
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
              inputId='timeFormat',
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
            )
          ),
          
          
          pickerInput(
            inputId='varsOrder_varClass',
            label='转换为有序型变量',
            choices=c('无'='',names(data_varClass())),
            #selected =names(data_varClass())[1],
            multiple=FALSE,
            options = list(`actions-box` = FALSE)
          )
        )
      )
    )
  })
  
  output$more3_varClass<-renderUI({
    if(input$varsOrder_varClass!='') {
      chcs<-unique(data_varClass()[,input$varsOrder_varClass])}
    else {chcs<-''}
    list(
      conditionalPanel(
        condition = "input['varsOrder_varClass']!=''",
        selectizeInput(
          inputId="order_varsOrder",
          label='有序变量各水平排序',
          choices=chcs,
          multiple=TRUE
        )
      ),
      panel(
        heading='保存数据集',
        textInputAddon(inputId='dataName_varClass',label='保存的数据名称',value='',placeholder = 'eg:data_newVarType',addon=icon('pencil'))
      )
    )
  })
  
  res_varClass<-reactive({
    input$go_varClass
    req(input$go_varClass)
    isolate({
      data_varClass()->dat
      if(input$auto_varClass){
        autoVarClass(data=dat,lenTab=input$lengthTab,thresh=input$threshold)->modeVars
        colnames(modeVars)->nameVars
        as.vector(modeVars)->modes
        for(i in 1:ncol(dat)){
          if(modeVars[i]=='char'){
            as.character(as.vector(dat[,i]))->dat[,i]
          }
          if(modeVars[i]=='num'){
            as.numeric(as.vector(dat[,i]))->dat[,i]
          }
        }
      }
      
      if(!input$auto_varClass){
        if(length(input$varsNum_varClass)>0){
          for(i in input$varsNum_varClass){
            as.numeric(as.vector(dat[,i]))->dat[,i]
          }
        }
        
        if(length(input$varsChar_varClass)>0){
          for(i in input$varsChar_varClass){
            as.character(as.vector(dat[,i]))->dat[,i]
          }
        }
        
        if(input$varsOrder_varClass!=''){
          #for(i in input$varsNum_varClass){
          ordered(as.vector(dat[,input$varsOrder_varClass]),levels=input$order_varsOrder)->dat[,input$varsOrder_varClass]
          #}
        }
        
        if(length(input$varsDate_varClass)>0){
          for(i in input$varsDate_varClass){
            parse_date_time(as.vector(dat[,i]),orders=paste(input$dateFormat,input$timeFormat,sep=''))->dat[,i]
          }
        }
        
        if(all(is.null(c(input$varsNum_varClass,input$varsDate_varClass,input$varsDate_varClass)))) {dat->dat}
      }
      if(input$dataName_varClass==''){
        assign(input$dataSel_varClass,dat,env=envMadis)
        LstMadis$Data[[input$dataSel_varClass]]<-dat
      } else {
        assign(input$dataName_varClass,dat,env=envMadis)
        LstMadis$Data[[input$dataName_varClass]]<-dat
      }
      
      #LstMadis$Data[[input$dataName_varMnp]]<-dat
      assign('LstMadis',LstMadis,envir=envMadis)
      
      return(dat)
    })
  })
  
  output$summary_varClass<-renderPrint({
    input$go_varClass
    isolate({
      res_varClass()->dt
      sapply(dt,class)
      tryCatch(print(pander(head(dt))),error=function(e)print(head(dt)))
      sapply(dt,class)->y
      unique(y)->x
      sapply(x,function(i)names(y)[which(y==i)])->res
      tryCatch(print(pander(res)),error=function(e)print(res))
      
      skim(dt)
    })
    
  })
  
  
  
  ###### 数据变形(reshape) ######
  
  output$more1_reshape<-renderUI({
    
    change_data()
    #?#
    list(
      panel(
        heading='选择数据集',
        pickerInput(
          inputId = "dataSel_reshape",
          label = "选择数据集",
          choices = ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))],
          selected =ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))][1],
          multiple = FALSE,
          options = list(`actions-box` = FALSE)
        )
        #selectInput('dataSel_reshape','选择数据集',ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))])
      )
    )
  })
  
  data_reshape<-reactive({
    
    change_data()
    #?#
    get(input$dataSel_reshape,envMadis)->dataReshape
    return(dataReshape)
  })
  
  output$more2_reshape<-renderUI({
    data_reshape()->dat
    list(
      panel(
        heading='选择转换方式',
        pickerInput(
          inputId='reshapeMethod',
          label='选择转换方式',
          choices=c(
            '行转列'='melt',
            '列转行'='cast'
          ),
          selected ='melt',
          multiple=FALSE,
          options = list(`actions-box` = FALSE)
        )
      ),
      conditionalPanel(
        condition = "input['reshapeMethod']=='melt'",
        panel(
          heading='设定行转列(melt)方法的各个参数',
          pickerInput(
            inputId='idVars',
            label='选择ID变量',
            choices=names(dat),
            #selected =names(dat)[1],
            multiple=TRUE,
            options = list(`actions-box` = TRUE)
          ),
          pickerInput(
            inputId='measureVars',
            label='选择测量变量',
            choices=names(dat),
            #selected =names(dat)[1],
            multiple=TRUE,
            options = list(`actions-box` = TRUE)
          ),
          textInputAddon(inputId = "varName_melt", label = "设定变量名称", placeholder = "eg:variable",value='variable',addon = icon("pencil")),
          flowLayout(
            awesomeCheckbox('naRm_melt','排除缺失值',FALSE),
            awesomeCheckbox('facsAsStrs_melt','因子转化成字符',TRUE)
          )
        )
      ),
      conditionalPanel(
        condition="input['reshapeMethod']=='cast'",
        panel(
          heading='设定列转行方法(dcast)的各个参数',
          pickerInput(
            inputId='lhs_cast',
            label='选择左变量',
            choices=names(dat),
            #selected =names(dat)[1],
            multiple=TRUE,
            options = list(`actions-box` = TRUE)
          ),
          pickerInput(
            inputId='rhs_cast',
            label='选择右变量',
            choices=names(dat),
            #selected =names(dat)[1],
            multiple=TRUE,
            options = list(`actions-box` = TRUE)
          ),
          pickerInput(
            inputId='valueVar_cast',
            label='选择值变量',
            choices=names(dat),
            #selected =names(dat)[1],
            multiple=TRUE,
            options = list(`actions-box` = TRUE)
          ),
          textInputAddon(inputId='argsMore_cast','其他参数(参考dcast)',value='',placeholder = 'eg:drop=TRUE',addon = icon("pencil")),
          pickerInput(
            inputId='fnAggre',
            label='选择合并函数',
            choices=c(
              '最小值'='min',
              '最大值'='max',
              '平均值'='mean',
              '中位数'='median',
              '标准差'='sd',
              '自定义函数'='myFun'
            ),
            selected ='min',
            multiple=FALSE,
            options = list(`actions-box` = FALSE)
          ),
          conditionalPanel(
            condition = "input['fnAggre']=='myFun'",
            textInputAddon(inputId='myFunAggre','输入自定义函数',value='',placeholder = 'eg:function(x)mean(x)',addon = icon("pencil"))
          )
        )
      ),
      panel(
        heading='保存数据',
        textInputAddon(inputId='dataName_reshape','保存为对象名称',value='',placeholder = 'eg:data_reshape',addon = icon("pencil"))
        
      )
    )
  })
  
  res_reshape<-reactive({
    input$go_reshape
    req(input$go_reshape)
    isolate({
      data_reshape()->dat
      if(input$reshapeMethod=='melt'){
        if(is.null(input$measureVars)){
          dat_reshape<-melt(data=dat,id.vars=input$idVars,variable.name=input$varName_melt)
        }
        
        if(is.null(input$idVars)){
          dat_reshape<-melt(data=dat,measure.vars = input$measureVars,variable.name=input$varName_melt)
          
        }
        
        if(!is.null(input$idVars)&&!is.null(input$measureVars)){
          dat_reshape<-melt(data=dat,id.vars=input$idVars,measure.vars = input$measureVars,variable.name=input$varName_melt)
        }
        
      }
      
      if(input$reshapeMethod=='cast'){
        if(input$fnAggre!='myFun'){
          if(input$argsMore_cast==''){
            as.formula(paste(paste(input$lhs_cast,collapse='+'),paste(input$rhs_cast,collapse='+'),sep='~'))->Form
            dat_reshape<-dcast(data=dat,formula=Form,fun.aggregate=eval(parse(text=input$fnAggre)),value.var=input$valueVar_cast)
          } else {
            as.formula(paste(paste(input$lhs_cast,collapse='+'),paste(input$rhs_cast,collapse='+'),sep='~'))->Form
            text_dcast<-paste(paste("dcast(data=dat","formula=Form","fun.aggregate=eval(parse(text=input$fnAggre))","value.var=input$valueVar_cast",input$argsMore_cast,sep=','),")")
            dat_reshape<-eval(parse(text=text_dcast))
          }
        } else {
          if(input$argsMore_cast==''){
            as.formula(paste(paste(input$lhs_cast,collapse='+'),paste(input$rhs_cast,collapse='+'),sep='~'))->Form
            dat_reshape<-dcast(data=dat,formula=Form,fun.aggregate=eval(parse(text=input$fnAggre)),value.var=input$valueVar_cast)
          } else {
            as.formula(paste(paste(input$lhs_cast,collapse='+'),paste(input$rhs_cast,collapse='+'),sep='~'))->Form
            text_dcast<-paste(paste("dcast(data=dat","formula=Form","fun.aggregate=eval(parse(text=input$fnAggre))","value.var=input$valueVar_cast",input$argsMore_cast,sep=','),")")
            dat_reshape<-eval(parse(text=text_dcast))
          }
        }
        
      }
      
      if(input$dataName_reshape==''){
        assign(paste0(input$dataSel_reshape,'reshaped'),dat_reshape,env=envMadis)
        LstMadis$Data[[paste0(input$dataSel_reshape,'reshaped')]]<-dat
      } else {
        assign(input$dataName_reshape,dat_reshape,env=envMadis)
        LstMadis$Data[[input$dataName_reshape]]<-dat
      }
      
      #LstMadis$Data[[input$dataName_varMnp]]<-dat
      assign('LstMadis',LstMadis,envir=envMadis)
      return(dat_reshape)
    })
  })
  
  
  output$summary_reshape<-renderPrint({
    input$go_reshape
    #?#input$dataSel_reshape
    get(input$dataSel_reshape,envMadis)->dataReshape
    res_reshape()->dt
    print(head(dt,n=10))
    print(summary(dt))
    print(summary(dataReshape))
    print(change_data())
  })
  
  
  
  
  ###### 数据去重(unique) ######
  output$more1_unique<-renderUI({
    
    change_data()
    #?#
    list(
      panel(
        heading='选择处理的数据集',
        pickerInput(
          inputId = "dataSel_unique",
          label = "选择数据集",
          choices = ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))],
          selected =ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))][1],
          multiple = FALSE,
          options = list(`actions-box` = FALSE)
        )
        #selectInput('dataSel_naImpute','选择数据集',ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))])
      )
    )
  })
  
  data_unique<-reactive({
    
    change_data()
    #?#
    get(input$dataSel_unique,envMadis)->datanaunique
    return(datanaunique)
  })
  
  output$more2_unique<-renderUI({
    panel(
      heading='设定数据集名称',
      textInputAddon('dataName_unique','输入保存的数据集名称',value='',placeholder = 'eg:dataUnique',addon = icon('pencil'))
    )
  })
  
  res_unique<-reactive({
    input$go_unique
    #input$dataName_unique
    req(input$go_unique)
    isolate({
      data_unique()->dat
      unique(dat)->dataUnique
      if(input$dataName_unique==''){
        assign(paste0(input$dataSel_unique,'Unique'),dataUnique,envMadis)
        LstMadis$Data[[paste0(input$dataSel_unique,'Unique')]]<-dat
      } else {
        assign(input$dataName_unique,dataUnique,envMadis)
        LstMadis$Data[[input$dataName_unique]]<-dat
      }
      
      #LstMadis$Data[[input$dataName_varMnp]]<-dat
      assign('LstMadis',LstMadis,envir=envMadis)
      return(dataUnique)
    })
  })
  
  output$summary_unique<-renderPrint({
    input$go_unique
    print(head(res_unique()))
    print(summary(res_unique()))
  })
  
  
  
  ###### 数据合并(Merge and Bind) ######
  output$more1_dataMerge<-renderUI({
    
    change_data()
    #?#
    list(
      panel(
        heading='选择需要合并的数据集',
        pickerInput(
          inputId = "dataSel1_dataMerge",
          label = "选择数据集1",
          choices = ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))],
          selected =ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))][1],
          multiple = FALSE,
          options = list(`actions-box` = FALSE)
        ),
        pickerInput(
          inputId = "dataSel2_dataMerge",
          label = "选择数据集2",
          choices = ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))],
          selected =ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))][1],
          multiple = FALSE,
          options = list(`actions-box` = FALSE)
        )
        #selectInput('dataSel1_dataMerge','选择数据集1',ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))]),
        #selectInput('dataSel2_dataMerge','选择数据集2',ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))])
      )
    )
  })
  
  data_dataMerge<-reactive({
    
    change_data()
    #?#
    get(input$dataSel1_dataMerge,envMadis)->dataMerge1
    get(input$dataSel2_dataMerge,envMadis)->dataMerge2
    return(list(dat1=dataMerge1,dat2=dataMerge2))
  })
  
  output$more2_dataMerge<-renderUI({
    data_dataMerge()[['dat1']]->dat1
    data_dataMerge()[['dat2']]->dat2
    
    list(
      panel(
        heading="选择合并的方式",
        pickerInput(
          inputId='method_dataMerge',
          label="合并方式",
          choices=c(
            '链接(Join)'='Merge',
            '合并(Bind)'='Bind'
          ),
          selected ='Merge',
          multiple=FALSE,
          options = list(`actions-box` = FALSE)
        )
      ),
      
      panel(
        heading='设定合并的相关参数',
        conditionalPanel(
          condition="input['method_dataMerge']=='Merge'",
          pickerInput(
            inputId='joinMethod',
            label='选择链接方式',
            choices=c(
              '左链接'='left',
              '右链接'='right',
              '内链接'='inner',
              '全链接'='full'
            ),
            selected ='left',
            multiple=FALSE,
            options = list(`actions-box` = FALSE)
          ),
          selectizeInput(
            inputId='byX',
            label='选择数据集1的链接变量',
            choices=names(dat1),
            multiple=T#,
            #options = list(`actions-box` = FALSE)
          ),
          selectizeInput(
            inputId='byY',
            label='选择数据集2的链接变量',
            choices=names(dat2),
            multiple=T#,
            #options = list(`actions-box` = FALSE)
          ),
          awesomeCheckbox('sortMerge','对数据重新排序？',FALSE)
        ),
        conditionalPanel(
          condition = "input['method_dataMerge']=='Bind'",
          pickerInput(
            inputId='bindMethod',
            label='合并的方式',
            choices=c(
              '上下合并'='rBind',
              '左右合并'='cBind'
            ),
            selected ='rBind',
            multiple=FALSE,
            options = list(`actions-box` = FALSE)
          ),
          helpText('注意，上下合并，要求两数据变量名完全一致，左右合并要求量数据行数相等')
        )
        
      ),
      panel(
        heading='设定新数据集的名称',
        textInputAddon(inputId='dataName_dataMerge',label='数据集名称',value='',placeholder = 'eg:data_Bind',addon = icon('pencil'))
      )
    )
  })
  
  res_dataMerge<-reactive({
    input$go_dataMerge
    req(input$go_dataMerge)
    isolate({
      data_dataMerge()[['dat1']]->dat1
      data_dataMerge()[['dat2']]->dat2
      
      if(input$method_dataMerge=='Merge'){
        if(input$joinMethod=='left'){
          datMerge<-merge(dat1,dat2,by.x=input$byX,by.y=input$byY,sort=input$sortMerge,all.x=T,all.y=F)
        }
        
        if(input$joinMethod=='right'){
          datMerge<-merge(dat1,dat2,by.x=input$byX,by.y=input$byY,sort=input$sortMerge,all.x=F,all.y=T)
        }
        
        if(input$joinMethod=='inner'){
          datMerge<-merge(dat1,dat2,by.x=input$byX,by.y=input$byY,sort=input$sortMerge,all.x=F,all.y=F)
        }
        
        if(input$joinMethod=='full'){
          datMerge<-merge(dat1,dat2,by.x=input$byX,by.y=input$byY,sort=input$sortMerge,all.x=T,all.y=T)
        }
      }
      
      if(input$method_dataMerge=='Bind'){
        if(input$bindMethod=='rBind'){
          datMerge<-rbind(dat1,dat2)
        }
        
        if(input$bindMethod=='cBind'){
          datMerge<-cbind(dat1,dat2)
        }
      }
      
      if(input$dataName_dataMerge==''){
        assign(paste0(input$dataSel1_dataMerge,input$dataSel2_dataMerge),datMerge,envMadis)
        LstMadis$Data[[paste0(input$dataSel1_dataMerge,input$dataSel2_dataMerge)]]<-dat
      } else {
        assign(input$dataName_dataMerge,datMerge,envMadis)
        LstMadis$Data[[input$dataName_dataMerge]]<-dat
      }
      #LstMadis$Data[[input$dataName_varMnp]]<-dat
      assign('LstMadis',LstMadis,envir=envMadis)
      return(datMerge)
    })
  })
  
  output$summary_dataMerge<-renderPrint({
    input$go_dataMerge
    print(head(res_dataMerge()))
    print(summary(res_dataMerge()))
  })
  
  
  
  ###### 缺失值填补(naImpute) ######
  
  output$more1_naImpute<-renderUI({
    
    change_data()
    #?#
    list(
      panel(
        heading='选择处理的数据集',
        pickerInput(
          inputId = "dataSel_naImpute",
          label = "选择数据集",
          choices = ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))],
          selected =ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))][1],
          multiple = FALSE,
          options = list(`actions-box` = FALSE)
        )
        #selectInput('dataSel_naImpute','选择数据集',ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))])
      )
    )
  })
  
  data_naImpute<-reactive({
    
    change_data()
    #?#
    get(input$dataSel_naImpute,envMadis)->datanaImpute
    return(datanaImpute)
  })
  
  output$more2_naImpute<-renderUI({
    data_naImpute()->dat
    list(
      panel(
        heading='选择填补的变量',
        pickerInput(
          inputId='var_naImpute',
          label='选择变量',
          choices = names(dat),
          selected =names(dat)[1],
          multiple=TRUE,
          options = list(`actions-box` = TRUE)
        )
      ),
      panel(
        heading='选择填补方法',
        pickerInput(
          inputId='method_impute',
          label='选择填补方法',
          choices = c(
            '均值{数值型}'='mean',
            '中位数{数值型}'='median',
            '最小值{数值型}'='min',
            '最大值{数值型}'='max',
            '众数{数值型或字符型}'='most',
            '随机抽取{数值型或字符型}'='random',
            # '树模型填补'='treeImpute',
            'MICE模型填补'='MICE',
            '自定义方法'='myFun'
          ),
          selected ='mean',
          multiple=FALSE,
          options = list(`actions-box` = FALSE)
        ),
        conditionalPanel(
          condition = "input['method_impute']=='myFun'",
          textInputAddon(inputId='funImpute',label='自定义填补函数',value='',placeholder = 'eg:function(x)mean(x)',addon = icon('pencil'))
        ),
        conditionalPanel(
          condition = "input['method_impute']=='treeImpute'",
          pickerInput(
            inputId='var_treeModel',
            label='选择纳入模型的变量',
            choices = names(dat),
            selected =names(dat)[1],
            multiple=TRUE,
            options = list(`actions-box` = TRUE)
          ),
          pickerInput(
            inputId='method_impute',
            label='选择填补方法',
            choices = c(
              '随机抽样'='sample',
              '模型预测'='pred'
            ),
            selected ='sample',
            multiple=FALSE,
            options = list(`actions-box` = FALSE)
          )
          #textInputAddon(inputId='funImpute',label='自定义填补函数',value='',placeholder = 'eg:function(x)mean(x)',addon = icon('pencil'))
        ),
        
        conditionalPanel(
          condition = "input['method_impute']=='MICE'",
          pickerInput(
            inputId='method_MICE',
            label='选择MICE方法',
            choices = c(
              'pmm',
              'cart',
              'midastouch',
              'sample',
              'mean',
              'logreg',
              'polr',
              'polyreg',
              'lda'
              
            ),
            selected ='cart',
            multiple=FALSE,
            options = list(`actions-box` = TRUE)
          )
          # ,
          # pickerInput(
          #   inputId='method_impute',
          #   label='选择填补方法',
          #   choices = c(
          #     '随机抽样'='sample',
          #     '模型预测'='pred'
          #   ),
          #   selected ='sample',
          #   multiple=FALSE,
          #   options = list(`actions-box` = FALSE)
          # )
          #textInputAddon(inputId='funImpute',label='自定义填补函数',value='',placeholder = 'eg:function(x)mean(x)',addon = icon('pencil'))
        )
        
      ),
      conditionalPanel(
        condition = "input['method_impute']=='random'",
        awesomeCheckbox('rep_naImpute','重复抽样？',TRUE)
      ),
      conditionalPanel(
        condition = "input['method_impute']!='treeImpute'",
        panel(
          heading='保存填补后的变量',
          textInputAddon(inputId='varName_naImpute',label='保存为新的变量名',value='',placeholder = 'eg:varImputed',addon = icon('pencil'))
        )
        #awesomeCheckbox('rep_naImpute','重复抽样？',TRUE)
      )
      #panel(
      #  heading='保存填补后的变量',
      #  textInputAddon(inputId='varName_naImpute',label='保存为新的变量名',value='',placeholder = 'eg:varImputed',addon = icon('pencil'))
      #)
    )
  })
  
  res_naImpute<-reactive({
    input$go_naImpute
    req(input$go_naImpute)
    isolate({
      data_naImpute()->dat
      input$var_naImpute->varImpute
      dat[,varImpute]->xImpute
      if(input$method_impute=='mean'){
        xImpute[is.na(xImpute)]<-mean(xImpute,na.rm=T)
      }
      
      if(input$method_impute=='median'){
        xImpute[is.na(xImpute)]<-median(xImpute,na.rm=T)
      }
      
      if(input$method_impute=='min'){
        xImpute[is.na(xImpute)]<-min(xImpute,na.rm=T)
      }
      
      if(input$method_impute=='max'){
        xImpute[is.na(xImpute)]<-max(xImpute,na.rm=T)
      }
      
      if(input$method_impute=='most'){
        xImpute[is.na(xImpute)]<-sort(table(xImpute),decreasing = T)[1]
      }
      
      if(input$method_impute=='random'){
        xImpute[is.na(xImpute)]<-sample(xImpute[!is.na(xImpute)],sum(is.na(xImpute)),rep=input$rep_naImpute)
      }
      
      if(input$method_impute=='treeImpute'){
        #xImpute[is.na(xImpute)]<-sample(xImpute[!is.na(xImpute)],sum(is.na(xImpute)),rep=input$rep_naImpute)
        imputeData(data=dat,impVars=input$var_naImpute,modelVars=input$var_treeModel,method=input$method_impute)->dat
      }
      
      if(input$method_impute=='myFun'){
        myFunImpute<-eval(parse(text=input$funImpute))
        xImpute[is.na(xImpute)]<-myFunImpute(xImpute[!is.na(xImpute)])
      }
      
      if(input$method_impute=='MICE'){
        A<-is.na(dat)
        A[,-which(names(dat)%in%input$var_naImpute)]<-F
        mice(dat,where=A,method=input$method_MICE)->res
        complete(res)->dat
      }
      
      if(!input$method_impute%in%c('treeImpute','MICE')){
        if(input$varName_naImpute==''){
          dat[,paste(input$var_naImpute,'imputed',sep='_')]<-xImpute
        } else {
          dat[,input$varName_naImpute]<-xImpute
        }
      }
      
      assign(input$dataSel_naImpute,dat,envMadis)
      return(dat)
      LstMadis$Data[[input$dataSel_naImpute]]<-dat
      assign('LstMadis',LstMadis,envir=envMadis)
    })
  })
  
  observeEvent(input$go_naImpute,{  #### newly added for update picker input values.
    updatePickerInput(session,inputId = 'var_naImpute',choices = names(res_naImpute()))
    # if(input$dataName_varMnp==''){
    #   NULL
    # } else {
    #   updatePickerInput(session,inputId = 'datSel_varMnp',choices = ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))])
    # }
    
  })
  
  
  output$summary_naImpute<-renderPrint({
    input$go_naImpute
    print(head(res_naImpute()))
    print(summary(res_naImpute()))
  })
  
  
  
  ###### 数据筛选(dataFilter) ######
  
  output$more1_dataFilter<-renderUI({
    
    change_data()
    #?#
    list(
      panel(
        heading='选择处理的数据集',
        pickerInput(
          inputId = "dataSel_dataFilter",
          label = "选择数据集",
          choices = ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))],
          selected =ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))][1],
          multiple = FALSE,
          options = list(`actions-box` = FALSE)
        )
        #selectInput('dataSel_dataFilter','选择数据集',ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))])
      )
    )
  })
  
  data_dataFilter<-reactive({
    
    change_data()
    #?#
    get(input$dataSel_dataFilter,envMadis)->dataFilter
    return(dataFilter)
  })
  
  output$more2_dataFilter<-renderUI({
    data_dataFilter()->dat
    list(
      panel(
        heading='选择筛选方法及参数',
        pickerInput(
          inputId='method_dataFilter',
          label='选择筛选方法',
          choices = c(
            '筛选变量'='vars_dataFilter',
            '筛选子集'='subset_dataFilter'
          ),
          selected ='vars_dataFilter',
          multiple = FALSE,
          options = list(`actions-box` = FALSE)
        ),
        conditionalPanel(
          condition = "input['method_dataFilter']=='vars_dataFilter'",
          pickerInput(
            inputId='varsKeep_dataFilter',
            label='选择保留的变量',
            choices = names(dat),
            selected =names(dat),
            multiple = TRUE,
            options = list(`actions-box` = TRUE)
          )
        ),
        conditionalPanel(
          condition = "input['method_dataFilter']=='subset_dataFilter'",
          pickerInput(
            inputId='methodSubset_dataFilter',
            label='筛选子集的方法',
            choices = c(
              '保留数据行'='rows_dataFilter',
              '按照逻辑表达式'='subsetExp_dataFilter'
            ),
            selected ='rows_dataFilter',
            multiple = FALSE,
            options = list(`actions-box` = FALSE)
          ),
          conditionalPanel(
            condition = "input['methodSubset_dataFilter']=='rows_dataFilter'",
            numericInput('startRow_dataFilter','输入起始行',min=1,max=Inf,value=1),
            numericInput('endRow_dataFilter','输入结束行',min=1,max=Inf,value=nrow(dat))
          ),
          conditionalPanel(
            condition = "input['methodSubset_dataFilter']=='subsetExp_dataFilter'",
            textInputAddon(inputId='textSubset_dataFilter','输入逻辑表达式',value='',placeholder = 'eg:age>10&sex==1',addon = icon('pencil'))
          )
        )
      ),
      panel(
        heading='设定数据集名称',
        textInputAddon(inputId='dataName_dataFilter',label='设定数据集的名称',value='',placeholder = 'eg:dataFilter',addon = icon('pencil'))
      )
    )
  })
  
  res_dataFilter<-reactive({
    input$go_dataFilter
    req(input$go_dataFilter)
    isolate({
      data_dataFilter()->dat
      if(input$method_dataFilter=='vars_dataFilter'){
        dat[,input$varsKeep_dataFilter]->dat
      }
      
      if(input$method_dataFilter=='subset_dataFilter'){
        if(input$methodSubset_dataFilter=='rows_dataFilter'){
          dat[input$startRow_dataFilter:input$endRow_dataFilter,]->dat
        } else {
          if(input$textSubset_dataFilter=='') {
            data->dat
          } else {
            subset(dat,eval(parse(text=input$textSubset_dataFilter)))->dat
          }
        }
      }
      
      if(input$dataName_dataFilter==''){
        assign(input$dataSel_dataFilter,dat,envMadis)
        LstMadis$Data[[input$dataSel_dataFilter]]<-dat
      } else {
        assign(input$dataName_dataFilter,dat,envMadis)
        LstMadis$Data[[input$dataName_dataFilter]]<-dat
      }
      
      #LstMadis$Data[[input$dataSel_naImpute]]<-dat
      assign('LstMadis',LstMadis,envir=envMadis)
      return(dat)
    })
  })
  
  output$summary_dataFilter<-renderPrint({
    input$go_dataFilter
    print(summary(res_dataFilter()))
    print(head(res_dataFilter()))
  })
  
  
  
  
  ###### 数据导出(dataExpt) ######
  
  output$more1_dataExpt<-renderUI({
    
    change_data()
    #?#
    list(
      panel(
        heading='选择处理的数据集',
        pickerInput(
          inputId = "dataSel_dataExpt",
          label = "选择数据集",
          choices = ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))],
          selected =ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))][1],
          multiple = FALSE,
          options = list(`actions-box` = FALSE)
        )
        #selectInput('dataSel_dataExpt','选择数据集',ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))])
      )
    )
  })
  
  data_dataExpt<-reactive({
    
    change_data()
    #?#
    get(input$dataSel_dataExpt,envMadis)->dataExpt
    return(dataExpt)
  })
  
  output$more2_dataExpt<-renderUI({
    list(
      panel(
        heading='保存的数据格式',
        pickerInput(
          inputId='dataType_dataExpt',
          label='选择数据格式',
          choices = c(
            '文本数据'='txtFile_dataExpt',
            'csv数据'='csvFile_dataExpt',
            'R数据文件'='RData_dataExpt'
          ),
          selected ='csvFile_dataExpt',
          multiple = FALSE,
          options = list(`actions-box` = FALSE)
        )
      ),
      panel(
        heading='设定参数',
        conditionalPanel(
          condition = "input['dataType_dataExpt']=='txtFile_dataExpt'||input['dataType_dataExpt']=='csvFile_dataExpt'",
          awesomeCheckbox('quote_dataExpt','字符类型是否带引号',FALSE),
          pickerInput(
            inputId='sep_dataExpt',
            label='文件分隔符',
            choices = c(
              '逗号分隔'=',',
              '制表符分隔'='\t',
              '空格分隔'=' '
            ),
            selected =',',
            multiple = FALSE,
            options = list(`actions-box` = FALSE)
          ),
          flowLayout(
            awesomeCheckbox('rowNames_dataExpt','是否保留行名',FALSE),
            awesomeCheckbox('colNames_dataExpt','是否保留列名',TRUE) 
          ),
          pickerInput(
            inputId='fileEncoding_dataExpt',
            label='字符集编码',
            choices = c(
              '国标(GB18030)'='GB18030',
              'UTF8编码'='utf8'
            ),
            selected ='GB18030',
            multiple = FALSE,
            options = list(`actions-box` = FALSE)
          )
        ),
        
        conditionalPanel(
          condition = "input['dataType_dataExpt']=='RData_dataExpt'",
          awesomeCheckbox('ascii_dataExpt','是否保存为ASCII格式？',FALSE)
        )
      ),
      panel(
        heading='设定文件名称',
        textInputAddon(inputId='fileName_dataExpt','保存的文件名称',value='',placeholder = 'eg:myData',addon = icon('pencil'))
      )
    )
  })
  
  output$downloadData <- downloadHandler(
    
    filename=function(){
      if(input$dataType_dataExpt=='txtFile_dataExpt'){
        return(ifelse(input$fileName_dataExpt!='',paste0(input$fileName_dataExpt,'.txt'),paste0(input$dataSel_dataExpt,'.txt')))
      }
      
      if(input$dataType_dataExpt=='csvFile_dataExpt'){
        return(ifelse(input$fileName_dataExpt!='',paste0(input$fileName_dataExpt,'.csv'),paste0(input$dataSel_dataExpt,'.csv')))
      }
      
      if(input$dataType_dataExpt=='RData_dataExpt'){
        return(ifelse(input$fileName_dataExpt!='',paste0(input$fileName_dataExpt,'.RData'),paste0(input$dataSel_dataExpt,'.RData')))
      }
      
    },
    content = function(File) {
      if(input$dataType_dataExpt!='RData_dataExpt'){
        write.table(data_dataExpt(),file=File,sep=input$sep_dataExpt,quote=input$quote_dataExpt,
                    row.names=input$rowNames_dataExpt,col.names=input$colNames_dataExpt,
                    fileEncoding = input$fileEncoding_dataExpt)
      } else {
        assign(input$fileName_dataExpt,data_dataExpt())
        save(list=input$fileName_dataExpt,file=File,ascii=input$ascii_dataExpt)
      }
    }
  )
  
  output$summary_dataExpt<-renderPrint({
    print(head(data_dataExpt()))
  })
  
  
  
  ###### r代码编写(Ace) ######
  
  
  output$result_Ace<-renderPrint({
    input$go_Ace
    isolate({
      print(eval(parse(text=input$code_Ace)))
    })
  })
  
  output$graph_Ace<-renderPlot({
    input$go_Ace
    isolate({
      plot(eval(parse(text=input$code_Ace)))
    })
  })
  
  
  
  
  
  
  
  
  
  
  ###### 写入LstMadis的变化input ######
  change_report<-reactive({
    input$go_myTable
    input$go_desc
    input$go_hTest
    input$go_myGlm
    input$go_myTree
    input$go_myCox
    input$go_myGplt
    input$go_myProphet
    input$go_myLme
    input$go_kmeans
    input$go_pca
    input$go_fa
    input$go_DT
  })
  
  
  
  ###### 分类统计表(myTable) ######
  
  output$more1_myTable<-renderUI({
    change_data()

    list(
      panel(
        heading='选择处理的数据集',
        pickerInput(
          inputId = "dataSel_myTable",
          label = "选择数据集",
          choices = ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))],
          selected =ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))][1],
          multiple = FALSE,
          options = list(`actions-box` = FALSE)
        )
        #selectInput('dataSel_dataExpt','选择数据集',ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))])
      )
    )
  })
  
  data_myTable<-reactive({
    change_data()
    get(input$dataSel_myTable,envMadis)->datamyTable
    return(datamyTable)
    
  })
  
  output$more2_myTable<-renderUI({
    list(
      panel(
        heading='选择分组的变量',
        pickerInput(
          inputId='lht_myTable',
          label='选择变量',
          choices = c('无'='',names(data_myTable())),
          selected='',
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ),
        
        pickerInput(
          inputId='rht_myTable',
          label='选择统计的变量',
          choices = names(data_myTable()),
          selected='',
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        )
      ),
      awesomeCheckbox('export_myTable','将该结果输出报告',FALSE)
    )
  })
  
  output$more3_myTable<-renderUI({
    list(
      panel(
        heading='分类统计表的结果',
        dataTableOutput('res_myTable'),
        status='primary'
      )
    )
  })
  
  
  res_myTable<-reactive({
    input$go_myTable
    req(input$go_myTable)
    # isolate({
      data_myTable()->dat
      paste(input$lht_myTable,collapse='+')->lhtV
      paste(input$rht_myTable,collapse='+')->rhtV
      paste(lhtV,rhtV,sep='~')->Formula
      descTab(Formula,dat)->res
      return(res)
    # })
  })
  
  
  observeEvent(input$go_myTable,{
    change_report()
    isolate({
      if(input$export_myTable){
        data_myTable()->dat
        LstMadis<-get('LstMadis',envMadis)
        #LstMadis$Data[[input$dataSel_myTable]]<-dat
        paste(input$lht_myTable,collapse='+')->lhtV
        paste(input$rht_myTable,collapse='+')->rhtV
        paste(lhtV,rhtV,sep='~')->Formula
        dat_myTable<-data.frame(Formula=Formula,data=input$dataSel_myTable)
        LstMadis$myTable<-unique(rbind(LstMadis$myTable,dat_myTable))
        subset(LstMadis$myTable,!is.na(data))->LstMadis$myTable
        assign('LstMadis',LstMadis,envir=envMadis)
      } else {
        NULL
      }
    })
  })
  
  output$res_myTable<-renderDataTable({
    input$go_myTable
    isolate({
      res_myTable()->resmyTable
      resmyTable
    })
  })

  
  
  
  
  
  
  ###### 统计图形制作(myGplt) ######
  
  output$more1_myGplt<-renderUI({
    change_data()
    
    list(
      panel(
        heading='选择处理的数据集',
        pickerInput(
          inputId = "dataSel_myGplt",
          label = "选择数据集",
          choices = ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))],
          selected =ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))][1],
          multiple = FALSE,
          options = list(`actions-box` = FALSE)
        )
        #selectInput('dataSel_dataExpt','选择数据集',ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))])
      )
    )
  })
  
  data_myGplt<-reactive({
    change_data()
    get(input$dataSel_myGplt,envMadis)->datamyGplt
    return(datamyGplt)
    
  })
  
  output$more2_myGplt<-renderUI({
    list(
      panel(
        flowLayout(
          heading='选择作图各属性参数(aes)',
          pickerInput(
            inputId='xvar_myGplt',
            label='选择x轴变量',
            choices = c('无'='NULL',names(data_myGplt())),
            selected='NULL',
            multiple = FALSE,
            options = list(`actions-box` = FALSE)
          ),
          
          pickerInput(
            inputId='yvar_myGplt',
            label='选择y轴变量',
            choices = c('无'='NULL',names(data_myGplt())),
            selected='NULL',
            multiple = FALSE,
            options = list(`actions-box` = FALSE)
          ),
          
          pickerInput(
            inputId='size_myGplt',
            label='设定点或线的大小',
            choices = c('无'='NULL',names(data_myGplt())),
            selected='NULL',
            multiple = FALSE,
            options = list(`actions-box` = FALSE)
          ),
          
          pickerInput(
            inputId='color_myGplt',
            label='设定点线颜色',
            choices = c('无'='NULL',names(data_myGplt())),
            selected='NULL',
            multiple = FALSE,
            options = list(`actions-box` = FALSE)
          ),
          
          pickerInput(
            inputId='fill_myGplt',
            label='设定面的填充',
            choices = c('无'='NULL',names(data_myGplt())),
            selected='NULL',
            multiple = FALSE,
            options = list(`actions-box` = FALSE)
          ),
          
          pickerInput(
            inputId='shape_myGplt',
            label='设定形状',
            choices = c('无'='NULL',names(data_myGplt())),
            selected='NULL',
            multiple = FALSE,
            options = list(`actions-box` = FALSE)
          ),
          
          pickerInput(
            inputId='alpha_myGplt',
            label='设定透明度',
            choices = c('无'='NULL',names(data_myGplt())),
            selected='NULL',
            multiple = FALSE,
            options = list(`actions-box` = FALSE)
          )
        ),
        awesomeCheckbox('export_myGplt','将该结果输出报告',FALSE)
      )
    )
  })
  
  output$more3_myGplt<-renderUI({
    list(
      tabsetPanel(
        tabPanel(
          'ggplot结果',
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
              choices = c('无'='NULL',names(data_myGplt())),
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
      )#,
      # flowLayout(
      #   actionBttn('go_myGplt','确定'),
      #   awesomeCheckbox('export_myGplt','将该结果输出报告',FALSE)
      # )
      
    )
  })
  
  
  res_myGplt<-reactive({
    input$go_myGplt
    req(input$go_myGplt)
    # isolate({
    data_myGplt()->dat
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
  
  
  observeEvent(input$go_myGplt,{
    change_report()
    isolate({
      if(input$export_myGplt){
        data_myGplt()->dat
        LstMadis<-get('LstMadis',envMadis)
        #LstMadis$Data[[input$dataSel_myGplt]]<-dat
        dat_myGplt<-data.frame(data=input$dataSel_myGplt,
                               x=input$xvar_myGplt,
                               y=input$yvar_myGplt,
                               size=input$size_myGplt,
                               fill=input$fill_myGplt,
                               color=input$color_myGplt,
                               shape=input$shape_myGplt,
                               alpha=input$alpha_myGplt,
                               facetVar=paste(input$facetVar_myGplt,collapse=';'),
                               geom=paste(input$geom_myGplt,collapse=';'),
                               smoothMethod = input$smoothMethod_myGplt,
                               barPos=input$barPos_myGplt,
                               labx=input$labx_myGplt,
                               laby=input$laby_myGplt,
                               title=input$title_myGplt,
                               Bins=input$Bins_myGplt,
                               theme=input$theme_myGplt,
                               Width=input$Width_myGplt,
                               Colour=input$Colour_myGplt,
                               Fill=input$Fill_myGplt,
                               Size=input$Size_myGplt,
                               Alpha=input$Alpha_myGplt,
                               Shape=input$Shape_myGplt
                               )
        LstMadis$myGplt<-unique(rbind(LstMadis$myGplt,dat_myGplt))
        subset(LstMadis$myGplt,!is.na(data))->LstMadis$myGplt
        assign('LstMadis',LstMadis,envir=envMadis)
      } else {
        NULL
      }
    })
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
  
  
  
  
  
  
  
  
  ###### 描述性分析(desc) ######
  
  output$more1_desc<-renderUI({
    
    change_data()
    #?#
    list(
      panel(
        heading='选择处理的数据集',
        pickerInput(
          inputId = "dataSel_desc",
          label = "选择数据集",
          choices = ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))],
          selected =ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))][1],
          multiple = FALSE,
          options = list(`actions-box` = FALSE)
        )
        #selectInput('dataSel_dataExpt','选择数据集',ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))])
      )
    )
  })
  
  data_desc<-reactive({
    
    change_data()
    #?#
    get(input$dataSel_desc,envMadis)->dataDesc
    return(dataDesc)
    
  })
  
  output$more2_desc<-renderUI({
    list(
      panel(
        heading='选择分析的变量',
        pickerInput(
          inputId='vars_desc',
          label='选择变量',
          choices = names(data_desc()),
          selected=names(data_desc())[1],
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ),
        #awesomeCheckbox('myFun_desc','是否自定义分析函数？',FALSE),
        conditionalPanel(
          condition = "!input['myFun_desc']",
          numericInput('digits_desc','结果保留小数位数',min=0,max=100,value=2)
          # numericInput('colsPlot_desc','多个图形排放列数',min=1,max=10,value=2)
          #awesomeCheckbox('export_desc','是否将该分析结果导出报告？',FALSE)
        ),
        conditionalPanel(
          condition = "input['myFun_desc']",
          textInputAddon('textFun_desc','输入自定义函数',value='',placeholder = 'eg:function(x)mean(x)',addon = icon('pencil'))
        ),
        awesomeCheckbox('myFun_desc','是否自定义分析函数？',FALSE),
        awesomeCheckbox('export_desc','是否导出报告？',FALSE)
      )
    )
  })
  
  output$more3_desc<-renderUI({
    list(
      panel(
        heading='描述性分析结果',
        verbatimTextOutput('res_desc'),
        status='primary'
      ),
      conditionalPanel(
        condition = "!input['myFun_desc']",
        panel(
          heading='图形结果',
          plotOutput('graph_desc',height='720px'),
          status='primary'
        )
      )
    )
  })
  
  
  res_desc<-reactive({
    
    input$go_desc
    req(input$go_desc)
    isolate({
      data_desc()->dat
      if(input$myFun_desc){
        data_desc()->dat
        input$vars_desc->vars_desc
        lapply(dat[,vars_desc],eval(parse(text=input$textFun_desc)))->resAll
      } else {
        input$vars_desc->vars_desc
        sapply(dat[,input$vars_desc],function(i)class(i)[1])->varTypes
        lapply(1:length(vars_desc),function(i){
          uniVar(data=dat,xvars=vars_desc[i],varType=varTypes[i],Digits=input$digits_desc,nameX=vars_desc[i])->resi
          return(resi)
        })->resAll
        
      }
      return(resAll)
    })
  })
  
  output$more4_desc<-renderUI({
    list(
      panel(
        heading='选择需要展示的结果',
        pickerInput(
          inputId='Res_desc',
          label='选择结果',
          choices = input$vars_desc,
          selected=input$vars_desc[1],
          multiple = FALSE,
          options = list(`actions-box` = FALSE)
        )
      )
    )
  })
  
  
  observeEvent(input$go_desc,{
    change_report()
    isolate({
      if(input$export_desc){
        data_desc()->dat
        LstMadis<-get('LstMadis',envMadis)
        #LstMadis$Data[[input$dataSel_desc]]<-dat
        dat_desc<-data.frame(xvars=input$vars_desc,Digits=input$digits_desc,dataName=input$dataSel_desc,stringsAsFactors = F)
        LstMadis$desc<-unique(rbind(LstMadis$desc,dat_desc))
        #LstMadis$desc[!is.na(xvars),]
        subset(LstMadis$desc,!is.na(xvars))->LstMadis$desc
        assign('LstMadis',LstMadis,envir=envMadis)
        #return(LstMadis)
      } else {
        NULL
      }
    })
  })
  
  output$res_desc<-renderPrint({
    
    res_desc()->resDesc
    which(input$vars_desc==input$Res_desc)->ind
    if(input$myFun_desc){
      tryCatch(print(pander(resDesc[[i]])),error=function(e)print(resDesc[[i]]))
    } else {
      tryCatch(print(pander(resDesc[[ind]]$resDesc)),error=function(e)print(resDesc[[ind]]$resDesc))
    }
  })
  
  output$graph_desc<-renderPlot({
    
    
    res_desc()->resDesc
    which(input$vars_desc==input$Res_desc)->ind
    plot(resDesc[[ind]]$graphDesc)
  })
  
  
  
  ###### 单因素分析(hTest) ######
  
  output$more1_hTest<-renderUI({
    change_data()
    list(
      panel(
        heading='选择处理的数据集',
        pickerInput(
          inputId = "dataSel_hTest",
          label = "选择数据集",
          choices = ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))],
          selected =ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))][1],
          multiple = FALSE,
          options = list(`actions-box` = FALSE)
        )
        #selectInput('dataSel_dataExpt','选择数据集',ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))])
      )
    )
  })
  
  data_hTest<-reactive({
    change_data()
    get(input$dataSel_hTest,envMadis)->dataHtest
    return(dataHtest)
  })
  
  output$more2_hTest<-renderUI({
    list(
      panel(
        heading='选择分析的变量',
        pickerInput(
          inputId='varsx_hTest',
          label='选择变量x',
          choices = names(data_hTest()),
          selected=names(data_hTest())[1],
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ),
        pickerInput(
          inputId='varsy_hTest',
          label='选择变量y',
          choices = c('无'='',names(data_hTest())),
          selected='',
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ),
        conditionalPanel(
          condition = "!input['myFun_hTest']",
          pickerInput(
            inputId='alter_hTest',
            label='选择备择假设',
            choices = c(
              '等于'='two.sided',
              '大于'='greater',
              '小于'='less'
            ),
            selected='two.sided',
            multiple = FALSE,
            options = list(`actions-box` = FALSE)
          ),
          numericInput('nullHyp_hTest','总体参数',value=0),
          numericInput('confLevel_hTest','置信水平',value=0.95,min=0,max=1),
          awesomeCheckbox('paired_hTest','数据是否为配对数据？',FALSE)
          # numericInput('colsPlot_hTest','多个图形排放列数',min=1,max=10,value=2)
        ),
        awesomeCheckbox('myFun_hTest','是否自定义分析函数？',FALSE),
        conditionalPanel(
          condition = "input['myFun_hTest']",
          aceEditor("textFun_hTest", mode="r", value="#The data name is dat",height='100px')
        ),
        awesomeCheckbox('export_hTest','将该结果输出报告',FALSE)
      )
    )
  })
  
  
  
  
  output$more3_hTest<-renderUI({
    list(
      panel(
        heading='统计检验结果',
        verbatimTextOutput('res_hTest'),
        status='primary'
      ),
      conditionalPanel(
        condition = "!input['myFun_hTest']",
        panel(
          heading='图形结果',
          plotOutput('graph_hTest',height='720px'),
          status='primary'
        )
      )
    )
  })
  
  res_hTest<-reactive({
    input$go_hTest
    req(input$go_hTest)
    isolate({
      data_hTest()->dat
      resAll<-list()
      for(i in 1:length(input$varsy_hTest)){
        for (j in 1:length(input$varsx_hTest)){
          resAll[[length(input$varsx_hTest)*(i-1)+j]]<-hTest(data=dat,xvars=input$varsx_hTest[j],yvars=input$varsy_hTest[i],alter=input$alter_hTest,paired=input$paired_hTest,nullHyp=input$nullHyp_hTest,confLevel=input$confLevel_hTest)
        }
      }
      #biVar(data=dat,xvars=input$varsx_hTest,yvars=input$varsy_hTest,alter=input$alter_hTest,paired=input$paired_hTest,nullHyp=input$nullHyp_hTest,confLevel=input$confLevel_hTest)->res
      return(resAll)
    })
  })
  
  
  observeEvent(input$go_hTest,{
    change_report()
    isolate({
      if(input$export_hTest){
        data_hTest()->dat
        LstMadis<-get('LstMadis',envMadis)
        #LstMadis$Data[[input$dataSel_hTest]]<-dat
        expand.grid(input$varsx_hTest,input$varsy_hTest)->varsInput
        names(varsInput)<-c('xvars','yvars')
        dat_hTest<-data.frame(varsInput,alter=input$alter_hTest,
                              paired=input$paired_hTest,nullHyp=input$nullHyp_hTest,confLevel=input$confLevel_hTest,dataName=input$dataSel_hTest,stringsAsFactors = F)
        LstMadis$hTest<-unique(rbind(LstMadis$hTest,dat_hTest))
        subset(LstMadis$hTest,!is.na(xvars))->LstMadis$hTest
        as.logical(LstMadis$hTest$paired)->LstMadis$hTest$paired
        assign('LstMadis',LstMadis,envir=envMadis)
        #return(LstMadis)
      } else {
        NULL
      }
    })
  })
  
  
  output$more4_hTest<-renderUI({
    list(
      panel(
        heading='选择展示的结果',
        pickerInput(
          inputId='Res_hTest',
          label='选择展示的结果',
          choices = apply(expand.grid(input$varsx_hTest,input$varsy_hTest),1,function(x)paste(x,collapse=','))
      )
    )
    )
  })
  
  
  output$res_hTest<-renderPrint({
    
    
    apply(expand.grid(input$varsx_hTest,input$varsy_hTest),1,function(x)paste(x,collapse=','))->namesRes
    
    which(namesRes==input$Res_hTest)->ind
    tryCatch(print(pander(res_hTest()[[ind]]$hTestRes)),error=function(e)print(res_hTest()[[ind]]$hTestRes))

  })
  
  output$graph_hTest<-renderPlot({
    
    
    
    apply(expand.grid(input$varsx_hTest,input$varsy_hTest),1,function(x)paste(x,collapse=','))->namesRes
    
    which(namesRes==input$Res_hTest)->ind
    
    plot(res_hTest()[[ind]]$hTestGraph)
  })
  
  
  
  
  ###### 线性模型(myGlm) ######
  
  
  output$more1_myGlm<-renderUI({
    change_data()
    list(
      panel(
        heading='选择处理的数据集',
        pickerInput(
          inputId = "dataSel_myGlm",
          label = "选择数据集",
          choices = ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))],
          selected =ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))][1],
          multiple = FALSE,
          options = list(`actions-box` = FALSE)
        )
        #selectInput('dataSel_dataExpt','选择数据集',ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))])
      )
    )
  })
  
  data_myGlm<-reactive({
    change_data()
    get(input$dataSel_myGlm,envMadis)->datamyGlm
    return(datamyGlm)
  })
  
  output$more2_myGlm<-renderUI({
    list(
      panel(
        heading='设定模型参数',
        pickerInput(
          inputId='varsy_myGlm',
          label='选择因变量y',
          choices = names(data_myGlm()),
          selected=names(data_myGlm())[1],
          multiple = FALSE,
          options = list(`actions-box` = FALSE)
        ),
        pickerInput(
          inputId='varsx_myGlm',
          label='选择自变量x',
          choices = c('无'='',names(data_myGlm())),
          selected='',
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ),
        pickerInput(
          inputId='family_myGlm',
          label='设定模型类型',
          choices = c('线性模型'='gaussian','logistic模型'='binomial','泊松回归'='poisson'),
          selected='gaussian',
          multiple=FALSE,
          options = list(`actions-box` = FALSE)
        ),
        awesomeCheckbox(
          inputId = 'reviseVarsx_myGlm',
          label='调整自变量',
          value = FALSE
        ),
        conditionalPanel(
          condition = "input['reviseVarsx_myGlm']",
          list(
            textInputAddon(
              inputId = 'newVarsx_myGlm',
              label = '自变量调整',
              placeholder = 'eg: log(age)',
              value='',
              addon = 'pencil'
            )
          )
        ),
        awesomeCheckbox(
          inputId = 'weightSet_myGlm',
          label='设定权重',
          value = FALSE
        ),
        conditionalPanel(
          condition = "input['weightSet_myGlm']",
          list(
            pickerInput(
              inputId='weightsVar_myGlm',
              label='选择权重变量',
              choices = names(data_myGlm()),
              selected=names(data_myGlm())[1],
              multiple = FALSE,
              options = list(`actions-box` = FALSE)
            )
          )
        ),
        awesomeCheckbox(
          inputId = 'subsetSet_myGlm',
          label='设定子集',
          value = FALSE
        ),
        conditionalPanel(
          condition = "input['subsetSet_myGlm']",
          list(
            textInputAddon(
              inputId = 'subsets_myGlm',
              label = '设定子集表达式',
              placeholder = 'eg: sex==1',
              value='',
              addon = 'pencil'
            )
          )
        ),
        
        awesomeCheckbox(
          inputId = 'lowerFormSet_myGlm',
          label='设定逐步回归最小模型',
          value = FALSE
        ),
        conditionalPanel(
          condition = "input['lowerFormSet_myGlm']",
          list(
            textInputAddon(
              inputId = 'lowerForm_myGlm',
              label = '最小模型表达式',
              placeholder = 'eg: sex+age',
              value='',
              addon = 'pencil'
            )
          )
        )
        
      ),
      awesomeCheckbox('export_myGlm','是否导出到报告中?',FALSE)
    )
  })
  
  
  
  
  output$more3_myGlm<-renderUI({
    list(
      tabsetPanel(
        tabPanel(
          '模型结果',
          verbatimTextOutput('res_myGlm'),
          status='primary'
        ),
        tabPanel(
          '全模型诊断图形结果',
          plotOutput('graphFull_myGlm',height='720px'),
          plotOutput('graphROCFull')
        ),
        tabPanel(
          '逐步回归模型诊断图形结果',
          plotOutput('graphStep_myGlm',height='720px'),
          plotOutput('graphROCStep')
          
        )
      )
    )
  })

  res_myGlm<-reactive({
    input$go_myGlm
    req(input$go_myGlm)
    isolate({
      data_myGlm()->dat
      if(input$reviseVarsx_myGlm){
        paste(input$varsy_myGlm,paste(input$newVarsx_myGlm,paste(input$varsx_myGlm,collapse='+'),sep='+'),sep='~')->Formula
        
      } else {
        paste(input$varsy_myGlm,paste(input$varsx_myGlm,collapse='+'),sep='~')->Formula
        
      }
      
      if(input$subsetSet_myGlm){
        Subset=input$subsets_myGlm
      } else {Subset='all'}
      
      if(input$weightSet_myGlm){
        weightsVar=input$weightsVar_myGlm
      } else {weightsVar=1}
      
      if(input$lowerFormSet_myGlm){
        lowerForm=input$lowerForm_myGlm
      } else {
        lowerForm='~1'
      }
      
      
      glmS(Formula=Formula,
            data=dat,
            weightsVar=weightsVar,
            subset=Subset,
            Family=input$family_myGlm,
            lower=lowerForm
            )->resmyGlm
      return(resmyGlm)
    })
  })

  
  observeEvent(input$go_myGlm,{
    change_report()
    isolate({
      if(input$export_myGlm){
        
        if(input$reviseVarsx_myGlm){
          paste(input$varsy_myGlm,paste(input$newVarsx_myGlm,paste(input$varsx_myGlm,collapse='+'),sep='+'),sep='~')->Formula
          
        } else {
          paste(input$varsy_myGlm,paste(input$varsx_myGlm,collapse='+'),sep='~')->Formula
          
        }
        
        if(input$subsetSet_myGlm){
          Subset=input$subsets_myGlm
        } else {Subset='all'}
        
        if(input$weightSet_myGlm){
          weightsVar=input$weightsVar_myGlm
        } else {weightsVar=1}
        
        if(input$lowerFormSet_myGlm){
          lowerForm=input$lowerForm_myGlm
        } else {
          lowerForm='~1'
        }
        


        data_myGlm()->dat
        LstMadis<-get('LstMadis',envMadis)
        #LstMadis$Data[[input$dataSel_myGlm]]<-dat
        dat_myGlm<-data.frame(Formula=Formula,data=input$dataSel_myGlm,weightsVar=weightsVar,subset=Subset,Family=input$family_myGlm,lower=lowerForm)
        LstMadis$myGlm<-unique(rbind(LstMadis$myGlm,dat_myGlm))
        subset(LstMadis$myGlm,!is.na(data))->LstMadis$myGlm
        assign('LstMadis',LstMadis,envir=envMadis)
        #return(LstMadis)
      } else {
        NULL
      }
    })
  })
  
  
  output$res_myGlm<-renderPrint({
    input$go_myGlm
    isolate({
      
      
      cat('\n')
      cat('######模型分析结果如下########')
      cat('\n')
      cat('######全模型分析结果如下########')
      tryCatch(print(pander(summary(res_myGlm()$glmResFull))),error=function(e)print(summary(res_myGlm()$glmResFull)))
      cat('\n')
      cat('######逐步回归模型分析结果如下########')
      tryCatch(print(pander(summary(res_myGlm()$glmResStep))),error=function(e)print(summary(res_myGlm()$glmResStep)))
      cat('\n')
      
    })


  })
  
  
  ROCFull<-reactive({
    if(input$family_myGlm=='binomial'){
      res_myGlm()$glmResFull->fit.tmp
      predict(fit.tmp,type='link')->pred.fit
      t.scores<-prediction(pred.fit,fit.tmp[['y']])
      cost.perf = performance(t.scores, "cost")
      t.scores@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]->cutoffVal
      perf1<-performance(t.scores,'tpr','fpr')
      perf2<-performance(t.scores,'auc')
      plot(perf1,main='ROC Curve for Regression Full Model')
      abline(c(0,0),c(1,1))
      text(0.8,0.2,paste('auc=',round(unlist(perf2@y.values),3),sep=''))
      text(0.8,0.1,paste('cutoff=',round(cutoffVal,3),sep=''))
    } else {
      NULL
    }
  })
  
  
  ROCStep<-reactive({
    if(input$family_myGlm=='binomial'){
      res_myGlm()$glmResStep->fit.tmp
      predict(fit.tmp,type='link')->pred.fit
      t.scores<-prediction(pred.fit,fit.tmp[['y']])
      cost.perf = performance(t.scores, "cost")
      t.scores@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]->cutoffVal
      perf1<-performance(t.scores,'tpr','fpr')
      perf2<-performance(t.scores,'auc')
      plot(perf1,main='ROC Curve for Regression Stepwise Model')
      abline(c(0,0),c(1,1))
      text(0.8,0.2,paste('auc=',round(unlist(perf2@y.values),3),sep=''))
      text(0.8,0.1,paste('cutoff=',round(cutoffVal,3),sep=''))
    } else {
      NULL
    }
  })

  output$graphFull_myGlm<-renderPlot({
    input$go_myGlm
    isolate({
      graphLst<-list(res_myGlm()$graphResFull,res_myGlm()$graphResStep)
      
      autoplot(res_myGlm()$glmResFull)
      
    })
  })
  

  output$graphROCFull<-renderPlot({
    ROCFull()
  })

  output$graphStep_myGlm<-renderPlot({
    input$go_myGlm
    isolate({
      graphLst<-list(res_myGlm()$graphResFull,res_myGlm()$graphResStep)
      autoplot(res_myGlm()$glmResStep)
      
    })
  })
  
  output$graphROCStep<-renderPlot({
    ROCStep()
  })
  
  
  
  
  ###### 决策树模型(myTree) ######
  
  
  output$more1_myTree<-renderUI({
    change_data()
    list(
      panel(
        heading='选择处理的数据集',
        pickerInput(
          inputId = "dataSel_myTree",
          label = "选择数据集",
          choices = ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))],
          selected =ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))][1],
          multiple = FALSE,
          options = list(`actions-box` = FALSE)
        )
        #selectInput('dataSel_dataExpt','选择数据集',ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))])
      )
    )
  })
  
  data_myTree<-reactive({
    change_data()
    get(input$dataSel_myTree,envMadis)->datamyTree
    return(datamyTree)
  })
  
  output$more2_myTree<-renderUI({
    list(
      panel(
        heading='设定模型参数',
        panel(
          heading='设定因变量',
        awesomeCheckbox(
          inputId = 'surv_myTree',
          label='是否为生存模型',
          value = FALSE
        ),
        conditionalPanel(
          condition = "input['surv_myTree']",
          list(
            pickerInput(
              inputId='timeVar_myTree',
              label='选择时间变量',
              choices = names(data_myTree()),
              selected=names(data_myTree())[1],
              multiple = FALSE,
              options = list(`actions-box` = FALSE)
            ),
            pickerInput(
              inputId='centVar_myTree',
              label='选择结局变量',
              choices = c('无',names(data_myTree())),
              selected=names(data_myTree())[1],
              multiple = FALSE,
              options = list(`actions-box` = FALSE)
            )
          )
        ),
        
        conditionalPanel(
          condition = "!input['surv_myTree']",
          list(
            pickerInput(
              inputId='varsy_myTree',
              label='选择因变量y',
              choices = names(data_myTree()),
              selected=names(data_myTree())[1],
              multiple = FALSE,
              options = list(`actions-box` = FALSE)
            )
          )
        )
        ),
        
        pickerInput(
          inputId='varsx_myTree',
          label='选择自变量x',
          choices = c('无'='',names(data_myTree())),
          selected='',
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ),
        pickerInput(
          inputId='method_myTree',
          label='设定树模型算法',
          choices = c('RPART'='rpart','CTREE'='ctree'),
          selected='rpart',
          multiple=FALSE,
          options = list(`actions-box` = FALSE)
        ),
        
        awesomeCheckbox(
          inputId = 'subsetSet_myTree',
          label='设定子集',
          value = FALSE
        ),
        conditionalPanel(
          condition = "input['subsetSet_myTree']",
          list(
            textInputAddon(
              inputId = 'subsets_myTree',
              label = '设定子集表达式',
              placeholder = 'eg: sex==1',
              value='',
              addon = 'pencil'
            )
          )
        ),
        panel(
          heading='设定模型参数',
          numericInput(
            inputId = 'maxDepth_myTree',
            label='设定最大生长深度',
            value = 3,
            min=1,
            max=30,
            step=1
          ),
          numericInput(
            inputId = 'minSplit_myTree',
            label='设定最小子节点样本量',
            value = 30,
            min=1,
            max=Inf,
            step=1
          ),
          numericInput(
            inputId = 'minBucket_myTree',
            label='设定最小叶节点样本量',
            value = 3,
            min=1,
            max=30,
            step=1
          ),
          numericInput(
            inputId = 'param_myTree',
            label='设定系数',
            value = 0.05,
            min=0,
            max=10
          )
        )
        ),
      awesomeCheckbox('export_myTree','是否导出到报告中?',FALSE)
    )
  })
  
  
  
  
  output$more3_myTree<-renderUI({
    list(
      tabsetPanel(
        tabPanel(
          '模型结果',
          heading='模型结果',
          htmlOutput('summary_myTree'),
          status='primary'
        ),
        tabPanel(
          '模型图形结果',
          plotOutput('graph_myTree',height='720px')
        )
      )
    )
  })
  
  res_myTree<-reactive({
    input$go_myTree
    req(input$go_myTree)
    isolate({
      data_myTree()->dat
      if(input$surv_myTree){
        if(input$centVar_myTree=='无'){
          paste('Surv(',input$timeVar_myTree,')',sep='')->rht_myTree
        } else {
          paste('Surv(',paste(input$timeVar_myTree,input$centVar_myTree,sep=','),')',sep='')->rht_myTree
        }
        # if(input$reviseVarsx_myTree){
        #   paste(rht_myTree,paste(newVarsx_myTree,paste(input$varsx_myTree,collapse='+'),sep='+'),sep='~')->Formula
        #   
        # } else {
          paste(rht_myTree,paste(input$varsx_myTree,collapse='+'),sep='~')->Formula
          
        # }
      } else {
        # if(input$reviseVarsx_myTree){
        #   paste(input$varsy_myTree,paste(newVarsx_myTree,paste(input$varsx_myTree,collapse='+'),sep='+'),sep='~')->Formula
        #   
        # } else {
          paste(input$varsy_myTree,paste(input$varsx_myTree,collapse='+'),sep='~')->Formula
          
        # }
      }
      
      
      if(input$subsetSet_myTree){
        Subset=input$subsets_myTree
      } else {Subset='all'}
      
      
      treeS(Formula=Formula,
            data=dat,
            subset=Subset,
            treeMethod=input$method_myTree,
            Minsplit=input$minSplit_myTree,
            Minbucket = input$minBucket_myTree,
            Maxdepth = input$maxDepth_myTree,
            CP=input$param_myTree,
            Mincrit=input$param_myTree
      )->resmyTree
      return(resmyTree)
    })
  })
  
  
  observeEvent(input$go_myTree,{
    change_report()
    isolate({
      if(input$export_myTree){

        
        
        if(input$surv_myTree){
          if(input$centVar_myTree=='无'){
            paste('Surv(',input$timeVar_myTree,')',sep='')->rht_myTree
          } else {
            paste('Surv(',paste(input$timeVar_myTree,input$centVar_myTree,sep=','),')',sep='')->rht_myTree
          }
          
          paste(rht_myTree,paste(input$varsx_myTree,collapse='+'),sep='~')->Formula
          
          
        } else {
          
          paste(input$varsy_myTree,paste(input$varsx_myTree,collapse='+'),sep='~')->Formula
          
          
        }
        
        
        

        if(input$subsetSet_myTree){
          Subset=input$subsets_myTree
        } else {Subset='all'}

        



        data_myTree()->dat
        LstMadis<-get('LstMadis',envMadis)
        #LstMadis$Data[[input$dataSel_myTree]]<-dat
        dat_myTree<-data.frame(Formula=Formula,
                              data=input$dataSel_myTree,
                              subset=Subset,
                              treeMethod=input$method_myTree,
                              Minsplit=input$minSplit_myTree,
                              Minbucket = input$minBucket_myTree,
                              Maxdepth = input$maxDepth_myTree,
                              CP=input$param_myTree,
                              Mincrit=input$param_myTree)
        LstMadis$myTree<-unique(rbind(LstMadis$myTree,dat_myTree))
        subset(LstMadis$myTree,!is.na(data))->LstMadis$myTree
        assign('LstMadis',LstMadis,envir=envMadis)
      } else {
        NULL
      }
    })
  })

  
  output$summary_myTree<-renderText({
    input$go_myTree
    isolate({
      
      
      # cat('\n')
      # cat('######模型分析结果如下########')
      # cat('\n')
      capture.output(res_myTree())->resTmp
      # print(pander(res_myTree()))
      paste(resTmp,collapse="<br>")
      # cat('\n')
      # cat('\n')
    })
    
    
  })
  
  output$graph_myTree<-renderPlot({
    input$go_myTree
    isolate({
      plot(res_myTree())
      
    })
  })
  
  
  
  
  
  
  ###### COXPH模型(myCox) ######
  
  
  output$more1_myCox<-renderUI({
    change_data()
    list(
      panel(
        heading='选择处理的数据集',
        pickerInput(
          inputId = "dataSel_myCox",
          label = "选择数据集",
          choices = ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))],
          selected =ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))][1],
          multiple = FALSE,
          options = list(`actions-box` = FALSE)
        )
        #selectInput('dataSel_dataExpt','选择数据集',ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))])
      )
    )
  })
  
  data_myCox<-reactive({
    change_data()
    get(input$dataSel_myCox,envMadis)->datamyCox
    return(datamyCox)
  })
  
  output$more2_myCox<-renderUI({
    list(
      panel(
        heading='设定模型参数',
        pickerInput(
          inputId='timeVar_myCox',
          label='选择时间变量',
          choices = names(data_myCox()),
          selected=names(data_myCox())[1],
          multiple = FALSE,
          options = list(`actions-box` = FALSE)
        ),
        pickerInput(
          inputId='centVar_myCox',
          label='选择结局变量',
          choices = c('无'='',names(data_myCox())),
          selected='',
          multiple = FALSE,
          options = list(`actions-box` = TRUE)
        ),
        pickerInput(
          inputId='varsx_myCox',
          label='选择自变量x',
          choices = c('无'='',names(data_myCox())),
          selected='',
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ),
        pickerInput(
          inputId='strataVar_myCox',
          label='选择分层变量',
          choices = c('无'='1',names(data_myCox())),
          selected='1',
          multiple=FALSE,
          options = list(`actions-box` = FALSE)
        ),
        awesomeCheckbox(
          inputId = 'reviseVarsx_myCox',
          label='调整自变量',
          value = FALSE
        ),
        conditionalPanel(
          condition = "input['reviseVarsx_myCox']",
          list(
            textInputAddon(
              inputId = 'newVarsx_myCox',
              label = '自变量调整',
              placeholder = 'eg: log(age)',
              value='',
              addon = 'pencil'
            )
          )
        ),
        awesomeCheckbox(
          inputId = 'weightSet_myCox',
          label='设定权重',
          value = FALSE
        ),
        conditionalPanel(
          condition = "input['weightSet_myCox']",
          list(
            pickerInput(
              inputId='weightsVar_myCox',
              label='选择权重变量',
              choices = names(data_myCox()),
              selected=names(data_myCox())[1],
              multiple = FALSE,
              options = list(`actions-box` = FALSE)
            )
          )
        ),
        awesomeCheckbox(
          inputId = 'subsetSet_myCox',
          label='设定子集',
          value = FALSE
        ),
        conditionalPanel(
          condition = "input['subsetSet_myCox']",
          list(
            textInputAddon(
              inputId = 'subsets_myCox',
              label = '设定子集表达式',
              placeholder = 'eg: sex==1',
              value='',
              addon = 'pencil'
            )
          )
        ),
        
        awesomeCheckbox(
          inputId = 'lowerFormSet_myCox',
          label='设定逐步回归最小模型',
          value = FALSE
        ),
        conditionalPanel(
          condition = "input['lowerFormSet_myCox']",
          list(
            textInputAddon(
              inputId = 'lowerForm_myCox',
              label = '最小模型表达式',
              placeholder = 'eg: sex+age',
              value='',
              addon = 'pencil'
            )
          )
        )
        
      ),
      awesomeCheckbox('export_myCox','是否导出到报告中?',FALSE)
    )
  })
  
  
  
  
  output$more3_myCox<-renderUI({
    list(
      tabsetPanel(
        tabPanel(
          '模型结果',
          heading='模型结果',
          verbatimTextOutput('res_myCox'),
          status='primary'
        ),
        tabPanel(
          '全模型诊断图形结果',
          plotOutput('graphFull_myCox',height='720px')
        ),
        tabPanel(
          '逐步回归模型诊断图形结果',
          plotOutput('graphStep_myCox',height='720px')
          
        ),
        tabPanel(
          '分层生存曲线',
          plotOutput('graphStrata_myCox',height='720px')
          
        )
      )
    )
  })
  
  res_myCox<-reactive({
    input$go_myCox
    req(input$go_myCox)
    isolate({
      data_myCox()->dat
      if(input$centVar_myCox==''){
        lht<-paste("Surv(",input$timeVar_myCox,")",sep='')
      } else {
        lht<-paste("Surv(",paste(input$timeVar_myCox,input$centVar_myCox,sep=','),")",sep='')
      }
      
      if(input$reviseVarsx_myCox){
        
        paste(lht,paste(input$newVarsx_myCox,paste(input$varsx_myCox,collapse='+'),sep='+'),sep='~')->Formula
        
      } else {
        paste(lht,paste(input$varsx_myCox,collapse='+'),sep='~')->Formula
        
      }
      
      if(input$subsetSet_myCox){
        Subset=input$subsetSet_myCox
      } else {Subset='all'}
      
      if(input$weightSet_myCox){
        weightsVar=input$weightSet_myCox
      } else {weightsVar=1}
      
      if(input$lowerFormSet_myCox){
        lowerForm=input$lowerFormSet_myCox
      } else {
        lowerForm='~1'
      }
      
      
      coxS(Formula=Formula,
            data=dat,
            weightsVar=weightsVar,
            subset=Subset,
            strataVar=input$strataVar_myCox,
            lower=lowerForm
      )->resmyCox
      return(resmyCox)
    })
  })
  
  
  observeEvent(input$go_myCox,{
    change_report()
    isolate({
      if(input$export_myCox){

        
        if(input$centVar_myCox==''){
          lht<-paste("Surv(",input$timeVar_myCox,")",sep='')
        } else {
          lht<-paste("Surv(",paste(input$timeVar_myCox,input$centVar_myCox,sep=','),")",sep='')
        }
        
        if(input$reviseVarsx_myCox){
          
          paste(lht,paste(input$newVarsx_myCox,paste(input$varsx_myCox,collapse='+'),sep='+'),sep='~')->Formula
          
        } else {
          paste(lht,paste(input$varsx_myCox,collapse='+'),sep='~')->Formula
          
        }
        
        if(input$subsetSet_myCox){
          Subset=input$subsetSet_myCox
        } else {Subset='all'}

        if(input$weightSet_myCox){
          weightsVar=input$weightSet_myCox
        } else {weightsVar=1}

        if(input$lowerFormSet_myCox){
          lowerForm=input$lowerFormSet_myCox
        } else {
          lowerForm='~1'
        }


        data_myCox()->dat
        LstMadis<-get('LstMadis',envMadis)
        #LstMadis$Data[[input$dataSel_myCox]]<-dat
        dat_myCox<-data.frame(Formula=Formula,
                              data=input$dataSel_myCox,
                              weightsVar=weightsVar,
                              subset=Subset,
                              strataVar=input$strataVar_myCox,
                              lower=lowerForm)
        LstMadis$myCox<-unique(rbind(LstMadis$myCox,dat_myCox))
        subset(LstMadis$myCox,!is.na(data))->LstMadis$myCox
        assign('LstMadis',LstMadis,envir=envMadis)
        #return(LstMadis)
      } else {
        NULL
      }
    })
  })
  
  
  output$res_myCox<-renderPrint({
    input$go_myCox
    isolate({
      
      
      cat('\n')
      cat('######模型分析结果如下########')
      cat('\n')
      cat('######全模型分析结果如下########')
      tryCatch(print(pander(res_myCox()$coxResFull)),error=function(e)print(res_myCox()$coxResFull))
      cat('\n')
      cat('######逐步回归模型分析结果如下########')
      tryCatch(print(pander(res_myCox()$coxResStep)),error=function(e)print(res_myCox()$coxResStep))
      cat('\n')
      
      
    })
    
    
  })
  
  output$graphFull_myCox<-renderPlot({
    input$go_myCox
    isolate({
      #graphLst<-list(res_myCox()$graphResFull,res_myCox()$graphResStep)

      autoplot(survfit(res_myCox()$coxResFull))

    })
  })
  
  output$graphStep_myCox<-renderPlot({
    input$go_myCox
    isolate({
      #graphLst<-list(res_myCox()$graphResFull,res_myCox()$graphResStep)
      autoplot(survfit(res_myCox()$coxResStep))

    })
  })
  
  output$graphStrata_myCox<-renderPlot({
    input$go_myCox
    isolate({
      autoplot(res_myCox()$fitStrata)
      
    })
  })
  
  
  
  
  ###### 混合效应模型(myLme) ######
  
  
  output$more1_myLme<-renderUI({
    change_data()
    list(
      panel(
        heading='选择处理的数据集',
        pickerInput(
          inputId = "dataSel_myLme",
          label = "选择数据集",
          choices = ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))],
          selected =ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))][1],
          multiple = FALSE,
          options = list(`actions-box` = FALSE)
        )
        #selectInput('dataSel_dataExpt','选择数据集',ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))])
      )
    )
  })
  
  data_myLme<-reactive({
    change_data()
    get(input$dataSel_myLme,envMadis)->datamyLme
    return(datamyLme)
  })
  
  output$more2_myLme<-renderUI({
    list(
      panel(
        heading='设定模型参数',
        pickerInput(
          inputId='varsy_fixed',
          label='选择固定效应因变量y',
          choices = names(data_myLme()),
          selected=names(data_myLme())[1],
          multiple = FALSE,
          options = list(`actions-box` = FALSE)
        ),
        
        pickerInput(
          inputId='varsx_fixed',
          label='选择固定效应自变量x',
          choices = c('无'='',names(data_myLme())),
          selected='',
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ),
        
        awesomeCheckbox(
          inputId = 'reviseVarsx_myLme',
          label='调整固定效应自变量',
          value = FALSE
        ),
        conditionalPanel(
          condition = "input['reviseVarsx_myLme']",
          list(
            textInputAddon(
              inputId = 'newVarsx_myLme',
              label = '自变量调整',
              placeholder = 'eg: log(age)',
              value='',
              addon = 'pencil'
            )
          )
        ),
        
        textInputAddon(
          inputId = 'random_myLme',
          label = '随机效应设置',
          placeholder = 'eg: 1|g1/g2',
          value='',
          addon = 'pencil'
        ),
        
        pickerInput(
          inputId='method_myLme',
          label='设定模型算法',
          choices = c('ML method'='ML','RMLE method'='RMLE'),
          selected='ML',
          multiple=FALSE,
          options = list(`actions-box` = FALSE)
        ),
        
        awesomeCheckbox(
          inputId = 'subsetSet_myLme',
          label='设定子集',
          value = FALSE
        ),
        
        conditionalPanel(
          condition = "input['subsetSet_myLme']",
          list(
            textInputAddon(
              inputId = 'subsets_myLme',
              label = '设定子集表达式',
              placeholder = 'eg: sex==1',
              value='',
              addon = 'pencil'
            )
          )
        )
      ),
      awesomeCheckbox('export_myLme','是否导出到报告中?',FALSE)
    )
  })
  
  
  
  
  output$more3_myLme<-renderUI({
    list(
      tabsetPanel(
        tabPanel(
          '模型结果',
          heading='模型结果',
          verbatimTextOutput('summary_myLme'),
          status='primary'
        )#,
        # tabPanel(
        #   '模型图形结果',
        #   plotOutput('graph_myTree',height='720px')
        # )
      )
    )
  })
  
  res_myLme<-reactive({
    input$go_myLme
    req(input$go_myLme)
    isolate({
      data_myLme()->dat
      
      
      if(input$reviseVarsx_myLme){
        
        paste(input$varsy_fixed,paste(input$newVarsx_myLme,paste(input$varsx_fixed,collapse='+'),sep='+'),sep='~')->formFixed
        
      } else {
        paste(input$varsy_fixed,paste(input$varsx_fixed,collapse='+'),sep='~')->formFixed
        
      }
      
      
      paste('~',input$random_myLme,sep='')->formRandom
      
      
      
      if(input$subsetSet_myLme){
        Subset=input$subsets_myLme
      } else {Subset='all'}
      
      
      lmeS(formulaFixed = formFixed,
           formulaRandom = formRandom,
            data=dat,
            subset=Subset,
            Method=input$method_myLme
      )->resmyLme
      return(resmyLme)
    })
  })
  
  
  observeEvent(input$go_myLme,{
    change_report()
    isolate({
      if(input$export_myLme){
        
        
        if(input$subsetSet_myLme){
          Subset=input$subsets_myLme
        } else {Subset='all'}
        
        if(input$reviseVarsx_myLme){
          
          paste(input$varsy_fixed,paste(input$newVarsx_myLme,paste(input$varsx_fixed,collapse='+'),sep='+'),sep='~')->formFixed
          
        } else {
          paste(input$varsy_fixed,paste(input$varsx_fixed,collapse='+'),sep='~')->formFixed
          
        }
        
        
        paste('~',input$random_myLme,sep='')->formRandom
        
        
        data_myLme()->dat
        LstMadis<-get('LstMadis',envMadis)
        #LstMadis$Data[[input$dataSel_myLme]]<-dat
        dat_myLme<-data.frame(formulaFixed=formFixed,
                              formulaRandom=formRandom,
                              Method=input$method_myLme,
                              data=input$dataSel_myLme,
                              subset=Subset
        )
        LstMadis$myLme<-unique(rbind(LstMadis$myLme,dat_myLme))
        subset(LstMadis$myLme,!is.na(data))->LstMadis$myLme
        assign('LstMadis',LstMadis,envir=envMadis)
      } else {
        NULL
      }
    })
  })
  
  
  output$summary_myLme<-renderPrint({
    input$go_myLme
    isolate({
      
      
       cat('\n')
      
       cat('######全模型结果########')
       cat('\n')

       tryCatch(print(pander(summary(res_myLme()$lmeResFull))),error=function(e)print(summary(res_myLme()$lmeResFull)))

       cat('\n')
       cat('######全模型结果########')
       cat('\n')
       
       tryCatch(print(pander(summary(res_myLme()$lmeResStep))),error=function(e)print(summary(res_myLme()$lmeResStep)))
       cat('\n')
    })
    
    
  })
  
  
  
  
  
  ###### 聚类分析(Kmeans) ######
  
  
  output$more1_kmeans<-renderUI({
    change_data()
    list(
      panel(
        heading='选择处理的数据集',
        pickerInput(
          inputId = "dataSel_kmeans",
          label = "选择数据集",
          choices = ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))],
          selected =ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))][1],
          multiple = FALSE,
          options = list(`actions-box` = FALSE)
        )
        #selectInput('dataSel_dataExpt','选择数据集',ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))])
      )
    )
  })
  
  data_kmeans<-reactive({
    change_data()
    get(input$dataSel_kmeans,envMadis)->datakmeans
    return(datakmeans)
  })
  
  output$more2_kmeans<-renderUI({
    list(
      panel(
        heading='设定聚类分析的相关参数',
        pickerInput(
          inputId='vars_kmeans',
          label='选择聚类需要的变量',
          choices = names(data_kmeans()),
          selected=names(data_kmeans())[1],
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ),
        panel(
          heading='设置聚类数目判断相关参数(cascadeKM)',
          numericInput(
            inputId = 'infgr_kmeans',
            label='设定最小聚类数目',
            value = 1,
            min=1,
            max=10
            
          ),
          numericInput(
            inputId = 'supgr_kmeans',
            label='设定最大聚类数目',
            value = 3,
            min=1,
            max=10
            
          ),
          
          pickerInput(
            inputId='crit_kmeans',
            label='设定判别标准',
            choices = c('calinski','ssi'),
            selected='calinski',
            multiple=FALSE,
            options = list(`actions-box` = FALSE)
          ),
          numericInput(
            inputId = 'iter_kmeans',
            label='设定迭代次数',
            value = 100,
            min=1,
            max=10
            
          )
        ),
        
        panel(
          heading='设置kmeans相关参数',
          numericInput(
            inputId = 'centers_kmeans',
            label='设定实际聚类数目',
            value = 1,
            min=1,
            max=10
            
          ),
          
          numericInput(
            inputId = 'iterMax_kmeans',
            label='设定最大迭代次数',
            value = 100,
            min=1,
            max=10
            
          ),
          
          
          pickerInput(
            inputId='method_kmeans',
            label='设定聚类算法',
            choices = c('Hartigan-Wong','Lloyd','Forgy'),
            selected='Hartigan-Wong',
            multiple=FALSE,
            options = list(`actions-box` = FALSE)
          ),
          awesomeCheckbox(
            inputId = 'subsetSet_kmeans',
            label='设定子集',
            value = FALSE
          ),
          
          conditionalPanel(
            condition = "input['subsetSet_kmeans']",
            list(
              textInputAddon(
                inputId = 'subsets_kmeans',
                label = '设定子集表达式',
                placeholder = 'eg: sex==1',
                value='',
                addon = 'pencil'
              )
            )
          ),
          numericInput(
            inputId = 'seed_kmeans',
            label='设定随机数种子',
            value = 1234,
            min=1,
            max=100000
            
          )
        ),
        
        textInputAddon(
          inputId = 'clusterName_kmeans',
          label = '设定聚类新变量名',
          placeholder = 'eg: clusterKmeans',
          value='',
          addon = 'pencil'
        ),
        
        awesomeCheckbox(
          inputId = 'addVar_kmeans',
          label='将聚类结果合并至数据中',
          value = FALSE
        )
        
     
      ),
      awesomeCheckbox('export_kmeans','是否导出到报告中?',FALSE)
    )
  })
  
  
  
  
  output$more3_kmeans<-renderUI({
    list(
      tabsetPanel(
        tabPanel(
          '聚类汇总结果',
          heading='模型结果',
          verbatimTextOutput('summary_kmeans'),
          status='primary'
        ),
        tabPanel(
          '聚类数量图形结果',
          plotOutput('graph_kmeans',height='720px')
        )
      )
    )
  })
  
  res_kmeans<-reactive({
    input$go_kmeans
    req(input$go_kmeans)
    isolate({
      data_kmeans()->dat
      
      if(input$subsetSet_kmeans){
        Subset=input$subsets_kmeans
      } else {Subset='all'}
      
      ifelse(input$clusterName_kmeans=='','clusterKmeans',input$clusterName_kmeans)->clusterKmeans
      
      
      Kmeans(
        data=dat,
        vars=input$vars_kmeans,
        infgr=input$infgr_kmeans,
        supgr=input$supgr_kmeans,
        Centers=input$centers_kmeans,
        Criterion=input$crit_kmeans,
        Iter=input$iter_kmeans,
        iterMax=input$iterMax_kmeans,
        Algorithm=input$method_kmeans,
        subset=Subset,
        clusterName=clusterKmeans,
        seed=input$seed_kmeans,
        addVar=input$addVar_kmeans
      )->resKmeans
      
      
      assign(input$dataSel_kmeans,resKmeans$resKmeans,envMadis)
      
      return(resKmeans)
    })
    
    
  })
  
  
  observeEvent(c(input$go_kmeans),{  #### newly added for update picker input values.
    #req(input$addVar_kmeans)
    req(input$go_kmeans)
    updatePickerInput(session,inputId = 'vars_kmeans',choices = names(res_kmeans()$resKmeans))
    # if(input$dataName_varMnp==''){
    #   NULL
    # } else {
    #   updatePickerInput(session,inputId = 'datSel_varMnp',choices = ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))])
    # }
    
  })
  
  observeEvent(input$go_kmeans,{
    change_report()
    isolate({
      if(input$export_kmeans){


        if(input$subsetSet_kmeans){
          Subset=input$subsets_kmeans
        } else {Subset='all'}
        
        ifelse(input$clusterName_kmeans=='','clusterKmeans',input$clusterName_kmeans)->clusterKmeans
        
        
        data_kmeans()->dat
        LstMadis<-get('LstMadis',envMadis)
        #LstMadis$Data[[input$dataSel_kmeans]]<-dat
        dat_kmeans<-data.frame(data=input$dataSel_kmeans,
                               vars=paste(input$vars_kmeans,collapse=','),
                               infgr=input$infgr_kmeans,
                               supgr=input$supgr_kmeans,
                               Centers=input$centers_kmeans,
                               Criterion=input$crit_kmeans,
                               Iter=input$iter_kmeans,
                               iterMax=input$iterMax_kmeans,
                               Algorithm=input$method_kmeans,
                               subset=Subset,
                               clusterName=clusterKmeans,
                               seed=input$seed_kmeans,
                               addVar=input$addVar_kmeans
        )
        LstMadis$Kmeans<-unique(rbind(LstMadis$Kmeans,dat_kmeans))
        subset(LstMadis$Kmeans,!is.na(data))->LstMadis$Kmeans
        assign('LstMadis',LstMadis,envir=envMadis)
      } else {
        NULL
      }
    })
  })

  
  output$summary_kmeans<-renderPrint({
    input$go_kmeans
    isolate({
      
      
      cat('\n')
      
      cat('######聚类结果########')
      cat('\n')
      
      tryCatch(print(pander(head(res_kmeans()$resKmeans))),error=function(e)print(head(res_kmeans()$resKmeans)))
      
      
      cat('\n')
      cat('\n')
    })
  })
  
  
  output$graph_kmeans<-renderPlot({
    input$go_kmeans
    isolate({
      plot(res_kmeans()$graphCrit)
      
      
    })
  })
  
  
  
  
  
  ###### 主成分分析(pcaS) ######
  
  
  output$more1_pca<-renderUI({
    change_data()
    list(
      panel(
        heading='选择处理的数据集',
        pickerInput(
          inputId = "dataSel_pca",
          label = "选择数据集",
          choices = ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))],
          selected =ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))][1],
          multiple = FALSE,
          options = list(`actions-box` = FALSE)
        )
        #selectInput('dataSel_dataExpt','选择数据集',ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))])
      )
    )
  })
  
  data_pca<-reactive({
    change_data()
    get(input$dataSel_pca,envMadis)->datapca
    return(datapca)
  })
  
  output$more2_pca<-renderUI({
    list(
      panel(
        heading='设定主成分分析的相关参数',
        pickerInput(
          inputId='vars_pca',
          label='选择分析的变量',
          choices = names(data_pca()),
          selected=names(data_pca())[1],
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ),
        
          numericInput(
            inputId = 'nfcts_pca',
            label='设定主成分数量',
            value = 1,
            min=1,
            max=10
            
          ),
          
          
          pickerInput(
            inputId='rotate_pca',
            label='设定因子旋转方法',
            choices = c('none','varimax','quartimax','promax','oblimin','simplimax','cluster'),
            selected='varimax',
            multiple=FALSE,
            options = list(`actions-box` = FALSE)
          ),
          awesomeCheckbox(
            inputId = 'subsetSet_pca',
            label='设定子集',
            value = FALSE
          ),
          
          conditionalPanel(
            condition = "input['subsetSet_pca']",
            list(
              textInputAddon(
                inputId = 'subsets_pca',
                label = '设定子集表达式',
                placeholder = 'eg: sex==1',
                value='',
                addon = 'pencil'
              )
            )
          ),
        textInputAddon(
          inputId = 'varName_pca',
          label = '设定主成分变量名称',
          placeholder = 'eg: PCAVar',
          value='',
          addon='pencil'
          
        ),
        awesomeCheckbox(
          inputId = 'addVar_pca',
          label='将主成分变量添加至数据',
          value = FALSE
        )
      ),
      awesomeCheckbox('export_pca','是否导出到报告中?',FALSE)
    )
  })
  
  
  
  
  output$more3_pca<-renderUI({
    list(
      tabsetPanel(
        tabPanel(
          '主成分分析汇总结果',
          heading='模型结果',
          verbatimTextOutput('summary_pca'),
          status='primary'
        ),
        tabPanel(
          '主成分分析图形结果',
          plotOutput('graph_pca',height='720px')
        )
      )
    )
  })
  
  res_pca<-reactive({
    input$go_pca
    req(input$go_pca)
    isolate({
      data_pca()->dat
      
      if(input$subsetSet_pca){
        Subset=input$subsets_pca
      } else {Subset='all'}
      
      pcaS(
        data=dat,
        vars=input$vars_pca,
        nfcts=input$nfcts_pca,
        Rotate=input$rotate_pca,
        Scores=T,
        subset=Subset,
        pcaVarName=input$varName_pca,
        addVar=input$addVar_pca
      )->respca
      assign(input$dataSel_pca,respca$dtPCA,envMadis)
      return(respca)
    })
    
    
  })
  
  
  observeEvent(input$go_pca,{  #### newly added for update picker input values.
    updatePickerInput(session,inputId = 'vars_pca',choices = names(res_pca()$dtPCA))
    # if(input$dataName_varMnp==''){
    #   NULL
    # } else {
    #   updatePickerInput(session,inputId = 'datSel_varMnp',choices = ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))])
    # }
    
  })
  
  observeEvent(input$go_pca,{
    change_report()
    isolate({
      if(input$export_pca){
        
        
        if(input$subsetSet_pca){
          Subset=input$subsets_pca
        } else {Subset='all'}
        
        
        data_pca()->dat
        LstMadis<-get('LstMadis',envMadis)
        #LstMadis$Data[[input$dataSel_pca]]<-dat
        dat_pca<-data.frame(data=input$dataSel_pca,
                            vars=paste(input$vars_pca,collapse=','),
                            nfcts=input$nfcts_pca,
                            Rotate=input$rotate_pca,
                            Scores=T,
                            subset=Subset,
                            pcaVarName=input$varName_pca,
                            addVar=input$addVar_pca
        )
        LstMadis$pca<-unique(rbind(LstMadis$pca,dat_pca))
        subset(LstMadis$pca,!is.na(data))->LstMadis$pca
        assign('LstMadis',LstMadis,envir=envMadis)
      } else {
        NULL
      }
    })
  })
  
  
  output$summary_pca<-renderPrint({
    input$go_pca
    isolate({
      
      
      cat('\n')
      
      cat('######主成分分析结果########')
      cat('\n')
      
      cat('######因子载荷########')
      cat('\n')
      tryCatch(print(pander(res_pca()$resPCA$loadings[])),error=function(e)print(res_pca()$resPCA$loadings[]))
      
      cat('\n')
      cat('\n')
      cat('######累积贡献率########')
      cat('\n')
      tryCatch(print(pander(res_pca()$cumVar)),error=function(e)print(res_pca()$cumVar))
      
      cat('\n')
      cat('\n')
      
    })
  })
  
  
  output$graph_pca<-renderPlot({
    input$go_pca
    isolate({
      plot(scree(res_pca()$dataScree))
      
      
    })
  })
  
  
  
  
  
  
  ###### 因子分析(faS) ######
  
  
  output$more1_fa<-renderUI({
    change_data()
    list(
      panel(
        heading='选择处理的数据集',
        pickerInput(
          inputId = "dataSel_fa",
          label = "选择数据集",
          choices = ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))],
          selected =ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))][1],
          multiple = FALSE,
          options = list(`actions-box` = FALSE)
        )
        #selectInput('dataSel_dataExpt','选择数据集',ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))])
      )
    )
  })
  
  data_fa<-reactive({
    change_data()
    get(input$dataSel_fa,envMadis)->datafa
    return(datafa)
  })
  
  output$more2_fa<-renderUI({
    list(
      panel(
        heading='设定因子分析的相关参数',
        pickerInput(
          inputId='vars_fa',
          label='选择分析的变量',
          choices = names(data_fa()),
          selected=names(data_fa())[1],
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ),
        
        numericInput(
          inputId = 'nfcts_fa',
          label='设定因子数量',
          value = 1,
          min=1,
          max=10
          
        ),
        
        
        pickerInput(
          inputId='rotate_fa',
          label='设定因子旋转方法',
          choices = c('none','varimax','quartimax','bentlerT',
                      'varmin','equamax','geominT','bifactor','promax','oblimin',
                      'simplimax','bentlerQ','geominQ','biquartimin','cluster'),
          selected='varimax',
          multiple=FALSE,
          options = list(`actions-box` = FALSE)
        ),
        pickerInput(
          inputId='scores_fa',
          label='设定因子分值',
          choices = c('regression','Thurstone','tenBerge','Anderson','Barlett'),
          selected='regression',
          multiple=FALSE,
          options = list(`actions-box` = FALSE)
        ),
        pickerInput(
          inputId='fm_fa',
          label='设定因子得分方式',
          choices = c('minres','uls','ols','wls','gls','pa','ml','minchi','minrank'),
          selected='minres',
          multiple=FALSE,
          options = list(`actions-box` = FALSE)
        ),
        awesomeCheckbox(
          inputId = 'subsetSet_fa',
          label='设定子集',
          value = FALSE
        ),
        
        conditionalPanel(
          condition = "input['subsetSet_fa']",
          list(
            textInputAddon(
              inputId = 'subsets_fa',
              label = '设定子集表达式',
              placeholder = 'eg: sex==1',
              value='',
              addon = 'pencil'
            )
          )
        ),
        textInputAddon(
          inputId = 'varName_fa',
          label = '设定因子得分变量名称',
          placeholder = 'eg: FAVar',
          value='',
          addon='pencil'
          
        ),
        awesomeCheckbox(
          inputId = 'addVar_fa',
          label='将因子得分合并至数据中',
          value = FALSE
        )
      ),
      awesomeCheckbox('export_fa','是否导出到报告中?',FALSE)
    )
  })
  
  
  
  
  output$more3_fa<-renderUI({
    list(
      tabsetPanel(
        tabPanel(
          '因子分析汇总结果',
          heading='模型结果',
          verbatimTextOutput('summary_fa'),
          status='primary'
        ),
        tabPanel(
          '因子分析图形结果',
          plotOutput('graph_fa',height='720px')
        )
      )
    )
  })
  
  res_fa<-reactive({
    input$go_fa
    req(input$go_fa)
    isolate({
      data_fa()->dat
      
      if(input$subsetSet_fa){
        Subset=input$subsets_fa
      } else {Subset='all'}
      
      faS(
        data=dat,
        vars=input$vars_fa,
        nfcts=input$nfcts_fa,
        Rotate=input$rotate_fa,
        Scores=input$scores_fa,
        FM=input$fm_fa,
        subset=Subset,
        faVarName=input$varName_fa,
        addVar=input$addVar_fa
      )->resfa
      assign(input$dataSel_fa,resfa$dtFA,envMadis)
      return(resfa)
    })
    
    
  })
  
  
  observeEvent(input$go_fa,{  #### newly added for update picker input values.
    updatePickerInput(session,inputId = 'vars_fa',choices = names(res_fa()$dtFA))
    # if(input$dataName_varMnp==''){
    #   NULL
    # } else {
    #   updatePickerInput(session,inputId = 'datSel_varMnp',choices = ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))])
    # }
    
  })
  
  observeEvent(input$go_fa,{
    change_report()
    isolate({
      if(input$export_fa){


        if(input$subsetSet_fa){
          Subset=input$subsets_fa
        } else {Subset='all'}

        data_fa()->dat
        LstMadis<-get('LstMadis',envMadis)
        #LstMadis$Data[[input$dataSel_fa]]<-dat
        dat_fa<-data.frame(data=input$dataSel_fa,
                           vars=paste(input$vars_fa,collapse=','),
                           nfcts=input$nfcts_fa,
                           Rotate=input$rotate_fa,
                           Scores=input$scores_fa,
                           FM=input$fm_fa,
                           subset=Subset,
                           faVarName=input$varName_fa,
                           addVar=input$addVar_fa
        )
        LstMadis$fa<-unique(rbind(LstMadis$fa,dat_fa))
        subset(LstMadis$fa,!is.na(data))->LstMadis$fa
        assign('LstMadis',LstMadis,envir=envMadis)
      } else {
        NULL
      }
    })
  })

  
  output$summary_fa<-renderPrint({
    input$go_fa
    isolate({
      
      
      cat('\n')
      
      cat('######主成分分析结果########')
      cat('\n')
      
      cat('######因子载荷########')
      cat('\n')
      tryCatch(print(pander(res_fa()$resFA$loadings[])),error=function(e)print(res_fa()$resFA$loadings[]))
      
      cat('\n')
      cat('\n')
      cat('######累积贡献率########')
      cat('\n')
      tryCatch(print(pander(res_fa()$cumVar)),error=function(e)print(res_fa()$cumVar))
      
      cat('\n')
      cat('\n')
      
    })
  })
  
  
  output$graph_fa<-renderPlot({
    input$go_fa
    isolate({
      plot(scree(res_fa()$dataScree))
      
      
    })
  })
  
  
  
  
  
  
  
  
  
  
  
  ###### 倾向得分匹配(PSM) ######
  
  
  output$more1_match<-renderUI({
    change_data()
    list(
      panel(
        heading='选择处理的数据集',
        pickerInput(
          inputId = "dataSel_match",
          label = "选择数据集",
          choices = ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))],
          selected =ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))][1],
          multiple = FALSE,
          options = list(`actions-box` = FALSE)
        )
        #selectInput('dataSel_dataExpt','选择数据集',ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))])
      )
    )
  })
  
  data_match<-reactive({
    change_data()
    get(input$dataSel_match,envMadis)->dataMatch
    return(dataMatch)
  })
  
  output$more2_match<-renderUI({
    list(
      panel(
        heading='选择匹配的相关变量',
        pickerInput(
          inputId='vary_match',
          label='选择分组变量',
          choices = names(data_match()),
          selected=names(data_match())[1],
          multiple = FALSE,
          options = list(`actions-box` = TRUE)
        ),
        
        
        pickerInput(
          inputId='varx_match',
          label='选择需要匹配的变量',
          choices = names(data_match()),
          selected=names(data_match())[1],
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        )
      ),
        
      panel(
        heading='选择匹配相关参数',
        
        pickerInput(
          inputId='method_match',
          label='设定匹配的方法',
          choices = c('exact','full','nearest','subclass',
                      'genetic'),
          selected='nearest',
          multiple=FALSE,
          options = list(`actions-box` = FALSE)
        ),
        
        pickerInput(
          inputId='distance_match',
          label='选择距离测量方法',
          choices = c('logit'),
          selected='logit',
          multiple=FALSE,
          options = list(`actions-box` = FALSE)
        ),
        
        numericInput(
          inputId = 'ratio_match',
          label = '选择匹配比例',
          value = 1,
          step = 1
        )
        
      ),
      
      panel(
        heading='保存数据集',
        textInputAddon(inputId='dataName_match',label='保存的数据名称',value='',placeholder = 'eg:data_newVarType',addon=icon('pencil'))
      )
        
    )
  })
  
  
  
  
  output$more3_match<-renderUI({
    list(
      tabsetPanel(
        tabPanel(
          title='匹配汇总结果',
          heading='模型结果',
          verbatimTextOutput('summary_match'),
          status='primary'
        )
        ,
        tabPanel(
          '匹配前后各组差异',
          
          panel(
            heading='匹配前各组的表格结果',
            DT::dataTableOutput('tableB_match',height='720px'),
            status='primary'
          ),
          
          panel(
            heading='匹配后各组的表格结果',
            DT::dataTableOutput('tableA_match',height='720px'),
            status='primary'
          )
          # icon=icon('calendar')
          
        )
    )
    )
  })
  
  res_match<-reactive({
    input$go_match
    req(input$go_match)
    isolate({
      data_match()->dat
      
      paste(input$vary_match,paste(input$varx_match,collapse='+'),sep='~')->formula_match
      
      matchIt(
        data=dat,
        formula=formula_match,
        Method=input$method_match,
        Distance=input$distance_match,
        Ratio=input$ratio_match
      )->resmatch
      
      if(input$dataName_match!=''){
        assign(input$dataName_match,resmatch$dataMatched,envMadis)
      } else {
        assign(input$dataSel_match,resmatch$dataMatched,envMadis)
      }
      
      return(list(resmatch=resmatch,formu=formula_match))
    })
    
    
  })
  
  
  observeEvent(input$go_match,{  #### newly added for update picker input values.
    change_data()
    updatePickerInput(session,inputId = 'dataSel_match',choices = setdiff(c(ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))],input$dataName_match),''))
  })
  
  
  output$summary_match<-renderPrint({
    input$go_match
    isolate({
      
      
      cat('\n')
      
      cat('######匹配的汇总结果########')
      print(pander(res_match()$resmatch$resMatch))
      
      cat('\n')
      cat('\n')
      
    })
  })
  
  
  output$tableB_match<-DT:::renderDataTable({
    input$go_match
    
    isolate({
      data_match()->dat
      res_match()$formu->formula_match
      
      descTab(formula_match,dat)
      
      
    })
  })
  
  output$tableA_match<-DT:::renderDataTable({
    input$go_match
    isolate({
      res_match()$resmatch$dataMatched->dat
      res_match()$formu->formula_match
      descTab(formula_match,dat)
      
      
    })
    
    
    
  })
  
  
  ###### 时间序列分析(myProphet) ######
  
  output$more1_myProphet<-renderUI({
    change_data()
    
    list(
      panel(
        heading='选择处理的数据集',
        pickerInput(
          inputId = "dataSel_myProphet",
          label = "选择数据集",
          choices = ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))],
          selected =ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))][1],
          multiple = FALSE,
          options = list(`actions-box` = FALSE)
        )
        #selectInput('dataSel_dataExpt','选择数据集',ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))])
      )
    )
  })
  
  data_myProphet<-reactive({
    change_data()
    get(input$dataSel_myProphet,envMadis)->datamyProphet
    return(datamyProphet)
    
  })
  
  output$more2_myProphet<-renderUI({
    list(
      panel(
        flowLayout(
          heading='选择时间序列分析各参数',
          pickerInput(
            inputId='tsvar_myProphet',
            label='选择日期时间变量',
            choices = c(names(data_myProphet())),
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
            choices = c(names(data_myProphet())),
            selected='NULL',
            multiple = TRUE,
            options = list(`actions-box` = TRUE)
          ),
          
          pickerInput(
            inputId='groupvars_myProphet',
            label='选择亚组分析变量',
            choices = c('无'='1',names(data_myProphet())),
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
        awesomeCheckbox('export_myProphet','将该结果输出报告',FALSE)
      )
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
    # isolate({
    data_myProphet()->dat
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
  
  
  observeEvent(input$go_myProphet,{
    change_report()
    isolate({
      if(input$export_myProphet){
        data_myProphet()->dat
        LstMadis<-get('LstMadis',envMadis)
        #LstMadis$Data[[input$dataSel_myProphet]]<-dat
        
        dat_myProphet<-data.frame(data=input$dataSel_myProphet,
                                  tsVar=input$tsvar_myProphet,
                                  tsFormat=paste(input$dateFormat_myProphet,input$timeFormat_myProphet,sep=''),
                                  measureVars=paste(input$measurevars_myProphet,collapse=';'),
                                  groupVars = paste(input$groupvars_myProphet,collapse=';'),
                                  Period = input$period_myProphet,
                                  FN=input$FN_myProphet,
                                  Cap=input$cap_myProphet,
                                  Floor=input$floor_myProphet,
                                  Growth=input$growth_myProphet,
                                  H=input$H_myProphet,
                                  yearlyS = input$yearlyS_myProphet,
                                  dailyS = input$dailyS_myProphet,
                                  weeklyS = input$weeklyS_myProphet
        )
        
        LstMadis$myProphet<-unique(rbind(LstMadis$myProphet,dat_myProphet))
        subset(LstMadis$myProphet,!is.na(data))->LstMadis$myProphet
        assign('LstMadis',LstMadis,envir=envMadis)
      } else {
        NULL
      }
    })
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
  
  
  ##### datatable ####
  output$more1_DT<-renderUI({
    change_data()
    list(
      panel(
        heading='选择处理的数据集',
        pickerInput(
          inputId = "dataSel_DT",
          label = "选择数据集",
          choices = ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))],
          selected =ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))][1],
          multiple = FALSE,
          options = list(`actions-box` = FALSE)
        )
      ),
      
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
  
  
  data_DT<-reactive({
    change_data()
    get(input$dataSel_DT,envMadis)->dataDT
    return(dataDT)
    
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
      
    data_DT()->dt
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
  
  observeEvent(input$go_DT,{
    change_report()
    dtCfg()->cfg
    cfg[cfg=='']<-NA
    isolate({
      if(input$export_dataMnp){
        data_DT()->dt
        LstMadis<-get('LstMadis',envMadis)
        #LstMadis$Data[[input$dataSel_DT]]<-dt
        
        dat_DT<-data.frame(data=input$dataSel_DT,
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
        )
        
        LstMadis$dataMnp<-unique(rbind(LstMadis$dataMnp,dat_DT))
        subset(LstMadis$dataMnp,!is.na(data))->LstMadis$dataMnp
        assign('LstMadis',LstMadis,envir=envMadis)
      } else {
        NULL
      }
    })
  })
  
  
  
  output$resMnp<-DT:::renderDataTable({
    res_DT()$tabRes
  })
  
  
  
  
  
  
  ###### 生成报告(report) ######
  
  observe({
    change_report()
    isolate({
      get('LstMadis',envMadis)->LstMadis
      save(LstMadis,file='LstMadis.RData')
    })

  })
  
  
  observeEvent(input$go_report,{
    isolate({
      out <- render('madisReportTemp.Rmd', switch(
        input$format_report,
        PDF = pdf_document(latex_engine ='xelatex',toc=T,toc_depth=4,includes=includes(in_header='header.tex')), HTML = html_document(toc=T,toc_detpth=4), Word = word_document()
      ))
      output$downloadReport <- downloadHandler(
        filename = function() {
          paste('my-report', Sys.Date(), sep = '.', switch(
            input$format_report, PDF = 'pdf', HTML = 'html', Word = 'docx'
          ))
        },
        content = function(file) {
          file.rename(out, file)
        }
      )
      
    })
  })
  
  observeEvent(input$format_report,{
    write.csv(data.frame(format=input$format_report),'formatReport.csv')
  })
  
  output$down_report<-renderUI({
    req(input$go_report)
    list(
      downloadButton('downloadReport','下载报告',class='fa-3x')
    )
    
  })
  
  
  
}


###### uiHeader ######

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
    strong('MADIS'),
    ###### 导入数据功能(data_Impt)######
    
    tabPanel(
      icon=icon('file-upload'),
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
            aceEditor("text_dataImpt", value=readr:::format_tsv(mtcars), mode="r", theme="chrome",height="150px")
            
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
    ), ## 数据读取
    
    
    ###### 数据处理 ######
    navbarMenu(
      '数据处理',
      icon=icon('file-upload'),
      
      ###### 数据处理-变量名修改 ######
      tabPanel(
        '变量名更改',
        icon=icon('pen'),
        sidebarLayout(
          sidebarPanel(
            uiOutput('more1_varName'),
            uiOutput('more2_varName'),
            actionBttn('go_varName','刷新')
            
          ),
          mainPanel(
            verbatimTextOutput('summary_varName')
            
          )
        )
        
      ),  # 变量名修改
      
      
      ###### 数据处理-生成变量 ######
      tabPanel(
        icon=icon('plus'),
        '生成新变量',
        sidebarLayout(
          sidebarPanel(
            uiOutput('more1_varMnp'),
            uiOutput('more2_varMnp'),
            actionBttn('go_varMnp','刷新')
            
            
            
          ),
          mainPanel(
            verbatimTextOutput('summary_varMnp')
          )
        )
        
      ),
      
      
      ###### 数据处理-变量类型转换 ######
      tabPanel(
        icon=icon('exchange-alt'),
        strong('变量类型转换'),
        sidebarLayout(
          sidebarPanel(
            uiOutput('more1_varClass'),
            uiOutput('more2_varClass'),
            uiOutput('more3_varClass'),
            actionBttn('go_varClass','刷新')
          ),
          mainPanel(
            verbatimTextOutput('summary_varClass')
          )
        )
        
      ),
      
      
      ###### 数据处理-数据变形 ######
      tabPanel(
        icon=icon('shapes'),
        '数据变形',
        sidebarLayout(
          sidebarPanel(
            uiOutput('more1_reshape'),
            uiOutput('more2_reshape'),
            actionBttn('go_reshape','刷新')
          ),
          
          mainPanel(
            verbatimTextOutput('summary_reshape')
          )
        )
      ),
      
      
      ###### 数据去重(unique) ######
      tabPanel(
        icon=icon('align-justify'),
        '数据去重',
        sidebarLayout(
          sidebarPanel(
            uiOutput('more1_unique'),
            uiOutput('more2_unique'),
            actionBttn('go_unique','刷新')
          ),
          
          mainPanel(
            verbatimTextOutput('summary_unique')
          )
        )
        
      ),
      
      ###### 数据处理-合并 ######
      tabPanel(
        icon=icon('object-ungroup'),
        '数据合并',
        sidebarLayout(
          sidebarPanel(
            uiOutput('more1_dataMerge'),
            uiOutput('more2_dataMerge'),
            actionBttn('go_dataMerge','刷新')
          ),
          
          mainPanel(
            verbatimTextOutput('summary_dataMerge')
          )
        )
      ),
      
      
      ###### 数据处理-缺失值填补 ######
      tabPanel(
        icon=icon('paint-roller'),
        strong('缺失值填补'),
        sidebarLayout(
          sidebarPanel(
            uiOutput('more1_naImpute'),
            uiOutput('more2_naImpute'),
            actionBttn('go_naImpute','刷新')
          ),
          
          mainPanel(
            verbatimTextOutput('summary_naImpute')
          )
        )
      ),
      
      
      ###### 数据处理-筛选数据(行(子集，行号)，列(变量)) ######
      tabPanel(
        icon=icon('filter'),
        '筛选数据',
        sidebarLayout(
          sidebarPanel(
            uiOutput('more1_dataFilter'),
            uiOutput('more2_dataFilter'),
            actionBttn('go_dataFilter','刷新')
          ),
          
          mainPanel(
            verbatimTextOutput('summary_dataFilter')
          )
        )
      )
    ),
    
    
    ###### 数据处理-导出数据集 ######
    tabPanel(
      icon=icon('download'),
      '数据导出',
      sidebarLayout(
        sidebarPanel(
          uiOutput('more1_dataExpt'),
          uiOutput('more2_dataExpt'),
          downloadButton("downloadData", "Download")
        ),
        
        mainPanel(
          verbatimTextOutput('summary_dataExpt')
        )
      )
    ),
    
    
    ###### 测试shinyAce 暂时屏蔽 ######
    # tabPanel(
    #   'R代码编辑区',
    #   sidebarLayout(
    #     sidebarPanel(
    #       panel(
    #         heading='代码输入区',
    #         aceEditor("code_Ace", mode="r", height='400px',value="#The environment is envMadis",theme='github')
    #       ),
    #       actionBttn("go_Ace", "运行代码")
    #     ),
    #     mainPanel(
    #       verticalLayout(
    #         fluid=FALSE,
    #         panel(
    #           heading='运行结果',
    #           verbatimTextOutput('result_Ace'),
    #           status='primary'
    #         ),
    #         panel(
    #           heading='图形结果',
    #           plotOutput('graph_Ace'),
    #           status='primary'
    #         )
    #         
    #       )
    #     )
    #   )
    # ),
    
    
    
    
    
    
    
    
    
    
    
    
    ###### 描述性分析 ######
    tabPanel(
      icon=icon('dice-one'),
      '单变量描述性分析结果',
      sidebarLayout(
        sidebarPanel(
          uiOutput('more1_desc'),
          uiOutput('more2_desc'),
          actionBttn('go_desc','确定')
        ),
        mainPanel(
          uiOutput('more4_desc'),
          uiOutput('more3_desc')
          
        )
      )
    ),
    
    
    ###### 统计检验和单因素分析表合并菜单 #####
    
    navbarMenu(
      icon=icon('dice-two'),
      '统计检验',
      ###### 单因素(统计检验)分析 ######
      tabPanel(
        '假设检验',
        icon=icon('heading'),
        sidebarLayout(
          sidebarPanel(
            uiOutput('more1_hTest'),
            uiOutput('more2_hTest'),
            actionBttn('go_hTest','确定')
          ),
          mainPanel(
            uiOutput('more4_hTest'),
            uiOutput('more3_hTest')
          )
        )
      ),
      ###### 分类统计表制作 ######
      tabPanel(
        icon=icon('table'),
        '描述性统计表',
        sidebarLayout(
          sidebarPanel(
            uiOutput('more1_myTable'),
            uiOutput('more2_myTable'),
            actionBttn('go_myTable','确定')
          ),
          mainPanel(
            uiOutput('more3_myTable')
          )
        )
      )
    ),
    
    ###### 统计图形制作 ######
    tabPanel(
      '统计图形',
      icon=icon('chart-pie'),
      sidebarLayout(
        sidebarPanel(
          uiOutput('more1_myGplt'),
          uiOutput('more2_myGplt'),
          actionBttn('go_myGplt','确定')
        ),
        mainPanel(
          uiOutput('more3_myGplt')
        )
      )
    ),
    
    
    
    
    
    ###### 模型 ######
    navbarMenu(
      icon=icon('medium'),
      '模型分析',
      tabPanel(
        '线性模型',
        icon=icon('chart-line'),
        sidebarLayout(
          sidebarPanel(
            uiOutput('more1_myGlm'),
            uiOutput('more2_myGlm'),
            actionBttn('go_myGlm','确定')
          ),
          mainPanel(
            uiOutput('more3_myGlm')
            
          )
        )
      ),
      tabPanel(
        icon=icon('ruler-horizontal'),
        'COX风险模型',
        sidebarLayout(
          sidebarPanel(
            uiOutput('more1_myCox'),
            uiOutput('more2_myCox'),
            actionBttn('go_myCox','确定')
          ),
          mainPanel(
            uiOutput('more3_myCox')
          )
        )
      ),
      tabPanel(
        '决策树模型',
        icon=icon('tree'),
        sidebarLayout(
          sidebarPanel(
            uiOutput('more1_myTree'),
            uiOutput('more2_myTree'),
            actionBttn('go_myTree','确定')
          ),
          mainPanel(
            uiOutput('more3_myTree')
          )
        )
      ),
      tabPanel(
        icon=icon('random'),
        '混合效应模型',
        sidebarLayout(
          sidebarPanel(
            uiOutput('more1_myLme'),
            uiOutput('more2_myLme'),
            actionBttn('go_myLme','确定')
          ),
          mainPanel(
            uiOutput('more3_myLme')
          )
        )
      )
      
      
    ),
    
    
    
    ###### 探索性数据分析 ######
    
    navbarMenu(
      icon=icon('gavel'),
      '数据挖掘',
      tabPanel(
        icon=icon('project-diagram'),
        '聚类分析',
        sidebarLayout(
          sidebarPanel(
            uiOutput('more1_kmeans'),
            uiOutput('more2_kmeans'),
            actionBttn('go_kmeans','确定')
          ),
          mainPanel(
            uiOutput('more3_kmeans')
          )
        )
      ),
      
      tabPanel(
        icon=icon('product-hunt'),
        '主成分分析',
        sidebarLayout(
          sidebarPanel(
            uiOutput('more1_pca'),
            uiOutput('more2_pca'),
            actionBttn('go_pca','确定')
          ),
          mainPanel(
            uiOutput('more3_pca')
          )
        )
      ),
      
      tabPanel(
        '因子分析',
        icon=icon('facebook-f'),
        sidebarLayout(
          sidebarPanel(
            uiOutput('more1_fa'),
            uiOutput('more2_fa'),
            actionBttn('go_fa','确定')
          ),
          mainPanel(
            uiOutput('more3_fa')
          )
        )
      ),
      ###### 倾向得分匹配 ######
      tabPanel(
        icon=icon('equals'),
        # icon=icon('chart-scatter'),
        '倾向得分匹配',
        sidebarLayout(
          sidebarPanel(
            uiOutput('more1_match'),
            uiOutput('more2_match'),
            actionBttn('go_match','确定')
          ),
          mainPanel(
            uiOutput('more3_match')
          )
        )
      )
    ),
    
    
    
    
    
    
    
    
    
    ###### 时间序列分析及预测 ######
    tabPanel(
      icon=icon('calendar-alt'),
      '时间序列',
      sidebarLayout(
        sidebarPanel(
          uiOutput('more1_myProphet'),
          uiOutput('more2_myProphet'),
          actionBttn('go_myProphet','确定')
        ),
        mainPanel(
          uiOutput('more3_myProphet')
        )
      )
    ),
    
    
    
    
    ####### datatable ###### 
    
    tabPanel(
      icon=icon('th'),
      '统计表格',
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
    
    
    
    
    
    
    
    
    ###### 自动化报告(report) ######
    tabPanel(
      icon=icon('file-pdf'),
      '生成报告',
      sidebarLayout(
        sidebarPanel(
          radioButtons('format_report', '选择生成的文档类型', c('PDF', 'HTML', 'Word'),inline = TRUE),
          actionBttn('go_report','确定')
          
        ),
        mainPanel(
          tabPanel('button',htmlOutput('down_report'))
          
        )
      )
    )
    
    
    
    ###### tail ######
  )
  
)


shinyApp(ui = ui, server = server)
