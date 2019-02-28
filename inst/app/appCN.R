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
  
  
  ###### 数据导入功能（data_Impt） ######
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
      cat('当前数据共：',nrow(data_dataImpt()),'观测（行）','\n')
      cat('当前数据共：',ncol(data_dataImpt()),'变量（列）','\n')
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
  })
  
  
  
  
  ###### 变量名修改功能（var_Name） ######
  
  output$more1_varName<-renderUI({
    
    change_data()
    #?#
    
    list(
      panel(heading='选择数据集',
            pickerInput(
              inputId = "dataSel_varName",
              label = "选择数据集",
              choices = ls(envMedstats)[-which(ls(envMedstats)%in%c('envMedstats','server','ui','LstMedstats'))],
              selected =ls(envMedstats)[-which(ls(envMedstats)%in%c('envMedstats','server','ui','LstMedstats'))][1],
              multiple = FALSE,
              options = list(`actions-box` = FALSE)
            )
      )
      
    )
  })
  
  data_varName<-reactive({
    change_data()
    get(input$dataSel_varName,envMedstats)->dat_varName
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
      assign(input$dataSel_varName,dat,envMedstats)
      return(dat)
    })
  })
  
  output$summary_varName<-renderPrint({
    print(summary(rename_varName()))
    print(rename_varName())
    
  })
  
  
  
  
  ###### 生成新变量（varMnp） ######
  output$more1_varMnp<-renderUI({

    change_data()
    list(
      panel(
        heading='选择数据集',
        pickerInput(
          inputId = "dataSel_varMnp",
          label = "选择数据集",
          choices = ls(envMedstats)[-which(ls(envMedstats)%in%c('envMedstats','server','ui','LstMedstats'))],
          selected =ls(envMedstats)[-which(ls(envMedstats)%in%c('envMedstats','server','ui','LstMedstats'))][1],
          multiple = FALSE,
          options = list(`actions-box` = FALSE)
        )
        #selectInput('dataSel_varMnp','选择数据集',ls(envMedstats)[-which(ls(envMedstats)%in%c('envMedstats','server','ui'))])
      )
    )
  })
  
  data_varMnp<-reactive({
    change_data()
    
    get(input$dataSel_varMnp,envMedstats)->data_varMnp
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
                textInputAddon(inputId='reGroupOther_varMnp',label='设定其他组（未定义组）分组标签',value='Others',addon=icon('pencil'))
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
      assign(input$dataSel_varMnp,dat,env=envMedstats)
    } else {
      assign(input$dataName_varMnp,dat,env=envMedstats)
    }
    
    #assign(input$dataSel_varMnp,dat,envMedstats)
    return(dat)
  })
  
  
  
  output$summary_varMnp<-renderPrint({
    res_varMnp()->dt
    print(head(dt))
    #print(summary(dt))
    
  })
  
  
  
  
  
  
  ###### 变量类型转换（varClass） ######
  
  output$more1_varClass<-renderUI({
    
    change_data()
    #?#
    list(
      panel(
        heading='选择数据集',
        pickerInput(
          inputId = "dataSel_varClass",
          label = "选择数据集",
          choices = ls(envMedstats)[-which(ls(envMedstats)%in%c('envMedstats','server','ui','LstMedstats'))],
          selected =ls(envMedstats)[-which(ls(envMedstats)%in%c('envMedstats','server','ui','LstMedstats'))][1],
          multiple = FALSE,
          options = list(`actions-box` = FALSE)
        )
        #selectInput('dataSel_varClass','选择数据集',ls(envMedstats)[-which(ls(envMedstats)%in%c('envMedstats','server','ui','LstMedstats'))])
      )
    )
  })
  
  data_varClass<-reactive({
    
    change_data()
    #?#
    get(input$dataSel_varClass,envMedstats)->data_varClass
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
        assign(input$dataSel_varClass,dat,env=envMedstats)
      } else {
        assign(input$dataName_varClass,dat,env=envMedstats)
      }
      
      return(dat)
    })
  })
  
  output$summary_varClass<-renderPrint({
    input$go_varClass
    isolate({
      res_varClass()->dt
      sapply(dt,class)
      print(head(dt))
      sapply(dt,class)->y
      unique(y)->x
      sapply(x,function(i)names(y)[which(y==i)])->res
      print(res)
      
      skim(dt)
    })
    
  })
  
  
  
  ###### 数据变形（reshape） ######
  
  output$more1_reshape<-renderUI({
    
    change_data()
    #?#
    list(
      panel(
        heading='选择数据集',
        pickerInput(
          inputId = "dataSel_reshape",
          label = "选择数据集",
          choices = ls(envMedstats)[-which(ls(envMedstats)%in%c('envMedstats','server','ui','LstMedstats'))],
          selected =ls(envMedstats)[-which(ls(envMedstats)%in%c('envMedstats','server','ui','LstMedstats'))][1],
          multiple = FALSE,
          options = list(`actions-box` = FALSE)
        )
        #selectInput('dataSel_reshape','选择数据集',ls(envMedstats)[-which(ls(envMedstats)%in%c('envMedstats','server','ui','LstMedstats'))])
      )
    )
  })
  
  data_reshape<-reactive({
    
    change_data()
    #?#
    get(input$dataSel_reshape,envMedstats)->dataReshape
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
          heading='设定行转列（melt）方法的各个参数',
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
          heading='设定列转行方法（dcast）的各个参数',
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
          textInputAddon(inputId='argsMore_cast','其他参数（参考dcast）',value='',placeholder = 'eg:drop=TRUE',addon = icon("pencil")),
          pickerInput(
            inputId='fnAggre',
            label='选择合并函数',
            choices=c(
              '最小值'='min',
              '最大值'='max',
              '平均值'='max',
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
        assign(paste0(input$dataSel_reshape,'reshaped'),dat_reshape,env=envMedstats)
      } else {
        assign(input$dataName_reshape,dat_reshape,env=envMedstats)
      }
      
      return(dat_reshape)
    })
  })
  
  
  output$summary_reshape<-renderPrint({
    input$go_reshape
    #?#input$dataSel_reshape
    get(input$dataSel_reshape,envMedstats)->dataReshape
    res_reshape()->dt
    print(head(dt,n=10))
    print(summary(dt))
    print(summary(dataReshape))
    print(change_data())
  })
  
  
  
  
  ###### 数据去重（unique） ######
  output$more1_unique<-renderUI({
    
    change_data()
    #?#
    list(
      panel(
        heading='选择处理的数据集',
        pickerInput(
          inputId = "dataSel_unique",
          label = "选择数据集",
          choices = ls(envMedstats)[-which(ls(envMedstats)%in%c('envMedstats','server','ui','LstMedstats'))],
          selected =ls(envMedstats)[-which(ls(envMedstats)%in%c('envMedstats','server','ui','LstMedstats'))][1],
          multiple = FALSE,
          options = list(`actions-box` = FALSE)
        )
        #selectInput('dataSel_naImpute','选择数据集',ls(envMedstats)[-which(ls(envMedstats)%in%c('envMedstats','server','ui','LstMedstats'))])
      )
    )
  })
  
  data_unique<-reactive({
    
    change_data()
    #?#
    get(input$dataSel_unique,envMedstats)->datanaImpute
    return(datanaImpute)
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
        assign(paste0(input$dataSel_unique,'Unique'),dataUnique,envMedstats)
      } else {
        assign(input$dataName_unique,dataUnique,envMedstats)
      }
      return(dataUnique)
    })
  })
  
  output$summary_unique<-renderPrint({
    input$go_unique
    print(head(res_unique()))
    print(summary(res_unique()))
  })
  
  
  
  ###### 数据合并（Merge and Bind） ######
  output$more1_dataMerge<-renderUI({
    
    change_data()
    #?#
    list(
      panel(
        heading='选择需要合并的数据集',
        pickerInput(
          inputId = "dataSel1_dataMerge",
          label = "选择数据集1",
          choices = ls(envMedstats)[-which(ls(envMedstats)%in%c('envMedstats','server','ui','LstMedstats'))],
          selected =ls(envMedstats)[-which(ls(envMedstats)%in%c('envMedstats','server','ui','LstMedstats'))][1],
          multiple = FALSE,
          options = list(`actions-box` = FALSE)
        ),
        pickerInput(
          inputId = "dataSel2_dataMerge",
          label = "选择数据集2",
          choices = ls(envMedstats)[-which(ls(envMedstats)%in%c('envMedstats','server','ui','LstMedstats'))],
          selected =ls(envMedstats)[-which(ls(envMedstats)%in%c('envMedstats','server','ui','LstMedstats'))][1],
          multiple = FALSE,
          options = list(`actions-box` = FALSE)
        )
        #selectInput('dataSel1_dataMerge','选择数据集1',ls(envMedstats)[-which(ls(envMedstats)%in%c('envMedstats','server','ui','LstMedstats'))]),
        #selectInput('dataSel2_dataMerge','选择数据集2',ls(envMedstats)[-which(ls(envMedstats)%in%c('envMedstats','server','ui','LstMedstats'))])
      )
    )
  })
  
  data_dataMerge<-reactive({
    
    change_data()
    #?#
    get(input$dataSel1_dataMerge,envMedstats)->dataMerge1
    get(input$dataSel2_dataMerge,envMedstats)->dataMerge2
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
            '链接（Join）'='Merge',
            '合并（Bind）'='Bind'
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
        assign(paste0(input$dataSel1_dataMerge,input$dataSel2_dataMerge),datMerge,envMedstats)
      } else {
        assign(input$dataName_dataMerge,datMerge,envMedstats)
      }
      return(datMerge)
    })
  })
  
  output$summary_dataMerge<-renderPrint({
    input$go_dataMerge
    print(head(res_dataMerge()))
    print(summary(res_dataMerge()))
  })
  
  
  
  ###### 缺失值填补（naImpute） ######
  
  output$more1_naImpute<-renderUI({
    
    change_data()
    #?#
    list(
      panel(
        heading='选择处理的数据集',
        pickerInput(
          inputId = "dataSel_naImpute",
          label = "选择数据集",
          choices = ls(envMedstats)[-which(ls(envMedstats)%in%c('envMedstats','server','ui','LstMedstats'))],
          selected =ls(envMedstats)[-which(ls(envMedstats)%in%c('envMedstats','server','ui','LstMedstats'))][1],
          multiple = FALSE,
          options = list(`actions-box` = FALSE)
        )
        #selectInput('dataSel_naImpute','选择数据集',ls(envMedstats)[-which(ls(envMedstats)%in%c('envMedstats','server','ui','LstMedstats'))])
      )
    )
  })
  
  data_naImpute<-reactive({
    
    change_data()
    #?#
    get(input$dataSel_naImpute,envMedstats)->datanaImpute
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
            '树模型填补'='treeImpute',
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
        imputeData(data=dat,impVars=input$var_naImpute,modelVars=input$var_treeModel)->dat
      }
      
      if(input$method_impute=='myFun'){
        myFunImpute<-eval(parse(text=input$funImpute))
        xImpute[is.na(xImpute)]<-myFunImpute(xImpute[!is.na(xImpute)])
      }
      
      if(input$method_impute=='MICE'){
        A<-is.na(dat)
        A[,-which(names(dat)%in%input$var_naImpute)]<-F
        mice(dat,where=A)->res
        complete(res)->dat
      }
      
      if(!input$method_impute%in%c('treeImpute','MICE')){
        if(input$varName_naImpute==''){
          dat[,paste(input$var_naImpute,'imputed',sep='_')]<-xImpute
        } else {
          dat[,input$varName_naImpute]<-xImpute
        }
      }
      
      assign(input$dataSel_naImpute,dat,envMedstats)
      return(dat)
    })
  })
  
  output$summary_naImpute<-renderPrint({
    input$go_naImpute
    print(head(res_naImpute()))
    print(summary(res_naImpute()))
  })
  
  
  
  ###### 数据筛选（dataFilter） ######
  
  output$more1_dataFilter<-renderUI({
    
    change_data()
    #?#
    list(
      panel(
        heading='选择处理的数据集',
        pickerInput(
          inputId = "dataSel_dataFilter",
          label = "选择数据集",
          choices = ls(envMedstats)[-which(ls(envMedstats)%in%c('envMedstats','server','ui','LstMedstats'))],
          selected =ls(envMedstats)[-which(ls(envMedstats)%in%c('envMedstats','server','ui','LstMedstats'))][1],
          multiple = FALSE,
          options = list(`actions-box` = FALSE)
        )
        #selectInput('dataSel_dataFilter','选择数据集',ls(envMedstats)[-which(ls(envMedstats)%in%c('envMedstats','server','ui','LstMedstats'))])
      )
    )
  })
  
  data_dataFilter<-reactive({
    
    change_data()
    #?#
    get(input$dataSel_dataFilter,envMedstats)->dataFilter
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
        assign(input$dataSel_dataFilter,dat,envMedstats)
      } else {
        assign(input$dataName_dataFilter,dat,envMedstats)
      }
      return(dat)
    })
  })
  
  output$summary_dataFilter<-renderPrint({
    input$go_dataFilter
    print(summary(res_dataFilter()))
    print(head(res_dataFilter()))
  })
  
  
  
  
  ###### 数据导出（dataExpt） ######
  
  output$more1_dataExpt<-renderUI({
    
    change_data()
    #?#
    list(
      panel(
        heading='选择处理的数据集',
        pickerInput(
          inputId = "dataSel_dataExpt",
          label = "选择数据集",
          choices = ls(envMedstats)[-which(ls(envMedstats)%in%c('envMedstats','server','ui','LstMedstats'))],
          selected =ls(envMedstats)[-which(ls(envMedstats)%in%c('envMedstats','server','ui','LstMedstats'))][1],
          multiple = FALSE,
          options = list(`actions-box` = FALSE)
        )
        #selectInput('dataSel_dataExpt','选择数据集',ls(envMedstats)[-which(ls(envMedstats)%in%c('envMedstats','server','ui','LstMedstats'))])
      )
    )
  })
  
  data_dataExpt<-reactive({
    
    change_data()
    #?#
    get(input$dataSel_dataExpt,envMedstats)->dataExpt
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
              '国标（GB18030）'='GB18030',
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
  
  
  
  ###### r代码编写（Ace） ######
  
  
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
  
  
  
  
  
  
  
  
  
  
  ###### 写入LstMedstats的变化input ######
  change_report<-reactive({
    input$go_myTable
    input$go_desc
    input$go_hTest
    input$go_myGlm
    input$go_myTree
    input$go_myCox
    input$go_myGplt
    input$go_myProphet
  })
  
  
  
  ###### 分类统计表（myTable） ######
  
  output$more1_myTable<-renderUI({
    change_data()

    list(
      panel(
        heading='选择处理的数据集',
        pickerInput(
          inputId = "dataSel_myTable",
          label = "选择数据集",
          choices = ls(envMedstats)[-which(ls(envMedstats)%in%c('envMedstats','server','ui','LstMedstats'))],
          selected =ls(envMedstats)[-which(ls(envMedstats)%in%c('envMedstats','server','ui','LstMedstats'))][1],
          multiple = FALSE,
          options = list(`actions-box` = FALSE)
        )
        #selectInput('dataSel_dataExpt','选择数据集',ls(envMedstats)[-which(ls(envMedstats)%in%c('envMedstats','server','ui','LstMedstats'))])
      )
    )
  })
  
  data_myTable<-reactive({
    change_data()
    get(input$dataSel_myTable,envMedstats)->datamyTable
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
      awesomeCheckbox('export_myTable','是否导出报告中?',FALSE)
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
        LstMedstats<-get('LstMedstats',envMedstats)
        LstMedstats$Data[[input$dataSel_myTable]]<-dat
        paste(input$lht_myTable,collapse='+')->lhtV
        paste(input$rht_myTable,collapse='+')->rhtV
        paste(lhtV,rhtV,sep='~')->Formula
        dat_myTable<-data.frame(Formula=Formula,data=input$dataSel_myTable)
        LstMedstats$myTable<-unique(rbind(LstMedstats$myTable,dat_myTable))
        subset(LstMedstats$myTable,!is.na(data))->LstMedstats$myTable
        assign('LstMedstats',LstMedstats,envir=envMedstats)
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

  
  
  
  
  
  
  ###### 统计图形制作（myGplt） ######
  
  output$more1_myGplt<-renderUI({
    change_data()
    
    list(
      panel(
        heading='选择处理的数据集',
        pickerInput(
          inputId = "dataSel_myGplt",
          label = "选择数据集",
          choices = ls(envMedstats)[-which(ls(envMedstats)%in%c('envMedstats','server','ui','LstMedstats'))],
          selected =ls(envMedstats)[-which(ls(envMedstats)%in%c('envMedstats','server','ui','LstMedstats'))][1],
          multiple = FALSE,
          options = list(`actions-box` = FALSE)
        )
        #selectInput('dataSel_dataExpt','选择数据集',ls(envMedstats)[-which(ls(envMedstats)%in%c('envMedstats','server','ui','LstMedstats'))])
      )
    )
  })
  
  data_myGplt<-reactive({
    change_data()
    get(input$dataSel_myGplt,envMedstats)->datamyGplt
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
        awesomeCheckbox('export_myGplt','是否导出报告中?',FALSE)
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
      #   awesomeCheckbox('export_myGplt','是否导出报告中?',FALSE)
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
        LstMedstats<-get('LstMedstats',envMedstats)
        LstMedstats$Data[[input$dataSel_myGplt]]<-dat
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
        LstMedstats$myGplt<-unique(rbind(LstMedstats$myGplt,dat_myGplt))
        subset(LstMedstats$myGplt,!is.na(data))->LstMedstats$myGplt
        assign('LstMedstats',LstMedstats,envir=envMedstats)
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
  
  
  
  
  
  
  
  
  ###### 描述性分析（desc） ######
  
  output$more1_desc<-renderUI({
    
    change_data()
    #?#
    list(
      panel(
        heading='选择处理的数据集',
        pickerInput(
          inputId = "dataSel_desc",
          label = "选择数据集",
          choices = ls(envMedstats)[-which(ls(envMedstats)%in%c('envMedstats','server','ui','LstMedstats'))],
          selected =ls(envMedstats)[-which(ls(envMedstats)%in%c('envMedstats','server','ui','LstMedstats'))][1],
          multiple = FALSE,
          options = list(`actions-box` = FALSE)
        )
        #selectInput('dataSel_dataExpt','选择数据集',ls(envMedstats)[-which(ls(envMedstats)%in%c('envMedstats','server','ui','LstMedstats'))])
      )
    )
  })
  
  data_desc<-reactive({
    
    change_data()
    #?#
    get(input$dataSel_desc,envMedstats)->dataDesc
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
        LstMedstats<-get('LstMedstats',envMedstats)
        LstMedstats$Data[[input$dataSel_desc]]<-dat
        dat_desc<-data.frame(xvars=input$vars_desc,Digits=input$digits_desc,dataName=input$dataSel_desc,stringsAsFactors = F)
        LstMedstats$desc<-unique(rbind(LstMedstats$desc,dat_desc))
        #LstMedstats$desc[!is.na(xvars),]
        subset(LstMedstats$desc,!is.na(xvars))->LstMedstats$desc
        assign('LstMedstats',LstMedstats,envir=envMedstats)
        #return(LstMedstats)
      } else {
        NULL
      }
    })
  })
  
  output$res_desc<-renderPrint({
    
    res_desc()->resDesc
    which(input$vars_desc==input$Res_desc)->ind
    if(input$myFun_desc){
      print(resDesc[[i]])
    } else {
      print(pander(resDesc[[ind]]$resDesc))
    }
  })
  
  output$graph_desc<-renderPlot({
    
    
    res_desc()->resDesc
    which(input$vars_desc==input$Res_desc)->ind
    plot(resDesc[[ind]]$graphDesc)
  })
  
  
  
  ###### 单因素分析（hTest） ######
  
  output$more1_hTest<-renderUI({
    change_data()
    list(
      panel(
        heading='选择处理的数据集',
        pickerInput(
          inputId = "dataSel_hTest",
          label = "选择数据集",
          choices = ls(envMedstats)[-which(ls(envMedstats)%in%c('envMedstats','server','ui','LstMedstats'))],
          selected =ls(envMedstats)[-which(ls(envMedstats)%in%c('envMedstats','server','ui','LstMedstats'))][1],
          multiple = FALSE,
          options = list(`actions-box` = FALSE)
        )
        #selectInput('dataSel_dataExpt','选择数据集',ls(envMedstats)[-which(ls(envMedstats)%in%c('envMedstats','server','ui','LstMedstats'))])
      )
    )
  })
  
  data_hTest<-reactive({
    change_data()
    get(input$dataSel_hTest,envMedstats)->dataHtest
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
        awesomeCheckbox('export_hTest','是否导出报告中?',FALSE)
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
          resAll[[length(input$varsx_hTest)*(i-1)+j]]<-biVars(data=dat,xvars=input$varsx_hTest[j],yvars=input$varsy_hTest[i],alter=input$alter_hTest,paired=input$paired_hTest,nullHyp=input$nullHyp_hTest,confLevel=input$confLevel_hTest)
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
        LstMedstats<-get('LstMedstats',envMedstats)
        LstMedstats$Data[[input$dataSel_hTest]]<-dat
        expand.grid(input$varsx_hTest,input$varsy_hTest)->varsInput
        names(varsInput)<-c('xvars','yvars')
        dat_hTest<-data.frame(varsInput,alter=input$alter_hTest,
                              paired=input$paired_hTest,nullHyp=input$nullHyp_hTest,confLevel=input$confLevel_hTest,dataName=input$dataSel_hTest,stringsAsFactors = F)
        LstMedstats$hTest<-unique(rbind(LstMedstats$hTest,dat_hTest))
        subset(LstMedstats$hTest,!is.na(xvars))->LstMedstats$hTest
        as.logical(LstMedstats$hTest$paired)->LstMedstats$hTest$paired
        assign('LstMedstats',LstMedstats,envir=envMedstats)
        #return(LstMedstats)
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
          choices = paste(apply(expand.grid(input$varsx_hTest,input$varsy_hTest),1,function(x)paste(x,collapse=' & ')))
      )
    )
    )
  })
  
  
  output$res_hTest<-renderPrint({
    
    
    apply(expand.grid(input$varsx_hTest,input$varsy_hTest),1,function(x)paste(x,collapse=' & '))->namesRes
    
    which(namesRes==input$Res_hTest)->ind
    print(pander(res_hTest()[[ind]]$hTestRes))

  })
  
  output$graph_hTest<-renderPlot({
    
    
    
    apply(expand.grid(input$varsx_hTest,input$varsy_hTest),1,function(x)paste(x,collapse=' & '))->namesRes
    
    which(namesRes==input$Res_hTest)->ind
    
    plot(res_hTest()[[ind]]$hTestGraph)
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ###### 线性模型（myGlm） ######
  
  
  output$more1_myGlm<-renderUI({
    change_data()
    list(
      panel(
        heading='选择处理的数据集',
        pickerInput(
          inputId = "dataSel_myGlm",
          label = "选择数据集",
          choices = ls(envMedstats)[-which(ls(envMedstats)%in%c('envMedstats','server','ui','LstMedstats'))],
          selected =ls(envMedstats)[-which(ls(envMedstats)%in%c('envMedstats','server','ui','LstMedstats'))][1],
          multiple = FALSE,
          options = list(`actions-box` = FALSE)
        )
        #selectInput('dataSel_dataExpt','选择数据集',ls(envMedstats)[-which(ls(envMedstats)%in%c('envMedstats','server','ui','LstMedstats'))])
      )
    )
  })
  
  data_myGlm<-reactive({
    change_data()
    get(input$dataSel_myGlm,envMedstats)->datamyGlm
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
        LstMedstats<-get('LstMedstats',envMedstats)
        LstMedstats$Data[[input$dataSel_myGlm]]<-dat
        dat_myGlm<-data.frame(Formula=Formula,data=input$dataSel_myGlm,weightsVar=weightsVar,subset=Subset,Family=input$family_myGlm,lower=lowerForm)
        LstMedstats$myGlm<-unique(rbind(LstMedstats$myGlm,dat_myGlm))
        subset(LstMedstats$myGlm,!is.na(data))->LstMedstats$myGlm
        assign('LstMedstats',LstMedstats,envir=envMedstats)
        #return(LstMedstats)
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
      print(pander(res_myGlm()$glmResFull))
      cat('\n')
      cat('######逐步回归模型分析结果如下########')
      print(pander(res_myGlm()$glmResStep))
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
  
  
  
  
  
  
  
  
  
  ###### 决策树模型（myTree） ######
  
  
  output$more1_myTree<-renderUI({
    change_data()
    list(
      panel(
        heading='选择处理的数据集',
        pickerInput(
          inputId = "dataSel_myTree",
          label = "选择数据集",
          choices = ls(envMedstats)[-which(ls(envMedstats)%in%c('envMedstats','server','ui','LstMedstats'))],
          selected =ls(envMedstats)[-which(ls(envMedstats)%in%c('envMedstats','server','ui','LstMedstats'))][1],
          multiple = FALSE,
          options = list(`actions-box` = FALSE)
        )
        #selectInput('dataSel_dataExpt','选择数据集',ls(envMedstats)[-which(ls(envMedstats)%in%c('envMedstats','server','ui','LstMedstats'))])
      )
    )
  })
  
  data_myTree<-reactive({
    change_data()
    get(input$dataSel_myTree,envMedstats)->datamyTree
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
          label='设定数模型算法',
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
        LstMedstats<-get('LstMedstats',envMedstats)
        LstMedstats$Data[[input$dataSel_myTree]]<-dat
        dat_myTree<-data.frame(Formula=Formula,
                              data=input$dataSel_myTree,
                              subset=Subset,
                              treeMethod=input$method_myTree,
                              Minsplit=input$minSplit_myTree,
                              Minbucket = input$minBucket_myTree,
                              Maxdepth = input$maxDepth_myTree,
                              CP=input$param_myTree,
                              Mincrit=input$param_myTree)
        LstMedstats$myTree<-unique(rbind(LstMedstats$myTree,dat_myTree))
        subset(LstMedstats$myTree,!is.na(data))->LstMedstats$myTree
        assign('LstMedstats',LstMedstats,envir=envMedstats)
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
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ###### COXPH模型（myCox） ######
  
  
  output$more1_myCox<-renderUI({
    change_data()
    list(
      panel(
        heading='选择处理的数据集',
        pickerInput(
          inputId = "dataSel_myCox",
          label = "选择数据集",
          choices = ls(envMedstats)[-which(ls(envMedstats)%in%c('envMedstats','server','ui','LstMedstats'))],
          selected =ls(envMedstats)[-which(ls(envMedstats)%in%c('envMedstats','server','ui','LstMedstats'))][1],
          multiple = FALSE,
          options = list(`actions-box` = FALSE)
        )
        #selectInput('dataSel_dataExpt','选择数据集',ls(envMedstats)[-which(ls(envMedstats)%in%c('envMedstats','server','ui','LstMedstats'))])
      )
    )
  })
  
  data_myCox<-reactive({
    change_data()
    get(input$dataSel_myCox,envMedstats)->datamyCox
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
        LstMedstats<-get('LstMedstats',envMedstats)
        LstMedstats$Data[[input$dataSel_myCox]]<-dat
        dat_myCox<-data.frame(Formula=Formula,
                              data=input$dataSel_myCox,
                              weightsVar=weightsVar,
                              subset=Subset,
                              strataVar=input$strataVar_myCox,
                              lower=lowerForm)
        LstMedstats$myCox<-unique(rbind(LstMedstats$myCox,dat_myCox))
        subset(LstMedstats$myCox,!is.na(data))->LstMedstats$myCox
        assign('LstMedstats',LstMedstats,envir=envMedstats)
        #return(LstMedstats)
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
      print(pander(res_myCox()$coxResFull))
      cat('\n')
      cat('######逐步回归模型分析结果如下########')
      print(pander(res_myCox()$coxResStep))
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
  
  
  
  
  
  
  
  
  
  
  
  
  ###### 时间序列分析（myProphet） ######
  
  output$more1_myProphet<-renderUI({
    change_data()
    
    list(
      panel(
        heading='选择处理的数据集',
        pickerInput(
          inputId = "dataSel_myProphet",
          label = "选择数据集",
          choices = ls(envMedstats)[-which(ls(envMedstats)%in%c('envMedstats','server','ui','LstMedstats'))],
          selected =ls(envMedstats)[-which(ls(envMedstats)%in%c('envMedstats','server','ui','LstMedstats'))][1],
          multiple = FALSE,
          options = list(`actions-box` = FALSE)
        )
        #selectInput('dataSel_dataExpt','选择数据集',ls(envMedstats)[-which(ls(envMedstats)%in%c('envMedstats','server','ui','LstMedstats'))])
      )
    )
  })
  
  data_myProphet<-reactive({
    change_data()
    get(input$dataSel_myProphet,envMedstats)->datamyProphet
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
        awesomeCheckbox('export_myProphet','是否导出报告中?',FALSE)
      )
    )
  })
  
  
  output$more3_myProphet<-renderUI({
    list(
      tabsetPanel(
        tabPanel(
          '表格结果',
          dataTableOutput(
            'resTab_myProphet'
          )
        ),
        tabPanel(
          '历史数据结果',
          tabsetPanel(
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
        LstMedstats<-get('LstMedstats',envMedstats)
        LstMedstats$Data[[input$dataSel_myProphet]]<-dat
        
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
        
        LstMedstats$myProphet<-unique(rbind(LstMedstats$myProphet,dat_myProphet))
        subset(LstMedstats$myProphet,!is.na(data))->LstMedstats$myProphet
        assign('LstMedstats',LstMedstats,envir=envMedstats)
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
  
  
  
  
  
  
  ###### 生成报告（report） ######
  
  observe({
    change_report()
    isolate({
      get('LstMedstats',envMedstats)->LstMedstats
      save(LstMedstats,file='LstMedstats.RData')
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
  
  output$down_report<-renderUI({
    req(input$go_report)
    list(
      downloadButton('downloadReport','下载报告',class='fa-3x')
    )
    
  })
  
  
}


###### uiHeader ######

ui<-fluidPage(
  shinythemes::themeSelector(),
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
    'Medstats',
    ###### 导入数据功能（data_Impt）######
    
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
            helpText('注意：数据需为txt或csv格式文件，或复制表格数据（excel，csv等文件）到下面的窗口中'),
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
    ), ## 数据读取
    
    
    ###### 数据处理 ######
    navbarMenu(
      '数据处理',
      
      
      ###### 数据处理-变量名修改 ######
      tabPanel(
        '变量名更改',
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
      
      
      ###### 数据去重（unique） ######
      tabPanel(
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
      
      
      ###### 数据处理-筛选数据（行（子集，行号），列（变量）） ######
      tabPanel(
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
    #         aceEditor("code_Ace", mode="r", height='400px',value="#The environment is envMedstats",theme='github')
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
    
    ###### 分类统计表制作 ######
    tabPanel(
      '分类统计表',
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
    ),
    
    
    
    
    
    ###### 统计图形制作 ######
    tabPanel(
      '统计图形',
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
    
    
    
    
    ###### 描述性分析 ######
    tabPanel(
      '描述性分析',
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
    
    
    ###### 单因素（统计检验）分析 ######
    tabPanel(
      '单因素分析',
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
    
    
    
    
    ###### 线性模型(glm) ######
    navbarMenu(
      '模型分析',
      tabPanel(
        '线性模型',
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
      )
      
      
    ),
    
    
    ###### 时间序列分析及预测 ######
    tabPanel(
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
    
    
    
    
    
    
    
    
    ###### 自动化报告(report) ######
    tabPanel(
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


#shinyApp(ui = ui, server = server)
