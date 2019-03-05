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
  
#serverstart
###### 变量名修改功能(var_Name) ######

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




###### 生成新变量(varMnp) ######
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






#serverstop

}


ui<-fluidPage(
  #navbarPage(

#uistart

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
  
  
  ###### 数据去重(unique) ######
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
  
  
  ###### 数据处理-筛选数据(行(子集，行号)，列(变量)) ######
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
)


#uistop

#)
)
shinyApp(server=server,ui=ui)

