#' dataMnp
#' 
#' a function provides data manipulating procedures, mainly based on data.table.
#' 
#' @param data  data.frame
#' @param subset  character string could be parsed as a logical expression or NA, eg. subset="sex==1"
#' @param newVars  new vars names, could be character string or NA, eg. newVars='ageNew'
#' @param newVarsFormulas character string could be parsed as formulas or NA, eg. "sum(age)"
#' @param newVarsBy  groupby var name or NA
#' @param indexNames  variable names generated in the jth procedre as data.table.
#' @param Formulas  character string could be parsed as expression, and seperated with ";" eg. "mean(disp);sum(disp)"
#' @param dimVars  group by variable names, same as k in data.table
#' @param dimNames rename the dimVars
#' @param dateVar  set the dateVar in the data.frame if any
#' @param dtOrders  set the date variable format, could be 'ymd', 'mdy', 'ymd hms', please refer to lubridate::parse_date_time for more detail.
#' @param margin  calculate margin results. 0 means no margin results should calculated, 1 for the first 1 dimVars, 2 for the first 2 dimVars, etc.
#' @param revisedNames new variable name generated using the results
#' @param revisedFormulas formulas corresponding to revisedNames
#' @param revisedMargin same as margin but only for revised vars
#' @param orderVars  please refer to data.table::setorderv for more detail.
#' @param orders please refer to data.table::setorderv for more detail.
#' @param Digits  set digits
#' @param tbVars vars needed to calculate tongbi(chinese, means Year over Year) index.
#' @param hbVars  vars needed to calculate huanbi(chinese, means month over month, quater over quater, etc.)
#' @param colOrder reorder column orders.
#' 
#' 
#' 
#' 
#' 
#' @examples 
#' expand.grid(2017:2018,1:12,1:28)->X
#' apply(X,1,function(x)paste(x,collapse='-'))->date
#' dt<-data.frame(ts=date,grp1=sample(letters[1:5],length(date),rep=T),y=rnorm(length(date)))
#' dataMnp(data=dt,indexNames=c('a','b','c'),Formulas='mean(y);sd(y);sd(y)/mean(y)',dimVars='year(ts);month(ts);grp1',dimNames = 'year;month;grp',dateVar = 'ts',dtOrders = 'ymd',orderVars = 'grp',orders='1',tbVars='a;b',hbVars=c('c','tt'),revisedNames = 'tt',revisedFormulas = 'a+b',colOrder='1;2;3;4;5;6;8;9')->res
#' 
#' # generate new variable 
#' dataMnp(data=mtcars,subset='vs==1',newVars='test',newVarsFormulas='mpg+disp',indexNames='sumMPG',Formulas='sum(mpg)',dimVars='gear',dimNames='VS')
#' 
#' # revise the result using revisedNames, revisedFormulas,revisedMargin args. it's same as DT[i,j,k][c('revisedvars'):=list(...),by=.(...)]
#' # the revisedMargin means groupby which dimVars, eg. 1 means the first 1 dimVars, and 2 means the first 2 dimVars, 0 means no groupby variables.
#' 
#' dataMnp(data=mtcars,newVarsFormulas='mpg+disp',indexNames='sumMPG',Formulas='sum(mpg)',dimVars='vs;gear',dimNames='VS;Gear',revisedNames='x',revisedFormulas='sum(sumMPG)',revisedMargin=o)
#' dataMnp(data=mtcars,newVarsFormulas='mpg+disp',indexNames='sumMPG',Formulas='sum(mpg)',dimVars='vs;gear',dimNames='VS;Gear',revisedNames='x',revisedFormulas='sum(sumMPG)',revisedMargin=1)
#' dataMnp(data=mtcars,newVarsFormulas='mpg+disp',indexNames='sumMPG',Formulas='sum(mpg)',dimVars='vs;gear',dimNames='VS;Gear',revisedNames='x',revisedFormulas='sum(sumMPG)',revisedMargin=2)
#' 
#' 
#' @export


dataMnp<-function(data,
                  subset=NA,
                  newVars=NA,
                  newVarsFormulas=NA,
                  newVarsBy=NA,
                  # newVarsMargin=NA,
                  indexNames,
                  Formulas,
                  dimVars,
                  dimNames=NA,
                  dateVar=NA,
                  dtOrders='ymd',
                  margin=0,   #汇总结果值标边际
                  #revisedVars=NA, ## 
                  revisedNames=NA,  ## 重命名新增指标,
                  revisedFormulas=NA,  ##新增指标公式
                  revisedMargin=0,  ## 新增指标边际
                  orderVars=NA,   ## 排序变量
                  orders=NA,   ## 排序
                  Digits=2,      ## 有效位数
                  tbVars=NA,         ## 同比
                  hbVars=NA,          ## 环比
                  colOrder=NA
                  
){
  #options(digits=Digits)
  require('data.table')
  require('lubridate')
  require('stringi')
  if(is.character(data)){eval(as.name(data))->data}
  
  which(sapply(data,function(i)class(i))=='factor')->indFac
  
  if(length(indFac)>0){
    for(i in indFac){
      as.character(data[,i])->data[,i]
    }
  }
  
  
  if(dateVar%in%names(data)){
    if(length(dtOrders)>0){
      parse_date_time(data[,dateVar],dtOrders)->xTmp
      which(is.na(xTmp))->indNa
      if(length(indNa)>0){
        
        data[-indNa,]->data 
      } else {
        data<-data
      }
      
    }
  }
  
  
  as.data.table(data)->data
  if(!all(newVars%in%c(NA,NULL,''))){
    setdiff(unlist(stri_split_fixed(newVars,';')),c(NA,'NULL',''))->newVars
    setdiff(unlist(stri_split_fixed(newVarsFormulas,';')),c(NA,'NULL',''))->newVarsFormulas
    if(!all(newVarsBy%in%c(NA,NULL,'')))setdiff(newVarsBy,c(NA,'NULL',''))->newVarsBy
    
    if(all(newVarsBy%in%c(NA,NULL,''))){
      data[,c(newVars):=lapply(newVarsFormulas,function(i)eval(parse(text=i)))]
    } else {
      data[,c(newVars):=lapply(newVarsFormulas,function(i)eval(parse(text=i))),
         by=as.data.table(data[,newVarsBy,with=F])
         ]
    }
    
  }
  
  
  if(!all(subset%in%c(NA,NULL,''))){
    setdiff(unlist(stri_split_fixed(subset,';')),c(NA,'','NULL'))->subset
    subset(data,eval(parse(text=subset)))->data
  }
  
  
  
  as.data.frame(data)->data
  if(!all(dateVar%in%c(NA,NULL,'')))setdiff(dateVar,NA)->dateVar
  if(!all(dtOrders%in%c(NA,NULL,'')))setdiff(dtOrders,NA)->dtOrders
  if(!all(Digits%in%c(NA,NULL,'')))setdiff(Digits,NA)->Digits
  ifelse(length(Digits)==0,2,Digits)->Digits
  if(!all(dimNames%in%c(NA,NULL,'')))setdiff(dimNames,NA)->dimNames
  if(dateVar%in%names(data)){
    if(length(dtOrders)>0){
      parse_date_time(data[,dateVar],dtOrders)->data[,dateVar]
    }
  }
  
 
  
  as.data.table(data)->dt
  
  
  
  if(!all(Formulas%in%c(NA,NULL,'')))setdiff(unlist(stri_split_fixed(Formulas,';')),NA)->Formulas
  if(!all(dimVars%in%c(NA,NULL,'')))setdiff(unlist(stri_split_fixed(dimVars,';')),NA)->dimVars
  if(!all(indexNames%in%c(NA,NULL,'')))setdiff(unlist(stri_split_fixed(indexNames,';')),NA)->indexNames
  if(!all(orderVars%in%c('',NA,'NULL','NA'))){
    setdiff(unlist(stri_split_fixed(orderVars,';')),NA)->orderVars
  }
  
  if(!all(orders%in%c('',NA,'NULL','NA'))){
    orders<-as.numeric(setdiff(unlist(stri_split_fixed(orders,';')),NA))
  }
  
  
  
  Lst<-list()
  
  if(all(dimVars%in%c('',NA,'NULL','NA'))){
    dt[,
       lapply(Formulas,function(i)eval(parse(text=i)))
       ]->res
    names(res)<-indexNames
  } else {
    #setkeyv(dt,dimVars)->dt
    if(dimNames%in%c('','NA','NULL',NA,NULL)){
      dimNames<-dimVars
    } else {
      setdiff(unlist(stri_split_fixed(dimNames,';')),NA)->dimNames
    }
    
    dt[,
       lapply(Formulas,function(i)eval(parse(text=i))),
       by=as.data.table(sapply(dimVars,function(ii)dt[,eval(parse(text=ii))]))
       ]->res
    
    names(res)[1:length(dimVars)]<-dimNames
    names(res)[-(1:length(dimVars))]<-indexNames
    
  }
  
  
  #unlist(stri_split_fixed(revisedVars,';'))->revisedVars
  if(!all(revisedNames%in%c(NA,NULL,'')))setdiff(unlist(stri_split_fixed(revisedNames,';')),NA)->revisedNames
  if(!all(revisedFormulas%in%c(NA,NULL,'')))setdiff(unlist(stri_split_fixed(revisedFormulas,';')),NA)->revisedFormulas
  
  #paste('100*prop.table(',revisedFormulas,')',sep='')->revisedFormulas
  if(!all(revisedMargin%in%c(NA,NULL,'')))setdiff(revisedMargin,NA)->revisedMargin
  
  if(!all(revisedNames%in%c('','NA',NULL,NA))){
    if(revisedMargin==0){
      res[,c(revisedNames):=lapply(revisedFormulas,function(i)eval(parse(text=i)))]
    } else{
      res[,c(revisedNames):=lapply(revisedFormulas,function(i)eval(parse(text=i))),
          by=as.data.table(sapply(dimNames[1:revisedMargin],function(ii)res[,eval(parse(text=ii))]))
          ]
    }
  }
  
  
  
  if(dateVar%in%names(data)&(!all(is.na(tbVars))|!all(is.na(hbVars)))){
    which(stri_detect_regex(dimVars,'(year|quarter|month|week)'))->indDate
    # for(i in indDate){
    #   as.numeric(res[,dimNames[i],with=F])->res[,dimNames[i],with=F]
    # }
    
    res[,lapply(.SD,function(i)as.numeric(i)),.SDcols=dimNames[indDate]]->res[,dimNames[indDate]]
    
    setorderv(res,dimNames,rep(1,length(dimNames)))
    
    dimNames[-indDate]->dimNames2
    if(!all(is.na(hbVars))){
      setdiff(unlist(stri_split_fixed(hbVars,';')),NA)->hbVars
      
      res[,c(paste(hbVars,'hb',sep='_')):=lapply(
        .SD,function(i)100*(i-shift(i,1,type='lag'))/shift(i,1,type='lag')
      ),.SDcols=hbVars,by=as.data.table(sapply(dimNames2,function(ii)res[,eval(parse(text=ii))]))]
    }
    
    if(!all(is.na(tbVars))){
      ifelse(any(stri_detect_fixed(dimVars,'month')),12,
             ifelse(any(stri_detect_fixed(dimVars,'quarter')),4,
                    ifelse(any(stri_detect_fixed(dimVars,'week')),52,1)))->TB
      
      setdiff(unlist(stri_split_fixed(tbVars,';')),NA)->tbVars
      res[,c(paste(tbVars,'tb',sep='_')):=lapply(
        .SD,function(i)100*(i-shift(i,TB,type='lag'))/shift(i,TB,type='lag')
      ),.SDcols=tbVars,by=as.data.table(sapply(dimNames2,function(ii)res[,eval(parse(text=ii))]))]
    }
    
    
  }
  
  if(!all(orders%in%c('',NA,'NULL','NA'))&&!all(orderVars%in%c('',NA,'NULL','NA'))){
    setorderv(res,orderVars,orders)
    res$rowID<-1:nrow(res)
    
  } else {
    if(all(dimVars%in%c('',NA,'NA',NULL,'NULL'))){
      res$rowID<-1:nrow(res)
    } else {
      setorderv(res,dimNames,rep(1,length(dimNames)))
      res$rowID<-1:nrow(res)
    }
    
  }
  res[,c(which(colnames(res)=='rowID'),which(colnames(res)!='rowID')),with=F]->res
  
  as.data.frame(res)->res
  for(i in 1:ncol(res)){
    if(class(res[,i])%in%c('integer','factor')){
      res[,i]->res[,i]
    }
    if(class(res[,i])%in%c('character')){
      as.factor(res[,i])->res[,i]
    }
    if(class(res[,i])%in%c('numeric')){
      if(any(stri_detect_fixed(res[,i],'.'))){
        round(res[,i],Digits)->res[,i]
      } else {
        as.integer(res[,i])->res[,i]
      }
      
    }
  }
  
  if(!colOrder%in%c('NA','NULL',NA,NULL)){
    colOrders<-as.numeric(setdiff(unlist(stri_split_fixed(colOrder,';')),NA))
    res[,colOrders]->res
  }
  
  
  data.table(res,stringsAsFactors = T)->res
  
  
  
  Lst[['tabRes']]<-res
  
  if(!all(margin%in%c(NA,NULL,'')))setdiff(margin,NA)->margin
  if(!all(dimVars%in%c('','NA','NULL',NA,NULL))&&margin>0){
    for(i in 0:(margin-1)){
      
      if(i==0){
        dt[,
           lapply(Formulas,function(i)eval(parse(text=i)))
           ]->resi
        names(resi)<-indexNames
        Lst[['margin_all']]<-resi
      }
      else{
        dimVarsi<-dimVars[1:i]
        
        dt[,
           lapply(Formulas,function(i)eval(parse(text=i))),
           by=as.data.table(sapply(dimVarsi,function(ii)dt[,eval(parse(text=ii))]))
           ]->resi
        names(resi)[(ncol(resi)-length(indexNames)+1):ncol(resi)]<-indexNames
        names(resi)[1:i]<-dimNames[1:i]
        Lst[[paste('margin_',paste(dimVarsi,collapse='_'))]]<-resi
      }
      
      
    }
    
  }
  
  
  return(Lst)
  
  
}
