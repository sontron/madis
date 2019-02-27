#' fn_aggre
#'
#' this is a wrapp version of aggregate function in R base package.
#'
#' @author sontron
#' @param data is a data.frame object that need to be processed
#' @param group are the dimension variables
#' @param val are the variables that needed calculated
#' @param subsets are subset of data object
#' @return same as aggregate function
#'
#' @examples
#' fn_aggre(iris,group='Species',val=c('Sepal.Length','Sepal.Width'),fun='mean')
#' fn_aggre(iris,group='Species',val=c('Sepal.Length','Sepal.Width'),fun='function(x)median(x,na.rm=T)')
#' fn_aggre(iris,group='Species',val=c('Sepal.Length','Sepal.Width'),
#' fun='mean',subsets=list(val=c('Species','Sepal.Length'),legal=list(c('setosa','virginica'),
#' c('[0,10]')),method=c('elements','ranges'),mode=c('character','numeric')))
#'
#'
#' @export


fn_aggre<-function(data=NA,group=NA,val=NA,fun='mean',subsets=NA){
  if(all(is.na(subsets))) {
    data } else {
      legal_data(data,val=subsets$val,legal=subsets$legal,method=subsets$method,mode=subsets$mode)->ind
      data<-data[ind,]
    }
  
  if(fun=='length') fun=function(x)sum(!is.na(x))
  df.val<-data[,val,drop=F]
  if(length(group)==1) df.by<-list(data[,group]) else df.by<-as.list(data[,group])
  
  aggregate(x=df.val,by=df.by,FUN=eval(parse(text=fun)))
}


#' myApply
#' a little change to apply. avoid error for apply function to a data.frame that only contain one var.
#'
#' @author sontron
#'
#' @export

myApply<-function(data,vars,MARGIN,FUN,na.rm=T,...){
  stopifnot(class(data)%in%c('data.frame','matrix')&&all(is.element(vars,names(data))))
  #if(length(vars)==1) {
  #  apply(as.matrix(data[,vars]),MARGIN,FUN,...)->resmyApply
  #} else {
  #  apply(data[,vars],MARGIN,FUN,...)->resmyApply
  #}
  # apply(as.matrix(data[,vars],ncol=length(vars)),MARGIN,FUN,...)->resmyApply
  apply(data[,vars,drop=F],MARGIN,FUN,...)->resmyApply
  return(resmyApply)
}


#' fn_discrete
#'
#' fn_discrete is a function simlar with legal_set, but return a regrouped value from input x.
#'
#' @author sontron
#' @param x is a vector which could be numeric, datetime or character
#' @param groups is simlar with L in legal_set, but different in that for elements and substrs it should be a list
#' @param Labels defines the labels for the returned value
#' @param method could be ranges, substrs and elements
#' @param mode could be numeric, character or datetime
#' @return a regrouped vector
#'
#' @examples
#' x=rnorm(100)
#' fn_discrete(x,groups=c('(-Inf,0]','(0,Inf)'),Labels=c('negative_value','positive_value'),method='ranges',mode='numeric')
#' x=sample(letters[1:5],100,rep=T)
#' fn_discrete(x,groups=list(c('a','b'),c('c','d','e')),Labels=c('a-b','c-e'),method='elements',mode='character')
#' x=c('google','goodbye','doodle','doodoge','go!','bad')
#' fn_discrete(x,groups=list(c('goo'),c('doo')),Labels=c('include_goo','include_doo'),method='substrs',mode='character')
#'
#'@export

fn_discrete<-function(x,groups,Labels=NULL,na.val='others',method=c('ranges','substrs','elements')[1],mode=c('numeric','character','datetime')[1]){
  stopifnot(method%in%c('elements','substrs','ranges')|mode%in%c('numeric','character','datetime'))
  require('stringi')
  
  ## numeric and datetime value within a given range
  if(method=='ranges'){
    len<-length(groups)
    sapply(1:len,function(i){
      if(grepl(',',groups[[i]])){
        unlist(strsplit(groups[[i]],",",fixed=T))->l
        if(mode=='numeric'){
          
          if(grepl("(",l[1],fixed=T)) {as.numeric(gsub("(^[[:punct:]]|[[:punct:]]$)","",l[1]))->range_l;x>range_l->ind.l}
          if(grepl(")",l[2],fixed=T)) {as.numeric(gsub("(^[[:punct:]]|[[:punct:]]$)","",l[2]))->range_r;x<range_r->ind.r}
          if(grepl("[",l[1],fixed=T)) {as.numeric(gsub("(^[[:punct:]]|[[:punct:]]$)","",l[1]))->range_l;x>=range_l->ind.l}
          if(grepl("]",l[2],fixed=T)) {as.numeric(gsub("(^[[:punct:]]|[[:punct:]]$)","",l[2]))->range_r;x<=range_r->ind.r}
        }
        
        if(mode=='datetime'){
          as.POSIXct(x)->x
          if(grepl("(",l[1],fixed=T)) {as.numeric(as.POSIXct(gsub("(^[[:punct:]]|[[:punct:]]$)","",l[1])))->range_l;x>range_l->ind.l}
          if(grepl(")",l[2],fixed=T)) {as.numeric(as.POSIXct(gsub("(^[[:punct:]]|[[:punct:]]$)","",l[2])))->range_r;x<range_r->ind.r}
          if(grepl("[",l[1],fixed=T)) {as.numeric(as.POSIXct(gsub("(^[[:punct:]]|[[:punct:]]$)","",l[1])))->range_l;x>=range_l->ind.l}
          if(grepl("]",l[2],fixed=T)) {as.numeric(as.POSIXct(gsub("(^[[:punct:]]|[[:punct:]]$)","",l[2])))->range_r;x<=range_r->ind.r}
        }
        ifelse(ind.l&ind.r,T,F)->res
      } else {
        
        if(mode=='numeric'){
          gsub("(^[[:punct:]]|[[:punct:]]$)","",L[i])->l
          as.numeric(l)->range_p
        }
        
        if(mode=='datetime'){
          gsub("(^[[:punct:]]|[[:punct:]]$)","",L[i])->l
          as.POSIXct(x)->x
          as.numeric(as.POSIXct(l))->range_p
        }
        
        ifelse(x==range_p,T,F)->res
      }
      return(res)
    })->res
    #apply(matrix(res,nc=len),1,any)->res
  }
  
  # discrete numeric values or characters within a given set.
  if(method=='elements'){
    length(groups)->len_ele
    sapply(1:len_ele,function(i){
      ifelse(is.element(x,groups[[i]]),T,F)->res
      return(res)
    })->res
  }
  
  # subset of character strings within a given set.
  if(method=='substrs'){
    length(groups)->len_substr
    sapply(1:len_substr,function(i){
      sapply(groups[[i]],function(j){
        stri_detect_fixed(x,j)->res
        return(res)
      })->res
      apply(as.matrix(res),1,any)->res
      return(res)
    })->res
  }
  
  apply(matrix(res,nc=length(groups)),1,function(x){
    which(x)->res2
    if(length(res2)!=0) res2 else res2<-NA
    return(res2)
  })->result
  
  if(is.null(Labels)) Labels<-paste('V',1:length(groups),sep='_')
  result.final<-Labels[result]
  result.final[is.na(result.final)]<-na.val
  return(result.final)
}


#' myDesc
#'
#' a function provide description statistics.
#'
#' @export

myDesc<-function(data,xvars,varType=c('numeric','character','factor','integer','ordered')[1],Digits=4,nameX='x',seed=123,tabSort=TRUE){
  if(is.character(data)) data=eval(as.name(data))
  as.data.frame(data[,xvars])->dt
  nameX=xvars
  varType=class(data[,xvars])[1]
  x=data[,xvars]
  names(dt)<-nameX
  if(varType%in%c('numeric','integer')){
    summary(x)->resNum
    if(length(resNum)==6) {
      names(resNum)<-c('Min','0.25Qu.','Median','Mean','0.75Qu.','Max')
      resNum[7]<-0
      names(resNum)[7]<-'NAs'
    }
    if(length(resNum)==7) {
      names(resNum)<-c('Min','0.25Qu.','Median','Mean','0.75Qu.','Max','NAs')
    }
    sdNum<-sd(x,na.rm=T)
    round(resNum,Digits)->resNumRd
    round(sdNum,Digits)->sdNumRd
    desNormalNum<-paste(resNumRd[4],'±',sdNumRd,sep='')
    names(desNormalNum)<-'NormalDist.'
    desNonnormalNum<-paste(resNumRd[3],'[',resNumRd[2],',',resNumRd[5],']',sep='')
    names(desNonnormalNum)<-'NonnormalDist.'
    as.data.frame(c(resNumRd,desNormalNum,desNonnormalNum))->resTab
    # if(length(x)<5000) {
    #   shapiro.test(x)->resShap
    # } else {
    #   set.seed(seed)
    #   shapiro.test(x[sample(1:length(x),5000,rep=F)])->resShap
    # }
    resShap<-ksnormTest(x)
    resTab<-as.data.frame(resTab)
    names(resTab)<-'DescRes'
    #resDesc<-list(resTabDesc=resTab,resShapiroTest=resShap)
    resDesc<-list(resTabDesc=resTab)
    graphDesc<-ggplot(dt,aes(x))+geom_histogram(color='white',fill='steelblue')+labs(x=nameX)+theme_bw()
  }
  
  if(varType%in%c('character','factor','ordered')){
    table(x,useNA = 'ifany')->tabChar
    names(tabChar)[which(is.na(names(tabChar)))]<-'NAs'
    resTab<-cbind(tabChar,round(tabChar/sum(tabChar,na.rm=T),Digits))
    as.data.frame(resTab)->resTab
    names(resTab)<-c('Freq','Perc')
    resDesc<-list(resTabDesc=resTab)
    graphDesc<-ggplot(dt,aes(x,fill=x))+geom_bar(width=0.35,color='white')+labs(x=nameX)+theme_bw()
  }
  
  resDescLst<-list(resDesc=resDesc,graphDesc=graphDesc)
  return(resDescLst)
  
}



#' legal_set
#'
#' This function return logic value that indicate which x values are in legal set that is set by L
#'
#' @author sontron
#' @param x is a vector which could be numeric, character or datetime type
#' @param L is a expression indicate legal sets for x
#' @param method specified the method should be used
#' @param mode is the mode of x
#'
#' @return logic value which could be T,F,NA
#'
#' @examples
#' x=rnorm(10)
#' L="[0,Inf]"
#' legal_set(x,L,method='ranges',mode='numeric')
#' x.char=c('a','b','cd')
#' L.char=c('a','c')
#' legal_set(x=x.char,L=L.char,method='elements',mode='character')
#' legal_set(x=x.char,L=L.char,method='substrs',mode='character')
#' Date=c('2016-1-1 14:24:00',NA,'2016-6-1','2016-12-31 24:00:00')
#' L.date='[2016-1-1,2016-12-31)'
#' legal_set(x=Date,L=L.date,method='ranges',mode='datetime')
#'
#' @note the expression of L is flexible,like "(0,3)","[7,11]","[0,Inf)",c('[-1,0]','(1,10)'),
#' "[2013-1-2,2-13-10-11]",c('apple','orange')
#'
#' @export
legal_set<-function(x,L,method=c('ranges','substrs','elements')[1],mode=c('numeric','character','datetime')[1],type=c('fixed','regex')[1]){
  
  stopifnot(method%in%c('elements','substrs','ranges')|mode%in%c('numeric','character','datetime'))
  require('stringi')
  
  ## numeric and datetime value within a given range
  if(method=='ranges'){
    len<-length(L)
    sapply(1:len,function(i){
      if(grepl(',',L[i])){
        unlist(strsplit(L[i],",",fixed=T))->l
        if(mode=='numeric'){
          
          if(grepl("(",l[1],fixed=T)) {as.numeric(gsub("(^[[:punct:]]|[[:punct:]]$)","",l[1]))->range_l;x>range_l->ind.l}
          if(grepl(")",l[2],fixed=T)) {as.numeric(gsub("(^[[:punct:]]|[[:punct:]]$)","",l[2]))->range_r;x<range_r->ind.r}
          if(grepl("[",l[1],fixed=T)) {as.numeric(gsub("(^[[:punct:]]|[[:punct:]]$)","",l[1]))->range_l;x>=range_l->ind.l}
          if(grepl("]",l[2],fixed=T)) {as.numeric(gsub("(^[[:punct:]]|[[:punct:]]$)","",l[2]))->range_r;x<=range_r->ind.r}
        }
        
        if(mode=='datetime'){
          as.POSIXct(x)->x
          if(grepl("(",l[1],fixed=T)) {as.numeric(as.POSIXct(gsub("(^[[:punct:]]|[[:punct:]]$)","",l[1])))->range_l;x>range_l->ind.l}
          if(grepl(")",l[2],fixed=T)) {as.numeric(as.POSIXct(gsub("(^[[:punct:]]|[[:punct:]]$)","",l[2])))->range_r;x<range_r->ind.r}
          if(grepl("[",l[1],fixed=T)) {as.numeric(as.POSIXct(gsub("(^[[:punct:]]|[[:punct:]]$)","",l[1])))->range_l;x>=range_l->ind.l}
          if(grepl("]",l[2],fixed=T)) {as.numeric(as.POSIXct(gsub("(^[[:punct:]]|[[:punct:]]$)","",l[2])))->range_r;x<=range_r->ind.r}
        }
        ifelse(ind.l&ind.r,T,F)->res
      } else {
        
        if(mode=='numeric'){
          gsub("(^[[:punct:]]|[[:punct:]]$)","",L[i])->l
          as.numeric(l)->range_p
        }
        
        if(mode=='datetime'){
          gsub("(^[[:punct:]]|[[:punct:]]$)","",L[i])->l
          as.POSIXct(x)->x
          as.numeric(as.POSIXct(l))->range_p
        }
        
        ifelse(x==range_p,T,F)->res
      }
      return(res)
    })->res
    apply(matrix(res,nc=len),1,any)->res
  }
  
  # discrete numeric values or characters within a given set.
  if(method=='elements'){
    ifelse(is.element(x,L),T,F)->res
  }
  
  # subset of character strings within a given set.
  if(method=='substrs'){
    #paste(L,collapse=';')->pattern
    len<-length(L)
    if(type=='fixed'){
      sapply(L,function(i){
        stri_detect_fixed(x,i)->res
        return(res)
      })->res
    }
    
    if(type=='regex'){
      sapply(L,function(i){
        stri_detect_regex(x,i)->res
        return(res)
      })->res
    }
    apply(matrix(res,nc=len),1,any)->res
  }
  return(res)
}


#' legal_data
#'
#' can be used for data.frame object
#'
#' @author sontron
#' @param data is a data.frame object
#' @param val is a character vector to specified the variables in data. It's same as x in legal_set.
#' @param legal is a list to specified legal sets for each val. It's same as L in legal_set
#' @param method is a character vector specified methods for each variable in val param.
#' @param mode is a character vector specified modes for each variable in val.
#'
#' @return  logical value such as T,F,NA.
#'
#' @examples
#' legal_data(data=iris,val=c('Species','Sepal.Length'),
#' legal=list(c('setosa','versicolor'),c('[3,6]')),method=c('elements','ranges'),
#' mode=c('character','numeric'))
#'
#' @export
legal_data<-function(data,val,legal,method=c('ranges','substrs','elements')[1],mode=c('numeric','character','datetime')[1],type=c('fixed','regex')[1]){
  
  stopifnot(method%in%c('elements','substrs','ranges')|mode%in%c('numeric','character','datetime'))
  require('stringi')
  
  sapply(1:length(val),function(i){
    x=data[,val[i]]
    L=legal[[i]]
    method[i]->methodi
    mode[i]->modei
    ## numeric and datetime value within a given range
    if(methodi=='ranges'){
      len<-length(L)
      sapply(1:len,function(i){
        if(grepl(',',L)){
          unlist(strsplit(L,",",fixed=T))->l
          if(modei=='numeric'){
            
            if(grepl("(",l[1],fixed=T)) {as.numeric(gsub("(^[[:punct:]]|[[:punct:]]$)","",l[1]))->range_l;x>range_l->ind.l}
            if(grepl(")",l[2],fixed=T)) {as.numeric(gsub("(^[[:punct:]]|[[:punct:]]$)","",l[2]))->range_r;x<range_r->ind.r}
            if(grepl("[",l[1],fixed=T)) {as.numeric(gsub("(^[[:punct:]]|[[:punct:]]$)","",l[1]))->range_l;x>=range_l->ind.l}
            if(grepl("]",l[2],fixed=T)) {as.numeric(gsub("(^[[:punct:]]|[[:punct:]]$)","",l[2]))->range_r;x<=range_r->ind.r}
          }
          
          if(modei=='datetime'){
            as.POSIXct(x)->x
            if(grepl("(",l[1],fixed=T)) {as.numeric(as.POSIXct(gsub("(^[[:punct:]]|[[:punct:]]$)","",l[1])))->range_l;x>range_l->ind.l}
            if(grepl(")",l[2],fixed=T)) {as.numeric(as.POSIXct(gsub("(^[[:punct:]]|[[:punct:]]$)","",l[2])))->range_r;x<range_r->ind.r}
            if(grepl("[",l[1],fixed=T)) {as.numeric(as.POSIXct(gsub("(^[[:punct:]]|[[:punct:]]$)","",l[1])))->range_l;x>=range_l->ind.l}
            if(grepl("]",l[2],fixed=T)) {as.numeric(as.POSIXct(gsub("(^[[:punct:]]|[[:punct:]]$)","",l[2])))->range_r;x<=range_r->ind.r}
          }
          ifelse(ind.l&ind.r,T,F)->res
        } else {
          
          if(modei=='numeric'){
            gsub("(^[[:punct:]]|[[:punct:]]$)","",L[i])->l
            as.numeric(l)->range_p
          }
          
          if(modei=='datetime'){
            gsub("(^[[:punct:]]|[[:punct:]]$)","",L[i])->l
            as.POSIXct(x)->x
            as.numeric(as.POSIXct(l))->range_p
          }
          
          ifelse(x==range_p,T,F)->res
        }
        return(res)
      })->res
      apply(matrix(res,nc=len),1,any)->res
    }
    
    # discrete numeric values or characters within a given set.
    if(methodi=='elements'){
      ifelse(is.element(x,L),T,F)->res
    }
    
    # subset of character strings within a given set.
    if(methodi=='substrs'){
      #paste(L,collapse=';')->pattern
      len<-length(L)
      if(type=='fixed'){
        sapply(L,function(i){
          stri_detect_fixed(x,i)->res
          return(res)
        })->res
      }
      
      if(type=='regex'){
        sapply(L,function(i){
          stri_detect_regex(x,i)->res
          return(res)
        })->res
      }
      apply(matrix(res,nc=len),1,any)->res
    }
    return(res)})->res
  apply(res,1,all)->result
  return(result)
}



#' varClass
#'
#' is a function that determine the variable classes automatically.
#'
#'
#' @export

varClass<-function(data,lenTab=10,thresh=0.8){
  t(sapply(data,function(x){
    if(!is.element(class(x)[1],c('character','factor','ordered'))){
      if(length(table(x))<=lenTab) {mode='char';descriptive='x'} else {mode='num';descriptive='x'}
    } else {
      if(sum(!is.na(as.numeric(as.vector(x))))/sum(!is.na(x))>=thresh){
        if(length(table(x))>lenTab) {mode='num';descriptive='x'} else{mode='char';descriptive='x'}
      } else {
        mode='char'
        if(length(table(x))>lenTab){descriptive=''} else{descriptive='x'}
      }
      
    }
    return(mode)
  }))->res
  
  return(res)
}



#' myDescVec
#'
#' a function provide description statistics vor vectors
#'
#' @export


myDescVec<-function(x,varType=c('numeric','character','factor','integer','ordered')[1],Digits=4,nameX='x',seed=123,tabSort=TRUE){
  if(is.character(data)) data=eval(as.name(data))
  as.data.frame(data[,xvars])->dt
  nameX=xvars
  varType=class(data[,xvars])[1]
  x=data[,xvars]
  names(dt)<-nameX
  if(varType%in%c('numeric','integer')){
    summary(x)->resNum
    if(length(resNum)==6) {
      names(resNum)<-c('Min','0.25Qu.','Median','Mean','0.75Qu.','Max')
      resNum[7]<-0
      names(resNum)[7]<-'NAs'
    }
    if(length(resNum)==7) {
      names(resNum)<-c('Min','0.25Qu.','Median','Mean','0.75Qu.','Max','NAs')
    }
    sdNum<-sd(x,na.rm=T)
    round(resNum,Digits)->resNumRd
    round(sdNum,Digits)->sdNumRd
    desNormalNum<-paste(resNumRd[4],'±',sdNumRd,sep='')
    names(desNormalNum)<-'NormalDist.'
    desNonnormalNum<-paste(resNumRd[3],'[',resNumRd[2],',',resNumRd[5],']',sep='')
    names(desNonnormalNum)<-'NonnormalDist.'
    as.data.frame(c(resNumRd,desNormalNum,desNonnormalNum))->resTab
    # if(length(x)<5000) {
    #   shapiro.test(x)->resShap
    # } else {
    #   set.seed(seed)
    #   shapiro.test(x[sample(1:length(x),5000,rep=F)])->resShap
    # }
    resShap<-ksnormTest(x)
    resTab<-as.data.frame(resTab)
    names(resTab)<-'DescRes'
    #resDesc<-list(resTabDesc=resTab,resShapiroTest=resShap)
    resDesc<-list(resTabDesc=resTab,resShapiroTest=resShap)
    graphDesc<-ggplot(dt,aes(x))+geom_histogram(color='white',fill='steelblue')+labs(x=nameX)
  }
  
  if(varType%in%c('character','factor','ordered')){
    table(x,useNA = 'ifany')->tabChar
    names(tabChar)[which(is.na(names(tabChar)))]<-'NAs'
    resTab<-cbind(tabChar,round(tabChar/sum(tabChar,na.rm=T),Digits))
    as.data.frame(resTab)->resTab
    names(resTab)<-c('Freq','Perc')
    resDesc<-list(resTabDesc=resTab)
    graphDesc<-ggplot(dt,aes(x))+geom_bar(width=0.35,color='white')+labs(x=nameX)
  }
  
  resDescLst<-list(resDesc=resDesc,graphDesc=graphDesc)
  return(resDescLst)
  
}



#' myhTest
#'
#' is a function of basic hTest collection based on data.frame
#'
#' @export

myhTest<-function(data,xvars,yvars='',alter=c('two.sided','less','greater')[1],paired=FALSE,confLevel=0.95,nullHyp=0,normalSampleSize=100) {
  require(vcdExtra)
  require(fBasics)
  if(is.character(data)) data=eval(as.name(data))
  nrow(data)->obsNo
  if(yvars==''){
    dt<-data.frame(x=data[,xvars[1]],stringsAsFactors = F)
    #names(dt)[1]<-'x'
    nameX<-xvars[1]
    if(class(dt$x)[1]%in%c('character','ordered')){
      table(dt$x)->Tab
      sum(Tab,na.rm=T)->sumTab
      myDesc(data=dt,xvars='x',varType='character')$resTabDesc->DescResult
      hTestRes<-list(DescResult=DescResult,hTestResult=prop.test(as.numeric(Tab),rep(sumTab,length(Tab)),alternative=alter,conf.level=confLevel))
      hTestGraph<-ggplot(dt,aes(x))+geom_bar(width=0.35,color='white')+labs(x=nameX)+theme_bw()
    } else {
      #pvalShapiro<-ifelse(obsNo>5000,shapiro.test(dt$x[sample(1:obsNo,5000)])$p.value,shapiro.test(dt$x)$p.value) ## 采用ksnormTest 见下行
      pvalShapiro<-ksnormTest(dt$x)@test$p.value[1]
      myDesc(data=dt,xvars='x',varType='numeric')$resTabDesc->DescResult
      if(pvalShapiro>0.05|obsNo>normalSampleSize){
        hTestRes<-list(DescResult=DescResult,hTestResult=t.test(dt$x,alternative=alter,mu=nullHyp,conf.level=confLevel))
      } else {
        
        hTestRes<-list(DescResult=DescResult,hTestResult=wilcox.test(dt$x,alternative=alter,mu=nullHyp,conf.level=confLevel))
      }
      hTestGraph<-ggplot(dt,aes(x))+geom_histogram(color='white',fill='steelblue')+labs(x=nameX)+theme_bw()
      
    }
    
    
  } else {
    data[,c(xvars[1],yvars[1])]->dt
    names(dt)<-c('x','y')
    xvars[1]->nameX
    yvars[1]->nameY
    
    if(class(dt$x)[1]=='numeric'&class(dt$y)[1]=='numeric'){
      dt$z<-dt$y-dt$x
      nameDiff=paste(nameY,nameX,sep='-')
      #pvalShapiroX<-ifelse(obsNo>5000,shapiro.test(dt$x[sample(1:obsNo,5000)])$p.value,shapiro.test(dt$x)$p.value)
      #pvalShapiroY<-ifelse(obsNo>5000,shapiro.test(dt$y[sample(1:obsNo,5000)])$p.value,shapiro.test(dt$y)$p.value)
      pvalShapiroX<-ksnormTest(dt$x)@test$p.value[1]
      pvalShapiroY<-ksnormTest(dt$y)@test$p.value[1]
      #pvalShapiroDiff<-ifelse(obsNo>5000,shapiro.test((dt$y-dt$x)[sample(1:obsNo,5000)])$p.value,shapiro.test(dt$y-dt$x)$p.value)
      #pvalShapiroDiff<-ksnormTest(dt$y-dt$x)@test@p.value[1]
      pvalShapiroDiff<-ksnormTest(dt$z)@test$p.value[1]
      if(paired){
        if(pvalShapiroDiff>0.05|obsNo>normalSampleSize){
          hTestRes<-list(hTestResult=t.test(dt$z,mu=nullHyp,alternative = alter,conf.level=confLevel))
        } else {
          hTestRes<-list(hTestResult=wilcox.test(dt$z,mu=nullHyp,alternative = alter,conf.level=confLevel))
        }
        hTestGraph<-ggplot(dt,aes(z))+geom_histogram(color='white',fill='steelblue')+labs(x=nameDiff)+theme_bw()
        
      } else {
        if((pvalShapiroX>0.05&pvalShapiroY>0.05)|obsNo>100){
          hTestRes<-list(hTestResult=cor.test(dt$x,dt$y,alternative = alter,conf.level = confLevel,method='pearson'))
        } else {
          hTestRes<-list(hTestResult=cor.test(dt$x,dt$y,alternative = alter,conf.level = confLevel,method='spearman'))
        }
        hTestGraph<-ggplot(dt,aes(x,y))+geom_point()+geom_smooth(method='lm')+labs(x=nameX,y=nameY)+theme_bw()
      }
    }
    
    if(class(dt$x)[1]%in%c('character','ordered')&class(dt$y)[1]%in%c('character','ordered')){
      table(dt$x,dt$y)->tab
      if(paired){
        hTestRes<-list(DescResult=tab,hTestResult=mcnemar.test(tab))
      } else {
        chisq.test(tab)->chisqTest
        as.vector(chisqTest$expected)->chisqExp
        if(any(chisqExp<1)||(sum(chisqExp<5)/length(chisqExp))>0.2) {
          fisher.test(tab)->fisherTest
          if(any(c(class(dt$x)[1],class(dt$y)[1])=='ordered')){
            hTestRes<-list(DescResult=tab,chisqTest=chisq.test(tab),fisherTest=fisherTest,CMHtest=CMHtest(tab))
          } else {
            hTestRes<-list(DescResult=tab,chisqTest=chisq.test(tab),fisherTest=fisherTest)
          }
          
        } else {
          if(any(c(class(dt$x)[1],class(dt$y)[1])=='ordered')){
            hTestRes<-list(DescResult=tab,chisqTest=chisq.test(tab),CMHtest=CMHtest(tab))
          } else {
            hTestRes<-list(DescResult=tab,chisqTest=chisq.test(tab))
          }
          #hTestRes<-list(DescResult=tab,chisqTest=chisq.test(tab),CMHtest=CMHtest(tab))
        }
      }
      hTestGraph<-ggplot(dt,aes(x,fill=y))+geom_bar(width=0.35,color='white',position='dodge')+labs(x=nameX)+scale_fill_discrete(nameY)+theme_bw()
      
    }
    
    if(all(c('numeric','character')%in%c(class(dt$x)[1],class(dt$y)[1]))){
      which(c(class(dt$x)[1],class(dt$y)[1])=='numeric')->indNum
      which(c(class(dt$x)[1],class(dt$y)[1])=='character')->indChar
      dt[,c(indNum,indChar)]->dt
      names(dt)<-c('x','grp')
      tapply(dt$x,dt$grp,function(i)myDesc(data=dt,xvars='x',varType='numeric')$resDesc$resTabDesc)->X
      sapply(as.vector(na.omit(unique(dt$grp))),function(i)myDesc(data=dt[dt$grp==i,],xvars='x',varType='numeric')$resDesc$resTabDesc)->X
      matrix(nr=9,nc=length(X))->matDesc
      for(i in 1:ncol(matDesc)){
        matDesc[,i]<-as.character(X[[i]])
      }
      colnames(matDesc)<-names(X)
      row.names(matDesc)<-c('Min','0.25Qu.','Median','Mean','0.75Qu.','Max','NAs','NormalDist.','NonnormalDist.')
      as.data.frame(matDesc)->matDesc
      c(nameX,nameY)[c(indNum,indChar)]->namesdt
      #pvalShapiroX<-ifelse(obsNo>5000,shapiro.test(dt$x[sample(1:obsNo,5000)])$p.value,shapiro.test(dt$x)$p.value)
      pvalShapiroX<-ksnormTest(dt$x)@test$p.value[1]
      if(pvalShapiroX>0.05|obsNo>normalSampleSize){
        if(length(unique(dt$grp))==2){
          hTestRes<-list(DescResult=matDesc,hTestResult=t.test(dt$x~dt$grp,alternative=alter,mu=nullHyp,conf.level=confLevel))
        } else {
          hTestRes<-list(DescResult=matDesc,wholeTest=summary(aov(dt$x~dt$grp,alternative=alter,conf.level=confLevel)),pairedHTest=pairwise.t.test(dt$x,dt$grp,p.adj='bonf',alternative = alter))
        }
      } else {
        if(length(unique(dt$grp))==2){
          hTestRes<-list(DescResult=matDesc,hTestResult=wilcox.test(dt$x~dt$grp,alternative=alter,mu=nullHyp,conf.level=confLevel))
        } else {
          hTestRes<-list(DescResult=matDesc,wholeTest=kruskal.test(dt$x,as.factor(dt$grp),alternative=alter,conf.level=confLevel),pairedHTest=pairwise.wilcox.test(dt$x,dt$grp,p.adj='bonf',alternative = alter))
        }
        
      }
      hTestGraph<-ggplot(dt,aes(grp,x,fill=grp))+geom_boxplot(width=0.35)+labs(x=namesdt[2],y=namesdt[1])+theme_bw()
    }
    
    if(all(c('numeric','ordered')%in%c(class(dt$x)[1],class(dt$y)[1]))){
      which(c(class(dt$x)[1],class(dt$y)[1])=='numeric')->indNum
      which(c(class(dt$x)[1],class(dt$y)[1])=='ordered')->indOrd
      dt[,c(indNum,indOrd)]->dt
      names(dt)<-c('x','grp')
      as.numeric(dt$grp)->dt$grp
      c(nameX,nameY)[c(indNum,indOrd)]->namesdt
      #pvalShapiroX<-ifelse(obsNo>5000,shapiro.test(dt$x[sample(1:obsNo,5000)])$p.value,shapiro.test(dt$x)$p.value)
      pvalShapiroX<-ksnormTest(dt$x)@test$p.value[1]
      hTestRes<-list(hTestResult=cor.test(dt$x,dt$grp,method='spearman',conf.level=confLevel,alternative=alter))
      hTestGraph<-ggplot(dt,aes(x,grp))+geom_point()+geom_smooth(method='lm')+labs(x=namesdt[1],y=namesdt[2])+theme_bw()
    }
    
  }
  
  return(list(hTestRes=hTestRes,hTestGraph=hTestGraph))
  
}



#' myGlm
#' 
#' is a function of glm models
#' 
#' @export

myGlm<-function(Formula,
                data,
                weightsVar=1,
                subset='all',
                Family=c('gaussian','binomial','poisson'),
                na.action=na.rm,
                lower='~1'
){
  require(fBasics)
  if(is.character(data)) data=eval(as.name(data))
  if(subset=='all'){
    data<-data
  } else {
    subset(data,eval(parse(text=subset)))->data
  }
  yVar=stri_split_fixed(Formula,'~')[[1]][1]
  Form1=as.formula(paste('~',stri_split_fixed(Formula,'~')[[1]][2]))
  Form2=as.formula(lower)
  unique(unlist(stri_split_regex(Formula,'[+*:~, -\\(\\)\\^]')))->varsAll
  intersect(names(data),varsAll)->varsAll
  if(is.character(Formula)) Formula=as.formula(Formula)
  
  if(weightsVar==1){
    data[,varsAll]->dat
    na.omit(dat)->dat
    Wt=rep(1,nrow(dat))
  } else {
    data[,c(varsAll,weightsVar)]->dat
    na.omit(dat)->dat
    Wt=dat[,weightsVar]
  }
  
  if(Family=='binomial'){
    as.numeric(as.factor(dat[,yVar]))-1->dat[,yVar]
  }
  
  glm(Formula,data=dat,family=Family,x=T,y=T,weights=Wt)->fit
  
  step(fit,scope=list(upper=Form1,lower=Form2),trace=F)->fitStep
  
  
  #graphGlmFull=autoplot(fit)
  #graphGlmStep=autoplot(fitStep)
  
  return(list(glmResFull=fit,glmResStep=fitStep))
  
  
}




#' myTree
#' 
#' is a function of Tree models
#' 
#' @export

myTree<-function(Formula,
                data,
                # weightsVar,
                subset='all',
                na.action=na.rm,
                Minsplit=30,
                Minbucket=10,
                Mincrit=0.05,
                Maxdepth=3,
                CP=0.01,
                treeMethod=c('ctree','rpart')[1]
){
  
  if(is.character(data)) data=eval(as.name(data))
  if(subset=='all'){
    data<-data
  } else {
    subset(data,eval(parse(text=subset)))->data
  }
  
  for(i in 1:ncol(data)){
    if(class(data[,i])=='character'){
      as.factor(data[,i])->data[,i]
    }
  }
  
  #unique(unlist(stri_split_regex(Formula,'[+*:~(), ]')))->varsAll
  unique(unlist(stri_split_regex(Formula,'[+*:~, -\\(\\)\\^]')))->varsAll
  intersect(names(data),varsAll)->varsAll2
  #setdiff(varsAll,c('Surv',''))->varsAll2
  data[,varsAll2]->dt
  na.omit(dt)->dt
  
  if(treeMethod=='ctree'){
    fitTree<-party:::ctree(as.formula(Formula),data=dt,controls=party:::ctree_control(maxdepth = Maxdepth,minsplit=Minsplit,minbucket = Minbucket,mincriterion = Mincrit))
  }
  
  if(treeMethod=='rpart'){
    fitTree<-as.party(rpart(as.formula(Formula),data=dt,control=rpart.control(maxdepth = Maxdepth,minsplit=Minsplit,minbucket = Minbucket,cp = CP)))
  }
  
  return(fitTree)
  
  
}


#' myCox
#' 
#' is a function of Coxph models
#' 
#' @export

myCox<-function(Formula,
                data,
                weightsVar=1,
                subset='all',
                #Family=c('gaussian','binomial','poisson'),
                strataVar='1',
                #na.action=na.rm,
                lower='~1'
){
  if(is.character(data)) data=eval(as.name(data))
  if(subset=='all'){
    data<-data
  } else {
    subset(data,eval(parse(text=subset)))->data
  }
  stri_split_fixed(Formula,'~')[[1]][1]->lht
  #yVar=stri_split_fixed(Formula,'~')[[1]][1]
  Form1=as.formula(paste('~',stri_split_fixed(Formula,'~')[[1]][2]))
  Form2=as.formula(lower)
  #unique(unlist(stri_split_regex(Formula,'[+*:~(), ]')))->varsAll
  unique(unlist(stri_split_regex(Formula,'[+*:~, -\\(\\)\\^]')))->varsAll
  #setdiff(varsAll,c('Surv',''))->varsAll2
  intersect(names(data),varsAll)->varsAll2
  if(strataVar!='1'){
    union(varsAll2,strataVar)->varsAll2
  }
  if(is.character(Formula)) Formula=as.formula(Formula)
  
  if(weightsVar==1){
    data[,varsAll2]->dat
    na.omit(dat)->dat
    Wt=rep(1,nrow(dat))
  } else {
    data[,c(varsAll2,weightsVar)]->dat
    na.omit(dat)->dat
    Wt=dat[,weightsVar]
  }
  
  
  
  coxph(Formula,data=dat,weights=Wt)->fit
  
  step(fit,scope=list(upper=Form1,lower=Form2),trace=F)->fitStep
  
  if(strataVar!='1'){
    paste(lht,strataVar,sep='~')->FormulaStrata
    as.formula(FormulaStrata)->FormulaStrata
    survfit(FormulaStrata,data=dat)->survFitStrata
  } else {
    survfit(fitStep)->survFitStrata
  }
  
  
  
  return(list(coxResFull=fit,coxResStep=fitStep,fitStrata=survFitStrata))
  
  
}



#' myTable
#' 
#' is a function of Coxph models
#' 
#' @export

myTable<-function(Formula,data){
  if(is.character(data)) data=eval(as.name(data))
  
  stri_split_fixed(Formula,'~')[[1]][1]->lht
  stri_split_fixed(Formula,'~')[[1]][-1]->rht
  
  unlist(stri_split_fixed(lht,'+'))->lhtVars
  unlist(stri_split_fixed(rht,'+'))->rhtVars
  
  if(length(lhtVars)>1){
    apply(data[,lhtVars],1,function(i)paste(i,collapse='_'))->data[,paste(lhtVars,collapse='_')]
    as.formula(paste(paste(lhtVars,collapse='_'),rht,sep='~'))->Formula
    mytable(Formula,data,method = 1)->res
  } else {
    if(lhtVars==''){
      data$noGroupVar=1
      as.formula(paste('noGroupVar',rht,sep='~'))->Formula
      mytable(Formula,data,method = 1)->res
    } else {
      data[,lhtVars]->data[,paste(lhtVars,collapse='_')]
      as.formula(paste(paste(lhtVars,collapse='_'),rht,sep='~'))->Formula
      mytable(Formula,data,method = 1)->res
    }
    
  }
  
  
  
  
  
  mytable2df(res)->res2
  return(res2)
  
}






#' predictNodes
#' 
#' predict the Node ID of tree object
predictNodes<-function (object, newdata, na.action = na.pass) {
  where <-
    if (missing(newdata))
      object$where
  else {
    if (is.null(attr(newdata, "terms"))) {
      Terms <- delete.response(object$terms)
      newdata <- model.frame(Terms, newdata, na.action = na.action,
                             xlev = attr(object, "xlevels"))
      if (!is.null(cl <- attr(Terms, "dataClasses")))
        .checkMFClasses(cl, newdata, TRUE)
    }
    rpart:::pred.rpart(object, rpart:::rpart.matrix(newdata))
  }
  as.integer(row.names(object$frame))[where]
}


#' myImpute
#'
#' is a imputation function
#'
#' @export

myImpute<-function(data,impVars,modelVars,seed=1,treeParams=list(),method=c('sample','pred')[1]){
  require(rpart)
  require(partykit)
  if(is.character(data)) data=eval(as.name(data))
  if(missing(impVars)) impVars=names(data)
  if(missing(modelVars)) modelVars=names(data)
  #if(missing(treeParams)) treeParams=list(minsplit=20,minbucket=7,maxdepth=4,cp=0.01)
  
  set.seed(seed)
  unique(c(impVars,modelVars))->Vars
  data[,Vars]->dat
  for(i in impVars){
    if(any(is.na(dat[,i]))) {
      paste(i,'~',paste(names(dat)[-which(names(dat)==i)],collapse='+'),sep='')->formula
      rpart(formula,data=dat,control=treeParams)->fit
      #as.party(fit)->fitParty
      if(length(unique(fit$where))==1){
        dat[is.na(dat[,i]),i]<-sample(dat[!is.na(dat[,i]),i],length(dat[is.na(dat[,i]),i]),rep=T)
      } else {
        #fitted(fitParty)[,1]->nodeRaw
        as.numeric(row.names(fit$frame)[fit$where])->nodeRaw
        dat[!is.na(dat[,i]),i]->iRaw
        if(method=='sample'){
          predictNodes(fit,newdata=dat[is.na(dat[,i]),])->nodePred
          iImp<-numeric(length(nodePred))
          for(j in unique(nodePred)){
            which(nodePred==j)->ind
            sample(iRaw[nodeRaw==j],length(ind),rep=T)->iImp[ind]
          }
          dat[is.na(dat[,i]),i]<-iImp
        }
        if(method=='pred'){
          if(class(dat[,i])[1]%in%c('character','factor','ordered')){
            as.vector(predict(fit,dat[is.na(dat[,i]),],type='class'))->responsePred
          } else {
            predict(fit,dat[is.na(dat[,i]),],type='vector')->responsePred
          }
          
          dat[is.na(dat[,i]),i]<-responsePred
        }
      }
    } else {
      dat[,i]->dat[,i]
    }
    
  }
  
  data[,Vars]<-dat
  return(data)
}


#' myGplt
#' 
#' a wrapper function based on ggplot for easy plot
#' @export


myGplt<-function(data,
                 x,
                 y='NULL',
                 size='NULL',
                 fill='NULL',
                 color='NULL',
                 shape='NULL',
                 alpha='NULL',
                 facetVar='NULL',
                 geom=c('box','hist','bar','line','jitter','point','smooth')[1],
                 labx='x',
                 laby='y',
                 title='my Plot',
                 theme=c('grey','bw','classic','dark')[1],
                 smoothMethod=c('lm','glm','loess','gam')[1],
                 barPos=c('stack','dodge')[1],
                 Bins=2,
                 # Colour='NULL',
                 # Fill='NULL',
                 # Size='NULL',
                 # Alpha=.5,
                 Width=.5,
                 ...
                 
                 
){
  
  geom=unlist(stri_split_fixed(geom,';'))
  facetVar=unlist(stri_split_fixed(facetVar,';'))
  if(is.character(data)) data=eval(as.name(data))
  
  
  
  
  myGeom<-function(geom,...){
    switch(geom,
           box=geom_boxplot(...,width=Width),
           hist=geom_histogram(...,aes(y=..density..),bins = Bins),
           bar=geom_bar(...,aes(y=..count..),position=barPos,width=Width),
           line=geom_line(...),
           jitter=geom_jitter(...),
           point=geom_point(...),
           smooth=geom_smooth(...,method=smoothMethod)
           
           )
  }
  
  
  P<-"ggplot(data=data,aes_string(x=x,y=y,size=size,fill=fill,color=color,shape=shape,alpha=alpha))"
  
  geoms<-paste(paste('myGeom(',paste("'",geom,"'",sep=''),')',sep=''),collapse='+')
  #names(sapply(geom,function(i)myGeom(i)))->geoms
  
  paste(P,geoms,sep='+')->graph
  if(!is.null(facetVar)&facetVar!='NULL'){
    paste('facet_wrap(~',paste(facetVar,collapse='+'),')',sep='')->facet
    paste(graph,facet,sep='+')->graph
  }
  
  myTheme<-function(theme){
    switch(theme,
           bw=theme_bw(),
           grey=theme_grey(),
           dark=theme_dark(),
           classic=theme_classic()
    )
  }
  
  themes<-paste("myTheme(",paste("'",theme,"'",sep=''),")",sep='')
  
  myLab<-paste("labs(",paste("x=labx","y=laby","title=title",sep=','),")",sep='')
  
  adjTitle<-'theme(plot.title=element_text(hjust=.5))'
  
  paste(graph,themes,myLab,adjTitle,sep='+')->Graph
  resGGplot<-eval(parse(text=Graph))
  ggplotly(resGGplot)->resPlotly
  return(list(resGGplot=resGGplot,resPlotly=resPlotly))
  
  
}





#' myGplt2
#' 
#' test
#' 
#' @export


myGplt2<-function(data,
                 x,
                 y='NULL',
                 size='NULL',
                 fill='NULL',
                 color='NULL',
                 shape='NULL',
                 alpha='NULL',
                 facetVar='NULL',
                 geom=c('box','hist','bar','line','jitter','point','smooth')[1],
                 labx='x',
                 laby='y',
                 title='my Plot',
                 theme=c('grey','bw','classic','dark')[1],
                 smoothMethod=c('lm','glm','loess','gam')[1],
                 barPos=c('stack','dodge')[1],
                 Bins='NULL',
                 Colour='NULL',
                 Fill='NULL',
                 Size='NULL',
                 Alpha='NULL',
                 Width='NULL',
                 Shape='NULL'
                 
                 
){
  
  
  if(is.character(data)) data=eval(as.name(data))
  geom=unlist(stri_split_fixed(geom,';'))
  facetVar=unlist(stri_split_fixed(facetVar,';'))
  if(is.character(data)) data=eval(as.name(data))
  
  
  
  
  myGeom<-function(Bins,Colour,Size,Fill,Alpha,Width,Shape,geom){
    switch(geom,
           box=paste('geom_boxplot(',paste(ifelse(Fill%in%c('NULL',NA,''),'','fill=Fill,'),
                                           ifelse(Width%in%c('NULL',NA,''),'','width=Width,'),
                                           ifelse(Alpha%in%c('NULL',NA,''),'','alpha=Alpha'),
                                           collapse=''
           ),')',sep=''),
           
           
           hist=paste('geom_histogram(aes(y=..density..),',paste(ifelse(Fill%in%c('NULL',NA,''),'','fill=Fill,'),
                                                                ifelse(Bins%in%c('NULL',NA,''),'','bins=Bins,'),
                                                                ifelse(Alpha%in%c('NULL',NA,''),'','alpha=Alpha,'),
                                                                ifelse(Colour%in%c('NULL',NA,''),'','color=Colour'),
                                                                collapse=''
           ),')',sep=''),
           
           
           bar=paste('geom_bar(aes(y=..count..),',paste(ifelse(Fill%in%c('NULL',NA,''),'','fill=Fill,'),
                                                       ifelse(Width%in%c('NULL',NA,''),'','width=Width,'),
                                                       ifelse(Alpha%in%c('NULL',NA,''),'','alpha=Alpha,'),
                                                       'position=barPos',
                                                       collapse=''
           ),')',sep=''),
           
           
           line=paste('geom_line(',paste(ifelse(Colour%in%c('NULL',NA,''),'','color=Colour'),
                                         collapse=''
           ),')',sep=''),
           
           
           jitter=paste('geom_jitter(',paste(ifelse(Colour%in%c('NULL',NA,''),'','color=Colour,'),
                                             ifelse(Alpha%in%c('NULL',NA,''),'','alpha=Alpha'),
                                             collapse=''
           ),')',sep=''),
           
           
           point=paste('geom_point(',paste(ifelse(Colour%in%c('NULL',NA,''),'','color=Colour,'),
                                           ifelse(Shape%in%c('NULL',NA,''),'','shape=Shape,'),
                                           ifelse(Size%in%c('NULL',NA,''),'','size=Size,'),
                                           ifelse(Alpha%in%c('NULL',NA,''),'','alpha=Alpha'),
                                           collapse=''
           ),')',sep=''),
           
           
           smooth=paste('geom_smooth(method=smoothMethod,',paste(ifelse(Colour%in%c('NULL',NA,''),'','color=Colour,'),
                                                                 collapse=''
           ),')',sep='')
    )->Geom
    stri_replace_all_regex(Geom,'(,\\)|, \\))',')')->Geom
    # return(Geom)
    return(eval(parse(text=Geom)))
    
  }
  
  
  P<-"ggplot(data=data,aes_string(x=x,y=y,size=size,fill=fill,color=color,shape=shape,alpha=alpha))"
  
  geoms<-paste(paste('myGeom(Bins=Bins,Colour=Colour,Fill=Fill,Size=Size,Alpha=Alpha,Width=Width,Shape=Shape,',paste("'",geom,"'",sep=''),')',sep=''),collapse='+')
  
  paste(P,geoms,sep='+')->graph
  if(!is.null(facetVar)&facetVar!='NULL'){
    paste('facet_wrap(~',paste(facetVar,collapse='+'),')',sep='')->facet
    paste(graph,facet,sep='+')->graph
  }
  
  myTheme<-function(theme){
    switch(theme,
           bw=theme_bw(),
           grey=theme_grey(),
           dark=theme_dark(),
           classic=theme_classic()
    )
  }
  
  themes<-paste("myTheme(",paste("'",theme,"'",sep=''),")",sep='')
  
  myLab<-paste("labs(",paste("x=labx","y=laby","title=title",sep=','),")",sep='')
  
  adjTitle<-'theme(plot.title=element_text(hjust=.5))'
  
  paste(graph,themes,myLab,adjTitle,sep='+')->Graph
  resGGplot<-eval(parse(text=Graph))
  ggplotly(resGGplot)->resPlotly
  return(list(resGGplot=resGGplot,resPlotly=resPlotly))
  
  
}



#' myProphet
#' 
#' is a revised version of prophet that combine some other functions from xts package for dealing data.frame, multiple variables and adding group variables. 
#' return the history data and graphs as well as predicted graph using prophet.
#' 
#' 
#' @export

myProphet<-function(data,
                      tsVar,
                      tsFormat='ymd',
                      measureVars,
                      groupVars=1,
                      Period='weeks',
                      FN,
                      Cap=-1,
                      Floor=-1,
                      Growth='linear',
                      # changepointRange=.8,
                      # changepointPriorScale = 0.05,
                      # mcmcSamples = 0,
                      # intervalWidth=.8,
                      H=10,
                      yearlyS='auto',
                      dailyS='auto',
                      weeklyS='auto'
                      
                      ){
  require(xts)
  require(lubridate)
  require(broom)
  
  if(is.character(data)) data=eval(as.name(data))
  
  unlist(stri_split_fixed(measureVars,';'))->measureVars
  unlist(stri_split_fixed(groupVars,';'))->groupVars
  
  which(is.na(data[,tsVar]))->ind
  if(length(ind)>0){data[-ind,]->dat} else {dat=data}
  
  # dat$cap<-Cap
  # dat$floor<-Floor
  
  
  if(groupVars%in%c('1','','NA','NULL')){
    dat[,'tsGroupVar']<-1
  } else {
    myApply(data = dat,vars = groupVars,MARGIN = 1,FUN = function(x)paste(x,collapse='_'))->dat$tsGroupVar
  }
  
  
  
  fn_aggre(dat,group=c(tsVar,'tsGroupVar'),val=measureVars,FN)->resTmp
  
  
  lapply(unique(resTmp[,'tsGroupVar']),function(i){
    subset(resTmp,tsGroupVar==i)->resTmpi
    resTmpi[,-which(names(resTmpi)=='tsGroupVar')]->resTmpii
    parse_date_time(resTmpii[,tsVar],orders=tsFormat)->TS
    xts(resTmpii[,-which(names(resTmpii)==tsVar),drop=F],TS)->resTmpii
    lapply(resTmpii,function(j){
      period.apply(j,endpoints(j,Period),FUN=eval(parse(text=FN)))
    })->resTmpj
    
    do.call(merge,resTmpj)->resTmpj
    as.data.frame(resTmpj)->resTmpj
    resTmpj[,'tsGroupVar']<-i
    return(resTmpj)
    
  })->resTmpNew
  
  do.call(rbind,resTmpNew)->resFinal
  resFinal[,tsVar]<-row.names(resFinal)
  if(groupVars%in%c('1','','NA','NULL')){
    resFinal[,-which(names(resFinal)=='tsGroupVar')]->resFinal
    melt(resFinal,id.vars=tsVar)->dfGraph
  } else {
    names(resFinal)[which(names(resFinal)=='tsGroupVar')]<-paste(groupVars,collapse='_')
    melt(resFinal,id.vars=c(tsVar,paste(groupVars,collapse='_')))->dfGraph
  }
  
  
  
  
  parse_date_time(resFinal[,tsVar],tsFormat)->resFinal[,tsVar]
  
  #dfGraph[,tsVar]<-row.names(dfGraph)
  parse_date_time(dfGraph[,tsVar],tsFormat)->dfGraph[,tsVar]
  
  row.names(resFinal)<-NULL
  row.names(dfGraph)<-NULL
  
  if(groupVars%in%c('1','','NA','NULL')){
    ggplot(dfGraph,aes_string(tsVar,'value',color='variable'))+geom_point()+geom_line()+facet_wrap(~variable,scales='free')->graph
  } else {
    
    #ggplot(dfGraph,aes_string(tsVar,'value',color='variable'))+geom_point()+geom_line()+facet_wrap(as.formula(paste('~',paste('variable',paste(groupVars,collapse='_'),sep='+'),sep='')),scales='free')->graph
    ggplot(dfGraph,aes_string(tsVar,'value',color=paste(groupVars,collapse='_')))+geom_point()+geom_line()+facet_wrap(~variable,scales='free')->graph
    
  }
  
  
  if(groupVars%in%c('1','','NA','NULL')){
    Lst<-list()
    for(i in 1:length(unique(dfGraph$variable))){
      unique(dfGraph$variable)[i]->ii
      subset(dfGraph,variable==ii)->dfGraphi
      which(is.na(dfGraphi[,tsVar]))->ind
      #dfGraphi[-ind,]->dfGraphi
      
      #dfGraphi$floor<-Floor
      names(dfGraphi)[which(names(dfGraphi)==tsVar)]<-'ds'
      names(dfGraphi)[which(names(dfGraphi)=='value')]<-'y'
      ifelse(Cap<0,max(dfGraphi$y,na.rm=T),Cap)->dfGraphi$cap
      ifelse(Floor<0,min(dfGraphi$y,na.rm=T),Floor)->dfGraphi$floor
      prophet(dfGraphi,
              growth=Growth,
              # changepoint.range=changepointRange,
              yearly.seasonality = yearlyS,
              daily.seasonality = dailyS,
              weekly.seasonality = weeklyS)->m
      future<-make_future_dataframe(m,periods=H,freq=stri_replace_last_regex(Period,'[a-z]',''))
      # future$cap<-Cap
      # future$floor<-Floor
      ifelse(Cap<0,max(dfGraphi$y,na.rm=T),Cap)->future$cap
      ifelse(Floor<0,min(dfGraphi$y,na.rm=T),Floor)->future$floor
      predict(m,future)->Pred
      
      prophet:::df_for_plotting(m,Pred)->dfGraphiNew
      
      # names(dfGraphiNew)[1]<-tsVar
      dfGraphiNew[,'variable']<-ii
      as.character(dfGraphiNew$variable)->dfGraphiNew$variable
      Lst[[i]]<-dfGraphiNew
    }
    do.call(rbind,Lst)->dfGraphNew
    
    names(dfGraphNew)[1]<-tsVar
    if(Growth=='logistic'){
      graphProphet<-ggplot(dfGraphNew,aes_string(tsVar,'y'))+
        geom_point(size=.75,na.rm=T)+geom_line(aes(y=yhat),color='#0072B2',na.rm=T)+
        geom_line(aes(y=floor),linetype='dashed',na.rm=T)+
        geom_line(aes(y=cap),linetype='dashed',na.rm=T)+
        geom_ribbon(aes(ymin=yhat_lower,ymax=yhat_upper),alpha=.2,fill='#0072B2',na.rm=T)+
        facet_wrap(~variable,scale='free')
    } else {
      graphProphet<-ggplot(dfGraphNew,aes_string(tsVar,'y'))+
        geom_point(size=.75,na.rm=T)+geom_line(aes(y=yhat),color='#0072B2',na.rm=T)+
        geom_ribbon(aes(ymin=yhat_lower,ymax=yhat_upper),alpha=.2,fill='#0072B2',na.rm=T)+
        facet_wrap(~variable,scale='free')
    }
    
    
  } else {
    unique(dfGraph$variable)->Var
    unique(dfGraph[,paste(groupVars,collapse='_')])->Grp
    Lst<-list()
    for(i in 1:length(Var)){
      for(j in 1:length(Grp)){
        subset(dfGraph,variable==Var[i]&dfGraph[,paste(groupVars,collapse='_')]==Grp[j])->dfGraphi
        which(is.na(dfGraphi[,tsVar]))->ind
        #dfGraphi[-ind,]->dfGraphi
        # dfGraphi$cap<-Cap
        # dfGraphi$floor<-Floor
        
        names(dfGraphi)[which(names(dfGraphi)==tsVar)]<-'ds'
        names(dfGraphi)[which(names(dfGraphi)=='value')]<-'y'
        ifelse(Cap<0,max(dfGraphi$y,na.rm=T),Cap)->dfGraphi$cap
        ifelse(Floor<0,min(dfGraphi$y,na.rm=T),Floor)->dfGraphi$floor
        prophet(dfGraphi,
                growth=Growth,
                # changepoint.range=changepointRange,
                yearly.seasonality = yearlyS,
                daily.seasonality = dailyS,
                weekly.seasonality = weeklyS)->m
        future<-make_future_dataframe(m,periods=H,freq=stri_replace_last_regex(Period,'[a-z]',''))
        # future$cap<-Cap
        # future$floor<-Floor
        
        ifelse(Cap<0,max(dfGraphi$y,na.rm=T),Cap)->future$cap
        ifelse(Floor<0,min(dfGraphi$y,na.rm=T),Floor)->future$floor
        predict(m,future)->Pred
        
        prophet:::df_for_plotting(m,Pred)->dfGraphiNew
        
        dfGraphiNew[,'variable']<-Var[i]
        dfGraphiNew[,paste(groupVars,collapse='_')]<-Grp[j]
        as.character(dfGraphiNew$variable)->dfGraphiNew$variable
        
        Lst[[(i-1)*length(Var)+j]]<-dfGraphiNew
        
      }
    }
    do.call(rbind,Lst)->dfGraphNew
    names(dfGraphNew)[1]<-tsVar
    
    if(Growth=='logistic'){
    graphProphet<-ggplot(dfGraphNew,aes_string(tsVar,'y',color=paste(groupVars,collapse='_')))+
      geom_point(size=.75,na.rm=T)+geom_line(aes(y=yhat),color='#0072B2',na.rm=T)+
      geom_ribbon(aes(ymin=yhat_lower,ymax=yhat_upper),alpha=.2,fill='#0072B2',na.rm=T)+
      geom_line(aes(y=floor),linetype='dashed',na.rm=T)+
      geom_line(aes(y=cap),linetype='dashed',na.rm=T)+
      facet_wrap(as.formula(paste('~',paste('variable',paste(groupVars,collapse='_'),sep='+'))),scale='free')
    } else {
      graphProphet<-ggplot(dfGraphNew,aes_string(tsVar,'y',color=paste(groupVars,collapse='_')))+
        geom_point(size=.75,na.rm=T)+geom_line(aes(y=yhat),color='#0072B2',na.rm=T)+
        geom_ribbon(aes(ymin=yhat_lower,ymax=yhat_upper),alpha=.2,fill='#0072B2',na.rm=T)+
        facet_wrap(as.formula(paste('~',paste('variable',paste(groupVars,collapse='_'),sep='+'))),scale='free')
    }
    
  }
  
  
  
  return(list(tabRes=resFinal,graphRes=graph,graphPredict=graphProphet))
}









#' medstats
#'
#' is a launcher function
#'
#' @export
fastR <- function() {
  library(shiny)
  runApp(system.file("app", package = "fastR"), launch.browser = TRUE)
}





