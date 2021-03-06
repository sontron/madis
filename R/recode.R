#' recode
#'
#' recode is a function recode a variable into recoded groups.
#'
#' @author sontron
#' @param x is a vector which could be numeric, datetime or character
#' @param groups is simlar with L in legal_set, but different in that for elements and substrs it should be a list
#' @param Labels defines the labels for the returned value
#' @param na.val label that are not included in the groups.
#' @param tsFormat tsFormat to define the ts variable.
#' @param method could be ranges, substrs and elements
#' @param mode could be numeric, character or datetime
#' @return a recoded vector
#'
#' @examples
#' x=rnorm(100)
#' recode(x,groups=c('(-Inf,0]','(0,Inf)'),Labels=c('negative_value','positive_value'),method='ranges',mode='numeric')
#' x=sample(letters[1:5],100,rep=T)
#' recode(x,groups=list(c('a','b'),c('c','d','e')),Labels=c('a-b','c-e'),method='elements',mode='character')
#' x=c('google','goodbye','doodle','doodoge','go!','bad')
#' recode(x,groups=list(c('goo'),c('doo')),Labels=c('include_goo','include_doo'),method='substrs',mode='character')
#' 
#' x=apply(expand.grid(2018,3:5,1:30),1,function(x)paste(x,collapse='-'))
#' recode(x,groups=c('[2018-3-1,2018-3-31]','[2018-4-1,2018-4-30]','[2018-5-1,2018-5-31]'),Labels=c('march','april','may'),method='ranges',mode='datetime',tsFormat='ymd')
#'
#'@export

recode<-function(x,
                 groups,
                 Labels=NULL,
                 na.val='others',
                 method=c('ranges','substrs','elements')[1],
                 mode=c('numeric','character','datetime')[1],
                 tsFormat='ymd'){
  stopifnot(method%in%c('elements','substrs','ranges')|mode%in%c('numeric','character','datetime'))
  require('stringi')
  require('lubridate')
  
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
          parse_date_time(x,orders=tsFormat)->x
          if(grepl("(",l[1],fixed=T)) {as.numeric(parse_date_time(gsub("(^[[:punct:]]|[[:punct:]]$)","",l[1]),orders=tsFormat))->range_l;x>range_l->ind.l}
          if(grepl(")",l[2],fixed=T)) {as.numeric(parse_date_time(gsub("(^[[:punct:]]|[[:punct:]]$)","",l[2]),orders=tsFormat))->range_r;x<range_r->ind.r}
          if(grepl("[",l[1],fixed=T)) {as.numeric(parse_date_time(gsub("(^[[:punct:]]|[[:punct:]]$)","",l[1]),orders=tsFormat))->range_l;x>=range_l->ind.l}
          if(grepl("]",l[2],fixed=T)) {as.numeric(parse_date_time(gsub("(^[[:punct:]]|[[:punct:]]$)","",l[2]),orders=tsFormat))->range_r;x<=range_r->ind.r}
        }
        ifelse(ind.l&ind.r,T,F)->res
      } else {
        
        if(mode=='numeric'){
          gsub("(^[[:punct:]]|[[:punct:]]$)","",L[i])->l
          as.numeric(l)->range_p
        }
        
        if(mode=='datetime'){
          gsub("(^[[:punct:]]|[[:punct:]]$)","",L[i])->l
          parse_date_time(x,orders=tsFormat)->x
          as.numeric(parse_date_time(l,orders=tsFormat))->range_p
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
