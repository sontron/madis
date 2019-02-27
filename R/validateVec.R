#' validateVec
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
#' validateVec(x,L,method='ranges',mode='numeric')
#' x.char=c('a','b','cd')
#' L.char=c('a','c')
#' validateVec(x=x.char,L=L.char,method='elements',mode='character')
#' validateVec(x=x.char,L=L.char,method='substrs',mode='character')
#' Date=c('2016-1-1 14:24:00',NA,'2016-6-1','2016-12-31 24:00:00')
#' L.date='[2016-1-1,2016-12-31)'
#' validateVec(x=Date,L=L.date,method='ranges',mode='datetime')
#'
#' @note the expression of L is flexible,like "(0,3)","[7,11]","[0,Inf)",c('[-1,0]','(1,10)'),
#' "[2013-1-2,2-13-10-11]",c('apple','orange')
#'
#' @export
validateVec<-function(x,L,method=c('ranges','substrs','elements')[1],mode=c('numeric','character','datetime')[1],type=c('fixed','regex')[1]){
  
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
