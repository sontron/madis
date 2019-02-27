#' validateData
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
#' validateData(data=iris,val=c('Species','Sepal.Length'),
#' legal=list(c('setosa','versicolor'),c('[3,6]')),method=c('elements','ranges'),
#' mode=c('character','numeric'))
#'
#' @export
validateData<-function(data,val,legal,method=c('ranges','substrs','elements')[1],mode=c('numeric','character','datetime')[1],type=c('fixed','regex')[1]){
  
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
