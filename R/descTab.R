#' descTab
#' 
#' this function is a revised version of mytable in moonBook package.
#' mytable(moonBook) can deal no more than two group variables. in descTab function, when there are more than two grouping 
#' variables, they are pasted together as one single variable
#' 
#' @examples
#' descTab(Formula='vs+gear+am~disp+mpg',data=mtcars)
#' 
#' @export

descTab<-function(Formula,data){
  
  
  if(is.character(data)) data=eval(as.name(data))
  
  stri_split_fixed(Formula,'~')[[1]][1]->lht
  stri_split_fixed(Formula,'~')[[1]][-1]->rht
  
  unlist(stri_split_fixed(lht,'+'))->lhtVars
  unlist(stri_split_fixed(rht,'+'))->rhtVars
  
  sapply(data[,rhtVars],function(i){
    if(class(i)%in%c('factor','character','ordered')){
      len=1000
    } 
    if(class(i)%in%c('integer','numeric')){
      len=length(unique(i))
    }
    return(len)
  })->lenRht
  
  (min(lenRht,na.rm=T)-1)->maxYlevel
  
  
  if(length(lhtVars)>1){
    apply(data[,lhtVars],1,function(i)paste(i,collapse='_'))->data[,paste(lhtVars,collapse='_')]
    as.formula(paste(paste(lhtVars,collapse='_'),rht,sep='~'))->Formula
    mytable(Formula,data,method = 1,max.ylev=maxYlevel)->res
  } else {
    if(lhtVars==''){
      data$noGroupVar=1
      as.formula(paste('noGroupVar',rht,sep='~'))->Formula
      mytable(Formula,data,method = 1,max.ylev=maxYlevel)->res
    } else {
      data[,lhtVars]->data[,paste(lhtVars,collapse='_')]
      as.formula(paste(paste(lhtVars,collapse='_'),rht,sep='~'))->Formula
      mytable(Formula,data,method = 1,max.ylev=maxYlevel)->res
    }
    
  }
  
  
  
  
  
  mytable2df(res)->res2
  return(res2)
  
}
