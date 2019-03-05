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
