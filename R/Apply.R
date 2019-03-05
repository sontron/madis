#' Apply
#' 
#' based on apply function in base R.
#'
#' @author sontron
#'
#' @export

Apply<-function(data,vars,MARGIN,FUN,na.rm=T,...){
  stopifnot(class(data)%in%c('data.frame','matrix')&&all(is.element(vars,names(data))))
  #if(length(vars)==1) {
  #  apply(as.matrix(data[,vars]),MARGIN,FUN,...)->resmyApply
  #} else {
  #  apply(data[,vars],MARGIN,FUN,...)->resmyApply
  #}
  # apply(as.matrix(data[,vars],ncol=length(vars)),MARGIN,FUN,...)->resmyApply
  if(length(vars)==1){
    as.vector(unlist(sapply(data[,vars,drop=F],FUN)))->resmyApply
  } else {
    apply(data[,vars,drop=F],MARGIN,FUN,...)->resmyApply
  }
  
  return(resmyApply)
}
