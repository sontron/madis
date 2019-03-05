#' autoVarClass
#'
#' automatically chage variable mode in a data.frame.
#' 
#' some discrete variables maybe coded into integer, eg. sex (male=1,female=2),and should be treated as character or factor when analyzing.
#' autoVarClass can be used to automatically detect variable mode in a data.frame.
#' 
#' @param data   a data.frame object.
#' 
#' @param lenTab length of a single variable elements.
#' 
#' @param thresh  thershold value when transform a variable into numeric.
#' 
#' @examples 
#' autoVarClass(data=mtcars,lenTab=10,thresh=0.75)
#'
#'
#' @export

autoVarClass<-function(data,lenTab=10,thresh=0.8){
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
