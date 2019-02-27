#' autoVarClass
#'
#' is a function that determine the variable classes automatically.
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
