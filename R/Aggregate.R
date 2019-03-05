#' Aggregate
#'
#' based on aggregate function in stats package. 
#'
#' @author sontron
#' @param data is a data.frame object that need to be processed
#' @param group are the dimension variables
#' @param val are the variables that needed calculated
#' @param subsets are subset of data object
#' @return same as aggregate function
#'
#' @examples
#' Aggregate(iris,group='Species',val=c('Sepal.Length','Sepal.Width'),fun='mean')
#' Aggregate(iris,group='Species',val=c('Sepal.Length','Sepal.Width'),fun='function(x)median(x,na.rm=T)')
#' Aggregate(iris,group='Species',val=c('Sepal.Length','Sepal.Width'),
#' fun='mean',subsets=list(val=c('Species','Sepal.Length'),legal=list(c('setosa','virginica'),
#' c('[0,10]')),method=c('elements','ranges'),mode=c('character','numeric')))
#'
#'
#' @export


Aggregate<-function(data=NA,group=NA,val=NA,fun='mean',subsets=NA){
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

