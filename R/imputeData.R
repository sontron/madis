#' imputeData
#'
#' is a imputation function
#'
#' @export

imputeData<-function(data,impVars,modelVars,seed=1,treeParams=list(),method=c('sample','pred')[1]){
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
