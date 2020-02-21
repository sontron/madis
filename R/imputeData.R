#' imputeData
#'
#' using tree model to impute na values in a data
#' 
#' @param data data.frame
#' 
#' @param impVars  variables needed imputed
#' 
#' @param modelVars  variables used in cart tree model
#' 
#' @param treeParams  parameters control aspects of cart tree.
#' 
#' @param method  impute methods, could be sample and pred. when using sample, the na values will be imputed from the raw data
#' in the terminal node of a tree model. when using pred, the na values will be imputed by the predicted value of cart tree model.
#' 
#' @examples 
#' 
#' mt<-mtcars
#' set.seed(123)
#' mt[cbind(sample(1:30,10,rep=F),sample(1:3,10,rep=T))]<-NA
#' imputeData(mt,impVars=names(mt)[1:3],modelVars=names(mt),treeParams=list(maxdepth=5,minbucket=7,minsplit=20,cp=0.01),method='sample')->mtImpSample
#' imputeData(mt,impVars=names(mt)[1:3],modelVars=names(mt),treeParams=list(maxdepth=5,minbucket=7,minsplit=20,cp=0.01),method='pred')->mtImpPred
#' 
#' plot(mtcars$mpg,mtImpSample$mpg)
#' plot(mtcars$mpg,mtImpPred$mpg)
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
  data[,Vars]->>dat
  # assign('dat',dat,envir = .GlobalEnv)
  for(i in impVars){
    if(any(is.na(dat[,i]))) {
      paste(i,'~',paste(names(dat)[-which(names(dat)==i)],collapse='+'),sep='')->formula
      rpart:::rpart(formula,data=dat,control=treeParams)->fit
      as.party(fit)->fit
      #as.party(fit)->fitParty
      if(length(unique(partykit:::predict.party(fit,type='node')))==1){
        dat[is.na(dat[,i]),i]<-sample(dat[!is.na(dat[,i]),i],length(dat[is.na(dat[,i]),i]),rep=T)
      } else {
        #fitted(fitParty)[,1]->nodeRaw
        #as.numeric(row.names(fit$frame)[fit$where])->nodeRaw
        partykit:::predict.party(fit,type='node')->nodeRaw
        dat[!is.na(dat[,i]),i]->iRaw
        if(method=='sample'){
          #predictNodes(fit,newdata=dat[is.na(dat[,i]),])->nodePred
          partykit:::predict.party(fit,newdata=dat[is.na(dat[,i]),],type='node')->nodePred
          iImp<-numeric(length(nodePred))
          for(j in unique(nodePred)){
            which(nodePred==j)->ind
            sample(iRaw[nodeRaw==j],length(ind),rep=T)->iImp[ind]
          }
          dat[is.na(dat[,i]),i]<-iImp
        }
        if(method=='pred'){
          # if(class(dat[,i])[1]%in%c('character','factor','ordered')){
          #   as.vector(predict(fit,dat[is.na(dat[,i]),],type='class'))->responsePred
          # } else {
          #   predict(fit,dat[is.na(dat[,i]),],type='vector')->responsePred
          # }
          partykit:::predict.party(fit,neadata=dat[is.na(dat[,i]),])->responsePred
          
          dat[is.na(dat[,i]),i]<-responsePred
        }
      }
    } else {
      dat[,i]->dat[,i]
    }
    
  }
  
  data[,Vars]<-dat
  rm(dat,envir = .GlobalEnv)
  return(data)
}



#' miceChar
#' 
#' revised function of mice
#' 
#' 
#' 
#' @export

miceChar<-function(data,Where,Method){
  sapply(data,class)->varClass
  which(varClass=='character')->indChar
  for(i in indChar){
    as.factor(data[,i])->data[,i]
  }
  mice(data,where=Where,method=Method)->res
  complete(res)->Res
  for(i in indChar){
    as.factor(Res[,i])->Res[,i]
  }
  
  return(Res)
  
}
