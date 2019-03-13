#' pcaS
#' 
#' PCA analysis used in shiny app.
#' 
#' 
#' @export


pcaS<-function(
  data,
  vars,
  nfcts=2,
  Rotate=c('none','varimax','quartimax','promax','oblimin','simplimax','cluster')[1],
  Scores=T,
  subset='all',
  pcaVarName='',
  addVar=T
){
  require('psych')
  
  if(is.character(data)) data=eval(as.name(data))
  if(subset=='all'){
    data<-data
  } else {
    subset(data,eval(parse(text=subset)))->data
  }
  
  # data[,vars]->dat
  # na.omit(dat)->dat
  unlist(stri_split_fixed(vars,','))->vars
  data[,vars]->dt
  which(sapply(dt,class)%in%c('numeric','integer'))->indVar
  dt[,indVar]->dt2
  principal(dt2,nfactors = nfcts,rotate=Rotate,scores=Scores)->res

  if(addVar){
    if(pcaVarName==''){
      cbind(data,res$scores)->data
    } else {
      colnames(res$scores)<-paste(colnames(res$scores),pcaVarName,sep='_')
      cbind(data,res$scores)->data
    }
  } else {
    NULL
  }
  
  
  return(list(resPCA=res,dtPCA=data,dataScree=dt2,cumVar=res$Vaccounted))
  
  
}
