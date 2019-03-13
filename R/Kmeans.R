#' Kmeans
#' 
#' kmeans analysis used in shiny app.
#' 

Kmeans<-function(
  data,
  vars,
  infgr=1,
  supgr=10,
  Centers=5,
  Criterion=c('calinski','ssi')[1],
  Iter=100,
  iterMax=10,
  Algorithm=c('Hartigan-Wong','Lloyd','Forgy')[1],
  subset='all',
  clusterName='kmeansCluster',
  seed=123,
  addVar=T
){
  require('vegan')
  require('stringi')
  if(is.character(data)) data=eval(as.name(data))
  if(subset=='all'){
    data<-data
  } else {
    subset(data,eval(parse(text=subset)))->data
  }
  
  #data[,vars]->dat
  #na.omit(dat)->dat
  # if(trans){
  #   as.data.frame(t(data))->data
  # }
  unlist(stri_split_fixed(vars,','))->vars
  data[,vars]->dt
  which(sapply(dt,class)%in%c('numeric','integer'))->indVar
  dt[,indVar]->dt2
  if(any(sapply(dt2,anyNA))){
    imputeData(dt2,impVars=names(dt2),modelVars=names(dt2),method='sample',treeParams = list(maxdepth=5,minbucket=7,minbucket=20))->dt2
  }
  
  graphCrit<-cascadeKM(dt2,inf.gr=infgr,sup.gr=supgr,criterion=Criterion,iter=Iter)
  set.seed(seed)
  kmeans(dt2,centers=Centers,iter.max=iterMax,algorithm = Algorithm)->resKmeans
  if(addVar){
    data[,clusterName]<-resKmeans$cluster
  } else {
    NULL
  }
  
  
  return(list(graphCrit=graphCrit,resKmeans=data))
  
  
}
