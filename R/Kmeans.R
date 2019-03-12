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
  seed=123
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
  
  graphCrit<-cascadeKM(data[,vars],inf.gr=infgr,sup.gr=supgr,criterion=Criterion,iter=Iter)
  set.seed(seed)
  kmeans(data[,vars],centers=Centers,iter.max=iterMax,algorithm = Algorithm)->resKmeans
  data[,clusterName]<-resKmeans$cluster
  
  return(list(graphCrit=graphCrit,resKmeans=data))
  
  
}
