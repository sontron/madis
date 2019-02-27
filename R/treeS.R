#' treeS
#' 
#' is a function of Tree models
#' 
#' @export

treeS<-function(Formula,
                 data,
                 # weightsVar,
                 subset='all',
                 na.action=na.rm,
                 Minsplit=30,
                 Minbucket=10,
                 Mincrit=0.05,
                 Maxdepth=3,
                 CP=0.01,
                 treeMethod=c('ctree','rpart')[1]
){
  
  if(is.character(data)) data=eval(as.name(data))
  if(subset=='all'){
    data<-data
  } else {
    subset(data,eval(parse(text=subset)))->data
  }
  
  for(i in 1:ncol(data)){
    if(class(data[,i])=='character'){
      as.factor(data[,i])->data[,i]
    }
  }
  
  #unique(unlist(stri_split_regex(Formula,'[+*:~(), ]')))->varsAll
  unique(unlist(stri_split_regex(Formula,'[+*:~, -\\(\\)\\^]')))->varsAll
  intersect(names(data),varsAll)->varsAll2
  #setdiff(varsAll,c('Surv',''))->varsAll2
  data[,varsAll2]->dt
  na.omit(dt)->dt
  
  if(treeMethod=='ctree'){
    fitTree<-partykit:::ctree(as.formula(Formula),data=dt,control=partykit:::ctree_control(maxdepth = Maxdepth,minsplit=Minsplit,minbucket = Minbucket,alpha = Mincrit))
    # fitTree<-as.party(fitTree)
  }
  
  if(treeMethod=='rpart'){
    fitTree<-as.party(rpart(as.formula(Formula),data=dt,control=rpart.control(maxdepth = Maxdepth,minsplit=Minsplit,minbucket = Minbucket,cp = CP)))
  }
  
  return(fitTree)
  
  
}
