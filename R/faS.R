#' faS
#' 
#' factor analysis used in shiny app.
#' 
#' @export


faS<-function(
  data,
  vars,
  nfcts=2,
  Rotate=c('none','varimax','quartimax','bentlerT',
           'varmin','equamax','geominT','bifactor','promax','oblimin',
           'simplimax','bentlerQ','geominQ','biquartimin','cluster')[1],
  Scores=c('regression','Thurstone','tenBerge','Anderson','Barlett')[1],
  FM=c('minres','uls','ols','wls','gls','pa','ml','minchi','minrank')[1],
  subset='all',
  faVarName=''
){
  require('psych')
  require('stringi')
  
  if(is.character(data)) data=eval(as.name(data))
  if(subset=='all'){
    data<-data
  } else {
    subset(data,eval(parse(text=subset)))->data
  }
  
  # data[,vars]->dat
  # na.omit(dat)->dat
  unlist(stri_split_fixed(vars,','))->vars
  fa(data[,vars],nfactors = nfcts,rotate=Rotate,scores=Scores,fm=FM)->res
  if(faVarName==''){
    cbind(data,res$scores)->data
  } else {
    colnames(res$scores)<-paste(colnames(res$scores),faVarName,sep='_')
    cbind(data,res$scores)->data
  }
  return(list(resFA=res,dtFA=data,dataScree=data[,vars],cumVar=res$Vaccounted))
  
  
}
