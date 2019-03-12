#' lmeS
#' 
#' lme function used in shiny app.
#' 
#' @param formulaFixed  formula of fixed effects
#' @param formulaRandom formula of random effects
#' @param data data.frame obj.
#' @param subset  logical expression or 'all' which means no subsetting.
#' @param Method could be 'ML' or 'REML'.
#' 
#' 
#' @export
lmeS<-function(formulaFixed,
               formulaRandom,
               Method=c('ML','REML')[1],
               data,
               subset='all'
){
  require('stringi')
  require('MASS')
  require('nlme')
  stri_replace_all_regex(formulaFixed,'[~+]{2,}','~')->formulaFixed
  stri_replace_all_regex(formulaRandom,'[~+]{2,}','~')->formulaRandom
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
  
  unique(unlist(stri_split_regex(formulaFixed,'[+*:~, -\\(\\)\\^\\/|]')))->varsAllFixed
  unique(unlist(stri_split_regex(formulaRandom,'[+*:~, -\\(\\)\\^\\/|]')))->varsAllRandom
  
  
  intersect(names(data),c(varsAllFixed,varsAllRandom))->varsAll2
  data[,varsAll2]->dt
  na.omit(dt)->dt
  
  as.formula(formulaFixed)->Fixed
  as.formula(formulaRandom)->Random
  lme(fixed=Fixed,random=Random,data=dt,method=Method)->fitFull
  if(Method=='ML'){
    stepAIC(fitFull)->fitStep
  } else {
    fitStep=fitFull
  }
  
  return(list(lmeResFull=fitFull,lmeResStep=fitStep))
  
  
  
  
}
