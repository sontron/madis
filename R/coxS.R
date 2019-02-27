#' coxS
#' 
#' is a function of Coxph models
#' 
#' @export

coxS<-function(Formula,
                data,
                weightsVar=1,
                subset='all',
                #Family=c('gaussian','binomial','poisson'),
                strataVar='1',
                #na.action=na.rm,
                lower='~1'
){
  if(is.character(data)) data=eval(as.name(data))
  if(subset=='all'){
    data<-data
  } else {
    subset(data,eval(parse(text=subset)))->data
  }
  stri_split_fixed(Formula,'~')[[1]][1]->lht
  #yVar=stri_split_fixed(Formula,'~')[[1]][1]
  Form1=as.formula(paste('~',stri_split_fixed(Formula,'~')[[1]][2]))
  Form2=as.formula(lower)
  #unique(unlist(stri_split_regex(Formula,'[+*:~(), ]')))->varsAll
  unique(unlist(stri_split_regex(Formula,'[+*:~, -\\(\\)\\^]')))->varsAll
  #setdiff(varsAll,c('Surv',''))->varsAll2
  intersect(names(data),varsAll)->varsAll2
  if(strataVar!='1'){
    union(varsAll2,strataVar)->varsAll2
  }
  if(is.character(Formula)) Formula=as.formula(Formula)
  
  if(weightsVar==1){
    data[,varsAll2]->dat
    na.omit(dat)->dat
    Wt=rep(1,nrow(dat))
  } else {
    data[,c(varsAll2,weightsVar)]->dat
    na.omit(dat)->dat
    Wt=dat[,weightsVar]
  }
  
  
  
  coxph(Formula,data=dat,weights=Wt)->fit
  
  step(fit,scope=list(upper=Form1,lower=Form2),trace=F)->fitStep
  
  if(strataVar!='1'){
    paste(lht,strataVar,sep='~')->FormulaStrata
    as.formula(FormulaStrata)->FormulaStrata
    survfit(FormulaStrata,data=dat)->survFitStrata
  } else {
    survfit(fitStep)->survFitStrata
  }
  
  
  
  return(list(coxResFull=fit,coxResStep=fitStep,fitStrata=survFitStrata))
  
  
}
