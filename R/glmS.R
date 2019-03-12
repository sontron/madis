#' glmS
#' 
#' is a function used in shiny app.
#' 
#' 
#' @export

glmS<-function(Formula,
                data,
                weightsVar=1,
                subset='all',
                Family=c('gaussian','binomial','poisson'),
                na.action=na.rm,
                lower='~1'
){
  require('stringi')
  stri_replace_all_regex(Formula,'[~+]{2,}','~')->Formula
  require(fBasics)
  if(is.character(data)) data=eval(as.name(data))
  if(subset=='all'){
    data<-data
  } else {
    subset(data,eval(parse(text=subset)))->data
  }
  yVar=stri_split_fixed(Formula,'~')[[1]][1]
  Form1=as.formula(paste('~',stri_split_fixed(Formula,'~')[[1]][2]))
  Form2=as.formula(lower)
  unique(unlist(stri_split_regex(Formula,'[+*:~, -\\(\\)\\^]')))->varsAll
  intersect(names(data),varsAll)->varsAll
  if(is.character(Formula)) Formula=as.formula(Formula)
  
  if(weightsVar==1){
    data[,varsAll]->dat
    na.omit(dat)->dat
    Wt=rep(1,nrow(dat))
  } else {
    data[,c(varsAll,weightsVar)]->dat
    na.omit(dat)->dat
    Wt=dat[,weightsVar]
  }
  
  if(Family=='binomial'){
    as.numeric(as.factor(dat[,yVar]))-1->dat[,yVar]
  }
  
  glm(Formula,data=dat,family=Family,x=T,y=T,weights=Wt)->fit
  
  step(fit,scope=list(upper=Form1,lower=Form2),trace=F)->fitStep
  
  
  return(list(glmResFull=fit,glmResStep=fitStep))
  
  
}
