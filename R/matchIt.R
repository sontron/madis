#' matchIt
#' 
#' matchIt is a simplyfied function based on matchIt from MatchIt package, and with some revisement used in madis
#' 
#' @export


matchIt<-function(formula,data,Method='nearest',Distance='logit',Ratio=1){
  require(stringi)
  require(MatchIt)
  names(data)->namesDt
  data$myRowID<-1:nrow(data)
  as.character(formula)->Form
  unlist(stri_split_regex(Form,'[~+]'))->vars
  stri_split_fixed(Form,'~')[[1]][1]->grpVar
  setdiff(vars,grpVar)->varsMatch
  names(which.max(table(data[,grpVar])))->larger
  
  ifelse(data[,grpVar]==larger,0,1)->data$matchGrp
  
  data[,c('myRowID',varsMatch,'matchGrp')]->dt
  formMatch<-as.formula(paste('matchGrp',paste(varsMatch,collapse='+'),sep='~'))
  matchit(formMatch,data=dt,method=Method,distance=Distance,ratio=Ratio)->matchOut
  
  match.data(matchOut)$myRowID->myRowIDMatched
  
  subset(data,myRowID%in%myRowIDMatched)->dataMatchedTmp
  dataMatchedTmp[,namesDt]->dataMatched
  return(list(resMatch=matchOut$nn,dataMatched=dataMatched))
  
}
