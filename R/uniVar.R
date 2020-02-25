#' uniVar
#'
#' a function provide description statistics on a single variable.
#' 
#' 
#' @param data  data.frame
#' @param xvars 
#' @param varType variable modes
#' @param Digits digits printed in the results.
#' @param nameX table head.
#' @param seed 
#' @param tabSort sort results of the output.
#' 
#' @examples 
#' uniVar(mtcars,xvars=c('disp'),varType=c('numeric'),Digits=3,nameX=c('disp'))
#'
#'
#'
#'
#'
#' @export

uniVar<-function(data,xvars,varType=c('numeric','character','factor','integer','ordered')[1],Digits=4,nameX='x',seed=123,tabSort=TRUE){
  if(is.character(data)) data=eval(as.name(data))
  as.data.frame(data[,xvars])->dt
  nameX=xvars
  varType=class(data[,xvars])[1]
  x=data[,xvars]
  names(dt)<-nameX
  if(varType%in%c('numeric','integer')){
    summary(x)->resNum
    if(length(resNum)==6) {
      names(resNum)<-c('Min','0.25Qu.','Median','Mean','0.75Qu.','Max')
      resNum[7]<-0
      names(resNum)[7]<-'NAs'
    }
    if(length(resNum)==7) {
      names(resNum)<-c('Min','0.25Qu.','Median','Mean','0.75Qu.','Max','NAs')
    }
    sdNum<-sd(x,na.rm=T)
    round(resNum,Digits)->resNumRd
    round(sdNum,Digits)->sdNumRd
    desNormalNum<-paste(resNumRd[4],'±',sdNumRd,sep='')
    names(desNormalNum)<-'NormalDist.'
    desNonnormalNum<-paste(resNumRd[3],'[',resNumRd[2],',',resNumRd[5],']',sep='')
    names(desNonnormalNum)<-'NonnormalDist.'
    as.data.frame(c(resNumRd,desNormalNum,desNonnormalNum))->resTab
    # if(length(x)<5000) {
    #   shapiro.test(x)->resShap
    # } else {
    #   set.seed(seed)
    #   shapiro.test(x[sample(1:length(x),5000,rep=F)])->resShap
    # }
    resShap<-ksnormTest(x)
    resTab<-as.data.frame(resTab)
    names(resTab)<-'DescRes'
    #resDesc<-list(resTabDesc=resTab,resShapiroTest=resShap)
    resDesc<-list(resTabDesc=resTab)
    graphDesc<-ggplot(dt,aes(x))+geom_histogram(color='white',fill='steelblue')+labs(x=nameX)+theme_bw()
  }
  
  if(varType%in%c('character','factor','ordered')){
    table(x,useNA = 'ifany')->tabChar
    names(tabChar)[which(is.na(names(tabChar)))]<-'NAs'
    
    # resTab<-cbind(tabChar,round(tabChar/sum(tabChar,na.rm=T),Digits))
    # as.data.frame(resTab)->resTab
    # names(resTab)<-c('Freq','Perc')
    # resDesc<-list(resTabDesc=resTab)
    
    matrix(tabChar,ncol=1)->tabMat
    
    apply(tabMat,2,function(x)paste(x,paste('(',round(100*x/sum(x),3),'%',')',sep='')))->tabMat
    row.names(tabMat)<-names(tabChar)
    as.table(tabMat)->tabMat
    resDesc<-list(resTabDesc=tabMat)
    graphDesc<-ggplot(dt,aes(x,fill=x))+geom_bar(width=0.35,color='white')+labs(x=nameX)+theme_bw()
  }
  
  resDescLst<-list(resDesc=resDesc,graphDesc=graphDesc)
  return(resDescLst)
  
}
