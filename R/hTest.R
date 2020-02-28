#' hTest
#'
#' hTest provides some basic hypothesis statistical tests, and can detect which test method should be used based on the variable
#' modes and distribution of continuous variables.
#' 
#' when there are multiple variables both in xvars and yvars params, hTest performs statistical tests on each x and y.
#' 
#' @param data  data.frame
#' 
#' @param xvars  variables on the right hand side.
#' 
#' @param  yvars variables on the left hand side. when yvars is null, statistical tests are made on xvars.
#' 
#' @param alter could be two.sided,less,greater.
#' 
#' @param paired logical, if the x variable and yvariable is paired.
#' 
#' @param confLevel confidence level,alpha=1-confLevel
#' 
#' @param nullHyp eg. mu=0 in t.test
#' 
#' @param normalSampleSize integer, when sample size exceed this value, always using parametric tests instead.
#' 
#' @examples 
#' hTest(iris,xvars=c('Sepal.Length','Sepal.Width'),yvars='Species',alter='two.sided')->res
#' res$hTestRes
#' 
#' plot(res$hTestGraph)
#' 
#'
#' @export

hTest<-function(data,xvars,yvars='',alter=c('two.sided','less','greater')[1],paired=FALSE,confLevel=0.95,nullHyp=0,normalSampleSize=30) {
  require(vcdExtra)
  require(fBasics)
  if(is.character(data)) data=eval(as.name(data))
  nrow(data)->obsNo
  if(yvars==''){
    dt<-data.frame(x=data[,xvars[1]],stringsAsFactors = F)
    nameX<-xvars[1]
    if(class(dt$x)[1]%in%c('character','ordered','factor')){
      table(dt$x)->Tab
      sum(Tab,na.rm=T)->sumTab
      uniVar(data=dt,xvars='x',varType='character')$resDesc$resTabDesc->DescResult
      hTestRes<-list(DescResult=DescResult,hTestResult=prop.test(as.numeric(Tab),rep(sumTab,length(Tab)),alternative=alter,conf.level=confLevel))
      hTestGraph<-ggplot(dt,aes(x))+geom_bar(width=0.35,color='white')+labs(x=nameX)+theme_bw()
    } else {
      pvalShapiro<-ksnormTest(dt$x)@test$p.value[1]
      noX<-sum(!is.na(dt$x))
      uniVar(data=dt,xvars='x',varType='numeric')$resDesc$resTabDesc->DescResult
      if(pvalShapiro>0.05|noX>normalSampleSize){
        hTestRes<-list(DescResult=DescResult,hTestResult=t.test(dt$x,alternative=alter,mu=nullHyp,conf.level=confLevel))
      } else {
        
        hTestRes<-list(DescResult=DescResult,hTestResult=wilcox.test(dt$x,alternative=alter,mu=nullHyp,conf.level=confLevel))
      }
      hTestGraph<-ggplot(dt,aes(x))+geom_histogram(color='white',fill='steelblue')+labs(x=nameX)+theme_bw()
      
    }
    
    
  } else {
    data[,c(xvars[1],yvars[1])]->dt
    names(dt)<-c('x','y')
    xvars[1]->nameX
    yvars[1]->nameY
    
    if(class(dt$x)[1]%in%c('numeric','integer')&class(dt$y)[1]%in%c('numeric','integer')){
      dt$z<-dt$y-dt$x
      nameDiff=paste(nameY,nameX,sep='-')
      
      noZ<-sum(!is.na(dt$z))
      noXY<-sum(apply(dt[,c('x','y')],1,function(i)all(!is.na(i))))
      pvalShapiroX<-ksnormTest(dt$x)@test$p.value[1]
      pvalShapiroY<-ksnormTest(dt$y)@test$p.value[1]
      
      pvalShapiroDiff<-ksnormTest(dt$z)@test$p.value[1]
      if(paired){
        if(pvalShapiroDiff>0.05|noZ>normalSampleSize){
          hTestRes<-list(hTestResult=t.test(dt$z,mu=nullHyp,alternative = alter,conf.level=confLevel))
        } else {
          hTestRes<-list(hTestResult=wilcox.test(dt$z,mu=nullHyp,alternative = alter,conf.level=confLevel))
        }
        hTestGraph<-ggplot(dt,aes(z))+geom_histogram(color='white',fill='steelblue')+labs(x=nameDiff)+theme_bw()
        
      } else {
        if((pvalShapiroX>0.05&pvalShapiroY>0.05)&noXY>normalSampleSize){
          hTestRes<-list(hTestResult=cor.test(dt$x,dt$y,alternative = alter,conf.level = confLevel,method='pearson'))
        } else {
          hTestRes<-list(hTestResult=cor.test(dt$x,dt$y,alternative = alter,conf.level = confLevel,method='spearman'))
        }
        hTestGraph<-ggplot(dt,aes(x,y))+geom_point()+geom_smooth(method='lm')+labs(x=nameX,y=nameY)+theme_bw()
      }
    }
    
    if(class(dt$x)[1]%in%c('character','ordered','factor')&class(dt$y)[1]%in%c('character','ordered','factor')){
      table(dt$x,dt$y)->tab
      
      as.table(apply(tab,2,function(x)paste(x,paste('(',round(100*x/sum(x),3),'%',')',sep=''))))->tab2
      row.names(tab2)<-row.names(tab)
      
      if(paired){
        hTestRes<-list(DescResult=tab,hTestResult=mcnemar.test(tab))
      } else {
        chisq.test(tab)->chisqTest
        as.vector(chisqTest$expected)->chisqExp
        if(any(chisqExp<1)||(sum(chisqExp<5)/length(chisqExp))>0.2) {
          
          tryCatch(fisher.test(tab,alternative = alter,conf.level = confLevel),error=function(e)fisher.test(tab,simulate.p.value = T,B=1e+7,alternative = alter,conf.level = confLevel))->fisherTest
          if(any(c(class(dt$x)[1],class(dt$y)[1])=='ordered')){
            hTestRes<-list(DescResult=tab2,chisqTest=chisq.test(tab),fisherTest=fisherTest,CMHtest=CMHtest(tab))
          } else {
            hTestRes<-list(DescResult=tab2,chisqTest=chisq.test(tab),fisherTest=fisherTest)
          }
          
        } else {
          if(any(c(class(dt$x)[1],class(dt$y)[1])=='ordered')){
            hTestRes<-list(DescResult=tab2,chisqTest=chisq.test(tab),CMHtest=CMHtest(tab))
          } else {
            hTestRes<-list(DescResult=tab2,chisqTest=chisq.test(tab))
          }
          
        }
      }
      hTestGraph<-ggplot(dt,aes(x,fill=y))+geom_bar(width=0.35,color='white',position='dodge')+labs(x=nameX)+scale_fill_discrete(nameY)+theme_bw()
      
    }
    
    if(all(c('numeric','character')%in%c(class(dt$x)[1],class(dt$y)[1]))||all(c('numeric','factor')%in%c(class(dt$x)[1],class(dt$y)[1]))||all(c('integer','factor')%in%c(class(dt$x)[1],class(dt$y)[1]))||all(c('integer','character')%in%c(class(dt$x)[1],class(dt$y)[1]))){
      which(c(class(dt$x)[1],class(dt$y)[1])%in%c('numeric','integer'))->indNum
      which(c(class(dt$x)[1],class(dt$y)[1])%in%c('character','factor'))->indChar
      dt[,c(indNum,indChar)]->dt
      names(dt)<-c('x','grp')
      tapply(dt$x,dt$grp,function(i)uniVar(data=dt,xvars='x',varType='numeric')$resDesc$resTabDesc)->X
      sapply(as.vector(na.omit(unique(dt$grp))),function(i)uniVar(data=dt[dt$grp==i,],xvars='x',varType='numeric')$resDesc$resTabDesc)->X
      matrix(nr=9,nc=length(X))->matDesc
      for(i in 1:ncol(matDesc)){
        matDesc[,i]<-as.character(X[[i]])
      }
      colnames(matDesc)<-names(X)
      row.names(matDesc)<-c('Min','0.25Qu.','Median','Mean','0.75Qu.','Max','NAs','NormalDist.','NonnormalDist.')
      as.data.frame(matDesc)->matDesc
      c(nameX,nameY)[c(indNum,indChar)]->namesdt
      
      pvalShapiroX<-ksnormTest(dt$x)@test$p.value[1]
      # table(dt$grp)->tabF
      tapply(dt$x,dt$grp,function(i)sum(!is.na(i)))->tabF
      if(pvalShapiroX>0.05|all(tabF>normalSampleSize)){
        if(length(unique(dt$grp))==2){
          hTestRes<-list(DescResult=matDesc,hTestResult=t.test(dt$x~dt$grp,alternative=alter,mu=nullHyp,conf.level=confLevel))
        } else {
          hTestRes<-list(DescResult=matDesc,wholeTest=summary(aov(dt$x~dt$grp,alternative=alter,conf.level=confLevel)),pairedHTest=pairwise.t.test(dt$x,dt$grp,p.adj='bonf',alternative = alter))
        }
      } else {
        if(length(unique(dt$grp))==2){
          hTestRes<-list(DescResult=matDesc,hTestResult=wilcox.test(dt$x~dt$grp,alternative=alter,mu=nullHyp,conf.level=confLevel))
        } else {
          hTestRes<-list(DescResult=matDesc,wholeTest=kruskal.test(dt$x,as.factor(dt$grp),alternative=alter,conf.level=confLevel),pairedHTest=pairwise.wilcox.test(dt$x,dt$grp,p.adj='bonf',alternative = alter))
        }
        
      }
      hTestGraph<-ggplot(dt,aes(grp,x,fill=grp))+geom_boxplot(width=0.35)+labs(x=namesdt[2],y=namesdt[1])+theme_bw()
    }
    
    if(all(c('numeric','ordered')%in%c(class(dt$x)[1],class(dt$y)[1]))||all(c('integer','ordered')%in%c(class(dt$x)[1],class(dt$y)[1]))){
      which(c(class(dt$x)[1],class(dt$y)[1])%in%c('numeric','integer'))->indNum
      which(c(class(dt$x)[1],class(dt$y)[1])=='ordered')->indOrd
      dt[,c(indNum,indOrd)]->dt
      names(dt)<-c('x','grp')
      as.numeric(dt$grp)->dt$grp
      c(nameX,nameY)[c(indNum,indOrd)]->namesdt
      #pvalShapiroX<-ifelse(obsNo>5000,shapiro.test(dt$x[sample(1:obsNo,5000)])$p.value,shapiro.test(dt$x)$p.value)
      pvalShapiroX<-ksnormTest(dt$x)@test$p.value[1]
      hTestRes<-list(hTestResult=cor.test(dt$x,dt$grp,method='spearman',conf.level=confLevel,alternative=alter))
      hTestGraph<-ggplot(dt,aes(x,grp))+geom_point()+geom_smooth(method='lm')+labs(x=namesdt[1],y=namesdt[2])+theme_bw()
    }
    
  }
  
  return(list(hTestRes=hTestRes,hTestGraph=hTestGraph))
  
}
