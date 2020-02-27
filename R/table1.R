#' table1
#'
#' generate table1
#' 
#' @param data  data.frame
#' 
#' @param grpVars grouping variable names in data. could also use ";" seperate different grouping variables, like grpVars="grp1;grp2"
#' 
#' @param testVars  variable names in data. could also use ";" seperate different vars, like "age;sex"
#' 
#' 
#' @examples
#' 
#' table1(iris,grpVars=c('Species'),testVars=c('Sepal.Length','Sepal.Width'))
#' 
#' table1(iris,grpVars=c('Species'),testVars=c('Sepal.Length;Sepal.Width'))
#' 
#' 
#' 
#' @export

table1<-function(data,grpVars='',testVars,Digits=5){
  require(stringi)
  if(is.character(data)) data=eval(as.name(data))
  grpVars<-unlist(stri_split_fixed(grpVars,';'))
  
  testVars<-unlist(stri_split_fixed(testVars,';'))
  
  if(length(grpVars)==1&!grpVars%in%c('',NA,NULL)){
    as.character(data[,grpVars])->data[,'GrpVar']
  } else {
    if(length(grpVars)>=2){
    apply(data[,grpVars],1,function(i)paste(i,collapse='_'))->data[,'GrpVar']
    } else {
      data[,'GrpVar']<-NA
    }
  }
  
  
  lst<-list()
  for(i in testVars){
    if(all(is.na(data[,'GrpVar']))){
      hTest(data,i)->resi
    } else {
      hTest(data,i,'GrpVar')->resi
      unique(data[,'GrpVar'])->Levels
    }
    
    
    if(class(data[,i])%in%c('numeric','integer')){
      
      if(all(is.na(data[,'GrpVar']))){
        if(names(resi$hTestRes$hTestResult$statistic)=='t'){  # normal distri.
          
          as.vector(unlist(resi$hTestRes$DescResult['NormalDist.',,drop=T]))->desci
          methi<-'t.test'
          
          
        } else {
          
          as.vector(unlist(resi$hTestRes$DescResult['NonnormalDist.',,drop=T]))->desci
          methi<-'wilcox.test'
        }
        
        pvali<-resi$hTestRes$hTestResult$p.value
        
      } else {
        if(length(unique(data[,'GrpVar']))==2){ # hTest for continuous var within two grps
          
          if(names(resi$hTestRes$hTestResult$statistic)=='t'){  # normal distri.
            
            as.vector(unlist(resi$hTestRes$DescResult['NormalDist.',,drop=T]))->desci
            methi<-'t.test'
            
          } else {
            
            as.vector(unlist(resi$hTestRes$DescResult['NonnormalDist.',,drop=T]))->desci
            methi<-'wilcox.test'
          }
          
          resi$hTestRes$hTestResult$p.value->pvali
          
        } else {  # hTest for continuous var for more than two grps
          if(class(resi$hTestRes$wholeTest)[1]=='htest'){  # kruskal.test, nonnormal dist.
            
            as.vector(unlist(resi$hTestRes$DescResult['NonnormalDist.',,drop=T]))->desci
            resi$hTestRes$wholeTest$p.value->pvali
            methi<-'kruskal.test'
          } else {  # aov.  normal dist.
            as.vector(unlist(resi$hTestRes$DescResult['NormalDist.',,drop=T]))->desci
            resi$hTestRes$wholeTest[[1]][1,5]->pvali
            methi<-'anova'
          }
          
          
        }
      }
      
      if(all(is.na(data[,'GrpVar']))){
        names(desci)<-'A'
      } else {
        names(desci)<-stri_replace_all_fixed(names(resi$hTestRes$DescResult),'.DescRes','')
      }
      
      desci<-desci[order(names(desci))]
      
      c(i,desci,methi,round(pvali,Digits))->rowi
      
    } else {   # chisq or fisher test
      
      resi$hTestRes$DescResult->desci
      
      if(is.element('fisherTest',names(resi$hTestRes))){
        resi$hTestRes$fisherTest$p.value->pvali
        methi<-'fisher.test'
      } else {
        if(all(is.na(data[,'GrpVar']))){
          resi$hTestRes$hTestResult$p.value->pvali
          } else {
            resi$hTestRes$chisqTest$p.value->pvali
          }
        
        if(all(is.na(data[,'GrpVar']))){
          methi<-'prop.test'
        } else {
          methi<-'chisq.test'
        }
        
      }
      if(ncol(desci)==1){
        NULL
      } else {
        desci[,order(colnames(desci))]->desci
      }
      
      
      c(i,rep('',ncol(desci)),methi,round(pvali,Digits))->rowi1
      
      cbind(paste('-',row.names(desci)),desci,'','')->rowi2
      rbind(rowi1,rowi2)->rowi
      
    }
    
    
    
    lst[[i]]<-rowi
  }
  do.call(rbind,lst)->res
  as.data.frame(res)->Res
  row.names(Res)<-NULL
  names(Res)[1]<-paste(ifelse(grpVars%in%c('',NA,NULL),'NULL',grpVars),collapse='_')
  names(Res)[ncol(Res)]<-'p.value'
  names(Res)[ncol(Res)-1]<-'test.method'
  
  if(all(is.na(data[,'GrpVar']))){
    nrow(data)->grpSizes
  } else {
    table(data[,'GrpVar'])->tabAll 
    tabAll[order(names(tabAll))]->grpSizes
    } 
  
  paste('(',grpSizes,')',sep='')->grpSizes2
  
  which(names(Res)=='test.method')-1->tail
  names(Res)[2:tail]<-paste(names(Res)[2:tail],grpSizes2,sep='')
  
  return(Res)
  
}


