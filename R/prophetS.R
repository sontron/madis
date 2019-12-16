#' prophetS
#' 
#' is a revised version of prophet that combine some other functions from xts package for dealing data.frame, multiple variables and adding group variables. 
#' return the history data and graphs as well as predicted graph using prophet.
#' 
#' @param data data.frame
#' 
#' @param tsVar  ts variable
#' 
#' @param tsFormat could be 'ymd','ym','y','mdy','ymd hms', etc.
#' 
#' @param measureVars measurement variables in data.
#' 
#' @param groupVars  grouping variables in data
#' 
#' @param Period could be 'years','quarters','months','weeks','days','hours','minutes','seconds',etc.
#' 
#' @param FN  function eg: 'mean','function(x)sum(x,na.rm=T)',etc.
#' 
#' @param Cap same as cap in prophet.
#' 
#' @param Floor same as floor in prophet.
#' 
#' @param Growth same as growth in prophet.
#' 
#' @param H same as h in prophet.
#' 
#' @param yearlyS TRUE,FALSE or "auto", same as yearly.seasonality in prophet.
#' 
#' @param dailyS TRUE,FALSE or "auto", same as daily.seasonality in prophet.
#' 
#' @param weeklyS TRUE,FALSE or "auto", same as weekly.seasonality in prophet.
#' 
#' @export

prophetS<-function(data,
                    tsVar,
                    tsFormat='ymd',
                    measureVars,
                    groupVars=1,
                    Period='weeks',
                    FN,
                    Cap=-1,
                    Floor=-1,
                    Growth='linear',
                    # changepointRange=.8,
                    # changepointPriorScale = 0.05,
                    # mcmcSamples = 0,
                    # intervalWidth=.8,
                    H=10,
                    yearlyS='auto',
                    dailyS='auto',
                    weeklyS='auto'
                    
){
  require(xts)
  require(lubridate)
  require(broom)
  
  if(is.character(data)) data=eval(as.name(data))
  
  unlist(stri_split_fixed(measureVars,';'))->measureVars
  unlist(stri_split_fixed(groupVars,';'))->groupVars
  
  
  parse_date_time(data[,tsVar],orders=tsFormat)->xTmp
  which(is.na(xTmp))->ind
  if(length(ind)>0){data[-ind,]->dat} else {dat=data}
  
  # dat$cap<-Cap
  # dat$floor<-Floor
  
  
  if(groupVars%in%c('1','','NA','NULL')){
    dat[,'tsGroupVar']<-1
  } else {
    if(length(groupVars)==1){
      dat$tsGroupVar<-dat[,groupVars]
    } else {
      myApply(data = dat,vars = groupVars,MARGIN = 1,FUN = function(x)paste(x,collapse='_'))->dat$tsGroupVar
    }
    
  }
  
  
  
  # fn_aggre(dat,group=c(tsVar,'tsGroupVar'),val=measureVars,FN)->resTmp  #using code below 
  dat[,c(tsVar,'tsGroupVar',measureVars)]->resTmp
  
  
  lapply(unique(resTmp[,'tsGroupVar']),function(i){
    subset(resTmp,tsGroupVar==i)->resTmpi
    resTmpi[,-which(names(resTmpi)=='tsGroupVar')]->resTmpii
    parse_date_time(resTmpii[,tsVar],orders=tsFormat)->TS
    xts(resTmpii[,-which(names(resTmpii)==tsVar),drop=F],TS)->resTmpii
    lapply(resTmpii,function(j){
      period.apply(j,endpoints(j,Period),FUN=eval(parse(text=FN)))
    })->resTmpj
    
    do.call(merge,resTmpj)->resTmpj
    as.data.frame(resTmpj)->resTmpj
    resTmpj[,'tsGroupVar']<-i
    resTmpj[,tsVar]<-row.names(resTmpj)
    return(resTmpj)
    
  })->resTmpNew
  
  do.call(rbind,resTmpNew)->resFinal
  # resFinal[,tsVar]<-row.names(resFinal)  # error occur for duplicate row.names
  if(groupVars%in%c('1','','NA','NULL')){
    resFinal[,-which(names(resFinal)=='tsGroupVar')]->resFinal
    melt(resFinal,id.vars=tsVar)->dfGraph
  } else {
    names(resFinal)[which(names(resFinal)=='tsGroupVar')]<-paste(groupVars,collapse='_')
    melt(resFinal,id.vars=c(tsVar,paste(groupVars,collapse='_')))->dfGraph
  }
  
  
  
  
  as_datetime(resFinal[,tsVar])->resFinal[,tsVar]
  
  # dfGraph[,tsVar]<-row.names(dfGraph)
  as_datetime(dfGraph[,tsVar])->dfGraph[,tsVar]
  
  row.names(resFinal)<-NULL
  row.names(dfGraph)<-NULL
  
  if(groupVars%in%c('1','','NA','NULL')){
    ggplot(dfGraph,aes_string(tsVar,'value',color='variable'))+geom_point()+geom_line()+facet_wrap(~variable,scales='free')->graph
  } else {
    
    # ggplot(dfGraph,aes_string(tsVar,'value',color='variable'))+geom_point()+geom_line()+facet_wrap(as.formula(paste('~',paste('variable',paste(groupVars,collapse='_'),sep='+'),sep='')),scales='free')->graph
    ggplot(dfGraph,aes_string(tsVar,'value',color=paste(groupVars,collapse='_')))+geom_point()+geom_line()+facet_wrap(~variable,scales='free')->graph
    
  }
  
  
  if(groupVars%in%c('1','','NA','NULL')){
    Lst<-list()
    for(i in 1:length(unique(dfGraph$variable))){
      unique(dfGraph$variable)[i]->ii
      subset(dfGraph,variable==ii)->dfGraphi
      which(is.na(dfGraphi[,tsVar]))->ind
      # dfGraphi[-ind,]->dfGraphi
      
      # dfGraphi$floor<-Floor
      names(dfGraphi)[which(names(dfGraphi)==tsVar)]<-'ds'
      names(dfGraphi)[which(names(dfGraphi)=='value')]<-'y'
      ifelse(Cap<0,max(dfGraphi$y,na.rm=T),Cap)->dfGraphi$cap
      ifelse(Floor<0,min(dfGraphi$y,na.rm=T),Floor)->dfGraphi$floor
      prophet(dfGraphi,
              growth=Growth,
              # changepoint.range=changepointRange,
              yearly.seasonality = yearlyS,
              daily.seasonality = dailyS,
              weekly.seasonality = weeklyS)->m
      future<-make_future_dataframe(m,periods=H,freq=stri_replace_last_regex(Period,'[a-z]',''))
      # future$cap<-Cap
      # future$floor<-Floor
      ifelse(Cap<0,max(dfGraphi$y,na.rm=T),Cap)->future$cap
      ifelse(Floor<0,min(dfGraphi$y,na.rm=T),Floor)->future$floor
      predict(m,future)->Pred
      
      prophet:::df_for_plotting(m,Pred)->dfGraphiNew
      
      # names(dfGraphiNew)[1]<-tsVar
      dfGraphiNew[,'variable']<-ii
      as.character(dfGraphiNew$variable)->dfGraphiNew$variable
      Lst[[i]]<-dfGraphiNew
    }
    do.call(rbind,Lst)->dfGraphNew
    
    
    names(dfGraphNew)[1]<-tsVar
    
    dfGraphNew[,c(tsVar,'variable','y')]->predData
    ifelse(is.na(predData$y),dfGraphNew$yhat,predData$y)->predData$y
    if(Growth=='logistic'){
      graphProphet<-ggplot(dfGraphNew,aes_string(tsVar,'y'))+
        geom_point(size=.75,na.rm=T)+geom_line(aes(y=yhat),color='#0072B2',na.rm=T)+
        geom_line(aes(y=floor),linetype='dashed',na.rm=T)+
        geom_line(aes(y=cap),linetype='dashed',na.rm=T)+
        geom_ribbon(aes(ymin=yhat_lower,ymax=yhat_upper),alpha=.2,fill='#0072B2',na.rm=T)+
        facet_wrap(~variable,scale='free')
    } else {
      graphProphet<-ggplot(dfGraphNew,aes_string(tsVar,'y'))+
        geom_point(size=.75,na.rm=T)+geom_line(aes(y=yhat),color='#0072B2',na.rm=T)+
        geom_ribbon(aes(ymin=yhat_lower,ymax=yhat_upper),alpha=.2,fill='#0072B2',na.rm=T)+
        facet_wrap(~variable,scale='free')
    }
    
    
  } else {
    unique(dfGraph$variable)->Var
    unique(dfGraph[,paste(groupVars,collapse='_')])->Grp
    Lst<-list()
    for(i in 1:length(Var)){
      for(j in 1:length(Grp)){
        subset(dfGraph,variable==Var[i]&dfGraph[,paste(groupVars,collapse='_')]==Grp[j])->dfGraphi
        which(is.na(dfGraphi[,tsVar]))->ind
        # dfGraphi[-ind,]->dfGraphi
        # dfGraphi$cap<-Cap
        # dfGraphi$floor<-Floor
        
        names(dfGraphi)[which(names(dfGraphi)==tsVar)]<-'ds'
        names(dfGraphi)[which(names(dfGraphi)=='value')]<-'y'
        ifelse(Cap<0,max(dfGraphi$y,na.rm=T),Cap)->dfGraphi$cap
        ifelse(Floor<0,min(dfGraphi$y,na.rm=T),Floor)->dfGraphi$floor
        prophet(dfGraphi,
                growth=Growth,
                # changepoint.range=changepointRange,
                yearly.seasonality = yearlyS,
                daily.seasonality = dailyS,
                weekly.seasonality = weeklyS)->m
        future<-make_future_dataframe(m,periods=H,freq=stri_replace_last_regex(Period,'[a-z]',''))
        # future$cap<-Cap
        # future$floor<-Floor
        
        ifelse(Cap<0,max(dfGraphi$y,na.rm=T),Cap)->future$cap
        ifelse(Floor<0,min(dfGraphi$y,na.rm=T),Floor)->future$floor
        predict(m,future)->Pred
        
        prophet:::df_for_plotting(m,Pred)->dfGraphiNew
        
        dfGraphiNew[,'variable']<-Var[i]
        dfGraphiNew[,paste(groupVars,collapse='_')]<-Grp[j]
        as.character(dfGraphiNew$variable)->dfGraphiNew$variable
        
        Lst[[(i-1)*length(Var)+j]]<-dfGraphiNew
        
      }
    }
    do.call(rbind,Lst)->dfGraphNew
    names(dfGraphNew)[1]<-tsVar
    dfGraphNew[,c(tsVar,'groupVars','variable','y')]->predData
    ifelse(is.na(predData$y),dfGraphNew$yhat,predData$y)->predData$y
    
    if(Growth=='logistic'){
      graphProphet<-ggplot(dfGraphNew,aes_string(tsVar,'y',color=paste(groupVars,collapse='_')))+
        geom_point(size=.75,na.rm=T)+geom_line(aes(y=yhat),color='#0072B2',na.rm=T)+
        geom_ribbon(aes(ymin=yhat_lower,ymax=yhat_upper),alpha=.2,fill='#0072B2',na.rm=T)+
        geom_line(aes(y=floor),linetype='dashed',na.rm=T)+
        geom_line(aes(y=cap),linetype='dashed',na.rm=T)+
        facet_wrap(as.formula(paste('~',paste('variable',paste(groupVars,collapse='_'),sep='+'))),scale='free')
    } else {
      graphProphet<-ggplot(dfGraphNew,aes_string(tsVar,'y',color=paste(groupVars,collapse='_')))+
        geom_point(size=.75,na.rm=T)+geom_line(aes(y=yhat),color='#0072B2',na.rm=T)+
        geom_ribbon(aes(ymin=yhat_lower,ymax=yhat_upper),alpha=.2,fill='#0072B2',na.rm=T)+
        facet_wrap(as.formula(paste('~',paste('variable',paste(groupVars,collapse='_'),sep='+'))),scale='free')
    }
    
  }
  
  resFinal[,c(ncol(resFinal),1:(ncol(resFinal)-1))]->resFinal
  
  return(list(tabRes=resFinal,graphRes=graph,tabPred=predData,graphPredict=graphProphet))
}
