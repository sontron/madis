% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prophetS.R
\name{prophetS}
\alias{prophetS}
\title{prophetS}
\usage{
prophetS(data, tsVar, tsFormat = "ymd", measureVars, groupVars = 1,
  Period = "weeks", FN, Cap = -1, Floor = -1, Growth = "linear",
  H = 10, yearlyS = "auto", dailyS = "auto", weeklyS = "auto")
}
\arguments{
\item{data}{data.frame}

\item{tsVar}{ts variable}

\item{tsFormat}{could be 'ymd','ym','y','mdy','ymd hms', etc.}

\item{measureVars}{measurement variables in data.}

\item{groupVars}{grouping variables in data}

\item{Period}{could be 'years','quarters','months','weeks','days','hours','minutes','seconds',etc.}

\item{FN}{function eg: 'mean','function(x)sum(x,na.rm=T)',etc.}

\item{Cap}{same as cap in prophet.}

\item{Floor}{same as floor in prophet.}

\item{Growth}{same as growth in prophet.}

\item{H}{same as h in prophet.}

\item{yearlyS}{TRUE,FALSE or "auto", same as yearly.seasonality in prophet.}

\item{dailyS}{TRUE,FALSE or "auto", same as daily.seasonality in prophet.}

\item{weeklyS}{TRUE,FALSE or "auto", same as weekly.seasonality in prophet.}
}
\description{
is a revised version of prophet that combine some other functions from xts package for dealing data.frame, multiple variables and adding group variables. 
return the history data and graphs as well as predicted graph using prophet.
}
