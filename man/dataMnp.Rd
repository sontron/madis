% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataMnp.R
\name{dataMnp}
\alias{dataMnp}
\title{dataMnp}
\usage{
dataMnp(data, subset = NA, newVars = NA, newVarsFormulas = NA,
  newVarsBy = NA, indexNames, Formulas, dimVars, dimNames = NA,
  dateVar = NA, dtOrders = "ymd", margin = 0, revisedNames = NA,
  revisedFormulas = NA, revisedMargin = 0, orderVars = NA,
  orders = NA, Digits = 2, tbVars = NA, hbVars = NA,
  colOrder = NA)
}
\arguments{
\item{data}{data.frame}

\item{subset}{character string could be parsed as a logical expression or NA, eg. subset="sex==1"}

\item{newVars}{new vars names, could be character string or NA, eg. newVars='ageNew'}

\item{newVarsFormulas}{character string could be parsed as formulas or NA, eg. "sum(age)"}

\item{newVarsBy}{groupby var name or NA}

\item{indexNames}{variable names generated in the jth procedre as data.table.}

\item{Formulas}{character string could be parsed as expression, and seperated with ";" eg. "mean(disp);sum(disp)"}

\item{dimVars}{group by variable names, same as k in data.table}

\item{dimNames}{rename the dimVars}

\item{dateVar}{set the dateVar in the data.frame if any}

\item{dtOrders}{set the date variable format, could be 'ymd', 'mdy', 'ymd hms', please refer to lubridate::parse_date_time for more detail.}

\item{margin}{calculate margin results. 0 means no margin results should calculated, 1 for the first 1 dimVars, 2 for the first 2 dimVars, etc.}

\item{revisedNames}{new variable name generated using the results}

\item{revisedFormulas}{formulas corresponding to revisedNames}

\item{revisedMargin}{same as margin but only for revised vars}

\item{orderVars}{please refer to data.table::setorderv for more detail.}

\item{orders}{please refer to data.table::setorderv for more detail.}

\item{Digits}{set digits}

\item{tbVars}{vars needed to calculate tongbi(chinese, means Year over Year) index.}

\item{hbVars}{vars needed to calculate huanbi(chinese, means month over month, quater over quater, etc.)}

\item{colOrder}{reorder column orders.}
}
\description{
a function provides data manipulating procedures, mainly based on data.table.
}
\examples{
expand.grid(2017:2018,1:12,1:28)->X
apply(X,1,function(x)paste(x,collapse='-'))->date
dt<-data.frame(ts=date,grp1=sample(letters[1:5],length(date),rep=T),y=rnorm(length(date)))
dataMnp(data=dt,indexNames=c('a','b','c'),Formulas='mean(y);sd(y);sd(y)/mean(y)',dimVars='year(ts);month(ts);grp1',dimNames = 'year;month;grp',dateVar = 'ts',dtOrders = 'ymd',orderVars = 'grp',orders='1',tbVars='a;b',hbVars=c('c','tt'),revisedNames = 'tt',revisedFormulas = 'a+b',colOrder='1;2;3;4;5;6;8;9')->res

# generate new variable 
dataMnp(data=mtcars,subset='vs==1',newVars='test',newVarsFormulas='mpg+disp',indexNames='sumMPG',Formulas='sum(mpg)',dimVars='gear',dimNames='VS')

# revise the result using revisedNames, revisedFormulas,revisedMargin args. it's same as DT[i,j,k][c('revisedvars'):=list(...),by=.(...)]
# the revisedMargin means groupby which dimVars, eg. 1 means the first 1 dimVars, and 2 means the first 2 dimVars, 0 means no groupby variables.

dataMnp(data=mtcars,newVarsFormulas='mpg+disp',indexNames='sumMPG',Formulas='sum(mpg)',dimVars='vs;gear',dimNames='VS;Gear',revisedNames='x',revisedFormulas='sum(sumMPG)',revisedMargin=o)
dataMnp(data=mtcars,newVarsFormulas='mpg+disp',indexNames='sumMPG',Formulas='sum(mpg)',dimVars='vs;gear',dimNames='VS;Gear',revisedNames='x',revisedFormulas='sum(sumMPG)',revisedMargin=1)
dataMnp(data=mtcars,newVarsFormulas='mpg+disp',indexNames='sumMPG',Formulas='sum(mpg)',dimVars='vs;gear',dimNames='VS;Gear',revisedNames='x',revisedFormulas='sum(sumMPG)',revisedMargin=2)


}
