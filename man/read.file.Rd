% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.file.R
\name{read.file}
\alias{read.file}
\title{read files}
\usage{
read.file(file, header = TRUE, sep = ",", dec = ".", ...)
}
\arguments{
\item{file}{File name.}

\item{header}{Whether file has header for varialbes, TRUE(default).}

\item{sep}{Field separator character, ','(default).}

\item{dec}{Decimal points'  character, '.'(default).}

\item{...}{Further arguments to be passed to read.table.}
}
\value{
this returned a data.frame.
}
\description{
\code{read.file} This function read files similar to asreml.read.table().
}
\details{
 files similar to asreml.read.table().
}
\examples{
library(myRPex)
read.example(package = "myRPex", setpath = TRUE)
df<-myRPex::read.file(file="fm.csv", header=TRUE)
names(df)

}
\references{
Yuanzhen Lin. R & ASReml-R Statistics. China Forestry Publishing House. 2016
myRPex website:https://github.com/yzhlinscau/myRPex
}
\author{
Yuanzhen Lin <yzhlinscau@163.com>
}
