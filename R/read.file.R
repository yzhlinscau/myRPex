#' @title read files.
#'
#' @description
#' \code{read.file} This function read files similar to asreml.read.table().
#'
#' @details
#' Read files similar to asreml.read.table().
#' @aliases read.file
#' @param file	 File name.
#' @param header	 Whether file has header for varialbes, TRUE(default).
#' @param sep	 Field separator character, ','(default).
#' @param dec	  Decimal points'  character, '.'(default).
#' @param ...	 Further arguments to be passed to read.table.
#' @return this returned a data.frame.
#' @author Yuanzhen Lin <yzhlinscau@@163.com>
#' @references
#' Yuanzhen Lin. R & ASReml-R Statistics. China Forestry Publishing House. 2016
#' myRPex website:https://github.com/yzhlinscau/myRPex
#' @examples
#' library(myRPex)
#' read.example(package = "myRPex", setpath = TRUE)
#' df<-myRPex::read.file(file="fm.csv", header=TRUE)
#' names(df)
#'
#' @export read.file
#' @export read.example
#' @export fdata

read.file<-function(file,header=TRUE,sep=',',dec='.',...){
  df<-read.table(file=file,header=header,sep=sep,dec=dec,...)
  aa<-names(df)

  sn<-grep('^[A-Z]{1}',aa)
  for(i in sn) df[,i]<-factor(df[,i])

  return(df)
}

fdata<-function(data,faS=NULL){
  data<-as.data.frame(data)
  if(is.null(faS)){
    aa<-names(data)

    sn<-grep('^[A-Z]{1}',aa)
  } else sn<-faS

  for(i in sn) data[,i]<-factor(data[,i])

  return(data)
}

read.example <- function(package,setpath = FALSE) {
  if (setpath== FALSE) {
    dir(system.file("extdata", package = package))
  } else {
    path<-system.file("extdata", package = package)
    setwd(path)
  }
}

