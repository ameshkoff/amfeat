#' Convert columns to appropriate type (pre link)
#'
#' Convert columns to appropriate type per link. Needs less memory, modifies the original data set
#'
#' @param ds data.table: data set
#' @param ds.list character vector of columns name needed to convert
#' @return Data.table
#' @seealso amm.conv for converting without modifying the original data set
#' @export

amm.convl <- function(ds, ds.list) {
  for (i in 1:length(ds.list)) {

    col.name<-ds.list[i]
    ds[,eval(as.name(col.name)) := type.convert(as.character(eval(as.name(col.name))))]

  }
  ds
}

#' Convert columns to appropriate type
#'
#' Convert columns to appropriate type. Needs more memory, does not modify the original data set
#'
#' @param ds data.table: data set
#' @param ds.list character vector of columns name needed to convert
#' @return Data.table
#' @seealso amm.convl for more memory efficient converting
#' @export

amm.conv <- function(ds, ds.list) {

  ds <- data.table(ds)

  for (i in 1:length(ds.list)) {

    col.name<-ds.list[i]
    ds[,eval(as.name(col.name)) := type.convert(as.character(eval(as.name(col.name))))]

  }
  ds
}
