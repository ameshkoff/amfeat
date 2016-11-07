#' Get columns by match
#'
#' Get column by regexp match. Please send only character columns name as ds.list param
#'
#' @param ds data.table: data set
#' @param ds.list character vector of columns name needed to convert
#' @param ptrn character: pattern to find
#' @return Character vector of column names
#' @seealso amm.gtmatcht

amm.gmatch <- function(ds
                      , ds.list
                      , ptrn) {

  rtrn <- character(0)

  for (i in 1:length(ds.list)) {

    if(length(ds[eval(as.name(ds.list[i])) %like% eval(ptrn), eval(as.name(ds.list[i]))]) > 0) {

      rtrn <- c(rtrn, ds.list[i])

    }
  }
  rtrn
}

#' Get columns by matching all the values
#'
#' Get column by regexp matching all the values of the column. Please send only character columns name as ds.list param
#'
#' @param ds data.table: data set
#' @param ds.list character vector of columns name needed to convert
#' @param ptrn character: pattern to find
#' @param excl character: values to exclude
#' @return Character vector of column names
#' @seealso amm.gmatch

amm.gmatcht <- function(ds
                       , ds.list
                       , excl = ""
                       , ptrn) {

  rtrn <- character(0)

  for (i in 1:length(ds.list)) {

    if(length(ds[eval(as.name(ds.list[i])) %like% eval(ptrn) & !(eval(as.name(ds.list[i])) %in% excl), eval(as.name(ds.list[i]))]) == nrow(ds[!(eval(as.name(ds.list[i])) %in% excl)])) {

      rtrn <- c(rtrn, ds.list[i])

    }
  }
  rtrn
}




