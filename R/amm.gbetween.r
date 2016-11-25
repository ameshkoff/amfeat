#' Get numeric columns between min and max
#'
#' Get numeric columns contained all values between min and max. Please send only numeric columns name as ds.list param
#'
#' @param ds data.table: data set
#' @param ds.list character vector of columns name needed to convert
#' @param excl numeric: values to exclude. NA and NaN are also possible
#' @param min.v numeric: min value
#' @param max.v numeric: max.value
#' @param ptrn character: pattern to narrow list of column names to apply function
#' @return Character vector of column names
#' @seealso amm.gbetweens
#' @export

amm.gbetween <- function(ds
                        , ds.list
                        , excl = ""
                        , min.v = 0
                        , max.v = 1
                        , ptrn = "") {

  rtrn <- character(0)

  for (i in 1:length(ds.list)) {

    if(ds[eval(as.name(ds.list[i])) %like% eval(ptrn) & !(eval(as.name(ds.list[i])) %in% excl), min(eval(as.name(ds.list[i])))] > eval(min.v) &
       ds[eval(as.name(ds.list[i])) %like% eval(ptrn) & !(eval(as.name(ds.list[i])) %in% excl), max(eval(as.name(ds.list[i])))] < eval(max.v)) {

      rtrn <- c(rtrn, ds.list[i])

    }
  }
  rtrn
}


#' Get numeric columns strict between min and max
#'
#' Get numeric columns with max value equal to max argument and so min. Please send only numeric columns name as ds.list param
#'
#' @param ds data.table: data set
#' @param ds.list character vector of columns name needed to convert
#' @param excl numeric: values to exclude. NA and NaN are also possible
#' @param min.v numeric: min value
#' @param max.v numeric: max.value
#' @param ptrn character: pattern to narrow list of column names to apply function
#' @return Character vector of column names
#' @seealso amm.gbetween
#' @export

amm.gbetweens <- function(ds
                         , ds.list
                         , excl = ""
                         , min.v = 0
                         , max.v = 1
                         , ptrn) {

  rtrn <- character(0)

  for (i in 1:length(ds.list)) {

    if(ds[eval(as.name(ds.list[i])) %like% eval(ptrn) & !(eval(as.name(ds.list[i])) %in% excl), min(eval(as.name(ds.list[i])))] == eval(min.v) &
       ds[eval(as.name(ds.list[i])) %like% eval(ptrn) & !(eval(as.name(ds.list[i])) %in% excl), max(eval(as.name(ds.list[i])))] == eval(max.v)) {

      rtrn <- c(rtrn, ds.list[i])

    }
  }
  rtrn
}

