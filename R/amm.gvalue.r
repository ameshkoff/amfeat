#' Get column by value
#'
#' Get column by value (using strict type equality)
#'
#' @param ds data.table: data set
#' @param ds.list character vector of columns name needed to convert
#' @param value: value in one of atomic types
#' @return Character vector of column names
#' @seealso amm.match, amm.gbetween
#' @export

amm.gvalue <- function(ds
                       , ds.list
                       , value) {

  rtrn <- character(0)

  for (i in 1:length(ds.list)) {

    if(length(ds[, sapply(eval(as.name(ds.list[i])), identical, eval(value))]) > 0) {
      rtrn <- c(rtrn, ds.list[i])
    }
  }

  rtrn
}
