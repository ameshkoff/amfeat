#' Get columns containing NA
#'
#' Get columns containing NA values
#'
#' @param ds data.table: data set
#' @param ds.list character vector of columns name needed to convert
#' @return Character vector of column names
#' @seealso amm.gnat

amm.gna <- function(ds, ds.list) {

  rtrn <- character(0)

  for (i in length(ds.list)) {

    if(nrow(ds[is.na(eval(as.name(ds.list[i])))]) > 0) {

      rtrn <- c(rtrn, ds.list[i])
    }

  }
  rtrn
}

#' Get full NA columns
#'
#' Get columns containing only NA values
#'
#' @param ds data.table: data set
#' @param ds.list character vector of columns name needed to convert
#' @return Character vector of column names
#' @seealso amm.gna

amm.gnat <- function(ds, ds.list) {

  rtrn <- character(0)

  for (i in length(ds.list)) {

    if(nrow(ds[is.na(eval(as.name(ds.list[i])))]) == nrow(ds)) {

      rtrn <- c(rtrn, ds.list[i])
    }

  }
  rtrn
}
