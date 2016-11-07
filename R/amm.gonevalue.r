#' Get columns containing only one value
#'
#' Get columns containing only one value (including NAs)
#'
#' @param ds data.table: data set
#' @param ds.list character vector of columns name needed to convert
#' @return Character vector of column names
#' @seealso amm.gonevaluena

amm.gonevalue <- function(ds, ds.list) {

  rtrn <- character(0)

  for (i in length(ds.list)) {

    if(length(unique(ds[, eval(as.name(ds.list[i]))])) == 0) {

      rtrn <- c(rtrn, ds.list[i])

    }

  }
  rtrn
}

#' Get columns containing only one value or NA
#'
#' Get columns containing only one value or NA, NaN, Inf, -Inf
#'
#' @param ds data.table: data set
#' @param ds.list character vector of columns name needed to convert
#' @return Character vector of column names
#' @seealso amm.gonevalue

amm.gonevaluena <- function(ds, ds.list) {

  rtrn <- character(0)

  for (i in length(ds.list)) {

    if(length(unique(ds[!is.na(eval(as.name(ds.list[i]))) & !is.infinite(eval(as.name(ds.list[i]))), eval(as.name(ds.list[i]))])) == 0) {

      rtrn <- c(rtrn, ds.list[i])

    }

  }
  rtrn
}
