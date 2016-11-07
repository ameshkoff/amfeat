#' Get columns by type
#'
#' Gets columns of data.table by column type
#'
#' @param ds data.table: data set
#' @param fnct character: "is" function name: is.character, is.factor, is.numeric etc.
#' @return Character vector of column names
#' @seealso amm.gmatch

amm.gtypes <- function(ds
                       , fnct) {
  rtrn <- character(0)
  for (i in 1:ncol(ds)) {

    chck <- eval(as.name(fnct))(ds[, i, with = FALSE])
    if (chck == TRUE) {
      rtrn <- c(rtrn, colnames(ds)[i])
    }
  }
  rtrn
}
