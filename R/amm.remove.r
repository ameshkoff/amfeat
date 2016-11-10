#' Replace column values
#'
#' Replace column values equal to arguments
#'
#' @param ds data.table: data set
#' @param ds.list character vector of columns name needed to convert
#' @return Character vector of column names
#' @seealso amm.match, amm.gbetween
#' @export

amm.remove <- function(ds
                       , ds.list) {

  ds <- data.table(ds)

  for (i in 1:length(ds.list)) {
    ds[, eval(as.name(ds.list[i])) := NULL]
  }
  ds
}
