#' Reset factor levels
#'
#' Resets factor levels for selected columns of data.table. Particularly important for dropping unused levels
#'
#' @param ds data.table: data set
#' @return Data.table
#' @seealso amm.factor for setting factors for selected columns
#' @export

amm.factorr <- function(ds) {

  ds <- data.table(ds)
  ds.list <- colnames(ds)

  for (i in 1:length(ds.list)) {
    if (is.factor(ds[,eval(as.name(ds.list[i]))])) {
      ds[,eval(as.name(ds.list[i])) := factor(eval(as.name(ds.list[i])))]
    }
  }
  ds
}


#' Set factors
#'
#' Sets factors for selected columns of data.table
#'
#' @param ds data.table: data set
#' @param ds.list character vector of columns name needed to convert
#' @return Data.table
#' @seealso amm.convn for converting without modifying the original data set
#' @export

amm.factor <- function(ds, ds.list) {

  ds <- data.table(ds)

  for (i in 1:length(ds.list)) {
    ds[,eval(as.name(ds.list[i])) := factor(eval(as.name(ds.list[i])))]
  }
  ds
}
