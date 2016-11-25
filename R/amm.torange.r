#' Categorical features to range ones
#'
#' Convert categorical features to range ones.
#'
#' @param ds data.table: data set
#' @param target.col character: Y variable for getting right order. If numeric, you can round it to some reasonable value
#' @param ds.list character vector of columns name needed to convert
#' @param ... some other arguments: what ones??
#' @return List of modified data set and modified ... argument
#' @seealso ...
#' @export

amm.torange <- function(ds
                        , target.col
                        , ds.list
                        , ...) {

  ds <- data.table(ds)

  add.ds <- list(...)

  for (i in ds.list) {

    lvls <- ds[, list(ln = length(.I)), by = list(target = eval(as.name(target.col)), vr = eval(as.name(i)))][target == 0, ln := -ln][
      , list(ln = mean(ln, na.rm=TRUE)), by = list(vr)][order(-ln)][, vr]

    ds[,eval(as.name(i)) := as.integer(factor(eval(as.name(i)), levels=lvls))]
    ds[eval(as.name(i)) == 1,eval(as.name(i)) := 0]
    ds[is.na(eval(as.name(i))), eval(as.name(i)) := 0]

    if (length(add.ds) > 0) {
      for (j in 1:length(add.ds)) {
        add.ds[[j]][, eval(as.name(i)) := as.integer(factor(eval(as.name(i)),levels=lvls))]
        add.ds[[j]][eval(as.name(i)) == 1, eval(as.name(i)) := 0]
        add.ds[[j]][is.na(eval(as.name(i))), eval(as.name(i)) := 0]
      }
    }
  }

  list(ds, add.ds)
}
