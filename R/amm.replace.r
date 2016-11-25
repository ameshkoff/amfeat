#' Replace column values
#'
#' Replace column values equal to arguments
#'
#' @param ds data.table: data set
#' @param ds.list character vector of columns name needed to convert
#' @param ds.string character vector: value to replace
#' @param ds.replace character: replacing character
#' @return Character vector of column names
#' @seealso amm.match, amm.gbetween
#' @export

amm.replace <- function(ds
                        , ds.list
                        , ds.string
                        , ds.replace) {

  ds <- data.table(ds)

  for (i in 1:length(ds.list)) {

    if (is.factor(ds[,eval(as.name(ds.list[i]))])) {

      ds[eval(as.name(ds.list[i])) %in% ds.string, eval(as.name(ds.list[i])) := as.factor(ds.replace)]

    } else {

      ds[eval(as.name(ds.list[i])) %in% ds.string, eval(as.name(ds.list[i])) := ds.replace]

    }
  }
  ds
}

#' Replace columns by match
#'
#' Replace columns by matching. Replaces the whole value
#'
#' @param ds data.table: data set
#' @param ds.list character vector of columns name needed to convert
#' @param ds.string character: value to replace
#' @param ds.replace character: replacing character
#' @return Character vector of column names
#' @seealso amm.match, amm.gbetween
#' @export

amm.replacem <- function(ds
                        , ds.list
                        , ds.string
                        , ds.replace) {

  ds <- data.table(ds)

  for (i in 1:length(ds.list)) {

    if (is.factor(ds[,eval(as.name(ds.list[i]))])) {

      ds[eval(as.name(ds.list[i])) %like% ds.string, eval(as.name(ds.list[i])) := as.factor(ds.replace)]

    } else {

      ds[eval(as.name(ds.list[i])) %like% ds.string, eval(as.name(ds.list[i])) := ds.replace]

    }
  }
  ds
}

#' Replace pattern in column by match
#'
#' Replace pattern in column by matching. Replaces only pattern, not the whole value
#'
#' @param ds data.table: data set
#' @param ds.list character vector of columns name needed to convert
#' @param ds.string character: value to replace
#' @param ds.replace character: replacing character
#' @return Character vector of column names
#' @seealso amm.match, amm.gbetween
#' @export

amm.replacemp <- function(ds
                         , ds.list
                         , ds.string
                         , ds.replace) {

  ds <- data.table(ds)

  for (i in 1:length(ds.list)) {

    if (is.factor(ds[,eval(as.name(ds.list[i]))])) {

      ds[eval(as.name(ds.list[i])) %like% ds.string, eval(as.name(ds.list[i])) :=
           as.factor(str_replace_all(eval(as.name(ds.list[i])), ds.string, ds.replace))]

    } else {

      ds[eval(as.name(ds.list[i])) %like% ds.string, eval(as.name(ds.list[i])) := str_replace_all(eval(as.name(ds.list[i])), ds.string, ds.replace)]

    }
  }
  ds
}

