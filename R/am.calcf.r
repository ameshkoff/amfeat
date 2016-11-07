#' Calculate new features
#'
#' Calculate new features based on the vector of specially formatted column pairs
#'
#' @param ds data.table: data set
#' @param ds.list data.table or character vector containing new columns temporary names obtained with am.scor of am.scorfl functions. If in the vector format names should be formatted in this way: oper***feature1_name***feature2_name. For example: mult***f10***f25. Data.table should contain character columns oper, f1, f2.
#' @param verbose logical
#' @return Data.table. rn - new feature name, cr - correlation, f1 - first original feature used, f2 - second original feature used
#' @seealso am.scor, am.scorfl

am.calcf <- function(ds, ds.list, verbose = FALSE) {

  if (!is.data.frame(ds.list)) {
    ds.list <- data.table(orig <- ds.list)
    ds.list[, oper := str_split_fixed(orig, "\\*\\*\\*", 3)[,1]]
    ds.list[, f1 := str_split_fixed(orig, "\\*\\*\\*", 3)[,2]]
    ds.list[, f2 := str_split_fixed(orig, "\\*\\*\\*", 3)[,3]]

  } else {
    ds.list <- data.table(ds.list)
  }

  if (verbose == TRUE) {
    print(paste("columns to create :", nrow(ds.list), ":", Sys.time()))
  }

  for (i in (1:nrow(ds.list))) {

    oper <- ds.list[i, oper]
    var.1 <- ds.list[i, f1]
    var.2 <- ds.list[i, f2]

    var.name <- paste(oper, var.1, var.2, sep = "___")

    if (verbose == TRUE) {
      print(paste(oper, var.1, var.2, var.name))
    }

    if (oper == "mult") {
      ds[,eval(as.name(var.name)) := as.double(eval(as.name(var.1))) * as.double(eval(as.name(var.2)))]
    }
    if (oper == "sum") {
      ds[,eval(as.name(var.name)) := as.double(eval(as.name(var.1))) + as.double(eval(as.name(var.2)))]
    }
    if (oper == "subt") {
      ds[,eval(as.name(var.name)) := as.double(eval(as.name(var.1))) - as.double(eval(as.name(var.2)))]
    }
    if (oper == "dist") {
      ds[,eval(as.name(var.name)) := (as.double(eval(as.name(var.1))) ^ 2 + as.double(eval(as.name(var.2))) ^ 2) ^ .5]
    }
    if (oper == "xor") {
      ds[,eval(as.name(var.name)) := as.numeric(xor(as.numeric(eval(as.name(var.1))), as.numeric(eval(as.name(var.2)))))]
    }
    if (oper == "mean") {
      ds[,eval(as.name(var.name)) := as.double(eval(as.name(var.1))) * .5 + as.double(eval(as.name(var.2))) * .5]
    }
    if (oper == "max") {
      ds[,eval(as.name(var.name)) := mapply(max
                                            , as.double(eval(as.name(var.1)))
                                            , as.double(eval(as.name(var.2))))]
    }
    if (oper == "min") {
      ds[,eval(as.name(var.name)) := mapply(min
                                            , as.double(eval(as.name(var.1)))
                                            , as.double(eval(as.name(var.2))))]
    }
    if (oper == "log") {
      ds[,eval(as.name(var.name)) := log(as.double(eval(as.name(var.1))) + abs(min(as.double(eval(as.name(var.1))))) + 1)]
    }
  }

  if (verbose == TRUE) {
    print(paste("columns created :", nrow(ds.list), ":", Sys.time()))
  }

  ds
}
