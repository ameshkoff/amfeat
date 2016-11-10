#' Autocorrelations between independent variables
#'
#' Define pairwise correlations between independent variables. Use sns argument to define what correlation you want to test. Be careful! Parallel computation in use.
#'
#' @param ds data.table: data set
#' @param corr.type character: correlation types; "spearman" (default) and "pearson" available; see Hmisc rcorr for details
#' @param cl.number number: cluster number for parallel computers; be very careful with this parameter! do not set it too big
#' @param sns number: Sensitivity. How strong should be probable intersectons between two features to test autocorrelations
#' @param out.file character: absolute or relative path to the files with output; due to parallel computations you do NOT see most of the info in the console; id default no outputs are used
#' @return Data.table. rn - feature name, cr - correlation value
#' @seealso You can use this data to choose and create new features you prefer with am.calcf function
#' @export

am.checkx <- function(ds
                      , ds.list
                      , corr.type = "spearman"
                      , cl.number = 1
                      , sns = .5
                      , out.file = "") {

  arg.unlst <- function(dtr) {

    dta <- unlist(str_split(str_replace_all(str_replace_all(dtr, "sum___", "sumSUM___"), "mult___", "multMULT___"), "(mult|sum)"))
    dta <- unlist(str_split(str_replace_all(str_replace_all(dtr, "log___", "logLOG___"), "dist___", "distDIST___"), "(log|dist)"))
    dta <- unlist(str_split(str_replace_all(str_replace_all(dtr, "subt___", "subtSUBT___"), "xor___", "xorXOR___"), "(xor|subt)"))
    dta <- unlist(str_split(str_replace_all(str_replace_all(dtr, "mean___", "meanMEAN___"), "oor___", "oorOOR___"), "(oor|mean)"))
    dta <- unlist(str_split(str_replace_all(str_replace_all(dtr, "max___", "maxMAX___"), "min___", "minMIN___"), "(max|min)"))
    dta <- dta[dta != ""]
    dta <- str_replace_all(dta, "SUM___", "sum___")
    dta <- str_replace_all(dta, "MULT___", "mult___")
    dta <- str_replace_all(dta, "SUBT___", "subt___")
    dta <- str_replace_all(dta, "LOG___", "log___")
    dta <- str_replace_all(dta, "DIST___", "dist___")
    dta <- str_replace_all(dta, "XOR___", "xor___")
    dta <- str_replace_all(dta, "MEAN___", "mean___")
    dta <- str_replace_all(dta, "MAX___", "max___")
    dta <- str_replace_all(dta, "MIN___", "min___")
    dta <- str_replace_all(dta, "___$", "")

    dta2 <- unlist(str_split(dtr, "___"))

    dta <- unique(c(dta, dta2))
    remove(dta2)

    dta <- dta[!(dta %in% c("sum", "mult", "dist", "xor", "subt", "mean", "max", "min", "log"))]
    dta <- dta[!(str_count(dta, "___") == 1)]

    dta
  }

  ds.list <- data.table(cl = ds.list)
  ds.list[, cl.b := sapply(cl, arg.unlst)]

  if (out.file == "") {
    cl <- makeCluster(cl.number)
  }
  else {
    cl <- makeCluster(cl.number, outfile=eval(out.file))
  }

    registerDoParallel(cl)

    print(paste("start testing autocorrelations :", length(ds.list), "features", sep=" "))

    rtrn <- foreach (i=1:nrow(ds.list), .packages=c("Hmisc","data.table","stringr"), .verbose=FALSE, .combine=rbind) %dopar% {

    tmp.r <- data.table(f1 = character(), f2 = character(), cr = double())

    c.shorter.pr <- function(c1, c2) {
      rtrn.w <- c1
      if (length(c1) > length(c2)) rtrn.w <- c2
      rtrn.w
    }

    print(paste("autocorrelations test colnumber :", i, "colname : ", ds.list[i, cl]))

    for (l in (i+1):nrow(ds.list)) {

      if (i < nrow(ds.list)) {

        arg.1 <- unlist(ds.list[i, cl.b])
        arg.2 <- unlist(ds.list[l, cl.b])
        res <- length(intersect(arg.1, arg.2))
        chck <- res / length(c.shorter.pr(arg.1, arg.2))

        if (chck > sns) {

          tmp.r <- rbind(tmp.r
                         , list(ds.list[i, cl]
                                , ds.list[l, cl]
                                , rcorr(x = ds[,eval(as.name(ds.list[i, cl]))], y = ds[,eval(as.name(ds.list[l, cl]))], type=eval(corr.type))$r[1,2]))

        }
      }
    }
    tmp.r
  }

  print(paste("end testing autocorrelations :", length(ds.list), "features", sep=" "))
  rtrn<-data.table(rtrn)

  gc()

  stopCluster(cl)
  rtrn
}
