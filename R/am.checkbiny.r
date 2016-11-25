#' Get correlation between predicted Y variable and residuals splitted per binary variables
#'
#' Imagine we want to test if residuals when c1 variable == 1 have stronger correlation to predicted variable than when c1 == 0.
#' If it is the case, use this functions.
#'
#' @param ds data.table: data set. Must include binary variables we want to test, residuals and the predicted Y variable
#' @param ds.list character vector: names of independent binary variables (X)
#' @param ds.y character: name of the predicted dependent variable (Y)
#' @param ds.resid character: name of the residuals variable
#' @param corr.type character: correlation types; "spearman" (default) and "pearson" available; see Hmisc rcorr for details
#' @return Data.table. rn - feature name, cr - correlation value, prc - per cent of of the whole dataset, vl - value of splitting variable
#' @seealso ...
#' @import data.table
#' @export

am.checbkbiny <- function(ds
                         , ds.list
                         , ds.y
                         , ds.resid
                         , corr.type = "spearman") {

  cl<-makeCluster(detectCores()-3)
  registerDoParallel(cl)

  print(paste("begin : total main steps quantity: ",length(ds.list),sep=""))
  rtrn <- foreach (i=1:length(ds.list), .packages=c("Hmisc","data.table"), .verbose=FALSE, .combine=rbind) %dopar% {
    tmp<-list(ds.list[i]
              , rcorr(x = ds[eval(as.name(ds.list[i])) == 1, eval(as.name(ds.resid))]
                      , y = ds[eval(as.name(ds.list[i])) == 1, eval(as.name(ds.y))], type=eval(corr.type))$r[1,2]
              , nrow(ds[eval(as.name(ds.list[i])) == 1]) / nrow(ds)
              , "1")
  }

  rtrn0 <- foreach (i=1:length(ds.list), .packages=c("Hmisc","data.table"), .verbose=FALSE, .combine=rbind) %dopar% {
    tmp<-list(ds.list[i]
              , rcorr(x = ds[eval(as.name(ds.list[i])) == 0, eval(as.name(ds.resid))]
                      , y = ds[eval(as.name(ds.list[i])) == 0, eval(as.name(ds.y))], type=eval(corr.type))$r[1,2]
              , nrow(ds[eval(as.name(ds.list[i])) == 0]) / nrow(ds)
              , "0")
  }

  print(paste("end : total main steps quantity: ",length(ds.list),sep=""))
  rtrn <- data.table(rtrn)
  rtrn0 <- data.table(rtrn0)

  rtrn <- rbind(rtrn, rtrn0)

  rtrn[, V2 := as.numeric(V2)]

  setnames(rtrn, c("V1","V2","V3","V4"),c("rn","cr","prc","vl"))
  rtrn[, rn := unlist(rn)]

  gc()

  stopCluster(cl)
  rtrn
}








