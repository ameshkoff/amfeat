#' Correlations between dependent variable (Y) and every independent variable
#'
#' Define pairwise correlations between the dependant variable (Y) and every independent variable (Xs). Be careful! Parallel computation in use.
#'
#' @param ds data.table: data set
#' @param ds.list character vector: names of independent variables (X)
#' @param ds.y character: name of the dependent variable (Y)
#' @param corr.type character: correlation types; "spearman" (default) and "pearson" available; see Hmisc rcorr for details
#' @param cl.number number: cluster number for parallel computers; be very careful with this parameter! do not set it too big
#' @param out.file character: absolute or relative path to the files with output; due to parallel computations you do NOT see most of the info in the console; id default no outputs are used
#' @return Data.table. rn - feature name, cr - correlation value
#' @seealso You can use this data to choose and create new features you prefer with am.calcf function
#' @import data.table
#' @import foreach
#' @import doParallel
#' @import stringr
#' @import Hmisc
#' @export

am.checky <- function(ds
                      , ds.list
                      , ds.y
                      , corr.type = "spearman"
                      , cl.number = 1
                      , out.file = "") {

  if (out.file == "") {
    cl <- makeCluster(cl.number)
  }
  else {
    cl <- makeCluster(cl.number, outfile=eval(out.file))
  }

  registerDoParallel(cl)


  print(paste("start calculating correlations to Y :", length(ds.list), "steps", sep=" "))

  rtrn <- foreach (i=1:length(ds.list), .packages=c("Hmisc","data.table"), .verbose=FALSE, .combine=rbind) %dopar% {
    tmp<-list(ds.list[i], rcorr(x = ds[, eval(as.name(ds.list[i]))], y = ds[, eval(as.name(ds.y))], type=eval(corr.type))$r[1,2])
  }

  print(paste("end calculating correlations to Y :", length(ds.list), "steps", sep=" "))

  rtrn <- data.table(rtrn)
  rtrn[, V2 := as.numeric(V2)]

  setnames(rtrn, c("V1","V2"), c("rn","cr"))
  rtrn[, rn := unlist(rn)]

  gc()

  stopCluster(cl)
  rtrn
}

.datatable.aware = TRUE

