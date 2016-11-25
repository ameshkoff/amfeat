#' Search for new correlations
#'
#' Search for new correlations based ot original feature pairs: all types in one step. Be careful! Parallel computation in use.
#'
#' @param ds data.table: data set
#' @param ds.list character vector: names of independent variables (X)
#' @param ds.y character: name of the dependent variable (Y)
#' @param corr.type character: correlation types; "spearman" (default) and "pearson" available; see Hmisc rcorr for details
#' @param treshold.v number: how much correlation from the new feature to Y should be more than the lesser from the original ones; default is 1.1 = 10 per cent more
#' @param treshold.a number: the minimal correlation from the new feature to Y; .05 default
#' @param cl.number number: cluster number for parallel computers; be very careful with this parameter! do not set it too big value
#' @param out.file character: absolute or relative path to the files with output; due to parallel computations you do NOT see most of the info in the console; id default no outputs are used
#' @return Data.table. rn - new feature name, cr - correlation, f1 - first original feature used, f2 - second original feature used
#' @seealso You can use this data to choose and create new features you prefer with am.calcf function
#' @export

am.scor <- function(ds
                     , ds.list
                     , ds.y
                     , corr.type = "spearman"
                     , treshold.v = 1.1
                     , treshold.a = .05
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
    tmp <- list(ds.list[i], rcorr(x = ds[,eval(as.name(ds.list[i]))], y = ds[,eval(as.name(ds.y))], type=eval(corr.type))$r[1,2])
  }

  print(paste("end calculating correlations to Y :", length(ds.list), "steps", sep=" "))
  rtrn <- data.table(rtrn)
  rtrn[, V2:=as.numeric(V2)]

  setnames(rtrn, c("V1","V2"), c("rn","cr"))
  rtrn[, rn:=unlist(rn)]

  # remove features with not defined correlations
  rtrn <- rtrn[!is.na(cr)]

  gc()

  print(paste("start searching new features :", length(ds.list), "original features; start time : ", Sys.time(), sep=" "))

  rtrn1 <- foreach (i=1:length(ds.list), .packages=c("Hmisc","data.table"), .verbose=TRUE, .combine=rbind) %dopar% {

    tmp.r <- data.table(rn = character(), cr = double(), f1 = character(), f2 = character())

    print(paste("column :", i, ds.list[i], as.character(Sys.time())))

    for (j in (i+1):length(ds.list)) {

      if (i < length(ds.list)) {

        tmp<-data.table(rn = "sum"
                        , cr = rcorr(x = ds[,eval(as.name(ds.list[i])) + eval(as.name(ds.list[j]))],
                                     y = ds[,eval(as.name(ds.y))], type=eval(corr.type))$r[1,2]
                        , f1 = ds.list[i]
                        , f2 = ds.list[j])
        tmp<-rbind(tmp, list(rn = "mult"
                             , cr = rcorr(x = ds[,as.double(eval(as.name(ds.list[i]))) * as.double(eval(as.name(ds.list[j])))]
                                          , y = ds[,eval(as.name(ds.y))], type=eval(corr.type))$r[1,2]
                             , f1 = ds.list[i]
                             , f2 = ds.list[j]))
        tmp<-rbind(tmp, list(rn = "subt"
                             , cr = rcorr(x = ds[,as.double(eval(as.name(ds.list[i]))) - as.double(eval(as.name(ds.list[j])))],
                                          y = ds[,eval(as.name(ds.y))], type=eval(corr.type))$r[1,2]
                             , f1 = ds.list[i]
                             , f2 = ds.list[j]))
        tmp<-rbind(tmp, list(rn = "subt"
                             , cr = rcorr(x = ds[,as.double(eval(as.name(ds.list[j]))) - as.double(eval(as.name(ds.list[i])))],
                                          y = ds[,eval(as.name(ds.y))], type=eval(corr.type))$r[1,2]
                             , f1 = ds.list[j]
                             , f2 = ds.list[i]))
        tmp<-rbind(tmp, list(rn = "dist"
                             , cr = rcorr(x = ds[, (as.double(eval(as.name(ds.list[j]))) ^ 2 + as.double(eval(as.name(ds.list[i]))) ^ 2) ^ .5],
                                          y = ds[, eval(as.name(ds.y))], type=eval(corr.type))$r[1,2]
                             , f1 = ds.list[j]
                             , f2 = ds.list[i]))
        tmp<-rbind(tmp, list(rn = "xor"
                             , cr = rcorr(x = as.numeric(ds[, xor(as.numeric(eval(as.name(ds.list[i]))), as.numeric(eval(as.name(ds.list[j]))))]),
                                          y = ds[, eval(as.name(ds.y))], type=eval(corr.type))$r[1,2]
                             , f1 = ds.list[i]
                             , f2 = ds.list[j]))

        if (j == i+1) {
          tmp<-rbind(tmp, list(rn = "log"
                               , cr = rcorr(x = ds[, log(as.double(eval(as.name(ds.list[i]))) + abs(min(as.double(eval(as.name(ds.list[i]))))) + 1)],
                                            y = ds[, eval(as.name(ds.y))], type=eval(corr.type))$r[1,2]
                               , f1 = ds.list[i]
                               , f2 = NA))
        }

        tmp<-tmp[!is.na(cr) & !is.nan(cr)]

        # check if the table of new correlations is empty
        if (nrow(tmp) > 0) {

          # some correlations may be negative
          tmp <- tmp[abs(cr) == max(abs(cr))]


          # check if the new correlations are important and are clearly more important than original ones
          tmp <- tmp[(cr / rtrn[rn == ds.list[i], cr]) > eval(treshold.v)
                     & ((cr / rtrn[rn == ds.list[j], cr]) > eval(treshold.v) | is.na(f2))
                     & cr > eval(treshold.a)
                     & !is.na(cr)]

          if (nrow(tmp) > 0) {
            tmp <- tmp[, .SD[1], by = list(cr)]
            tmp.r <- rbind(tmp.r, tmp[, list(rn = paste(rn, f1, f2, sep = "***"), cr, f1, f2)])
          }
        }
      }
    }
    tmp.r
  }
  print(paste("end searching new features :", length(ds.list), "original features; end time : ", Sys.time(), sep=" "))

  rtrn1 <- data.table(rtrn1)

  rtrn <- rbind(rtrn, rtrn1, fill = TRUE)

  stopCluster(cl)
  rtrn
}
