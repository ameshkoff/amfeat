#' Flexible search for new correlations
#'
#' Search for new correlations based ot original feature pairs: choose new feature types you want to apply. Be careful! Parallel computation in use.
#'
#' @param ds data.table: data set
#' @param ds.list character vector: names of independent variables (X)
#' @param ds.y character: name of the dependent variable (Y)
#' @param corr.type character: correlation types; "spearman" (default) and "pearson" available; see Hmisc rcorr for details
#' @param threshold.v number: how much correlation from the new feature to Y should be more than the lesser from the original ones; default is 1.1 = 10 per cent more
#' @param threshold.a number: the minimal correlation from the new feature to Y; .05 default
#' @param oper character vector: one or more operations you want to apply with the feature pairs
#' @param cl.number number: cluster number for parallel computers; be very careful with this parameter! do not set it too big value
#' @param out.file character: absolute or relative path to the files with output; due to parallel computations you do NOT see most of the info in the console; id default no outputs are used
#' @return Data.table. rn - new feature name, cr - correlation, f1 - first original feature used, f2 - second original feature used
#' @details Details info
#' \itemize{
#'  \item{"oper"}{You can use following operations: "sum", "mult", "subt", "dist", "mean", "min", "max", "xor", "log", "let". Most of them are self describing. "Let" supposed you use encoded letter ranges beginning with A as 1 * 27 ^ -1, B as 2 * 27 ^ -1, AA as 1 * 27 ^ 0 and so forth.}
#' }
#' @seealso You can use this data to choose and create new features you prefer with am.calcf function
#' @export

am.scorfl <- function(ds
                      , ds.list
                      , ds.y
                      , corr.type = "spearman"
                      , threshold.v = 1.1
                      , threshold.a = .05
                      , oper = c("sum", "mult", "subt", "dist", "mean", "min", "max", "xor", "log", "let")
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


  # sums

  if (length(intersect(oper, "sum")) > 0) {

    print(paste("begin sum : orginal features: ",length(ds.list)," : ",Sys.time(),sep=""))

    rtrn1 <- foreach (i=1:length(ds.list), .packages=c("Hmisc","data.table"), .verbose=FALSE, .combine=rbind) %dopar% {

      tmp.r <- data.table(rn = character(), cr = double(), f1 = character(), f2 = character())

      print(paste("column sum :", i, ds.list[i], as.character(Sys.time())))

      for (j in (i+1):length(ds.list)) {

        if (i < length(ds.list)) {

          tmp<-data.table(rn = "sum"
                          , cr = rcorr(x = ds[,eval(as.name(ds.list[i])) + eval(as.name(ds.list[j]))],
                                       y = ds[,eval(as.name(ds.y))], type=eval(corr.type))$r[1,2]
                          , f1 = ds.list[i]
                          , f2 = ds.list[j])

          tmp<-tmp[!is.na(cr) & !is.nan(cr)]

          # check if the table of new correlations is empty
          if (nrow(tmp) > 0) {

            # some correlations may be negative
            tmp <- tmp[abs(cr) == max(abs(cr))]


            # check if the new correlations are important and are clearly more important than original ones
            tmp <- tmp[(cr / rtrn[rn == ds.list[i], cr]) > eval(threshold.v)
                       & (cr / rtrn[rn == ds.list[j], cr]) > eval(threshold.v)
                       & cr > eval(threshold.a)
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
    print(paste("end sum : orginal features: ",length(ds.list)," : ",Sys.time(),sep=""))

    rtrn1 <- data.table(rtrn1)
    # rtrn1[, cr := as.numeric(cr)]

    rtrn <- rbind(rtrn, rtrn1, fill = TRUE)
  }


  # mults

  if (length(intersect(oper, "mult")) > 0) {

    print(paste("begin mult : orginal features: ",length(ds.list)," : ",Sys.time(),sep=""))

    rtrn1 <- foreach (i=1:length(ds.list), .packages=c("Hmisc","data.table"), .verbose=FALSE, .combine=rbind) %dopar% {

      tmp.r <- data.table(rn = character(), cr = double(), f1 = character(), f2 = character())

      print(paste("column mult :", i, ds.list[i], as.character(Sys.time())))

      for (j in (i+1):length(ds.list)) {

        if (i < length(ds.list)) {

          tmp<-data.table(rn = "mult"
                          , cr = rcorr(x = ds[,as.double(eval(as.name(ds.list[i]))) * as.double(eval(as.name(ds.list[j])))]
                                       , y = ds[,eval(as.name(ds.y))], type=eval(corr.type))$r[1,2]
                          , f1 = ds.list[i]
                          , f2 = ds.list[j])

          tmp<-tmp[!is.na(cr) & !is.nan(cr)]

          # check if the table of new correlations is empty
          if (nrow(tmp) > 0) {

            # some correlations may be negative
            tmp <- tmp[abs(cr) == max(abs(cr))]


            # check if the new correlations are important and are clearly more important than original ones
            tmp <- tmp[(cr / rtrn[rn == ds.list[i], cr]) > eval(threshold.v)
                       & (cr / rtrn[rn == ds.list[j], cr]) > eval(threshold.v)
                       & cr > eval(threshold.a)
                       & !is.na(cr)]

            if (nrow(tmp) > 0) {
              tmp.r <- rbind(tmp.r, tmp[, list(rn = paste(rn, f1, f2, sep = "***"), cr, f1, f2)])
            }
          }
        }
      }
      tmp.r
    }
    print(paste("end mult : orginal features: ",length(ds.list)," : ",Sys.time(),sep=""))

    rtrn1 <- data.table(rtrn1)
    # rtrn1[, cr := as.numeric(cr)]

    rtrn <- rbind(rtrn, rtrn1, fill = TRUE)
  }


  # subts

  if (length(intersect(oper, "subt")) > 0) {

    print(paste("begin subt : orginal features: ",length(ds.list)," : ",Sys.time(),sep=""))

    rtrn1 <- foreach (i=1:length(ds.list), .packages=c("Hmisc","data.table"), .verbose=FALSE, .combine=rbind) %dopar% {

      tmp.r <- data.table(rn = character(), cr = double(), f1 = character(), f2 = character())

      print(paste("column subt :", i, ds.list[i], as.character(Sys.time())))

      for (j in (i+1):length(ds.list)) {

        if (i < length(ds.list)) {

          tmp<-data.table(rn = "subt"
                          , cr = rcorr(x = ds[,as.double(eval(as.name(ds.list[i]))) - as.double(eval(as.name(ds.list[j])))],
                                       y = ds[,eval(as.name(ds.y))], type=eval(corr.type))$r[1,2]
                          , f1 = ds.list[i]
                          , f2 = ds.list[j])
          tmp<-rbind(tmp, list(rn = "subt"
                               , cr = rcorr(x = ds[,as.double(eval(as.name(ds.list[j]))) - as.double(eval(as.name(ds.list[i])))],
                                            y = ds[,eval(as.name(ds.y))], type=eval(corr.type))$r[1,2]
                               , f1 = ds.list[j]
                               , f2 = ds.list[i]))

          tmp<-tmp[!is.na(cr) & !is.nan(cr)]

          # check if the table of new correlations is empty
          if (nrow(tmp) > 0) {

            # some correlations may be negative
            tmp <- tmp[abs(cr) == max(abs(cr))]


            # check if the new correlations are important and are clearly more important than original ones
            tmp <- tmp[(cr / rtrn[rn == ds.list[i], cr]) > eval(threshold.v)
                       & (cr / rtrn[rn == ds.list[j], cr]) > eval(threshold.v)
                       & cr > eval(threshold.a)
                       & !is.na(cr)]

            if (nrow(tmp) > 0) {
              tmp.r <- rbind(tmp.r, tmp[, list(rn = paste(rn, f1, f2, sep = "***"), cr, f1, f2)])
            }
          }
        }
      }
      tmp.r
    }
    print(paste("end subt : orginal features: ",length(ds.list)," : ",Sys.time(),sep=""))

    rtrn1 <- data.table(rtrn1)
    # rtrn1[, cr := as.numeric(cr)]

    rtrn <- rbind(rtrn, rtrn1, fill = TRUE)
  }


  # distances

  if (length(intersect(oper, "dist")) > 0) {

    print(paste("begin dist : orginal features: ",length(ds.list)," : ",Sys.time(),sep=""))

    rtrn1 <- foreach (i=1:length(ds.list), .packages=c("Hmisc","data.table"), .verbose=FALSE, .combine=rbind) %dopar% {

      tmp.r <- data.table(rn = character(), cr = double(), f1 = character(), f2 = character())

      print(paste("column dist :", i, ds.list[i], as.character(Sys.time())))

      for (j in (i+1):length(ds.list)) {

        if (i < length(ds.list)) {

          tmp<-data.table(rn = "dist"
                          , cr = rcorr(x = ds[, (as.double(eval(as.name(ds.list[j]))) ^ 2 + as.double(eval(as.name(ds.list[i]))) ^ 2) ^ .5],
                                       y = ds[, eval(as.name(ds.y))], type=eval(corr.type))$r[1,2]
                          , f1 = ds.list[j]
                          , f2 = ds.list[i])

          tmp<-tmp[!is.na(cr) & !is.nan(cr)]

          # check if the table of new correlations is empty
          if (nrow(tmp) > 0) {

            # some correlations may be negative
            tmp <- tmp[abs(cr) == max(abs(cr))]


            # check if the new correlations are important and are clearly more important than original ones
            tmp <- tmp[(cr / rtrn[rn == ds.list[i], cr]) > eval(threshold.v)
                       & (cr / rtrn[rn == ds.list[j], cr]) > eval(threshold.v)
                       & cr > eval(threshold.a)
                       & !is.na(cr)]

            if (nrow(tmp) > 0) {
              tmp.r <- rbind(tmp.r, tmp[, list(rn = paste(rn, f1, f2, sep = "***"), cr, f1, f2)])
            }
          }
        }
      }
      tmp.r
    }
    print(paste("end dist : orginal features: ",length(ds.list)," : ",Sys.time(),sep=""))

    rtrn1 <- data.table(rtrn1)
    # rtrn1[, cr := as.numeric(cr)]

    rtrn <- rbind(rtrn, rtrn1, fill = TRUE)
  }


  # XORs

  if (length(intersect(oper, "xor")) > 0) {

    print(paste("begin xor : orginal features: ",length(ds.list)," : ",Sys.time(),sep=""))

    rtrn1 <- foreach (i=1:length(ds.list), .packages=c("Hmisc","data.table"), .verbose=FALSE, .combine=rbind) %dopar% {

      tmp.r <- data.table(rn = character(), cr = double(), f1 = character(), f2 = character())

      print(paste("column xor :", i, ds.list[i], as.character(Sys.time())))

      for (j in (i+1):length(ds.list)) {

        if (i < length(ds.list)) {

          tmp<-data.table(rn = "xor"
                          , cr = rcorr(x = as.numeric(ds[, xor(as.numeric(eval(as.name(ds.list[i]))), as.numeric(eval(as.name(ds.list[j]))))]),
                                       y = ds[, eval(as.name(ds.y))], type=eval(corr.type))$r[1,2]
                          , f1 = ds.list[i]
                          , f2 = ds.list[j])

          tmp<-tmp[!is.na(cr) & !is.nan(cr)]

          # check if the table of new correlations is empty
          if (nrow(tmp) > 0) {

            # some correlations may be negative
            tmp <- tmp[abs(cr) == max(abs(cr))]


            # check if the new correlations are important and are clearly more important than original ones
            tmp <- tmp[(cr / rtrn[rn == ds.list[i], cr]) > eval(threshold.v)
                       & (cr / rtrn[rn == ds.list[j], cr]) > eval(threshold.v)
                       & cr > eval(threshold.a)
                       & !is.na(cr)]

            if (nrow(tmp) > 0) {
              tmp.r <- rbind(tmp.r, tmp[, list(rn = paste(rn, f1, f2, sep = "***"), cr, f1, f2)])
            }
          }
        }
      }
      tmp.r
    }
    print(paste("end xor : orginal features: ",length(ds.list)," : ",Sys.time(),sep=""))

    rtrn1 <- data.table(rtrn1)
    # rtrn1[, cr := as.numeric(cr)]

    rtrn <- rbind(rtrn, rtrn1, fill = TRUE)
  }


  # mean

  if (length(intersect(oper, "mean")) > 0) {

    print(paste("begin mean : orginal features: ",length(ds.list)," : ",Sys.time(),sep=""))

    rtrn1 <- foreach (i=1:length(ds.list), .packages=c("Hmisc","data.table"), .verbose=FALSE, .combine=rbind) %dopar% {

      tmp.r <- data.table(rn = character(), cr = double(), f1 = character(), f2 = character())

      print(paste("column mean :", i, ds.list[i], as.character(Sys.time())))

      for (j in (i+1):length(ds.list)) {

        if (i < length(ds.list)) {

          tmp<-data.table(rn = "mean"
                          , cr = rcorr(x = as.numeric(ds[, as.double(eval(as.name(ds.list[i]))) * .5 +  as.double(eval(as.name(ds.list[j]))) * .5]),
                                       y = ds[, eval(as.name(ds.y))], type=eval(corr.type))$r[1,2]
                          , f1 = ds.list[i]
                          , f2 = ds.list[j])

          tmp<-tmp[!is.na(cr) & !is.nan(cr)]

          # check if the table of new correlations is empty
          if (nrow(tmp) > 0) {

            # some correlations may be negative
            tmp <- tmp[abs(cr) == max(abs(cr))]


            # check if the new correlations are important and are clearly more important than original ones
            tmp <- tmp[(cr / rtrn[rn == ds.list[i], cr]) > eval(threshold.v)
                       & (cr / rtrn[rn == ds.list[j], cr]) > eval(threshold.v)
                       & cr > eval(threshold.a)
                       & !is.na(cr)]

            if (nrow(tmp) > 0) {
              tmp.r <- rbind(tmp.r, tmp[, list(rn = paste(rn, f1, f2, sep = "***"), cr, f1, f2)])
            }
          }
        }
      }
      tmp.r
    }
    print(paste("end mean : orginal features: ",length(ds.list)," : ",Sys.time(),sep=""))

    rtrn1 <- data.table(rtrn1)
    # rtrn1[, cr := as.numeric(cr)]

    rtrn <- rbind(rtrn, rtrn1, fill = TRUE)
  }


  # max

  if (length(intersect(oper, "max")) > 0) {

    print(paste("begin max : orginal features: ",length(ds.list)," : ",Sys.time(),sep=""))

    rtrn1 <- foreach (i=1:length(ds.list), .packages=c("Hmisc","data.table"), .verbose=FALSE, .combine=rbind) %dopar% {

      tmp.r <- data.table(rn = character(), cr = double(), f1 = character(), f2 = character())

      print(paste("column max :", i, ds.list[i], as.character(Sys.time())))

      for (j in (i+1):length(ds.list)) {

        if (i < length(ds.list)) {

          tmp<-data.table(rn = "max"
                          , cr = rcorr(x = as.numeric(ds[, mapply(max
                                                                  , as.double(eval(as.name(ds.list[i])))
                                                                  , as.double(eval(as.name(ds.list[i]))))]),
                                       y = ds[, eval(as.name(ds.y))], type=eval(corr.type))$r[1,2]
                          , f1 = ds.list[i]
                          , f2 = ds.list[j])

          tmp<-tmp[!is.na(cr) & !is.nan(cr)]

          # check if the table of new correlations is empty
          if (nrow(tmp) > 0) {

            # some correlations may be negative
            tmp <- tmp[abs(cr) == max(abs(cr))]


            # check if the new correlations are important and are clearly more important than original ones
            tmp <- tmp[(cr / rtrn[rn == ds.list[i], cr]) > eval(threshold.v)
                       & (cr / rtrn[rn == ds.list[j], cr]) > eval(threshold.v)
                       & cr > eval(threshold.a)
                       & !is.na(cr)]

            if (nrow(tmp) > 0) {
              tmp.r <- rbind(tmp.r, tmp[, list(rn = paste(rn, f1, f2, sep = "***"), cr, f1, f2)])
            }
          }
        }
      }
      tmp.r
    }
    print(paste("end max : orginal features: ",length(ds.list)," : ",Sys.time(),sep=""))

    rtrn1 <- data.table(rtrn1)
    # rtrn1[, cr := as.numeric(cr)]

    rtrn <- rbind(rtrn, rtrn1, fill = TRUE)
  }


  # min

  if (length(intersect(oper, "min")) > 0) {

    print(paste("begin min : orginal features: ",length(ds.list)," : ",Sys.time(),sep=""))

    rtrn1 <- foreach (i=1:length(ds.list), .packages=c("Hmisc","data.table"), .verbose=FALSE, .combine=rbind) %dopar% {

      tmp.r <- data.table(rn = character(), cr = double(), f1 = character(), f2 = character())

      print(paste("column min :", i, ds.list[i], as.character(Sys.time())))

      for (j in (i+1):length(ds.list)) {

        if (i < length(ds.list)) {

          tmp<-data.table(rn = "min"
                          , cr = rcorr(x = as.numeric(ds[, mapply(min
                                                                  , as.double(eval(as.name(ds.list[i])))
                                                                  , as.double(eval(as.name(ds.list[i]))))]),
                                       y = ds[, eval(as.name(ds.y))], type=eval(corr.type))$r[1,2]
                          , f1 = ds.list[i]
                          , f2 = ds.list[j])

          tmp<-tmp[!is.na(cr) & !is.nan(cr)]

          # check if the table of new correlations is empty
          if (nrow(tmp) > 0) {

            # some correlations may be negative
            tmp <- tmp[abs(cr) == max(abs(cr))]


            # check if the new correlations are important and are clearly more important than original ones
            tmp <- tmp[(cr / rtrn[rn == ds.list[i], cr]) > eval(threshold.v)
                       & (cr / rtrn[rn == ds.list[j], cr]) > eval(threshold.v)
                       & cr > eval(threshold.a)
                       & !is.na(cr)]

            if (nrow(tmp) > 0) {
              tmp.r <- rbind(tmp.r, tmp[, list(rn = paste(rn, f1, f2, sep = "***"), cr, f1, f2)])
            }
          }
        }
      }
      tmp.r
    }
    print(paste("end min : orginal features: ",length(ds.list)," : ",Sys.time(),sep=""))

    rtrn1 <- data.table(rtrn1)
    # rtrn1[, cr := as.numeric(cr)]

    rtrn <- rbind(rtrn, rtrn1, fill = TRUE)
  }


  # logs

  if (length(intersect(oper, "log")) > 0) {

    print(paste("begin log : orginal features: ",length(ds.list)," : ",Sys.time(),sep=""))

    rtrn1 <- foreach (i=1:length(ds.list), .packages=c("Hmisc","data.table"), .verbose=FALSE, .combine=rbind) %dopar% {

      tmp.r <- data.table(rn = character(), cr = double(), f1 = character(), f2 = character())

      print(paste("column log :", i, ds.list[i], as.character(Sys.time())))

      tmp.r <- rbind(tmp.r
                     , list(rn = paste("log", ds.list[i], sep = "***")
                            , cr = rcorr(x = ds[, log(as.double(eval(as.name(ds.list[i]))) + abs(min(as.double(eval(as.name(ds.list[i]))))) + 1)],
                                         y = ds[, eval(as.name(ds.y))], type=eval(corr.type))$r[1,2]
                            , f1 = ds.list[i]
                            , f2 = NA))

      tmp.r <- tmp.r[!is.na(cr) & !is.nan(cr)]

      # check if the table of new correlations is empty
      if (nrow(tmp.r) > 0) {

        # some correlations may be negative
        tmp.r <- tmp.r[abs(cr) == max(abs(cr))]

        # check if the new correlations are important and are clearly more important than original ones
        tmp.r <- tmp.r[(cr / rtrn[rn == ds.list[i], cr]) > eval(threshold.v)
                       & cr > eval(threshold.a)
                       & !is.na(cr)]
      }

      tmp.r
    }
    print(paste("end log : orginal features: ",length(ds.list)," : ",Sys.time(),sep=""))

    rtrn1 <- data.table(rtrn1)
    # rtrn1[, cr := as.numeric(cr)]

    rtrn <- rbind(rtrn, rtrn1, fill = TRUE)
  }


  # letters encoding

  if (length(intersect(oper, "let")) > 0) {

    print(paste("begin let : orginal features: ",length(ds.list)," : ",Sys.time(),sep=""))

    rtrn1 <- foreach (i=1:length(ds.list), .packages=c("Hmisc","data.table"), .verbose=FALSE, .combine=rbind) %dopar% {

      tmp.r <- data.table(rn = character(), cr = double(), f1 = character(), f2 = character())

      print(paste("column let :", i, ds.list[i], as.character(Sys.time())))

      for (j in (i+1):length(ds.list)) {

        if (i < length(ds.list)) {

          tmp <- data.table(rn = "let"
                            , cr = rcorr(x = ds[, eval(as.name(ds.list[i])) * 27 ^
                                                  (ceiling(log(eval(as.name(ds.list[j])) + .00000001, 27)) + 1) +
                                                  eval(as.name(ds.list[j]))],
                                         y = ds[,eval(as.name(ds.y))], type=eval(corr.type))$r[1,2]
                            , f1 = ds.list[i]
                            , f2 = ds.list[j])

          tmp <- tmp[!is.na(cr) & !is.nan(cr)]

          # check if the table of new correlations is empty
          if (nrow(tmp) > 0) {

            # some correlations may be negative
            tmp <- tmp[abs(cr) == max(abs(cr))]


            # check if the new correlations are important and are clearly more important than original ones
            tmp <- tmp[(cr / rtrn[rn == ds.list[i], cr]) > eval(threshold.v)
                       & (cr / rtrn[rn == ds.list[j], cr]) > eval(threshold.v)
                       & cr > eval(threshold.a)
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
    print(paste("end let : orginal features: ",length(ds.list)," : ",Sys.time(),sep=""))

    rtrn1 <- data.table(rtrn1)
    # rtrn1[, cr := as.numeric(cr)]

    rtrn <- rbind(rtrn, rtrn1, fill = TRUE)
  }



  stopCluster(cl)

  # remove de facto duplicates
  rtrn <- rtrn[, .SD[1], by = list(cr)]

  rtrn
}
