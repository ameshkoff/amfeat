% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/am.scor.r
\name{am.scor}
\alias{am.scor}
\title{Search for new correlations}
\usage{
am.scor(ds, ds.list, ds.y, corr.type = "spearman", treshold.v = 1.1,
  treshold.a = 0.05, cl.number = 1, out.file = "")
}
\arguments{
\item{ds}{data.table: data set}

\item{ds.list}{character vector: names of independent variables (X)}

\item{ds.y}{character: name of the dependent variable (Y)}

\item{corr.type}{character: correlation types; "spearman" (default) and "pearson" available; see Hmisc rcorr for details}

\item{treshold.v}{number: how much correlation from the new feature to Y should be more than the lesser from the original ones; default is 1.1 = 10 per cent more}

\item{treshold.a}{number: the minimal correlation from the new feature to Y; .05 default}

\item{cl.number}{number: cluster number for parallel computers; be very careful with this parameter! do not set it too big value}

\item{out.file}{character: absolute or relative path to the files with output; due to parallel computations you do NOT see most of the info in the console; id default no outputs are used}
}
\value{
Data.table. rn - new feature name, cr - correlation, f1 - first original feature used, f2 - second original feature used
}
\description{
Search for new correlations based ot original feature pairs: all types in one step. Be careful! Parallel computation in use.
}
\seealso{
You can use this data to choose and create new features you prefer with am.calcf function
}

