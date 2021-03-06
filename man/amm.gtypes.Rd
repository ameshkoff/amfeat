% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/amm.gtypes.r
\name{amm.gtypes}
\alias{amm.gtypes}
\title{Get columns by type}
\usage{
amm.gtypes(ds, fnct)
}
\arguments{
\item{ds}{data.table: data set}

\item{fnct}{character: "is" function name: is.character, is.factor, is.numeric etc.}
}
\value{
Character vector of column names
}
\description{
Gets columns of data.table by column type
}
\seealso{
amm.gmatch
}

