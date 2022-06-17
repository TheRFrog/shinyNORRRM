\name{MinWeight}
\alias{MinWeight}
\docType{data}
\title{The oxides molecular weights of normative minerals}
\description{
Data frame with normative mineral names, chemical nomenclature, oxides molecular weights and theoretical density of minerals used in CIPW computations.
}
\usage{data(MinWeight)}
\format{
  The format is a data frame with 36 rows and 6 columns, including the row names.
}
\details{
This table is calculated from the 2011 table values after the IUPAC (International Union of Pure and Applied Chemistry), Commission on Isotopic Abundances and Atomic Weights. Note that row names are the name of the normative minerals. 
}
\references{
Wieser, M.E., and others. 2013. Pure and Applied Chemistry. International Union of Pure and Applied Chemistry (IUPAC). v. 78, no. 11, pp. 2051--2066. 
}
\examples{
data(MinWeight)
MinWeight['Quartz','ConsWeight']
##

MinWeight['Quartz','Density']
##
}
\seealso{
\code{\link{AtomWeight}},\code{\link{OxiWeight}}
}

\keyword{datasets}


