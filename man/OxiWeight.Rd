\name{OxiWeight}
\alias{OxiWeight}
\docType{data}
\title{The molecular weights}
\description{
Data frame with chemical nomenclature and molecular weights used in the ultimate Norm computation.
}
\usage{data(OxiWeight)}
\format{
  The format is a data frame with 26 rows and 3 columns, including the row names.
}
\details{
This table is calculated from the 2011 table values after the IUPAC (International Union of Pure and Applied Chemistry), Commission on Isotopic Abundances and Atomic Weights. Note that row names are the chemical nomenclature of the oxides and the third column is the oxide weight rounded. 
}

\references{
Wieser, M.E., and others. 2013. Pure and Applied Chemistry. International Union of Pure and Applied Chemistry (IUPAC). v. 78, no. 11, pp. 2051--2066. 
}
\examples{
data(OxiWeight)
OxiWeight ['SiO2','OWeight']
##
rm(OxiWeight)#cleanup
}
\seealso{
\code{\link{AtomWeight}},\code{\link{MinWeight}}
}

\keyword{datasets}


