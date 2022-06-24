\name{AtomWeight}
\alias{AtomWeight}
\docType{data}
\title{The standard atomic weights}
\description{
Data frame with symbols, element names, and standard atomic weights.
}
\usage{data(AtomWeight)}
\format{
  The format is a data frame with 92 rows and 3 columns, including the row names.
}
\details{
This table is based on the 2011 table after the IUPAC (International Union of Pure and Applied Chemistry), Commission on Isotopic Abundances and Atomic Weights. Note that row names are the symbol of the elements. 
}

\source{
\url{https://iupac.org/}
}
\references{
Wieser, M.E., and others. 2013. Pure and Applied Chemistry. International Union of Pure and Applied Chemistry (IUPAC). v. 78, no. 11, pp. 2051--2066. 
}
\examples{
data(AtomWeight)
AtomWeight['H','AWeight']
rm(AtomWeight)#cleanup
##
}
\seealso{
\code{\link{MinWeight}},\code{\link{OxiWeight}}
}

\keyword{datasets}



