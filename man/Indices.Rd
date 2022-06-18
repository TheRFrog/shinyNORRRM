\name{Indices}
\alias{Indices}
\title{Indices}

\usage{Indices(data, Calcite)
}
\arguments{
  \item{data}{
a whole rock major adjusted oxide chemical data.  }
   \item{Calcite}{
if it is TRUE, CaO will be used in the computation. 
  }
}

\value{
Returns a dataset with several petrological parameters.
}

\description{
The adjusted oxide data, 
}

\references{
Elizondo-Pacheco, L. A., Ramírez-Fernández, J. A., De Ignacio, C., González-Guzmán, R., Rodríguez-Saavedra, P., Leal-Cuellar, V. A. & Montalvo-Arrieta, J. C. (2022). Generation of Arc-Like and OIB-Like Magmas Triggered by Slab Detachment in the Eastern Mexican Alkaline Province: Petrological Evidence from the Cenozoic Sierra de San Carlos-Cruillas Complex, Tamaulipas. Journal of Petrology, 63(5), egac027.
}

\examples{
#create a dataframe with major elements indicating the appropriate parameters of rocks
#example of dataframes: EAP.csv, IUGS.csv, and TephraKam.csv
data(EAP)
data <- adjRock(EAP, Type= "Volcanic", Fe.adjustment = "Middlemost", 
Cancrinite = FALSE, Calcite = FALSE)
Indices(data, Calcite = FALSE)
##
}
\seealso{
\code{\link{ultimateCIPW}}
\code{\link{adjRock}}
}

\author{
Renee Gonzalez Guzman \email{rguzman@geociencias.unam.mx}
}
\keyword{programming}


