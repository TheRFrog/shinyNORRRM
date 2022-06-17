\name{adjRock}
\alias{adjRock}
\title{Recalculate the compositions of the rocks in an anhydrous basis (volatile-free; 100\% adjusted)}
\usage{adjRock(data, Type = "Volcanic", Fe.adjustment = "Middlemost", Cancrinite= FALSE, Calcite= FALSE)
}
\arguments{
  \item{data}{
a whole rock major and trace element chemical data (as percent \%). It is important to note that Fe2O3 or FeO could be Fe2O3T or FeOT if any of this values are NA or 0.
  }
  \item{Type}{
rock type, if the argument is "Volcanic" is shown the volcanic rock type after (Le Bas et al., 1986), iron oxidation ratio is calculated following the equation of Le Maitre (1976) for Volcanic rocks, if the argument is "Plutonic", then Plutonic adjusts are done.
  }
  \item{Fe.adjustment}{
if the argument is "Middlemost", iron oxidation ratio is calculated depending on rock type at Middlemost (1989).
if the argument is "Le Maitre", iron oxidation ratio is calculated depending on rock type at Le Maitre (1976).
if the argument is  "Fe+3/Fe+2", iron oxidation ratio is calculated depending on their own concentration).
                     
  }
  \item{Cancrinite}{
if the argument is TRUE, CO2 will be used in whole rock adjust.  
  }
  \item{Calcite}{
if the argument is TRUE, CO2 will be used in whole rock adjust. 
  }
}

\value{
Returns a dataset of adjust oxide data (.adj) on an anhydrous basis.
}

\description{
The oxide data, from SiO2 to P2O5 (or CO2) are first recalculated to 100 percent on an anhydrous basis, and then Fe oxidation ratio adjustment is done according to the recommendations of Middlemost (1989), or Le Maitre (1976), or else the measured Fe2O3/FeO ratios are maintained. Finally, the oxide data is recalculated to 100 percent on an anhydrous basis from Fe2O3/FeO ratios calculated.
}


\references{
Le Maitre, R.W. 1976. Some problems of the projection of chemical data into mineralogical classifications. Contribution Mineralogical Petrology. v. 56, pp. 181--189.
Middlemost, E.A.K. 1989. Iron oxidation ratios, norms and the classification of volcanic rocks. Chemical Geology. v. 77, pp. 19--26.
}
\examples{
#create a dataframe with major elements indicating the appropriate parameters of rocks
#example of dataframes: EAP.csv, IUGS.csv, and TephraKam.csv
data(EAP)
adjRock(EAP, Type= "Volcanic", Fe.adjustment = "Middlemost", Cancrinite = FALSE, Calcite = FALSE)
}
\seealso{
\code{\link{ultimateCIPW}}
\code{\link{Indices}}
}
\author{
\strong{Maintainer}: Reneé González-Guzmán \email{rguzman@geociencias.unam.mx} (\href{https://orcid.org/0000-0001-7773-6401}{ORCID})
}
\keyword{programming}
