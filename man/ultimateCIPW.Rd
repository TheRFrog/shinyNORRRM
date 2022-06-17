\name{ultimateCIPW}
\alias{ultimateCIPW}
\title{CIPW Norm (major and trace elements)}
\usage{ultimateCIPW(data, Type= "Volcanic", Fe.adjustment = "Middlemost", Cancrinite = FALSE, Calcite = FALSE)
}
\arguments{
   \item{data}{
a whole rock major and trace element chemical data. It is important to note that Fe2O3 or FeO could be Fe2O3T or FeOT if any of this values are NA or 0. On the other hands, the format of CO2 and F column must be 'CO2.' and 'F.', respectively.
  }
  \item{Type}{
rock type, if is TRUE is shown the volcanic rock type after \code{\link{TASplot}} in output, and combined with AdjTAS = FALSE, iron oxidation ratio is calculated following the equation of Le Maitre (1976) for Volcanic rocks, if is FALSE, then Plutonic adjusts are done. 
  }
  \item{Fe.adjustment}{
if is TRUE, iron oxidation ratio is calculated depending on rock type at \code{\link{TASplot}}.
  }
  \item{Cancrinite}{
if is TRUE, CO2 will be used in whole rock adjust. If the concentration of CO2 is more than 0, and the modal cancrinite is present then Sodium carbonate is calculated. 
  }
  \item{Cancrinite}{
if is TRUE, CO2 will be used in whole rock adjust. If the concentration of CO2 is more than 0, and the modal calcite is present then Calcite is calculated. 
  }
  \item{Calcite}{
rounds the values to 'digits' after the decimal point. Default = 3,  in order to keep their sum of rock oxides as close to 100 as possible.
  }
}

\value{
Calculate the CIPW Norm and others geochemical parameters.
}

\description{
Computed from the chemical composition, the normative mineralogy is an alternative approach for mineralogical classification and useful for set up the naming of igneous rocks (as parts of the TAS classification, \code{\link{TASplot}}). The CIPW Norm (acronym from the surnames of the authors: Cross, Iddings, Pirrson and Washington, Cross et al., 1902) is the most commonly used calculation algorithm to estimate the standard mineral assemblages for igneous rocks, generated over more than a hundred years ago and thereafter modified by some authors to the passage of the years (e.g., Verma et al., 2002). It is based upon assumptions about the order of mineral formation and known phase relationships of rocks and minerals, using simplified mineral formulas. 

In this option, the trace elements that can be used are Ba, Cl, Co, Cr Cs, F, Li, Ni, Rb, S, Sr, V and Zr. Additionally, the oxides concentrations of Cr2O3, NiO and SO3 (expressed as wt. percent) are handled like trace elements as well. However Cr2O3 are preferable to Cr and NiO to Ni. Sulfur concentration when available is reported as either SO3 or S, in which case their separate identity should be maintained. 
}

\examples{
#create a dataframe with major elements indicating the appropriate parameters of rocks
data(Andes)#example of dataframe
CIPW.trace(Andes)
##
}
\references{
Cross, W., Iddings, J.P., Pirsson, L. V., Washington, Henry S. 1902, A quantitative chemico-mineralogical classification and nomenclature of igneous rocks: The Journal of Geology, v. 10, no. 6, pp. 555--690. 

Verma, S.P., Torres-Alvarado, I.S., and Velasco-Tapia, F., 2003, A revised CIPW norm: Schweizerische Mineralogische und Petrographische Mitteilungen, v. 83, no. 2, pp. 197--216. 
}

\author{
Renee Gonzalez Guzman \email{rguzman@geociencias.unam.mx}
}

\seealso{
\code{\link{Indices}}
\code{\link{adjRock}}
}

\keyword{programming}







