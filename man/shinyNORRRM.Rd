% Please edit documentation in R/shinyNORRRM.R
\name{shinyNORRRM}
\alias{shinyNORRRM}
\title{shinyNORRRM - launches the shinyNORRRM app}
\usage{
shinyNORRRM(host = "127.0.0.1", browser = NULL)
}
\arguments{
\item{host}{host link (defaults to the local machine "127.0.0.1")}

\item{port}{port number (randomly chosen unless specified as a certain number)}

\item{browser}{path to browser exe (defaults to standard browser)}
}
\value{
A shiny app
}
\description{
Starts the shinyNORRRM app in the client's browser.
}
\details{
shinyNORRRM is the interface of NORRRM
}
\examples{
\dontrun{
## Launch application on localhost (127.0.0.1)
## -------------------------------------------
## By default shinyNORRRM starts the application on localhost
## and a randomly selected port (e.g. 9876), in which case you can connect
## to the running application by navigating your browser to
## http://localhost:9876.
shinyNORRRM()

## Launch application on a different host
## --------------------------------------
## You can also run the application on a different host
## by specifying a hostname and port. Just make sure to
## use an open port on your machine. Here "open" means
## that the port should not be used by another service
## and the port is opened by your firewall.
shinyNORRRM(host="your-hostname", port=8888)

## Launch application on a different browser
## ----------------------------------------
## To run the shinyNORRRM app on a different browser than your standard browser
## use the "browser" argument to set the path to the respective .exe file (e.g., Windows system)
launch_interim(browser = "C:/Program Files/Mozilla Firefox/firefox.exe")
}

}
\keyword{shinyNORRRM}
