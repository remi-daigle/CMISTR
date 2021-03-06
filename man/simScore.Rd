% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simScore.R
\name{simScore}
\alias{simScore}
\title{Simulate raw scores based on risk and uncertainty}
\usage{
simScore(risk, certainty, n = 1000)
}
\arguments{
\item{risk}{the risk score assigned by the assessor}

\item{certainty}{the certainty score assigned by the assessor}

\item{n}{the number of simulations for the calculation of the final score}
}
\value{

}
\description{
Simulate raw scores based on risk and uncertainty
}
\examples{
scores <- simScore(risk=2,certainty=1)
}
