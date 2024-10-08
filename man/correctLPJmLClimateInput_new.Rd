% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/correctLPJmLClimateInput_new.R
\name{correctLPJmLClimateInput_new}
\alias{correctLPJmLClimateInput_new}
\title{correctLPJmLClimateInput_new}
\usage{
correctLPJmLClimateInput_new(x)
}
\arguments{
\item{x}{magpie object provided by the read function}
}
\value{
Magpie objects with results on cellular level, weight, unit and description.
}
\description{
Correct LPJmL climate input variables
}
\examples{

\dontrun{
readSource("LPJmLClimateInput_new", subtype, convert="onlycorrect")
}

}
\seealso{
\code{\link{readLPJmLClimateInput_new}}
}
\author{
Marcos Alves, Felicitas Beier
}
