\name{numberOfTreatments}
\alias{numberOfTreatments}

\title{
  Number of Treatments slot accessor function
}
\description{
  Return the value of the slot \code{number.of.treatments}
}
\usage{
numberOfTreatments(x)
}
\arguments{
  \item{x}{
    an object of class \code{ClinicalExperiment}
}
}
\details{
  Returns the value of the slot \code{number.of.treatments}
}
\value{
  the value of the slot \code{number.of.treatments}
}
\author{
  Balasubramanian Narasimhan
}

\examples{
expt <- ClinicalExperiment(number.of.factors = 3,
                           number.of.factor.levels = c(2, 2, 3),
                           number.of.treatments = 3)
numberOfTreatments(expt)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ design }
