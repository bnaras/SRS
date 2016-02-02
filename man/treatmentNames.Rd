\name{treatmentNames}
\alias{treatmentNames}

\title{
  Number of Factors slot accessor function
}
\description{
  Return the value of the slot \code{treatment.names}
}
\usage{
treatmentNames(x)
}
\arguments{
  \item{x}{
    an object of class \code{ClinicalExperiment}
}
}
\details{
  Returns the value of the slot \code{treatment.names}
}
\value{
  the value of the slot \code{treatment.names}
}
\author{
  Balasubramanian Narasimhan
}

\examples{
expt <- ClinicalExperiment(number.of.factors = 3,
                           number.of.factor.levels = c(2, 2, 3),
                           number.of.treatments = 3)
treatmentNames(expt)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ design }
