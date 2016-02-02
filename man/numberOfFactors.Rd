\name{numberOfFactors}
\alias{numberOfFactors}

\title{
  Number of Factors slot accessor function
}
\description{
  Return the value of the slot \code{number.of.factors}
}
\usage{
numberOfFactors(x)
}
\arguments{
  \item{x}{
    an object of class \code{ClinicalExperiment}
}
}
\details{
  Returns the value of the slot \code{number.of.factors}
}
\value{
  the value of the slot \code{number.of.factors}
}
\author{
  Balasubramanian Narasimhan
}

\examples{
expt <- ClinicalExperiment(number.of.factors = 3,
                           number.of.factor.levels = c(2, 2, 3),
                           number.of.treatments = 3)
numberOfFactors(expt)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ design }
