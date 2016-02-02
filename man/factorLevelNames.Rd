\name{factorLevelNames}
\alias{factorLevelNames}
\title{
  Factor Names Slot Accessor function
}
\description{
  Return the value of the slot \code{factor.level.names}
}
\usage{
factorLevelNames(x)
}

\arguments{
  \item{x}{
    An object of class \code{ClinicalExperiment}
}
}
\details{
  Returns the value of the slot \code{factor.level.names}
}
\value{
  The value of the slot \code{factor.level.names}
}

\author{
  Balasubramanian Narasimhan
}


\examples{
expt <- ClinicalExperiment(number.of.factors = 3,
                           number.of.factor.levels = c(2, 2, 3),
                           number.of.treatments = 3)
factorLevelNames(expt)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ design }
