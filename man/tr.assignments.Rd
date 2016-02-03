\name{tr.assignments}
\alias{tr.assignments}

\title{
  Treatment Assignment  slot accessor function
}
\description{
  Return the value of the slot \code{tr.assignments}
}
\usage{
tr.assignments(x)
}

\arguments{
  \item{x}{
    an object of class \code{PocockSimonRandomizer}
  }
}
\details{
  Returns the value of the slot \code{tr.assignments}
}
\value{
  the value of the slot \code{tr.assignments}
}
\author{
  Balasubramanian Narasimhan
}

\examples{
expt <- ClinicalExperiment(number.of.factors = 2,
                           number.of.factor.levels = c(2, 2))
randomizer <- new("PocockSimonRandomizer", expt, as.integer(12345))
randomizer <- randomize(randomizer, "Subject 1", c("F1-1", "F2-2"))
randomizer <- randomize(randomizer, "Subject 2", c("F1-2", "F2-1"))
tr.assignments(randomizer)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ design }
