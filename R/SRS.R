#'
#' ClinicalExperiment: Object representing Clinical Experiment
#'
#' @description A class to encapsulate a clinical experiment with its
#'     associated arms and factors. Objects can be created by calls of
#'     the form `new("ClinicalExperiment", ...)`, although the
#'     `ClinicalExperiment` constructor function is recommended.The arguments
#'     include the number of factors in the experiment, the names of
#'     the factors, the number of levels of each factor, the number of
#'     treatments and the treatment names. Many, if not all, of the
#'     arguments are optional, in which case a two-treatment,
#'     two-factor experiment with two levels of each factor is
#'     assumed.
#'
#' @name ClinicalExperiment-class
#' @import methods
#' @docType class
#' @seealso ClinicalExperiment
#' @exportClass ClinicalExperiment
#'
#' @slot number.of.factors an integer specifying the number of factors, defaults to 2
#' @slot factor.names a character vector specifying the character names, defaults to `F1` and `F2`
#' @slot number.of.factor.levels A vector specifying the number of levels of each factor, defaults to `c(2,2)`
#' @slot factor.level.names a list of character vectors specifying the factor level names, defaults to `F1-1`, `F1-2`, `F2-1`, `F2-2`, etc.
#' @slot number.of.treatments the number of treatments, defaults to 2
#' @slot treatment.names the names of the treatments; defaults to `c("Tr1", "Tr2")`

#'
#' @examples
#'
#' showClass("ClinicalExperiment")
#' ##
#' ## Construct a Clinical Experiment with 3 factors, with levels 2, 2, 3
#' ## respectively, and three treatments with default names for all.
#' ##
#' expt <- ClinicalExperiment(number.of.factors = 3,
#'                            number.of.factor.levels = c(2, 2, 3),
#'                            number.of.treatments = 3)
#' print(expt)
setClass("ClinicalExperiment",
         representation(number.of.factors="integer",
                        factor.names="character",
                        factor.level.names="list",
                        number.of.factor.levels="integer",
                        number.of.treatments="integer",
                        treatment.names="character"),
         prototype(number.of.factors=as.integer(2),
                   factor.names=c("F1", "F2"),
                   factor.level.names=list(c("F1-1", "F1-2"), c("F2-1", "F2-2")),
                   number.of.factor.levels=as.integer(c(2,2)),
                   number.of.treatments=as.integer(2),
                   treatment.names=c("Tr1", "Tr2")))

##
## Validate a clinical experiment object
##
.validClinicalExperiment <- function(object) {
    retval <- NULL
    if (length(object@number.of.factors) != 1) {
        retval <- c(retval, "number.of.factors is not an integer")
    }

    if (object@number.of.factors <= 0) {
        retval <- c(retval, "number.of.factors is not positive")
    }

    if (length(object@factor.names) != object@number.of.factors) {
        retval <- c(retval, "factor.names length mismatch with number.of.factors")
    }

    if (any(sapply(object@factor.names, function(x) nchar(x) == 0))) {
        retval <- c(retval, "some factor.names are empty")
    }

    if (length(object@factor.level.names) != object@number.of.factors) {
        retval <- c(retval, "factor.level.names length mismatch with number.of.factors")
    }

    if (length(object@number.of.factor.levels) != object@number.of.factors) {
        retval <- c(retval, "number.of.factor.levels length mismatch with number.of.factors")
    }

    for (i in seq_len(object@number.of.factors)) {
        if (length(object@factor.level.names[[i]]) != object@number.of.factor.levels[i]) {
            retval <- c(retval, "factor.level.names mismatch with number.of.factors.levels")
        }
    }

    if (length(object@number.of.treatments) != 1) {
        retval <- c(retval, "number.of.treatments is not a single integer")
    }

    if (object@number.of.treatments < 2) {
        retval <- c(retval, "number.of.treatments must be at least 2")
    }

    if (length(object@treatment.names) != object@number.of.treatments) {
        retval <- c(retval, "treatment names length mismatch with number.of.treatments")
    }

    if (is.null(retval)) {
        return(TRUE)
    } else {
        return(retval)
    }
}

setValidity("ClinicalExperiment", .validClinicalExperiment)


##
#' Constructor function for ClinicalExperiment class
#' @param number.of.factors the number of factors, defaults to 2
#' @param factor.names names of factors defaults to `F1` and `F2`
#' @param number.of.factor.levels the number of levels of each factor, defaults to `c(2,2)`
#' @param factor.level.names a list of character vectors specifying the factor level names, defaults to `F1-1`, `F1-2`, `F2-1`, `F2-2`, etc.
#' @param number.of.treatments the number of treatments, defaults to 2
#' @param treatment.names the names of the treatments; defaults to `c("Tr1", "Tr2")`
#' @return a ClinicalExperiment class object
#' @export
#' @examples
#'
#' expt <- ClinicalExperiment(number.of.factors = 3,
#'                            number.of.factor.levels = c(2, 2, 3),
#'                            number.of.treatments = 3)
#'
ClinicalExperiment <- function(number.of.factors=2,
                               factor.names,
                               number.of.factor.levels,
                               factor.level.names,
                               number.of.treatments=2,
                               treatment.names) {

    if (length(number.of.factors) != 1) {
        stop("number.of.factors is not a single integer")
    } else if (number.of.factors < 2) {
        stop("number.of.factors must be at least 2!")
    }

    if (missing(factor.names)) {
        factor.names <- sapply(seq_len(number.of.factors),
                               function(x) paste("F", x, sep=""))
    }

    if (missing(number.of.factor.levels)) {
        number.of.factor.levels <- rep(2, number.of.factors)
    }

    if (missing(treatment.names)) {
        treatment.names <- sapply(seq_len(number.of.treatments),
                                  function(x) paste("Tr", x, sep=""))
    }

    if (missing(factor.level.names)) {
        factor.level.names <- lapply(seq_len(number.of.factors),
                                     function(x) paste(factor.names[x], seq_len(number.of.factor.levels[x]), sep="-"))
        ##factor.level.names <- lapply(number.of.factor.levels,
        ##                           function(x) as.character(1:x))
    }

    new("ClinicalExperiment",
        number.of.factors = as.integer(number.of.factors),
        factor.names = factor.names,
        factor.level.names = factor.level.names,
        number.of.factor.levels = as.integer(number.of.factor.levels),
        number.of.treatments = as.integer(number.of.treatments),
        treatment.names = treatment.names)
}

#' Is an object a ClinicalExperiment object?
#' @param object an object
#' @return TRUE if x is a ClinicalExperiment object, FALSE otherwise
#' @export
is.ClinicalExperiment <- function(object) is(object, "ClinicalExperiment")

#' @rdname ClinicalExperiment-class
#' @export
setGeneric("numberOfFactors", function(object) standardGeneric("numberOfFactors"))

#' @docType methods
#' @param object the ClinicalExperiment object
#' @rdname ClinicalExperiment-class
#' @export
setMethod("numberOfFactors", "ClinicalExperiment", function(object) object@number.of.factors)

#' @rdname ClinicalExperiment-class
#' @export
setGeneric("factorNames", function(object) standardGeneric("factorNames"))

#' @docType methods
#' @rdname ClinicalExperiment-class
#' @export
setMethod("factorNames", "ClinicalExperiment", function(object) object@factor.names)

#' @rdname ClinicalExperiment-class
#' @export
setGeneric("factorLevelNames", function(object) standardGeneric("factorLevelNames"))

#' @docType methods
#' @rdname ClinicalExperiment-class
#' @export
setMethod("factorLevelNames", "ClinicalExperiment", function(object) object@factor.level.names)

#' @rdname ClinicalExperiment-class
#' @export
setGeneric("numberOfTreatments", function(object) standardGeneric("numberOfTreatments"))

#' @docType methods
#' @rdname ClinicalExperiment-class
#' @export
setMethod("numberOfTreatments", "ClinicalExperiment", function(object) object@number.of.treatments)

#' @rdname ClinicalExperiment-class
#' @export
setGeneric("treatmentNames", function(object) standardGeneric("treatmentNames"))

#' @docType methods
#' @rdname ClinicalExperiment-class
#' @export
setMethod("treatmentNames", "ClinicalExperiment", function(object) object@treatment.names)


#' @docType methods
#' @rdname ClinicalExperiment-class
#' @export
setMethod("show", "ClinicalExperiment",
          function(object) {
              cat("An object of class ClinicalExperiment\n")
              cat("Factor Names: "); print(slot(object,  "factor.names"));
              cat("Factor levels:\n"); print(slot(object, "factor.level.names"));
              cat("Treatment Names: "); print(slot(object, "treatment.names"));
          })


#'
#' A Randomizer using the minimization method of Pocock and Simon
#'
#' @description A randomizer implementing the minimization method of Pocock
#' and Simon (1975). The definition here closely follows the description in
#' the paper. Objects can be created by calls of the form
#' `new("PocockSimonRandomizer", expt, seed, tr.ratios, d.func, g.func, p.func)`.
#' Arguments include a ClinicalExperiment object,
#' a random number seed, the ratio of treatment counts, a function
#' to use for computing imbalance for each treatment, a function to
#' compute the overall imbalance, and a function that provides the
#' probability allocation to address the imbalance.
#'
#' @name PocockSimonRandomizer-class
#' @import methods
#' @docType class
#'
#' @exportClass PocockSimonRandomizer
#'
#' @slot expt a ClinicalExperiment object defining the experiment context
#' @slot seed A seed for the random number generator for reproducibility
#' @slot stateTable A matrix of counts indicating the marginal distribution of each factor level per treatment
#' @slot tr.assignments a data frame of treatment assignments so far
#' @slot tr.ratios the treatment arm ratio, for example 2:1 in a two treatment experiment
#' @slot d.func A function that computes the imbalance for each treatment
#' @slot g.func A function that computes the overall imbalance to be minimized
#' @slot p.func A function that computes the probability vector of treatment assignments
#'
#' @examples
#' showClass("PocockSimonRandomizer")
#' expt <- ClinicalExperiment(number.of.factors = 2,
#'              number.of.factor.levels = c(2, 2))
#' randomizer <- new("PocockSimonRandomizer", expt, 1281L)
#' randomizeSubject(randomizer) <- list(subject.id = "Subject 1",
#'                                      factor.values = c("F1-1", "F2-2"))
#' randomizeSubject(randomizer) <- list(subject.id = "Subject 2",
#'                                      factor.values = c("F1-2", "F2-1"))
#' print(randomizer)
setClass("PocockSimonRandomizer",
         representation(expt = "ClinicalExperiment",
                        seed = "integer",
                        stateTable = "matrix",
                        tr.assignments = "data.frame",
                        tr.ratios = "numeric",
                        d.func = "function",
                        g.func = "function",
                        p.func = "function")
         )

#' @docType methods
#' @importFrom knitr kable
#' @rdname PocockSimonRandomizer-class
#' @export
setMethod("show", "PocockSimonRandomizer",
          function(object) {
              cat("An object of class PocockSimonRandomizer\n")
              cat("Clinical Experiment:\n"); show(slot(object,  "expt"));
              cat("Treatment randomization ratio:"); print(knitr::kable(slot(object, "tr.ratios")));
              cat("RNG seed:"); print(slot(object, "seed"));
              cat("\nCurrent State:\n"); print(knitr::kable(slot(object, "stateTable")));
              cat("\nTreatment Assignments:\n"); print(knitr::kable(slot(object, "tr.assignments")));
              cat("\nFactor-specific measure of imbalance:\n"); print(slot(object, "d.func"));
              cat("Overall measure of imbalance:\n"); print(slot(object, "g.func"));
              cat("Probability function for biased coin:\n"); print(slot(object, "p.func"));
          })
##
## Helper default functions. Internal
##

##
## Default measure of imbalance is range, as in paper.
## Result is vector of length = number of treatments
##
.default.d.func <- function(x) {
    diff(range(x))
}

##
## Default measure of overall imbalance. Just sum of imbalances.
##
.default.g.func <- function(x) {
    sum(x)
}

##
## Default probablity assignments based on overall imbalance
##
.default.p.func <- function(overallImbalance) {
    number.of.treatments <- length(overallImbalance)
    p.star <- 2/3
    k <- which(overallImbalance == min(overallImbalance))
    if (length(k) > 1) {
        k <- sample(k, 1)
    }
    p.vec <- rep((1-p.star)/(number.of.treatments-1), number.of.treatments)
    p.vec[k] <- p.star
    p.vec
}

##
## Another alternative probability function based on overall imbalance
## Deterministically assign the treatment that will minimize overall imbalance
##
.alt.p.func.best <- function(overallImbalance) {
    number.of.treatments <- length(overallImbalance)
    k <- which(overallImbalance == min(overallImbalance))
    if (length(k) > 1) {
        k <- sample(k, 1)
    }
    p.vec <- rep(0, number.of.treatments)
    p.vec[k] <- 1
    p.vec
}

##
## Another alternative probability function based on overall imbalance
## Load the die so that the treatment that will minimize overall imbalance
## is heavily favored and the remaining ones are all equiprobable
## .alt.p.func.best (FAVORED.PROB = 1) and .default.p.func (FAVORED.PROB = 0.75)
## are special cases of this.
##
.alt.p.func.prob <- function(overallImbalance) {
    FAVORED.PROB <- 0.75
    number.of.treatments <- length(overallImbalance)
    k <- which(overallImbalance == min(overallImbalance))
    if (length(k) > 1) {
        k <- sample(k, 1)
    }
    p.vec <- rep((1-FAVORED.PROB)/(number.of.treatments-1), number.of.treatments)
    p.vec[k] <- FAVORED.PROB
    p.vec
}

##
## Another alternative probability function based on overall imbalance
## Load the die proportionately according to imbalance.
## Have to ensure overallImbalance is never negative!
##
.alt.p.func.prop <- function(overallImbalance) {
    p.vec <- overallImbalance / sum(overallImbalance)
    p.vec
}

setMethod("initialize", "PocockSimonRandomizer",
          function (.Object, expt = ClinicalExperiment(), seed=12345L, ##stateTable,
                    tr.ratios, d.func=.default.d.func,
                    g.func=.default.g.func, p.func=.default.p.func) {
              ##print("I am called")
              ##if (missing(seed)) seed <- 12345
              ##              if (missing(stateTable)) {
              factor.names <- expt@factor.names
              factor.level.names <- expt@factor.level.names
              treatment.names <- expt@treatment.names
              m <- expt@number.of.treatments
              n <- sum(expt@number.of.factor.levels)
              stateTable <- matrix(0, nrow=m, ncol=n)
              rownames(stateTable) <- treatment.names
              colnames(stateTable) <- unlist(sapply(seq_len(expt@number.of.factors),
                                                    function(i)
                                                        sapply(seq_len(expt@number.of.factor.levels[i]),
                                                               function(j) paste(factor.names[i],
                                                                                 factor.level.names[[i]][j],
                                                                                 sep=":"))))
              ##              }
              if (missing(tr.ratios)) {
                  m <- expt@number.of.treatments
                  tr.ratios <- rep(1, m)
              }
              tr.names <- names(tr.ratios)
              tr.ratios <- tr.ratios / sum(tr.ratios)
              if (is.null(tr.names)) {
                  names(tr.ratios) <- treatment.names
              } else {
                  ## Ensure the names are kosher
                  if (!all(tr.names %in% treatment.names)) {
                      stop("Improper names for treatment ratios!")
                  }
                  tr.ratios <- tr.ratios[expt@treatment.names]
              }
              .Object@expt <- expt
              .Object@seed <- as.integer(seed)
              .Object@stateTable <- stateTable
              tr.assignments <- c(lapply(factor.names, function(x) character()),
                                  list(character()))
              names(tr.assignments) <- c(factor.names, "Treatment")
              .Object@tr.assignments <- do.call(data.frame, tr.assignments)
              .Object@tr.ratios <- tr.ratios
              .Object@d.func <- d.func
              .Object@g.func <- g.func
              .Object@p.func <- p.func
              set.seed(seed)
              .Object
          })

##
## Validator for PocockSimonRandomizer
##
.validPocockSimonRandomizer <- function(object) {
    retval <- NULL
    expt <- object@expt
    m <- expt@number.of.treatments
    n <- sum(expt@number.of.factor.levels)

    if (nrow(object@stateTable) != m) {
        retval <- c(retval, "State matrix has the wrong number of rows")
    }

    if (ncol(object@stateTable) != n) {
        retval <- c(retval, "State matrix has the wrong number of columns")
    }

    treatment.names <- expt@treatment.names

    if (!all(rownames(object@stateTable) == treatment.names)) {
        retval <- c(retval, "State matrix has the wrong row names")
    }

    expected.colnames <- unlist(sapply(seq_len(expt@number.of.factors),
                                       function(i)
                                           sapply(seq_len(expt@number.of.factor.levels[i]),
                                                  function(j) paste(expt@factor.names[i],
                                                                    expt@factor.level.names[[i]][j], sep=":"))))

    if (!all(colnames(object@stateTable) == expected.colnames)) {
        retval <- c(retval, "State matrix has the wrong column names")
    }


    if (any(object@tr.ratios) <= 0) {
        retval <- c(retval, "Treatment ratios should be positive")
    }

    if (is.null(retval)) {
        return(TRUE)
    } else {
        return(retval)
    }
}

setValidity("PocockSimonRandomizer", .validPocockSimonRandomizer)

## ##
## ## Constructor function Don't need this because I have the initializer!
## "PocockSimonRandomizer" <- function(expt, seed) {
##   my.expt <- as(expt, "ClinicalExperiment")
##   new("PocockSimonRandomizer",
##       my.expt,
##       as.integer(seed))
## }


##
## The State table contains all the relevant marginal information.
## For testing purposes, it is convenient to be able to start the
## Randomizer at particular states. Hence this generic method
##
## Disabled in version 0.07 since this needs to be reconciled with treatment
## assignments. Instead, use tr.assignments replace method below
## if (!isGeneric("stateTable<-")) {
##     if (is.function("stateTable<-")) {
##         setGeneric("stateTable<-", "stateTable<-")
##     } else {
##         setGeneric("stateTable<-",
##                    function(x, value) standardGeneric("stateTable<-"))
##     }

## }

## setReplaceMethod("stateTable", "PocockSimonRandomizer", function(x, value) {
##     expt <- x@expt
##     treatment.names <- expt@treatment.names
##     ##print("In statetable")
##     ##print(treatment.names)
##     if (!all(rownames(value) == treatment.names)) {
##         stop("Matrix has the wrong row names")
##     }
##     ##print("check passed")

##     expected.colnames <- unlist(sapply(seq_len(expt@number.of.factors),
##                                        function(i)
##                                            sapply(seq_len(expt@number.of.factor.levels[i]),
##                                                   function(j) paste(expt@factor.names[i],
##                                                                     expt@factor.level.names[[i]][j], sep=":"))))

##     if (!all(colnames(value) == expected.colnames)) {
##         stop("Matrix has the wrong column names")
##     }

##     x@stateTable <- value
##     x
## })

##
## This sets the list of treatment assignments so far
##

#' @rdname PocockSimonRandomizer-class
#' @export
setGeneric("tr.assignments<-",
           function(x, value) standardGeneric("tr.assignments<-"))

#'
#' @docType methods
#' @param x the randomizer
#' @param assignments a conformable data frame of treatment assignments so far,
#'    with appropriately named variables
#' @rdname PocockSimonRandomizer-class
#' @export
setReplaceMethod("tr.assignments", "PocockSimonRandomizer", function(x, value) {
    ##
    ## Check for appropriate values
    ##

    if (!is.data.frame(value)) {
        stop("Expecting data frame for treatment assignments!")
    }
    expt <- x@expt
    number.of.factors <- expt@number.of.factors
    if (ncol(value) != (number.of.factors + 1)) {
        stop("Wrong dimension for treatment assignment data frame!")
    }
    factor.names <- expt@factor.names
    if (any(names(value) != c(factor.names, "Treatment"))) {
        stop("Wrong variable names for treatment assignments!")
    }

    if (nrow(value) > 0) {
        ##
        ## Now check for the values of the factor levels
        ##
        for (i in seq_len(number.of.factors)) {
            ## print(i)
            ## print(value[, i])
            ## print(expt@factor.level.names[[i]])
            if (! all(value[, i] %in% expt@factor.level.names[[i]])) {
                stop(paste("Wrong value for variable", names(value)[i], "in treatment assignments!"))
            }
        }
        if (! all(value[, ncol(value)] %in% expt@treatment.names)) {
            stop(paste("Bad treatement names in treatment assignments!"))
        }
    }

    ## Now that value is kosher, the state table has to be changed appropriately too,
    ## so that things are consistent.
    ## Update state
    m <- expt@number.of.treatments
    n <- sum(expt@number.of.factor.levels)
    state.matrix <- matrix(0, nrow=m, ncol=n)
    rownames(state.matrix) <- expt@treatment.names
    colnames(state.matrix) <- unlist(sapply(seq_len(expt@number.of.factors),
                                            function(i)
                                                sapply(seq_len(expt@number.of.factor.levels[i]),
                                                       function(j) paste(expt@factor.names[i],
                                                                         expt@factor.level.names[[i]][j], sep=":"))))


    for (i in seq_len(nrow(value))) {
        tr.name <- value[i, "Treatment"]
        named.factors <- paste(factor.names, value[i, factor.names], sep=":")
        state.matrix[tr.name, named.factors] <- state.matrix[tr.name, named.factors] + 1
    }

    ##print(state.matrix)
    x@stateTable <- state.matrix
    x@tr.assignments <- value
    x
})

##
## Add some accessors
## tr.assigments, stateTable

#'
#' @rdname PocockSimonRandomizer-class
#' @export
setGeneric("tr.assignments", function(x) standardGeneric("tr.assignments"))

#' @docType methods
#' @rdname PocockSimonRandomizer-class
#' @export
setMethod("tr.assignments", "PocockSimonRandomizer", function(x) x@tr.assignments)

#' @rdname PocockSimonRandomizer-class
#' @export
setGeneric("stateTable", function(x) standardGeneric("stateTable"))

#'
#' @docType methods
#' @rdname PocockSimonRandomizer-class
#' @export
setMethod("stateTable", "PocockSimonRandomizer", function(x) x@stateTable)

#' @rdname PocockSimonRandomizer-class
#' @export
setGeneric("computeImbalances",
           function(object, factor.values) standardGeneric("computeImbalances"))

##
## We need a way of getting at the imbalance for each possible treatment, so we need a
## generic function that computes imbalances
##
#'
#' Compute imbalances for each treatment assignment given a set of factor values
#' @docType methods
#' @param object a PocockSimonRandomizer object
#' @param factor.values the factor values
#' @return a matrix of imbalances
#' @rdname PocockSimonRandomizer-class
#' @export
setMethod("computeImbalances",
          signature(object="PocockSimonRandomizer", factor.values="character"),
          function (object, factor.values) {
              ##   factor.values <- as.integer(factor.values)
              if (missing(factor.values)) {
                  stop("Need factor values")
              }
              expt <- object@expt
              number.of.factors <- expt@number.of.factors
              if (length(factor.values) != number.of.factors) {
                  stop("Not correct number of factors")
              }
              number.of.factor.levels <- expt@number.of.factor.levels
              factor.level.names <- expt@factor.level.names
              factor.values.kosher <- sapply(seq_len(number.of.factors),
                                             function(x) factor.values[x] %in% factor.level.names[[x]])
              if (! all(factor.values.kosher)) {
                  stop("Incorrect factor values provided")
              }

              treatment.names <- expt@treatment.names
              number.of.treatments <- expt@number.of.treatments
              d.func <- object@d.func
              factor.names <- expt@factor.names
              state.matrix <- object@stateTable
              tr.ratios <- object@tr.ratios
              named.factors <- paste(factor.names, factor.values, sep=":")
              f.mat <- state.matrix[, named.factors]
              ##print("our mat")
              ##print(f.mat)
              ##print(treatment.names)

              result <- sapply(treatment.names, function(x) {
                  ##print("fmat")
                  ##print(f.mat)
                  new.mat <- f.mat
                  new.mat[x, ] <- new.mat[x, ] + 1
                  ##print(new.mat)
                  exp.mat <- apply(new.mat, 2, function(x) tr.ratios * sum(x))
                  ## The D function in the paper
                  ##print(exp.mat)
                  apply(new.mat - exp.mat, 2, d.func)
              })
              colnames(result) <- treatment.names
              ##print("Imbalances")
              ##print(result)
              result
          })


#' @rdname PocockSimonRandomizer-class
#' @param imbalances the imbalances for each treatment
#' @export
setGeneric("computeOverallImbalance",
           function(object, imbalances) standardGeneric("computeOverallImbalance"))

##
## The G function in the paper
##
#' Compute the overall imbalance given individual imbalances
#' @docType methods
#' @return the imbalance
#' @rdname PocockSimonRandomizer-class
#' @export
setMethod("computeOverallImbalance",
          signature(object="PocockSimonRandomizer", imbalances="matrix"),
          function (object, imbalances) {
              g.func <- object@g.func
              apply(imbalances, 2, g.func)
          })

## if (!isGeneric("stateTable<-")) {
##     if (is.function("stateTable<-")) {
##         setGeneric("stateTable<-", "stateTable<-")
##     } else {
##         setGeneric("stateTable<-",
##                    function(x, value) standardGeneric("stateTable<-"))
##     }

## }


#' @rdname PocockSimonRandomizer-class
#' @export
setGeneric("randomizeSubject<-",
           function(object, value) standardGeneric("randomizeSubject<-"))

## We want reference semantics for Randomization.
## So we make randomize a replace method.
#'
#' Randomize a subject given a subject id and the set of factor values
#' @docType methods
#' @param value a named list of two items: `subject.id`, a character string, and `factor values` a vector of factor values
#' @rdname PocockSimonRandomizer-class
#' @export
setReplaceMethod("randomizeSubject",
                 signature(object="PocockSimonRandomizer", value = "list"),
                 function (object, value) {
                     if (!is.list(value) || (length(value) != 2L) ||
                         names(value) != c("subject.id", "factor.values")) {
                         stop("Expecting a list of two items named subject.id and factor.values")
                     }
                     subject.id <- value$subject.id
                     if (length(subject.id) > 1) {
                         stop("Need a single subject id for randomization")
                     }

                     if (subject.id %in% rownames(object@tr.assignments)) {
                         stop(paste("Subject ID", subject.id, "already randomized!"))
                     }
                     expt <- object@expt
                     number.of.factors <- expt@number.of.factors
                     factor.values <- value$factor.values
                     if (length(factor.values) != number.of.factors) {
                         stop("Incorrect number of factors")
                     }
                     factor.level.names <- expt@factor.level.names
                     factor.values.kosher <- sapply(seq_len(number.of.factors),
                                                    function(x) factor.values[x] %in% factor.level.names[[x]])
                     if (! all(factor.values.kosher)) {
                         stop("Incorrect factor values provided")
                     }

                     number.of.treatments <- expt@number.of.treatments
                     treatment.names <- expt@treatment.names
                     factor.names <- expt@factor.names
                     named.factors <- paste(factor.names, factor.values, sep=":")
                     imbalances <- computeImbalances(object, factor.values)
                     overallImbalance <- computeOverallImbalance(object, imbalances)
                     ##print("Overall imbalance")
                     ##print(overallImbalance)
                     p.func <- object@p.func
                     tr.ratios <- object@tr.ratios

                     p.vec <- p.func(overallImbalance)
                     ##print("pvec")
                     ##print(p.vec)
                     tr.index <- sample(number.of.treatments, 1, prob=p.vec)
                     tr.name <- treatment.names[tr.index]
                     ##print(paste("Treatment is", tr.name))

                     ## Update state
                     state.matrix <- object@stateTable
                     state.matrix[tr.name, named.factors] <- state.matrix[tr.name, named.factors] + 1
                     object@stateTable <- state.matrix

                     ## Update assignment table
                     current.assignment <- data.frame(as.list(factor.values), tr.name, stringsAsFactors=FALSE)
                     rownames(current.assignment) <- subject.id
                     colnames(current.assignment) <- c(factor.names, "Treatment")
                     if (nrow(object@tr.assignments) == 0) {
                         assignments <- current.assignment
                     } else {
                         assignments <- rbind(object@tr.assignments, current.assignment)
                     }
                     ##tr.assignments(object) <- assignments
                     object@tr.assignments <- assignments
                     object
                 })


#' @rdname PocockSimonRandomizer-class
#' @export
setGeneric("lastRandomization",
           function(object) standardGeneric("lastRandomization"))

#'
#' Return the last randomization performed
#' @docType methods
#' @return a one-row data frame of the last treatment assignment
#' @importFrom knitr kable
#' @rdname PocockSimonRandomizer-class
#' @export
setMethod("lastRandomization",
          signature(object="PocockSimonRandomizer"),
          function (object) {
              n <- nrow(object@tr.assignments)
              if (n < 1) {
                  stop("No assignment yet!")
              } else {
                  print(knitr::kable(object@tr.assignments[n, ]))
              }
          })


