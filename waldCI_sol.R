#WaldCI source code from solutions
setClass("waldCI",
         slots = c(level = "numeric",
                   mean = "numeric",
                   sterr = "numeric"))


setValidity("waldCI", function(object) {
  if (object@level <= 0 | object@level >= 1) {
    stop("level must be in (0, 1)")
  }
  if (object@sterr <= 0) {
    stop("stderr must be positive")
  }
  if (!is.finite(object@sterr)) {
    stop("Infinite CI not supported")
  }
  return(TRUE)
})

##' Create `waldCI` object. Either `lb` and `ub`, or `mean` and `sterr` must be
##' provided.
##' @param level Confidence level
##' @param lb lower bound, optional
##' @param ub upper bound, optional
##' @param mean mean, optional
##' @param sterr standard error, optional
##' @return A `waldCI` object
##' @export
makeCI <- function(level,
                   lb = NULL,
                   ub = NULL,
                   mean = NULL,
                   sterr = NULL) {
  if (!is.null(mean) && !is.null(stderr) &&
      is.null(lb) && is.null(ub)) {
    return(new("waldCI", level = level, mean = mean, sterr = sterr))
  } else if (is.null(mean) && is.null(sterr) &&
             !is.null(lb) && !is.null(ub)) {
    if (lb > ub) {
      stop("lb must be less than ub")
    }
    z <- qnorm((1 + level) / 2)
    mean <- (lb + ub) / 2
    sterr   <- (ub - lb) / (2 * z)
    return(new("waldCI", level = level, mean = mean, sterr = sterr))
  } else {
    stop("Input must be either lb/ub or mean/sterr")
  }
}

##' Internal function to compute bounds from stored mean/sterr
##' @param ci Confidence level
##' @return A vector of length 2 for lower and upper bounds
.getBounds <- function(ci) {
  z <- qnorm((1 + ci@level)/2)
  lb <- ci@mean - z*ci@sterr
  ub <- ci@mean + z*ci@sterr
  return(c(lb, ub))
}

##' Show method for waldCI object
##' @param object A waldCI object
##' @return `object`, invisibly
##' @export
setMethod("show", "waldCI", function(object) {
  bound <- .getBounds(object)
  cat(round(object@level*100))
  cat("% CI: (")
  cat(bound[1])
  cat(", ")
  cat(bound[2])
  cat(")\n")
  return(invisible(object))
})

##' Internal function to get a named slot
##' @param object A `waldCI` object
##' @param slotname Which slot? Character.
##' @return A length-1 vector with the requested `slotname`.
.getSlot <- function(object, slotname) {
  if (slotname %in% c("level", "mean", "sterr")) {
    return(slot(object, slotname))
  }
  bounds <- .getBounds(object)
  if (slotname == "lb") {
    return(bounds[1])
  } else if (slotname == "ub") {
    return(bounds[2])
  } else {
    stop("Invalid slotname")
  }
}

##' Getters and Setters for Wald CI objects
##' @param object A `waldCI` object
##' @param value New value when setting
##' @return Getters return scalar; setters return the object
##' @rdname ci_setters_getters
##' @export
setGeneric("level", function(object) {
  standardGeneric("level")
})

##' @rdname ci_setters_getters
##' @export
setMethod("level", "waldCI", function(object) .getSlot(object, "level"))

# mean is already s3 generic, so just use that
##' @rdname ci_setters_getters
##' @export
mean.waldCI <- function(x, ...) .getSlot(x, "mean")

##' @rdname ci_setters_getters
##' @export
setGeneric("sterr", function(object) {
  standardGeneric("sterr")
})

##' @rdname ci_setters_getters
##' @export
setMethod("sterr", "waldCI", function(object) .getSlot(object, "level"))

##' @rdname ci_setters_getters
##' @export
setGeneric("lb", function(object) {
  standardGeneric("lb")
})

##' @rdname ci_setters_getters
##' @export
setMethod("lb", "waldCI", function(object) .getSlot(object, "lb"))

##' @rdname ci_setters_getters
##' @export
setGeneric("ub", function(object) {
  standardGeneric("ub")
})

##' @rdname ci_setters_getters
##' @export
setMethod("ub", "waldCI", function(object) .getSlot(object, "ub"))

##' @rdname ci_setters_getters
##' @export
setGeneric("level<-", function(object, value) {
  standardGeneric("level<-")
})

##' @rdname ci_setters_getters
##' @export
setMethod("level<-", "waldCI", function(object, value) {
  return(makeCI(level = value,
                mean = object@mean,
                sterr = object@sterr))
})

##' @rdname ci_setters_getters
##' @export
setGeneric("mean<-", function(object, value) {
  standardGeneric("mean<-")
})

##' @rdname ci_setters_getters
##' @export
setMethod("mean<-", "waldCI", function(object, value) {
  return(makeCI(level = object@level,
                mean = value,
                sterr = object@sterr))
})

##' @rdname ci_setters_getters
##' @export
setGeneric("sterr<-", function(object, value) {
  standardGeneric("sterr<-")
})

##' @rdname ci_setters_getters
##' @export
setMethod("sterr<-", "waldCI", function(object, value) {
  return(makeCI(level = object@level,
                mean = object@mean,
                sterr = value))
})

##' @rdname ci_setters_getters
##' @export
setGeneric("lb<-", function(object, value) {
  standardGeneric("lb<-")
})

##' @rdname ci_setters_getters
##' @export
setMethod("lb<-", "waldCI", function(object, value) {
  return(makeCI(level = object@level,
                lb = value,
                ub = .getBounds(object)[2]))
})

##' @rdname ci_setters_getters
##' @export
setGeneric("ub<-", function(object, value) {
  standardGeneric("ub<-")
})

##' @rdname ci_setters_getters
##' @export
setMethod("ub<-", "waldCI", function(object, value) {
  return(makeCI(level = object@level,
                lb = .getBounds(object)[2],
                ub = value))
})

##' Check whether a CI overlaps with a constant (`contains`) or another CI
##' (`overlap`)
##' @param ci A `waldCI` object
##' @param value A numeric value
##' @param ci1 A `waldCI` object
##' @param ci2 A `waldCI` object
##' @return Logical.
##' @rdname contains_overlaps
##' @export
contains <- function(ci, value) {
  stopifnot(is.numeric(value))
  stopifnot(length(value) == 1)
  bounds <- .getBounds(ci)
  return(value > bounds[1] & value < bounds[2])
}

##' @rdname contains_overlaps
##' @export
overlap <- function(ci1, ci2) {
  b1 <- .getBounds(ci1)
  b2 <- .getBounds(ci2)
  return(max(b1[1], b2[1]) <= min(b1[2], b2[2]))
}

##' Convert waldCI to numeric
##' @param x waldCI object
##' @return Length 2 vector of the bounds of `x`.
##' @export
setGeneric("as.numeric")
setMethod("as.numeric", "waldCI", function(x) {
  return(.getBounds(x))
})

##' Apply a monotonic transformation to a `waldCI`
##'
##' Monotonic functions are *not* enforced, but non-monotonic functions that
##' return nonsense results (e.g. ub < lb) will likely error.
##' @param ci A `waldCI` object
##' @param fn Any function that takes in a scalar and returns a scalar.
##' @return The transformed waldCI
##' @export
transformCI <- function(ci, fn) {
  stopifnot(is.function(fn))
  stopifnot(is(ci, "waldCI"))
  return(makeCI(level = ci@level,
                mean = fn(ci@mean),
                sterr = fn(ci@sterr)))
}