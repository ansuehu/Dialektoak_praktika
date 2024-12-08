
#' @title Computes a Dialectometrical measure
#'
#' @param data object coercible to data.frame containing the data.
#' @param formula formula indicating which variables represent ids and variables, see details.
#' @param value.var character of length 1 indicating the variable containing the values.
#' @param measure character of length 1 indiciating the selected.
#' @param binaryIndex character of length 1 indicating the binary index to use in multiple answers
#' @param subset index indicating which subset of data to take.
#'
#' @details Formula takes a expression of type 'var1 + var2 ~ var3' indication the index variables on
#' the LHS and reponse variables on the RHS. 
#' 
#' 
#' @export
#'
#' @import data.table
#' @useDynLib diaMeasures diaMeasure_C
#' 
#' @examples
#' 
#' data(dsample)
#' measure <- diaMeasure(dsample, gender + location ~ question, 'answer', 'ird')
#' print(measure)
#'
diaMeasure <- function(data, formula, value.var, measure = c('ird', 'ipi', 'levenshtein', 'iri', 'ipd'),
                       binaryIndex = c('dice', 'jaccard'), subset){
    
    mf <- match.call()
    m <- match(c('data', 'formula', 'value.var', 'subset'), names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf[[1L]] <- quote(data.table::dcast.data.table)
    mf$fun.agg <- quote(function(x) list(as.character(x)))
    if (!data.table::is.data.table(data)){
        mf$data <- substitute(data.table::as.data.table(data), list(data = mf$data))
    }
    mf <- as.list(eval(mf, parent.frame()))
    availableMethods <- c('ird', 'ipi', 'levenshtein', 'iri', 'ipd')
    idnbr <- length(all.vars(formula[[2]]))
    measure <- match(match.arg(measure), availableMethods)
    if (measure != 'levenshtein'){
        binaryIndex <- match(match.arg(binaryIndex), c('dice', 'jaccard'))
    } else {
        binaryIndex <- 'empty'
    }
    attrs <- list(Size = length(mf[[1]]), Labels = do.call(paste, mf[1:idnbr]),
                  class = 'diaMeasure', idVars = mf[1:idnbr])
    mf <- mf[-(1:idnbr)]
    result <- .Call(diaMeasure_C, mf, measure, binaryIndex, attrs)
    return (result)
}


#' Coertion to matrix
#'
#' Coerce the diaMeasure object into a  matrix class. Uses the as.matrix method from the package dist.
#'
#' @param x object of class diaMeasure
#' @param ... unused
#' 
#' @export
as.matrix.diaMeasure <- function(x, ...){
    d <- sqrt(length(x) * 2 +  length(attr(x, 'Labels')))
    y <- matrix(attr(x, 'diagv'), d, d)
    y[upper.tri(y)] <- x
    y[lower.tri(y)] <- t(y)[lower.tri(y)]
    colnames(y) <- rownames(y) <- attr(x, 'Labels')
    return (y)
}

#' Prints diaMeasure
#'
#' Print lower or upper part of the coerced matrix.
#'
#' @param x object of clas diaMeasure.
#' @param lower logical of length one indicating if only the lower part should be passed.
#' @param justify argument that is passed to format.
#' @param digits digits to be shown.
#' @param ... unused
#'
#' @export
print.diaMeasure <- function(x, lower = TRUE, justify = 'none', digits = getOption('digits'), ...){
    y <- as.matrix(x)
    z <- format(y, justify = 'none', digits = digits)
    if (lower){
        z[row(z) < col(z)] <- ''
    } else {
        z[row(z) > col(z)] <- ''
    }
    print(noquote(z))
    return (invisible(x))
}


#' Coerce to diaMeasure
#'
#' Coertion to diaMeasure, see the corresponding method.
#'
#' @param x Object to be coerced
#' @param ... parameters passed to other methods
#' @export
as.diaMeasure <- function(x, ...) UseMethod('as.diaMeasure')


#' Coerce matrix to diaMeasure
#'
#' Coerce matrix to diaMeasure.
#'
#' @param x square matrix
#' @param idVars attributes
#' @param ... parameters passed to other methods
#' 
#' @export
as.diaMeasure.matrix <- function(x, idVars, ...){
    if (nrow(x) != ncol(x)) stop ('"x" should be square')
    r <- range(diag(x))
    if ((r[2] - r[1]) > 1e-07) stop ('"x" non equal values in diagonal')
    y <- x[upper.tri(x)]
    attr(y, 'diagv') <- x[1]
    attr(y, 'Size') <- ncol(x)
    if (is.null(colnames(x))) colnames(x) <- as.character(1:ncol(x))
    attr(y, 'Labels') <- colnames(x)
    attr(y, 'class') <- 'diaMeasure'
    if (missing(idVars)) idVars <- data.frame(Labels = colnames(x), stringAsFactors = FALSE)
    attr(y, 'idVars') <- idVars
    return (y)
}
