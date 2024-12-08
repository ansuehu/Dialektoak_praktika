#' @title Classic Dialektometrical ad-hoc classification
#'
#' Applies certain ad-hoc measures used in Dialektometrics. 
#'
#' The available classification methods are Med MinMwMax and MedMw.
#'
#' @param measure Object coercible to data.frame containing the data.
#' @param method Classification method, see details.
#' @param n Number of groups to be made.
#' @param ... arguments passing to other method.
#'
#' @export
#'
#' @examples
#' 
#' data(dsample)
#' measure <- diaMeasure(dsample, location ~ question, 'answer', 'ird')
#' diaClass <- diaClassification(measure, 'MinMwMax', 6)
#' diaClass2 <- diaClassification(measure, 'Med', 2, 'Itziar')
#'
#' diaClassVector <- diaClassification(as.matrix(measure)[, 1], 'Med', 2)
#' 
#'
diaClassification <- function(measure, method = c('Med', 'MinMwMax', 'MedMw'), n, ...)
    UseMethod('diaClassification')

#' @export
diaClassification.numeric <- function(measure, method, n, ids, na.rm = FALSE, ...){
    if (method == 'Med'){
        
       breaks <- quantile(measure, seq(0L, 1L, by = 1L / n), na.rm = na.rm)
       if (anyDuplicated(breaks)){
           warning ('Too few observations for significant classification in Med')
           breaks <- unique(breaks)
       }
       
       result <- cut(measure, breaks, labels = FALSE, include.lowest = TRUE)
       
   } else if (method == 'MinMwMax'){
       
       meanx <- mean(measure); minx <- min(measure); maxx <- max(measure)
       lw <- (abs(meanx - minx) - 1e-06) / (n / 2)
       up <- (abs(maxx - meanx) + 1e-06) / (n / 2)
       breaks <- c(minx, 1:(n / 2) * lw + minx, meanx + 1:(n / 2) * up)
       result <- cut(measure, breaks, labels = FALSE, include.lowest = TRUE)
       
   } else { # MedMw

       splitPoint <- measure[which.min(abs(measure - mean(measure)))]
       lowerBreaks <- quantile(measure[measure < splitPoint], seq(0, 1L, 2L / n), na.rm = na.rm)
       higherBreaks <- quantile(measure[measure >= splitPoint], seq(0, 1L, 2L / n), na.rm = na.rm)[-1]
       breaks <- c(lowerBreaks, higherBreaks)
        if (anyDuplicated(breaks)){
            warning ('Too few observations for significant classification in MedMw.')
            breaks <- unique(breaks)
        }
        result <- cut(measure, breaks, labels = FALSE, include.lowest = TRUE)
        
    }
    attr(result, 'breaks') <- breaks
    return (result)
}
    
#' @export    
diaClassification.diaMeasure <- function(measure, method = c('Med', 'MinMwMax', 'MedMw'), n, ids, na.rm = FALSE, ...){
    method <- match.arg(method)
    if (method %in% c('MinMwMax', 'MedMw') & (n %% 2L != 0) )
        stop ('For method ', method, ' the parameter n must be multiple of 2')
    measureMat <- as.matrix(measure)
    
    if (missing(ids)){
        
        ids <- attr(measure, 'Labels')
        d <- ncol(measureMat) - 1L
        measureMat <- matrix(measureMat[row(measureMat) != col(measureMat)], d, d + 1, FALSE)
        
    } else {
        
        ids <- unique(ids)
        outlawIds <- ids[!(ids %in% attr(measure, 'Labels'))]
        if (length(outlawIds) != 0L){
            stop ('ids ', paste(outlawIds, collapse = ', '), ' are not in the "Labels" attribute of measure')
        }
        nrows <- ncol(measureMat) - 1L
        ncols <- length(ids)
        matched <- match(ids, attr(measure, 'Labels'))
        measureMat <- matrix(measureMat[row(measureMat) != col(measureMat) & col(measureMat) %in% matched], nrows, ncols, TRUE)
        
    }
    allIds <- attr(measure, 'Labels')
    result <- setNames(vector('list', length(ids)), ids)

    for (i in seq_along(ids)){
        id <- ids[i]
        result[[id]] <- setNames(
            diaClassification(measureMat[, i], method, n, na.rm = na.rm),
            setdiff(attr(measure, 'Labels'), id))
    }
    
    attr(result, 'class') <- 'diaClassification'
    attr(result, 'range') <- 1L:n
    attr(result, 'Labels') <- allIds
    attr(result, 'measureIsDistance') <- attr(measure, 'diagv') == 0L
    
    return (result)
}




