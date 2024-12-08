#!/usr/bin/Rscript

COLORS <- 'red'
options(warn = 1L)

## FUNS ##
loadMeasure <- function(measure, database, user, password, projectParts, returnSet = FALSE, formula = id ~ question, labelsCol = 'id'){
    if (measure == 'Euclidean Distance'){
        set <- loadDiatech(
            database = database,
            user = user,
            password = password,
            parts = projectParts,
            type = 'geodist')
        measure <- getMeasures(set = set, labelsCol = labelsCol)
    } else {
        set <- loadDiatech(
            database = database,
            user = user,
            password = password,
            parts = projectParts,
            type = 'lingdata')
        set <- set[nchar(set$answer) != 0L, ]
        measure <- getMeasures(set, formula = formula, value.var = 'answer', measure = measure, binaryIndex = 'dice')
    }
    if (returnSet) return (list(set = set, measure = measure))
    return (measure)
}
## END FUNS ##

args <- commandArgs(TRUE)

args <- c('2', 'diatech2', 'root', 'zexU6udr',
          '/home/iker/diatech/newVersion/bourciez/matrix.csv',
          '/home/iker/diatech/newVersion/bourciez', '/home/iker/diatech/newVersion/bourciez',
	  '1', '1', '1', '2', '12', '6', '7')

args <- c('1', 'diatech3', 'root', 'zexU6udr',
          '/home/iker/diatech/newVersion/naf/matrix.csv',
          '/home/iker/diatech/newVersion/naf', '/home/iker/diatech/newVersion/naf',
	  '43', '3', '1', '2', '12', '4160')

args <- c('1', 'diatech2', 'root', 'zexU6udr',
          '/home/iker/diatech/newVersion/bourciez/matrix.csv',
          '/home/iker/diatech/newVersion/bourciez', '/home/iker/diatech/newVersion/bourciez',
	  '1', '3', '1', '2', '12', '1')

args <- c('4', 'diatech2', 'root', 'zexU6udr', '/home/iker/diatech/newVersion/bourciez/matrix.csv',
          '/home/iker/diatech/newVersion/bourciez',
	  '/home/iker/diatech/newVersion/bourciez', '1', '3', '3', '2', '12')





library(diaMeasures)
library(ggplot2)
library(diatechServer)
library(stats)
library(grid)
library(gridSVG)
library(gtable)
library(rjson)

#-- args meaning --#

#### common arguments to all maps ####

##-- argument 1º --##
## maptype :
##    "1" polygon map with no statistic
##    "2" polygon map with statistic
##    "3" centroid map (baricenter map) or honeycomb map (boundary map)
##    "4" cluster map
##    "5" multidimentional scalling

##-- argument 2º --##
## database to be used "diatech".

##-- argument 3º --##
## database user to be passed

##-- argument 4º --##
## password for the database

##-- argument 5º --##
## full path to save the distance matrix

##-- argument 6º --##
## file path to save the resulting json file.

##-- argument 7º --##
## file path where the neigbourhood object is

##-- argument 8º --##
## Projects parts :
##  "0" all Parts
##  "[0-9]+,[0-9]+, ... "  parts of the project to be analized

MAPTYPE <- args[1]
DATABASE <-  args[2]
USER <- args[3]
PASSWORD <- args[4]
MATRIX.PATH <- args[5]
JSON.PATH <- file.path(args[6], 'measure.json')
PROJECT.PATH <- args[7]
PROJECT.PARTS <- strsplit(args[8], ',')[[1]]

######################################################
#### arguments for POLYGON MAP WITH NO STATISTIC  ####
######################################################

##-- argument 9º --##
## MEASURE:
##     "1": IPI distance
##     "2": IRI distance
##     "3": Levenshtein distance
##     "4": IPD similarity
##     "5": IRD similarity
##     "7": Euclidean distance

##-- argument 10º --##
## GROUPS:
##     "1": Med
##     "2": MinMwMax
##     "3": MedMw

##-- argument 11º --##
## minimun number of groups. Usually 2.

##-- argument 12º --##
## maximun number of groups. Usually 12.

##-- argument 13º --##
## location id to be considered the center.

####

if (MAPTYPE == '1'){ ## polygon maps with no statistics
    
    MEASURE <- c('ipi', 'iri', 'levenshtein', 'ipd', 'ird', 'Euclidean Distance')[as.numeric(args[9])]
    GROUPING <- c('Med', 'MinMwMax', 'MedMw')[as.numeric(args[10])]
    MIN.GROUP <- as.numeric(args[11])
    MAX.GROUP <- as.numeric(args[12])
    LOCATION <- args[13]
    
    measure <- loadMeasure(MEASURE, DATABASE, USER, PASSWORD, PROJECT.PARTS)
    write.csv(as.matrix(measure), file = MATRIX.PATH)
    
    ngroups <- MIN.GROUP:MAX.GROUP
    if (GROUPING %in% c('MinMwMax', 'MedMw')) ngroups <- ngroups[ngroups %% 2L == 0L]

    cls <- getClassification(
        measure = measure,
        type = 'classic',
        method = GROUPING,
        ids = LOCATION,
        ns = ngroups)
    
    ## histogram ##
    
    mat <- as.matrix(measure)
    ind <- match(LOCATION, colnames(mat))
    #options(warn = -1)

    g <- ggplot(as.data.frame(mat[-ind, ind]),  aes(x = mat[-ind, ind])) +
      geom_density(aes(y=..density..))
    allBreaks <- c()
    breakLabels <- c()
    bins <- c()
    
    for (cl in cls){
        
        nbins <- max(attr(cl, 'range'))
        bins <- c(bins, nbins)
        breaks <- round(unname(sort(attr(cl[[1]], 'breaks'))), 2L)
        allBreaks <- c(allBreaks, breaks)
        breakLabels <- c(breakLabels, rep(paste0('group_', nbins), nbins + 1L))
        g <- g +
          geom_histogram(breaks = breaks, aes(y=..density..), colour="black", fill=COLORS, alpha = 0.5) 
        
    }
    
    g <- g +
      scale_x_continuous(breaks = allBreaks) +
      ylab("Density") + xlab("Measure") + ggtitle("Histogram") + theme_bw()
    
    plot(g)
    
    gridSVG::grid.export(file.path(dirname(JSON.PATH), 'histogram.svg'), strict = FALSE)
    grobs <- grid::grid.ls(print = FALSE)$name
    rectNames <- grep('geom_rect.rect.', grobs, value = TRUE)
    
    for (n in seq_along(rectNames)){

        grid.garnish(rectNames[n], nbins = bins[n])
        grid.garnish(rectNames[n], binNumber = 1L:bins[n], group = FALSE)
        
    }

    gridSVG::grid.export(file.path(dirname(JSON.PATH), 'histogram.svg'), strict = FALSE)
    dev.off()
    
    ## json ##
    id <- names(cls[[1]])
    if (length(id) > 1)
        stop ('If no nb argument is given only one reference location is used')

    tojson <- function(x){
        id <- names(x)
        x[[id]][id] <- 0
        result <- list(colors = x[[id]])
        return (result)
    }

    json <- lapply(cls, tojson)
    names(json) <- sapply(cls, function(x) max(attr(x, 'range')))
    
    write.csv(file = file.path(dirname(JSON.PATH), 'grobs.csv'), data.frame(allTags, 1:length(allTags)))
    write(x = rjson::toJSON(json), file = JSON.PATH)
    write(x = rjson::toJSON(breakLabels), file = file.path(dirname(JSON.PATH), 'xLabels.json'))
    
}

######################################################
#### arguments for POLYGON MAP WITH STATISTIC     ####
######################################################

##-- argument 9º --##
## MEASURE:
##     "1": IPI distance
##     "2": IRI distance
##     "3": Levenshtein distance
##     "4": IPD similarity
##     "5": IRD similarity
##     "7": Euclidean distanceJ

##-- argument 10º --##
## GROUPS:
##     "1": Med
##     "2": MinMwMax
##     "3": MedMw

##-- argument 11º --##
## minimun number of groups. Usually 2.

##-- argument 12º --##
## maximun number of groups. Usually 12.

##-- argument 13º --##
## statistic to be parsed
## STATISTIC:
##     "1": sd
##     "2": skewness
##     "3": median
##     "4": maxima
##     "5": minima
##     "7": correlation

##-- argument 14º --##
## extra argument if the statistic is the correlation. 

####

if (args[1] == '2'){ ## polygon maps with statistics

    MEASURE <- c('ipi', 'iri', 'levenshtein', 'ipd', 'ird', 'Euclidean Distance')[as.numeric(args[9])]
    GROUPING <- c('Med', 'MinMwMax', 'MedMw')[as.numeric(args[10])]
    MIN.GROUP <- as.numeric(args[11])
    MAX.GROUP <- as.numeric(args[12])
    STATISTIC <- c('sd', 'skewness', 'median', 'max', 'min', 'cor')[as.numeric(args[13])]
    
    measure <- loadMeasure(MEASURE, DATABASE, USER, PASSWORD, PROJECT.PARTS)
    
    if (STATISTIC == 'cor'){
        
        MEASURE2 <- c('ipi', 'iri', 'levenshtein', 'ipd', 'ird', 'Euclidean Distance')[as.numeric(args[13])]
        measure2 <- loadMeasure(MEASURE2, DATABASE, USER, PASSWORD, PROJECT.PARTS)
        stat <- getStatistic(measure, STATISTIC, measure2)
        
    } else {
        
        stat <- getStatistic(measure, STATISTIC)
        
    }
    
    ngroups <- MIN.GROUP:MAX.GROUP
    if (GROUPING %in% c('MinMwMax', 'MedMw')) ngroups <- ngroups[ngroups %% 2 == 0]

    cls <-  getClassification(
        statistic = stat,
        type = 'classic',
        method = GROUPING,
        ns = ngroups)
    
    ## histogram

    class(stat) <- c('numeric')
    
    g <- ggplot(data.frame(stat),  aes(x = stat)) +
      geom_density(aes(y=..density..))
    allBreaks <- c()
    breakLabels <- c()
    bins <- c()
    
    for (cl in cls){
        
        nbins <- length(attr(cl, 'breaks')) - 1L
        bins <- c(bins, nbins)
        breaks <- round(unname(sort(attr(cl, 'breaks'))), 2L)
        allBreaks <- c(allBreaks, breaks)
        breakLabels <- c(breakLabels, rep(paste0('group_', nbins), nbins + 1L))
        g <- g +
          geom_histogram(breaks = breaks, aes(y=..density..), colour="black", fill=COLORS, alpha = 0.5) 
        
    }
    
    g <- g +
      scale_x_continuous(breaks = allBreaks) +
      ylab("Density") + xlab("Statistic") + ggtitle("Histogram") + theme_bw()
    options(warn = -1L)
    plot(g)
    options(warn = 1L)
    
    gridSVG::grid.export(file.path(dirname(JSON.PATH), 'histogram.svg'), strict = FALSE)
    grobs <- grid.ls(print = FALSE)$name
    
    rectNames <- grep('geom_rect.rect.', grobs, value = TRUE)
    
    for (n in seq_along(rectNames)){
        
        grid.garnish(rectNames[n], nbins = bins[n])
        grid.garnish(rectNames[n], binNumber = 1L:bins[n], group = FALSE)

    }
        
    grid.garnish('axis.2-1-2-1', breaklabels = breakLabels, group = FALSE)

    gridSVG::grid.export(file.path(dirname(JSON.PATH), 'histogram.svg'), strict = FALSE)
    dev.off()

    ## json ##
    tojson <- function(x, stat){
        names(x) <- attr(stat, 'Labels')
        data.frame(id = attr(stat, 'Labels'), colors = x)
    }
    
    json <- lapply(cls, tojson, stat = stat)
    names(json) <- sapply(cls, function(x) length(attr(x, 'breaks')) - 1L)
    write(x = rjson::toJSON(json), file = JSON.PATH)
    write(x = rjson::toJSON(breakLabels), file = file.path(dirname(JSON.PATH), 'xLabels.json'))
    
}

######################################################
#### arguments for HONEYCOMB and CENTROID MAPS    ####
######################################################

##-- argument 9º --##
## MEASURE:
##     "1": IPI distance
##     "2": IRI distance
##     "3": Levenshtein distance
##     "4": IPD similarity
##     "5": IRD similarity
##     "7": Euclidean distance

##-- argument 10º --##
## GROUPS:
## GROUPS:
##     "1": Med
##     "2": MinMwMax
##     "3": MedMw


##-- argument 11º --##
## minimun number of groups. Usually 2.

##-- argument 12º --##
## maximun number of groups. Usually 12.

if (args[1] %in% c('3', '4')){ ## honeycomb and centroid maps

    MEASURE <- c('ipi', 'iri', 'levenshtein', 'ipd', 'ird', 'Euclidean Distance')[as.numeric(args[9])]
    GROUPING <- c('Med', 'MinMwMax', 'MedMw')[as.numeric(args[10])]
    MIN.GROUP <- as.numeric(args[11])
    MAX.GROUP <- as.numeric(args[12])
    
    vars <- load(file.path(PROJECT.PATH, 'nb.rda'))
    if (!identical(vars, 'nb')) stop ('the rda in BOUNDARY.PATH must only contain one object. "nb"')
    
    measure <- loadMeasure(MEASURE, DATABASE, USER, PASSWORD, PROJECT.PARTS)
    measure[is.na(measure)] <- 0.5
    
    ngroups <- MIN.GROUP:MAX.GROUP
    if (GROUPING %in% c('MinMwMax', 'MedMw')) ngroups <- ngroups[ngroups %% 2 == 0]

    cls <- getClassification(
        measure = measure,
        type = 'classic',
        method = GROUPING,
        ns = ngroups,
        nb = nb)
    
    ## histogram
    mat <- as.matrix(measure)
    
    g <- ggplot(data.frame(attr(cls, 'Values')),  aes(x = attr(cls, 'Values'))) +
      geom_density(aes(y=..density..))
    
    allBreaks <- c()
    breakLabels <- c()
    bins <- c()
    
    for (cl in cls){
        
        nbins <- length(attr(cl, 'breaks')) - 1L
        bins <- c(bins, nbins)
        breaks <- round(unname(sort(attr(cl, 'breaks'))), 2L)
        allBreaks <- c(allBreaks, breaks)
        breakLabels <- c(breakLabels, rep(paste0('group_', nbins), nbins + 1L))
        g <- g +
          geom_histogram(breaks = breaks, aes(y=..density..), colour="black", fill=COLORS, alpha = 0.5) 
        
    }
    
    g <- g +
      scale_x_continuous(breaks = allBreaks) +
      ylab("Density") + xlab("Measure") + ggtitle("Histogram") + theme_bw()
    options(warn = -1L)
    plot(g)
    options(warn = 1L)
    
    gridSVG::grid.export(file.path(dirname(JSON.PATH), 'histogram.svg'), strict = FALSE)
    grobs <- grid.ls(print = FALSE)$name
    
    rectNames <- grep('geom_rect.rect.', grobs, value = TRUE)
    
    for (n in seq_along(rectNames)){
        
        grid.garnish(rectNames[n], nbins = bins[n])
        grid.garnish(rectNames[n], binNumber = 1L:bins[n], group = FALSE)

    }
        
    grid.garnish('axis.2-1-2-1', breaklabels = breakLabels, group = FALSE)
    grid.garnish('axis.1-1-1-1', breaklabels = breakLabels, group = FALSE)

    gridSVG::grid.export(file.path(dirname(JSON.PATH), 'histogram.svg'), strict = FALSE)
    dev.off()

    ## json ##
    tojson <- function(x, ids, ids1, ids2)
        data.frame(id = ids, ids1 = ids1, ids2 = ids2, colors = x)

    ids <- names(cls[[1]])
    idSplit <- strsplit(ids, '_')
    ids1 <- sapply(idSplit, '[', 1L)
    ids2 <- sapply(idSplit, '[', 2L)
    json <- lapply(cls, tojson, ids = ids, ids1 = ids1, ids2 = ids2)
    names(json) <- sapply(cls, function(x) length(attr(x, 'breaks')) - 1L)
    write(x = rjson::toJSON(json), file = JSON.PATH)
    write(x = rjson::toJSON(breakLabels), file = file.path(dirname(JSON.PATH), 'xLabels.json'))
    
}


######################################################
####          arguments for CLUSTER               ####
######################################################

##-- argument 9º --##
## MEASURE:
##     "1": IPI distance
##     "2": IRI distance
##     "3": Levenshtein distance
##     "4": IPD similarity ## not accepted
##     "5": IRD similarity ## not accepted
##     "7": Euclidean distance 

##-- argument 10º --##
## GROUPS:
##     "1": ward
##     "2": average
##     "3": complete

##-- argument 11º --##
## minimun number of groups. Usually 2.

##-- argument 12º --##
## maximun number of groups. Usually 12.
if (args[1] == '5'){ ## cluster maps
    
    library(ggdendro)
    library(rjson)
    
    MEASURE <- c('ipi', 'iri', 'levenshtein', 'ipd', 'ird', 'Euclidean Distance')[as.numeric(args[9])]
    GROUPING <- c('ward.D2', 'average', 'complete')[as.numeric(args[10])]
    MIN.GROUP <- as.numeric(args[11])
    MAX.GROUP <- as.numeric(args[12])

    rlist <- loadMeasure(MEASURE, DATABASE, USER, PASSWORD, PROJECT.PARTS, returnSet = TRUE,
                         formula = location ~ question, labelsCol = 'location')
    rlist$measure[is.na(rlist$measure)] <- mean(rlist$measure)
    hc <- hclust(as.dist(rlist$measure), method = GROUPING)
    
    ddata <- dendro_data(as.dendrogram(hc))
    segments <- ddata$segments
    labels <- label(ddata)
    segments <- data.frame(x = c(segments$x, segments$xend), y = c(segments$y, segments$yend))

    do_polygon <- function(df){
        
        df <- df[df$y < cuth, ]
        forward <- df[order(df$x, - df$y), ]
        backward <- df[order(df$x, df$y), ]
        
        inc <- cummax(backward$y)
        dec <-  rev(cummax(rev(forward$y)))
        ys <- ifelse(inc > dec, dec, inc)
        xs <- ifelse(inc > dec, backward$x, forward$x)

        if (length(xs) == 1L) return (NULL)
        
        return (list(x = xs, y = ys))
        
    }

    p <- ggdendrogram(hc, rotate = FALSE, size = 2)
    polygonTags <- c()
    polyNumber <- c()
    nseq <- MIN.GROUP:MAX.GROUP
    jsonResult <- vector('list', length(nseq))
    
    for (n in nseq){
        
        cutted <- cutree(hc, n)
        cuth <- hc$height[length(hc$height) - n + 1L] + 1e-05
        labels$group <- cutted[match(labels$label, names(cutted))]
        ids <- rlist$set$id[match(names(cutted), rlist$set$location)]
        jsonResult[[n - 1L]] <- data.frame(ids = ids, location = labels$label, cluster = labels$group)
        breaks <- sort(tapply(labels$x, labels$group, max))
        groups <- cut(x = segments$x, breaks = c(0, breaks), labels = FALSE, include.lowest = TRUE)
        polygons <- lapply(split(segments, groups), do_polygon)
        polygons <- polygons[!sapply(polygons, is.null)]
        names(cutted) <- rlist$set$id[match(names(cutted), rlist$set$location)]
        for (polygon in polygons)
            p <- p + geom_polygon(aes(x, y), data = as.data.frame(polygon), fill = COLORS, alpha = 0.5)
        
        polygonTags <- c(polygonTags, rep(n, length(polygons)))
        polyNumber <- c(polyNumber, unique(cutted[hc$order]))
        
        
    }
    
    plot(p)
    grid.export(file.path(dirname(JSON.PATH), 'dendrogram.svg'), strict = FALSE)
    polyElements <- grep('geom_polygon', grid.ls(print = FALSE)$name, value = TRUE, fixed = TRUE)

    for (n in 1L:length(polyElements))
        grid.garnish(
            polyElements[n],
            polygon_group = as.character(polygonTags[n]),
            polygon_number = polyNumber[n])
    
    grid.export(file.path(dirname(JSON.PATH), 'dendrogram.svg'), strict = FALSE)
    names(jsonResult) <- as.character(nseq)
    
    ## end to fun
    write(rjson::toJSON(jsonResult), file = JSON.PATH)
        
}


######################################################
####          arguments for MDscalling            ####
######################################################

##-- argument 9º --##
## MEASURE:
##     "1": IPI distance
##     "2": IRI distance
##     "3": Levenshtein distance
##     "4": IPD similarity ## not accepted
##     "5": IRD similarity ## not accepted
##     "7": Euclidean distance 

##-- argument 10º --##
## K: dimentions. Currently 1, 2 or 3.
if (args[1] == '6'){ ## md scalling
    
    library(GGally)

    MEASURE <- c('ipi', 'iri', 'levenshtein', 'ipd', 'ird', 'Euclidean Distance')[as.numeric(args[9])]
    K <- as.integer(args[10])
    if (K > 3L) stop ('K can not be higher than 3')
    
    res <- loadMeasure(MEASURE, DATABASE, USER, PASSWORD, PROJECT.PARTS, returnSet = TRUE)

    similarity <- attr(res$measure, 'diagv') == 100L
    if (similarity) stop ('Not dissimilarity measure')
    mdScale <- cmdscale(as.dist(res$measure), k = K, eig = TRUE)
    help(cmdscale)
    
    points <- mdScale$points
    colorProportions <- split(
        apply(points, 2, function(x) {
                  y <- abs(min(x)) + x
                  y / max(y)
          }),
        rep(1:K, each = nrow(points)))
    names(colorProportions) <- paste('Coordinate', 1L:ncol(points))
    ord <- match(rownames(points), as.character(res$set$id))
    df <- cbind(
        id  = rownames(points),
        location = res$set$location[ord],
        data.frame(colorProportions, check.names = FALSE))
    
    ## plot
    dfplot <- cbind(df[1:2], mdScale$points)
    names(dfplot) <- names(df)
    names(dfplot) <- gsub(' ', '_', names(dfplot))
    g <- ggpairs(data = dfplot,
                 columns = 3L:ncol(dfplot), # columns to plot, default to all.
                 title = "MDS", upper = 'blank') + theme_bw()
    print(g)
    options(warn = -1L)
    gridSVG::grid.export(file.path(dirname(JSON.PATH), 'scatterplot.svg'), strict = FALSE)
    options(warn = 1L)
    grobs <- grid.ls(print = FALSE)$name
    scatters <- grep('geom_point.points.', grobs, fixed = TRUE, value = TRUE)

    for (scatter in scatters){
        
        grid.garnish(
            scatter,
            id = as.character(dfplot$id),
            locations = as.character(dfplot$location),
            group = FALSE)
        
    }
    
    options(warn = -1L)
    gridSVG::grid.export(file.path(dirname(JSON.PATH), 'scatterplot.svg'), strict = FALSE)
    options(warn = 1L)
    dev.off()
    write(x = toJSON(df), file = JSON.PATH)
}


######################################################
####        arguments for FuzzyClustering         ####
######################################################

##-- argument 9º --##
## MEASURE:
##     "1": IPI distance
##     "2": IRI distance
##     "3": Levenshtein distance
##     "4": IPD similarity ## not accepted
##     "5": IRD similarity ## not accepted
##     "7": Euclidean distance 

##-- argument 10º --##
## K: dimentions. Integer
if (args[1] == '7'){

    library(cluster)
    library(rjson)

    MEASURE <- c('ipi', 'iri', 'levenshtein', 'ipd', 'ird', 'Euclidean Distance')[as.numeric(args[9])]
    K <- as.integer(args[10])
    
    measure <- loadMeasure(MEASURE, DATABASE, USER, PASSWORD, PROJECT.PARTS, returnSet = TRUE)
    similarity <- attr(measure$measure, 'diagv') == 100L
    if (similarity) stop ('Not dissimilarity measure')
    
    measure$measure[is.na(measure$measure)] <- 0L
    clusters <- fanny(as.matrix(measure$measure), K, diss = TRUE, memb.exp = 1.5)
    colnames(clusters$membership) <- paste0('dimention_', 1L:K)

    ## cluster proportions
    ord <- match(rownames(clusters$membership), as.character(measure$set$id))
    mapped <- cbind(
        data.frame(
            id = rownames(clusters$membership),
            location = measure$set$location[ord]),
        clusters$membership)
    
    ## cluster centroids
    mesMatrix <- as.matrix(measure$measure)
    centerIds <- vector('character', K)
    for (k in 1L:K){
        
        groupElements <- names(clusters$clustering)[clusters$clustering == k]
        indexes <- which(colnames(mesMatrix) %in% groupElements)
        centerIds[k] <- names(which.min(rowSums(mesMatrix[indexes, indexes, drop = FALSE])))
        
    }

    centerLocs <- measure$set$location[match(centerIds, as.character(measure$set$id))]
    
    mappedCentroids <- data.frame(
        id = centerIds,
        location = centerLocs,
        values = 1L:K)

    ## write result
    result <- rjson::toJSON(list(partition = mapped, centroids = mappedCentroids))
    write(x = result, file = JSON.PATH)
    
    ## cmdscalle
    mdScale <- cmdscale(as.dist(measure$measure), k = 2L, eig = TRUE)
    colnames(mdScale$points) <- c('x', 'y')
    df <- data.frame(mdScale$points)
    g <- ggplot(df, aes(x = x, y = y)) + geom_point(shape = clusters$clustering) + theme_bw() +
      xlab('First Component') + ylab('Second Component') + ggtitle('MDS of linguistical distance')
    plot(g)
    
    gridSVG::grid.export(file.path(dirname(JSON.PATH), 'scatterplot.svg'), strict = FALSE)
    grobs <- grid.ls(print = FALSE)$name
    pointsName <- grep('geom_point', grobs, value = TRUE)
    tnames <- measure$set$location[match(rownames(df), measure$set$id)]
    grid.garnish(pointsName, location = tnames, group = FALSE)
    grid.garnish(pointsName, location_id = rownames(df), group = FALSE)
    grid.garnish(pointsName, cluster = unname(clusters$clustering), group = FALSE)
    
    gridSVG::grid.export(file.path(dirname(JSON.PATH), 'scatterplot.svg'), strict = FALSE)
    dev.off()
    
}


######################################################
####        arguments for PCA         ####
######################################################
