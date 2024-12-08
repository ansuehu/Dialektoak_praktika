do_voronoiOld <- function(mat, ids, bound){
    if (anyDuplicated(mat) != 0L) stop ('duplicated points in location coordinates')
    if (nrow(mat) == 1L)  return (list(v = bound, mats = mat))
    if (!missing(bound)){
        rw <- as.numeric(t(sp::bbox(bound)))
        tl <- deldir::tile.list(deldir::deldir(mat[, 1L], mat[, 2L], rw = rw))
    } else {
        tl <- deldir::tile.list(deldir::deldir(mat[, 1L], mat[, 2L]))
    }
    ## polygons
    polys <- vector(mode = 'list', length = length(tl))
    mats <- vector(mode = 'list', length = length(tl))
    for (nbr in seq_along(tl)){
        xy <- tl[[nbr]]
        p <- matrix(c(xy$x, xy$x[1L], xy$y, xy$y[1L]), length(xy$x) + 1L, 2L)
        mats[[nbr]] <- p
        polys[[nbr]] <- sp::Polygons(list(sp::Polygon(p)), ID = as.character(ids[nbr]))
    }
    spPolys <- sp::SpatialPolygons(polys, proj4string = sp::CRS('+proj=merc'))
    vdf <- data.frame(
        x = mat[,1],
        y = mat[,2],
        row.names = sapply(methods::slot(spPolys, 'polygons'), slot, 'ID'))
    v <- sp::SpatialPolygonsDataFrame(spPolys, data = vdf)

    if (!missing(bound)){
        v <- rgeos::gIntersection(
            as(v, 'SpatialPolygons'),
            as(bound, 'SpatialPolygons'),
            byid = TRUE,
            id = rownames(v@data))
        v <- as(v, "SpatialPolygonsDataFrame")
        v@data <- vdf
        
        polygonCount <- sapply(v@polygons, function(x) length(x@Polygons))
        mPolygonInd <- which(polygonCount > 1L)
        
        if (any(polygonCount > 1L)){
            
            leftovers <- vector('list', sum(polygonCount[mPolygonInd]) - length(mPolygonInd))
            counter <- 0L
            vneigh <- spdep::poly2nb(v)
            oids <- c()
            
            for (i in mPolygonInd){
                
                counter <- counter + 1L
                spPolySubset <- v[i, ]
                polygons <- spPolySubset@polygons[[1]]@Polygons
                baricenter <- sp::coordinates(spPolySubset)
                whereIsBaricenter <- sapply(polygons, function(x) splancs::inout(baricenter, x@coords))
                frames <- split(polygons, whereIsBaricenter)
                id <- spPolySubset@polygons[[1]]@ID
                oids <- c(oids, id)
                v@polygons[i][[1]] <- sp::Polygons(frames[['TRUE']], spPolySubset@polygons[[1]]@ID)

                for (leftover in frames[['FALSE']]){
                    leftoverId <- paste0('leftover_', counter, '__ID_', id)
                    leftovers[[counter]] <- sp::Polygons(list(leftover), leftoverId)
                }
            }
            if (nrow(mat) == 179) browser()
            leftoverPolys <- sp::SpatialPolygons(leftovers, proj4string = sp::CRS('+proj=merc'))
            allPolys <- maptools::spRbind(v, leftoverPolys)
            nb <- spdep::poly2nb(allPolys)
            indexes <- (length(spPolys) + 1):length(allPolys)
            
            for (i in indexes){
                polysSubset <- allPolys[c(i, nb[[i]]), ]
                boundaryFrame <- rgeos::gUnionCascaded(polysSubset)
                boundaryPoints <- boundaryFrame@polygons[[1]]@Polygons[[1]]@coords
                ids <- sapply(allPolys[nb[[i]], ]@polygons, slot, 'ID')
                bounds <- sp::SpatialPolygons(
                    list(sp::Polygons(list(sp::Polygon(boundaryPoints)), 'no_id')),
                    proj4string = sp::CRS('+proj=merc'))
                newPolygons <- do_voronoi(sp::coordinates(allPolys[nb[[i]], ]), ids, bounds)$v
                v@polygons[nb[[i]]] <- newPolygons@polygons
            }
        }
    }
    return (list(v = v, mats = mats))
}
