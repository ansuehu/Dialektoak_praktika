#!/usr/bin/Rscript

## genereate maps 

## This script generates the maps used by Diatech. 

library(diatechServer)
args <- commandArgs(TRUE)

## args ##

##-- argument 1º --##
## folder where the location point csvs are stored as well as the boundary csvs. The results will be
## stored there.

##-- argument 2º --##
## basename of the file where the location point csvs are stored.

##-- argument 3º --#
## "1" or "0" indicating to use the boundaries or not.

wd <- file.path(args[1], args[2])
locationsFile <- file.path(wd, paste0(args[2], '.csv'))
locations <- read.csv(locationsFile, sep = ';')
if (ncol(locations) == 1L)
    stop ('only one column in data point csvs, the separator is assumed to be ";"')
names(locations) <- c('id', 'x', 'y')

if (args[3] == '1'){
    write('Creating maps with boundary.', stdout())
    boundFiles <- list.files(wd, paste0(args[2], '_bound_[0-9].csv'), full.names = TRUE)
    if (length(boundFiles) == 0)
        stop ('A bound was requested but no files containing the boundaries were found')
    bounds <- lapply(boundFiles, read.csv, head = FALSE, sep = ';', quote = '"')
    maps <- generateMaps(locations, bounds)
} else {
    write('Creating maps with NO boundary.', stdout())
    maps <- generateMaps(locations)
}

write('Writting maps.', stdout())
 writeMaps(
     map = maps,
     neighPath = file.path(wd, 'nb.rda'),
     jpegPath = file.path(wd, 'map.jpg'),
     polySvgPath = file.path(wd, 'polygon.svg'),
     centSvgPath = file.path(wd, 'centroid.svg'),
     honeSvgPath = file.path(wd, 'honeycomb.svg'))













