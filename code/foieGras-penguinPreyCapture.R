library(ggplot2)
library(foieGras)

#'-----------------------------------------------------#
#'  __Function to merge tracks and predation events__  #
#'-----------------------------------------------------#
merge.tracks.preds <- function(tracks, preds){
  # drop tracks not in events
  tracks$id <- as.character(tracks$id)
  tracks <- tracks[tracks$id %in% unique(preds$id),]
  tracks$preyCount <- 0

  # Get only "foraging events"
  predsl <- preds
  #predsl <- preds[preds$forage == 1,]
  predsl <- split(predsl, with(predsl, interaction(id)), drop = TRUE)
  # Unique events
  uniqueEventsl2 <- lapply(predsl, function(x) x[round(diff(as.POSIXct(x$dtUTC))) > 2,])

  # Loop events
  tracks <- split(tracks, tracks$id)
  for (i in 1:length(tracks)){
    # go through events and find the nearest track timestamp for each
    events <- uniqueEventsl2[[tracks[[i]]$id[1]]]
    events$idx <- sapply(events$dtUTC, function(dt)
      which.min(abs(as.numeric(difftime(dt, tracks[[i]]$dtUTC, unit='sec')))))
    # populate tracks
    for (idx in unique(events$idx)){
      tracks[[i]]$preyCount[idx] <- sum(events$idx == idx)
    }
  }
  # merge tracks back together
  tracks <- do.call(rbind, tracks)
  row.names(tracks) <- NULL
  return(tracks)
}

#'------------------#
#' __1. Load data__ #
#'------------------#
#'# load peng (ssm tracks)
load("./data/gps/misc/foiegras_tracks_SSM.RData")
# get regular locations
locs <- foieGras::grab(fit, "p")
# process to match my format
peng <- as.data.frame(locs)
names(peng)[2] <- 'dtUTC'
# Load SVM results
preds <- readRDS("./data/preycapture/preycapture_events_raw.rds")
# filter SVM
preds <- preds[preds$id %in% unique(peng$id),]

#'-----------------------#
#' __2. Aggregate data__ #
#'-----------------------#
# Merge regularised tracks and prey capture events
tracks <- merge.tracks.preds(peng, preds)
# spatial aggrgetaion of data (sum)
pred.sum <- aggregate(preyCount ~ id  + round(lon, 3) + round(lat, 3), data=tracks, FUN=sum)
names(pred.sum) <- c('id', 'lon', 'lat', 'preyCount')
# remove zero captures
pred.sum <- pred.sum[pred.sum$preyCount > 0,]

# Plot example
ggplot(pred.sum, aes(x=lon, y=lat, size=preyCount)) +
  geom_point(shape=1) + theme_bw()

saveRDS(pred.sum, './data/misc/foieGras_penguinPreyCapture.rds')

