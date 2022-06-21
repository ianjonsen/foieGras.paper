require(tidyverse)
peng <- readRDS("data/tracks4ian_20190109.RDS") %>%
  rename(date = dtUTC) %>%
  filter(id %in% c("G123f02",
                   "G124m10",
                   "G126m05",
                   #"L002m01",
                   #"L001m01",
                   #"L008m01",
                   #"L012f01",
                   "L013m01")) %>%
  mutate(id = as.character(id)) #%>%
#  filter((id == "L008m01" & date < ISOdatetime(2017,10,08,04,00,00,tz="UTC")
#          | (id != "L008m01"))) #%>%
#  filter((id == "L012f01" & date < ISOdatetime(2017,10,13,06,00,00,tz="UTC")
 #         | (id != "L012f01")))

## Due to the penguins' frequent diving, GPS tags were set at a very high sampling rate (approximately 3 s) to minimise artefacts caused by interruptions of the GPS signal. To make this high-volume data more manageable, we first sub-sampled the raw GPS data by retaining every 5th observation from each track, reducing observation frequency to approximately 15 s.

## subsample to similar avg observation freq
ps1 <- peng %>%
  filter(id %in% "L013m01") %>%
  group_by(id) %>%
  slice(seq(1, nrow(.), by = 10)) %>%
  ungroup()
ps2 <- peng %>%
  filter(id %in% "G124m10") %>%
  group_by(id) %>%
  slice(seq(1, nrow(.), by = 5)) %>%
  ungroup()
ps3 <- peng %>%
  filter(id %in% "G126m05") %>%
  group_by(id) %>%
  slice(seq(1, nrow(.), by = 6)) %>%
  ungroup()
ps4 <- peng %>%
  filter(id %in% "G123f02") %>%
  group_by(id) %>%
  slice(seq(1, nrow(.), by = 4)) %>%
  ungroup()

peng.sub <- bind_rows(ps1, ps2, ps3, ps4) %>%
  mutate(lc = "G") %>%
  select(id,date,lc,lon,lat)

saveRDS(peng.sub, file = "data/ex33_lipe_gps_tracks.RDS", compress="xz")
