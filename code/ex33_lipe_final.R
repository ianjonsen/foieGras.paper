peng <- readRDS("data/tracks4ian_20190109.RDS") %>%
  rename(date = dtUTC) %>%
  filter(id %in% c("G123f02",
                   "G123m06",
                   "G123m06",
                   "G124m10",
                   "G126m05",
                   "G128f06",
                   "L013m01")) %>%
  mutate(id = as.character(id))

peng.sub <- peng %>%
  group_by(id) %>%
  amt::make_track(., lon, lat, date, id=id) %>%
  amt::track_resample(rate = minutes(5), tolerance = seconds(10)) %>%
  select(id, date=t_, lon=x_, lat=y_) %>%
  ungroup()

fmp <- fit_mpm(peng.sub, model = "jmpm")

fit <- peng %>%
  group_by(id) %>%
  slice(seq(1, nrow(.), by = 5)) %>%
  ungroup() %>%
  mutate(lc = "G") %>%
  select(id,date,lc,lon,lat) %>%
  fit_ssm(
    .,
    min.dt = 4,
    model = "crw",
    time.step = 5 / 60
  )

fmp.crw <- fit_mpm(fit, what="p", model = "jmpm")


p1 <- plot(fmp, pages = 1, ncol = 1, ask = FALSE, pal = "Cividis") &
  theme(legend.position = "none")

p2 <- plot(fmp.crw, pages = 1, ncol = 1, ask = FALSE, pal = "Cividis") &
  theme(legend.position = "none")



