## get SESE data w error ellipses
##
require(tidyverse)
require(foieGras)

## Use 2021 IMOS deployments @ Kerguelen
ct160 <- imosQCdev::pull_smru_tables("ct160",
                                     "~/R/imos/imos_sat/past/mdb",
                                     p2mdbtools = "/opt/homebrew/Cellar/mdbtools/1.0.0/bin/")

ids <- c("ct160-267-20",
         "ct160-300-20",
         "ct160-302-20",
         "ct160-307-20",
         "ct160-308-20",
         "ct160-314-20",
         "ct160-318-20",
         "ct160-341-20")

d160 <- ct160$diag %>%
  select(id=ref,
         date=d_date,
         lc=lq,
         lon, lat,
         smaj=semi_major_axis,
         smin=semi_minor_axis,
         eor=ellipse_orientation) %>%
  mutate(lc = factor(lc,
                     levels = c(3,2,1,0,-1,-2,-9),
                     labels = c(3,2,1,0,"A","B","Z"),
                     ordered = TRUE)) %>%
  filter(id %in% ids) %>%
  filter((id == "ct160-267-20" & date > ISOdate(2021,02,15, tz = "UTC")) |
           id != "ct160-267-20") %>%
  filter((id == "ct160-300-20" & date > ISOdate(2021,01,21, tz = "UTC") &
            date < ISOdate(2021,10,02, tz="UTC")) |
           id != "ct160-300-20") %>%
  filter((id == "ct160-302-20" & date > ISOdate(2021,01,04, tz = "UTC") &
            date < ISOdate(2021,05,14, tz="UTC")) |
           id != "ct160-302-20") %>%
  filter((id == "ct160-307-20" & date > ISOdate(2021,02,03, tz = "UTC") &
            date < ISOdate(2021,10,04, tz="UTC")) |
           id != "ct160-307-20") %>%
  filter((id == "ct160-308-20" & date < ISOdate(2021,07,27, tz="UTC")) |
           id != "ct160-308-20") %>%
  filter((id == "ct160-314-20" & date > ISOdate(2021,01,17, tz="UTC") &
            date < ISOdate(2021,06,24, tz="UTC")) |
           id != "ct160-314-20") %>%
  filter((id == "ct160-318-20" & date < ISOdate(2021,04,16, tz="UTC")) |
           id != "ct160-318-20") %>%
  filter((id == "ct160-341-20" & date < ISOdate(2021,04,18, tz="UTC")) |
           id != "ct160-341-20")

## Use these tracks for ex3.2
sese <- d160 %>% filter(id %in% c("ct160-302-20",
                                  "ct160-307-20",
                                  "ct160-314-20",
                                  "ct160-341-20"))

fit <- fit_ssm(d160, vmax = 3, model = "mp", time.step = 12)

my.aes <- aes_lst(conf=F, line=T)
my.aes$df$col[3] <- grey(0.3)
my.aes$df$size[3] <- 0.1
my.aes$df$fill[5] <- grey(0.4)
my.aes$df$fill[6] <- grey(0.7)
my.aes$df$size[1] <- 1.5
map(fit, what = "p", aes = my.aes, normalise=T, group=T)

save(sese, file = "data/ex32_sese.RData", compress="xz")
