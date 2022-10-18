## See https://rethomics.github.io/damr.html for details
library(sleepr)


metadata<-read.csv(file="Metadata.csv")
metadata<-link_dam_metadata(metadata,getwd())

dt <- load_dam(metadata,FUN = sleepr::sleep_dam_annotation)
summary(dt)

ggetho(dt, aes(z=activity)) +
  stat_tile_etho() +
  stat_ld_annotations()

ggetho(dt, aes(z=asleep)) +
  stat_tile_etho() +
  stat_ld_annotations()

ggetho(dt, aes(y=activity, colour=Treatment), time_wrap = hours(24)) +
  stat_pop_etho() +
  stat_ld_annotations()

ggetho(dt, aes(z=moving)) +
  stat_tile_etho() +
  stat_ld_annotations()

pl <- ggetho(dt, aes(x=t, z=activity), 
             multiplot = 2
) + 
  stat_bar_tile_etho() +
  facet_wrap( ~ id)
pl

