#---- packages -----------------------------------------------------------------

library(dplyr)
library(geobr)
library(sf)
library(raster)
library(fasterize)
#-------------------------------------------------------------------------------

# opening farm size data

df <- read.csv("tables/farm_size_per_mun.csv")

# filtering total area of each mun

total_area <- df %>%
  filter(Grupos.de.área.total..Código.=='110085')

farm_sizes <- df %>%
  filter(!Grupos.de.área.total..Código.=='110085')

# get table with size classes and codes -- to get a clear view

area_cat <- unique(farm_sizes[,c(12,13)])

# define a treshold for farm sizes (here >1000ha), but you can change that

area_cat$farm_sizes_bin <- NA
area_cat$farm_sizes_bin[1:15] <- "<1000ha"
area_cat$farm_sizes_bin[16:19] <- ">=1000ha"

# add this new column to the dataframe

farm_sizes <- left_join(farm_sizes,area_cat)

# summarise data to get proportions

farm_sizes_bin <- farm_sizes %>%
  group_by_at(c(6,7,8,20))%>% 
  summarise(total_area_ha = sum(Valor,na.rm = T))%>%
  # adding column of the area of all property groups
  left_join(total_area[,5:8])%>%
  # changing name
  rename(mun_area=Valor)%>%
  #calculating the proportional area of each bin class
  mutate(prop_area_bin=round(total_area_ha/mun_area,2))

# there's quite a lot of 0's. Maybe changing the threshold might be a good idea

hist(farm_sizes_bin$prop_area_bin)

# spatializing the results

# here there are two options. You can either just rasterize the municipalities, like I did, or you can transform Brazil in a grid of points with the same resol. of mapbiomas for instance, extract the proportion value in each point and then convert it to raster. And then calculate the values at the 10km grid.


# municipalities boundaries for 2020

mun <- geobr::read_municipality(year = "2020")

names(farm_sizes_bin)[1] <- "code_muni"

# add proportion of the area of the mun covered by large farms
mun <- left_join(mun,farm_sizes_bin[farm_sizes_bin$farm_sizes_bin=='>=1000ha',])


# transform in raster (using municipality boundary)

#  use some actual raster to get the projection and resol. right (below I did res 1 degree)

r <- raster(mun, res = 1)

mun_r <- fasterize(mun, r, field = "prop_area_bin", fun="first")

# check output

plot(mun_r)
