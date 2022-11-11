#---- packages -----------------------------------------------------------------

library(sidrar)
library(geobr)
library(dplyr)
library(tidyr)
library(sf)

#------------------------------------------------------------------------------

# municipalities boundaries for 2020

mun <- geobr::read_municipality(year = "2020")

# region boundaries (N,S,SE,NE)

regions <- read_region() # check the codes to subset the API request

list_regions_code <- regions$code_region

# select columns of interest

mun_df <- mun[,1:2]

# get rid of the geometry

st_geometry (mun_df) <- NULL

# n of groups to stratify the acess to the API

num_groups = 20
# 1150
list_df <- mun_df %>% 
  group_by((row_number()-1) %/% (n()/num_groups)) %>%
  nest %>% pull(data)

#listing mun code

mun_code <- lapply(list_df,function(x)as.character(unique(x$code_muni)))

# table

df <- 6754

# check parameter for specific table

info_sidra(df, wb = TRUE)

# filters for ibge datasets

period <- c('2017')
territorial_level <- "City"
variable <- c(184) #  Área dos estabelecimentos agropecuários (Hectares) - casas decimais: padrão = 0, máximo = 3
classific <- "c220" #Grupos de área total(20):

# # just checking the table
# m <- get_sidra(x = df,variable = variable,classific = c("c220"))


f <- function(x)get_sidra(x = df,variable = variable,period = period,classific = c("c220"),geo=territorial_level,geo.filter = list(x))

property_data <- lapply(mun_code,f)

# combining the data again

property_data_df <- as.data.frame(do.call(rbind,property_data))


# checking doubles!

property_data_df <- property_data_df[!duplicated(property_data_df),]

write.csv(property_data_df,"tables/farm_size_per_mun.csv",row.names = F)


