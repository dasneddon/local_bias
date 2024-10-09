library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("canadianmaps")
library(plyr)
library(stringr)
library(usmap)
library(spData) 

gravitymap <- function (x){
sf_use_s2(FALSE)

worldz <- ne_countries(scale = "large", returnclass = "sf")
class(worldz)
(sites <- data.frame(longitude = c(-172.54, -47.74), latitude = c(23.81,
                                                                  90)))


(sites <- st_as_sf(geodata, coords = c("lon", "lat"),
                   crs=4369, agr = "constant"))

sites <- st_transform(sites, st_crs("ESRI:102010"))
library("maps")
states <- st_transform(us_states, st_crs("ESRI:102010"))
head(states)
provs <- st_sf(PROV[,c(5, 10, 11, 12)])
ak <- st_transform(alaska, st_crs("ESRI:102010"))
hi <- st_transform(hawaii, st_crs("ESRI:102010"))
akhi <- rbind(ak,hi)
akhi <- cbind(akhi, st_coordinates(st_centroid(akhi)))
akhi <- st_sf(akhi[c(2,7,8,9)])
provs <- st_transform(provs, st_crs("ESRI:102010"))


states <- cbind(states, st_coordinates(st_centroid(states)))
states <- states[c(2, 7, 8, 9)]
colnames(provs) <- colnames(akhi) <- colnames(states)

head(states)
library("tools")

head(states)
stpr <- st_sf(rbind(as.data.frame(states),
                    as.data.frame(provs),
                    as.data.frame(akhi)))

  tf <- data.frame(us_ca_split[x])
  prov_abr2 <- prov_abr
  colnames(prov_abr2) <- colnames(st_abbr)
  tabrs <- rbind(st_abbr, prov_abr2)
  colnames(tabrs)[1] <- "NAME"
  colnames(tf) <- colnames(us_ca)
  tf$value <- asinh(tf$value)
  tf <- tf[c(1, 7)]
  colnames(tf)[1] <- "State"
  tstpr <- merge(stpr, tabrs, by = "NAME")
  tstpr <- merge(tstpr, tf, by = "State")

  ggplot(data = worldz) +
    geom_sf() +
    geom_sf(data = tstpr, 
            aes(fill =value)) +
    scale_fill_gradient(low="red", high= "green") +
    coord_sf(xlim = c(-6500000, 3800000), 
             ylim = c(4700000, -1700000), 
             expand = FALSE, 
             crs = st_crs("ESRI:102010"))
  return {NULL}
}
###https://r-spatial.org/r/2018/10/25/ggplot2-sf-2.html###

