library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("canadianmaps")
source("local_bias_script.R")
library(plyr)
library(stringr)

world <- ne_countries(scale = "large", returnclass = "sf")
class(world)
(sites <- data.frame(longitude = c(-172.54, -47.74), latitude = c(23.81,
                                                                  90)))


(sites <- st_as_sf(geodata, coords = c("lon", "lat"),
                   crs = 4369, agr = "constant"))


library("maps")
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
head(states)
provs <- PROV[,c(5, 10, 11, 12)]


states <- cbind(states, st_coordinates(st_centroid(states)))
colnames(provs) <- colnames(states)

head(states)
library("tools")
states$ID <- toTitleCase(states$ID)
head(states)
stpr <- st_sf(rbind(as.data.frame(states),as.data.frame(provs)))
gravitymap <- function (x){
  tf <- data.frame(us_ca_split[x])
  prov_abr2 <- prov_abr
  colnames(prov_abr2) <- colnames(st_abbr)
  tabrs <- rbind(st_abbr, prov_abr2)
  colnames(tabrs)[1] <- "ID"
  colnames(tf) <- colnames(us_ca)
  tf <- tf[c(1, 7)]
  colnames(tf)[1] <- "state"
  tstpr <- merge(stpr, tabrs, by = "ID")
  tstpr <- merge(tstpr, tf, by = "State")

  ggplot(data = world) +
    geom_sf() +
    geom_sf(data = tstpr, 
            aes(fill =value)) +
    scale_fill_gradient(low="red", high= "green") +
    geom_sf(data = sites,  
            size = 1,
            shape = 23, 
            fill = "darkred") +
    coord_sf(xlim = c(-6500000, 3800000), 
             ylim = c(4700000, -1700000), 
             expand = FALSE, 
             crs = st_crs("ESRI:102010"))
}
###https://r-spatial.org/r/2018/10/25/ggplot2-sf-2.html###

