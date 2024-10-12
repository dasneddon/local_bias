rm(list=ls())
#LIBRARIES
library(readr)
library(geosphere)
library(utf8)
library(statcanR)
library(bea.R)
library(censusapi)
library("ggplot2")
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("canadianmaps")
library(plyr)
library(stringr)
library(usmap)
library(spData) 
library("tools")
library("modelsummary")
library(AER)
options("modelsummary_factory_default" = "kableExtra")
options(scipen = 999)

#FUNCTION FOR CREATING MAPS
gravitymap <- function (x){
  theme_set(theme_bw())
  sf_use_s2(FALSE)
  
  worldz <- ne_countries(scale = "large", returnclass = "sf")
  class(worldz)
  (sites <- data.frame(longitude = c(-172.54, -47.74), latitude = c(23.81,
                                                                    90)))
  
  
  (sites <- st_as_sf(geodata, coords = c("lon", "lat"),
                     crs=4369, agr = "constant"))
  
  sites <- st_transform(sites, st_crs("ESRI:102010"))
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
  tf$value <- log(tf$value)
  tf <- tf[c(2, 7)]
  colnames(tf)[1] <- "State"
  tstpr <- merge(stpr, tabrs, by = "NAME")
  tstpr <- merge(tstpr, tf, by = "State")
  
  ggplot(data = worldz) +
    geom_sf() +
    ggtitle(paste("Import Gravity Map for", prov_abr[prov_abr$from==x,])) +
    geom_sf(data = tstpr, 
            aes(fill =value)) +
    scale_fill_gradient(low="red", high= "green") +
    coord_sf(xlim = c(-6500000, 3800000), 
             ylim = c(4700000, -1700000), 
             expand = FALSE, 
             crs = st_crs("ESRI:102010"))

}


#DATA IMPORTS
url_df <- read.csv("url_df.csv")
usfips <- read.csv("https://www2.census.gov/geo/docs/reference/state.txt",
                   sep = "|")
usfips <- usfips[c(1, 2)]
colnames(usfips) <- c("STATE", "state")

##GEOGRAPHIC DATA
###Centers of Population
st_lat_long <- read_csv("st_pop_center.csv") #From Census.gov - 2020
canprov <- read_csv("canprovcenter.csv") #Calculated with ChatGPT - 2021
st_abbr <- read_csv("st_abbr.csv")



#DISTANCE DATA
geodata <- as.data.frame(rbind(as.matrix(st_lat_long),
                               as.matrix(canprov)))
geoframe <- data.frame(matrix(0, nrow(geodata)^2, 3))
colnames(geoframe) <- c("from", "to", "km")
c <- 1
for (i in geodata[,1]){
  for (w in geodata[,1]){
    
    lat_a <- as.numeric(geodata[geodata$state==i,2])
    lat_b <- as.numeric(geodata[geodata$state==w,2])
    lon_a <- as.numeric(geodata[geodata$state==i,3])
    lon_b <- as.numeric(geodata[geodata$state==w,3])
    geoframe[c,1] <- i
    geoframe[c,2] <- w
    geoframe[c,3] <- as.numeric(distHaversine(c(lon_a, lat_a),
                                              c(lon_b, lat_b)))/1000
    c <- c+1
  }
}
geoframe["fttag"] <- paste0(geoframe$from,geoframe$to)
geoframe <- geoframe[,c(3, 4)]




##CANADIAN DATA
can_exports <- read.csv(url_df$candata)
can_exports <- can_exports[,c(2, 4, 5, 12)]
#DATAFRAME FOR US STATE AND PROVINCIAL ABBREVIATIONS FOR UNIFORMITY
#NEEDED FOR MERGING
prov_abr <- data.frame(GEO = unique(can_exports$GEO))
prov_abr["from"] <- c("NL", "PE", "NS", "NB", "QC", "ON", "MB", "SK", "AB", 
                      "BC", "YT", "NT", "NU")
provto_abr <- prov_abr
provto_abr$GEO <- paste("To", prov_abr$GEO)
colnames(provto_abr) <- c("To_GEO","to")
colnames(can_exports)[2] <- "To_GEO"
#FORMAT can_exports  FOR MERGING
can_exports <- merge(can_exports, prov_abr, by="GEO")
can_exports <- merge(can_exports, provto_abr, by="To_GEO")
rm(provto_abr)
can_exports["value"] <- can_exports$VALUE*1000
can_exports <- can_exports[,-(1:4)]
can_exports["fttag"] <- paste0(can_exports$to,can_exports$from)
can_exports["intra"] <- -1
for (i in 1:nrow(can_exports)){
  if(can_exports$from[i]==can_exports$to[i]){
    can_exports$intra[i] <- 1
  }else
    can_exports$intra[i] <- 0
}
can_exports <- can_exports[can_exports$intra == 0,]
can_exports$intra <- NULL
tmp <- can_exports
can_exports <- tmp[,c(2,1,3,4)]
rm(tmp)
can_exports["domestic"] <- 0


###IMPORT AGGREGATE AND CONDENSE CAN_IMPORTS DATAFRAME
can_imports <- read.csv("ODPFN022_201912N.csv")
can_imports <- can_imports[can_imports$Country.Pays=="US",]
colnames(can_imports) <- c("YearMonth", 
                           "HS2", 
                           "Country", 
                           "Province", 
                           "State", 
                           "Value")
can_imports <- aggregate(can_imports$Value
                         ~ can_imports$Country
                         + can_imports$Province
                         + can_imports$State,
                         FUN = sum)
for (i in 1:nrow(can_imports)){
  for (u in 1:3){
    can_imports[i,u] <- as_utf8(can_imports[i,u])
  }
}
can_imports <- can_imports[,c(2, 3, 4)]
colnames(can_imports) <- c("to", "from", "value")
can_imports["fttag"] <- paste0(can_imports$to,can_imports$from)
can_imports["domestic"] <- 1

###CANADIAN GDP
can_gdp <- read.csv(url_df$can_gdp)
can_gdp <- can_gdp[,c("GEO","VALUE")]
can_gdp <- merge(can_gdp, prov_abr, by="GEO")
cagdp_f <- data.frame(from=can_gdp$from,
                      from_gdp=can_gdp$VALUE)
cagdp_t <- data.frame(to=can_gdp$from,
                      to_gdp=can_gdp$VALUE)
##MERGE CANADIAN IMPORTS AND EXPORTS

##US DATA
bkey <- "EB73777D-FF2C-43F1-A94A-E1BFFBA2744D"

userSpecList <- list('UserID' = bkey,
                     'Method' = 'GetData',
                     'datasetname' = 'Regional',
                     'GeoFips' = 'STATE',
                     'LineCode' = '3',
                     'TableName' = 'SAGDP1',
                     'Year' = '2019')
us_gdp <- beaGet(userSpecList)
us_gdp <- us_gdp[-1,]
us_gdp <- us_gdp[-(52:59),]
us_gdp <- us_gdp[,c(3, 6)]
exch_usca <- 1.3269 #USD TO CAD 2019 https://www.bankofcanada.ca/rates/exchange/annual-average-exchange-rates/
us1219 <- 1.07 #$1 USD IN 2012 IN 2019 DOLLARS 
us_gdp <- merge(us_gdp, st_abbr, by="GeoName")
usgdp_f <- data.frame(from=us_gdp$State,
                      from_gdp=as.numeric(us_gdp$`DataValue_2019`)*us1219*exch_usca)
usgdp_t <- data.frame(to=us_gdp$State,
                      to_gdp=as.numeric(us_gdp$`DataValue_2019`)*us1219*exch_usca)
rm(us_gdp, exch_usca, us1219)
#MERGE BOTH COUNTRIES GDPs
gdp_f <- rbind(cagdp_f,usgdp_f)
rm(cagdp_f, usgdp_f)
gdp_t <- rbind(cagdp_t,usgdp_t)
rm(cagdp_t, usgdp_t)
##POPULATION
###US
st_pop <- getCensus(
  name = "dec/dhc",
  vars = "P1_001N",
  region = "state:*",
  vintage = 2020
)
st_pop$state <- as.numeric(st_pop$state)
colnames(st_pop) <- c("STATE", "pop")
st_pop <- merge(st_pop, usfips, by="STATE")
st_pop <- st_pop[,c(3, 2)]
st_pop_from <- data.frame(st_pop$state, st_pop$pop)
st_pop_to <- data.frame(st_pop$state, st_pop$pop)
colnames(st_pop_from) <- c("from","from_pop")
colnames(st_pop_to) <- c("to","to_pop")

###CANADA

prov_pop <- read.csv("prov_pop.csv")
prov_pop <- prov_pop[prov_pop$CHARACTERISTIC_ID==1 
                     & prov_pop$ALT_GEO_CODE !=  1, c(5, 12)]
colnames(prov_pop) <- c("GEO", "pop")
prov_pop <- merge(prov_pop, prov_abr, by="GEO")
prov_pop$to <- prov_pop$from
prov_pop_from <- data.frame(prov_pop$from, prov_pop$pop)
prov_pop_to <- data.frame(prov_pop$to, prov_pop$pop)
colnames(prov_pop_from) <- c("from","from_pop")
colnames(prov_pop_to) <- c("to","to_pop")

pop_from <- rbind(st_pop_from,prov_pop_from)
pop_to <- rbind(st_pop_to,prov_pop_to)
#PULL TOGETHER can_exports, can_imports, geoframe, populations, and GDPs
us_ca <- rbind(can_imports, can_exports)
us_ca <- merge(us_ca, geoframe, by = "fttag")
us_ca <- merge(us_ca, gdp_f, by = "from")
us_ca <- merge(us_ca, gdp_t, by = "to")
us_ca <- us_ca[,c(2,1,7,8,5,6,4)]
us_ca <- us_ca <- merge(us_ca, pop_from, by = "from")
us_ca <- us_ca <- merge(us_ca, pop_to, by = "to")
us_ca <- us_ca[us_ca$value != 0,]
#SPLIT TO EACH PROVINCE
us_ca_split <- split(us_ca, list(us_ca$to))
#REGRESSION

reg1 <- lm(log(value[domestic==0])
           ~ log(from_gdp[domestic==0])
           + log(to_gdp[domestic==0])
           + log(km[domestic==0])
           + domestic[domestic==0],
           data = us_ca)

summary(reg1)

reg2 <- lm(log(value)
           ~ log(from_gdp)
           + log(to_gdp) 
           + log(km) 
           + domestic, 
           data=us_ca)

summary(reg2)

reg3 <- lm(log(value[(from_gdp > 10^4) & (to_gdp > 10^4)])
           ~ log(from_gdp[(from_gdp > 10^4) & (to_gdp > 10^4)])
           + log(to_gdp[(from_gdp > 10^4) & (to_gdp > 10^4)]) 
           + log(km[(from_gdp > 10^4) & (to_gdp > 10^4)]) 
           + domestic[(from_gdp > 10^4) & (to_gdp > 10^4)], 
           data=us_ca)

summary(reg3)

reg4 <- lm(log(value)
           ~ log(from_gdp)
           + log(to_gdp) 
           + log(km) 
           + domestic, 
           data=us_ca,
           weights = from_gdp + to_gdp)

summary (reg4)

reg5 <- ivreg(log(value)
           ~ log(from_gdp)
           + log(to_gdp) 
           + log(km) 
           + domestic 
           | log(from_pop)
           + log(to_pop)
           + log(km)
           + domestic, 
           data=us_ca)

summary(reg5)

reg6 <- lm(log(value)
           ~ log(from_pop)
           + log(to_pop) 
           + log(km) 
           + domestic, 
           data=us_ca)

summary(reg6)


plot ((us_ca$km), log(us_ca$value),
      pch = 20,
      cex = 0.5,
      main = "Distance vs Total Goods Imports",
      xlab = "Distance (km)",
      ylab = "log(Goods Imports)",
      col = alpha("red",0.5),
      las = 1
      )

title <- "US Canada Trade Data - Summary Statistics"
frmla <- (`Distance (km)` = km) + 
  (`Imports` = value)  ~ 
    (`N` = length) + 
    Mean + 
    (`St. Dev.` = sd) + 
    (`Min` = min) + 
    (`Max` = max)
#Output a table
datasummary(frmla, data = us_ca, title = title, fmt = fmt_significant(2))

#REGRESSION DATA FRAME
reg_frame <- data.frame(reg1$coefficients,
                        reg2$coefficients,
                        reg3$coefficients,
                        reg4$coefficients,
                        reg5$coefficients,
                        reg6$coefficients)

for(i in unique(prov_abr$from)){
  ggsave(paste0(i,".png"),gravitymap(i),
         device = "png",
         path="maps",
         create.dir = TRUE)
}

