rm(list=ls())
#LIBRARIES
library(geosphere)
library(utf8)
library(statcanR)
library(bea.R)
#DATA IMPORTS
url_df <- read.csv("url_df.csv")
##GEOGRAPHIC DATA
st_lat_long <- read_csv("st_lat_long.csv")
canprov <- read_csv("canprov.csv")
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
can_exports["value"] <- can_exports$VALUE
can_exports <- can_exports[,-(1:4)]
can_exports["fttag"] <- paste0(can_exports$from,can_exports$to)
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
can_imports <- can_imports[complete.cases(can_imports),]
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
can_imports["fttag"] <- paste0(can_imports$from,can_imports$to)
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
#PULL TOGETHER can_exports, can_imports, geoframe, and GDPs
us_ca <- rbind(can_imports, can_exports)
us_ca <- merge(us_ca, geoframe, by = "fttag")
us_ca <- merge(us_ca, gdp_f, by = "from")
us_ca <- merge(us_ca, gdp_t, by = "to")
us_ca <- us_ca[,c(2,1,7,8,5,6,4)]
#REGRESSION

reg2 <- lm(asinh(value) ~ asinh(from_gdp) +
             asinh(to_gdp) + asinh(km) +
             domestic, data=us_ca)
summary(reg2)

