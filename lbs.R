
library(readr)
library(geosphere)
library(utf8)
library(statcanR)
library(bea.R)
library(censusapi)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(canadianmaps)
library(plyr)
library(stringr)
library(usmap)
library(spData) 
library(tools)
library(modelsummary)
library(AER)
library(MASS)
library(fixest)
options("modelsummary_factory_default" = "kableExtra")
options(scipen = 999)
#US DATA
url_df <- read.csv("url_df.csv")
usfips <- read.csv("https://www2.census.gov/geo/docs/reference/state.txt",
                   sep = "|")
usfips <- usfips[c(1, 2)]
colnames(usfips) <- c("STATE", "state")
can_exports <- read.csv(url_df$candata)
can_exports <- can_exports[,c(2, 4, 5, 12)]
can_imports <- read.csv("ODPFN022_201912N.csv")
##GEOGRAPHIC DATA
###Centers of Population
st_lat_long <- read.csv("st_pop_center.csv") #From Census.gov - 2020
canprov <- read.csv("canprovcenter.csv") #Calculated with ChatGPT - 2021
st_abbr <- read.csv("st_abbr.csv")
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
can_exports["domestic"] <- 1
###AGGREGATE AND CONDENSE CAN_IMPORTS DATAFRAME

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
can_imports["domestic"] <- 0
###CANADIAN GDP
can_gdp <- read.csv(url_df$can_gdp)
can_gdp <- can_gdp[,c("GEO","VALUE")]
can_gdp <- merge(can_gdp, prov_abr, by="GEO")
cagdp_f <- data.frame(from=can_gdp$from,
                      from_gdp=can_gdp$VALUE)
cagdp_t <- data.frame(to=can_gdp$from,
                      to_gdp=can_gdp$VALUE)

##US GDP
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
us_gdp <- merge(us_gdp, st_abbr, by="GeoName")
usgdp_f <- data.frame(from=us_gdp$State,
                      from_gdp=as.numeric(us_gdp$`DataValue_2019`)*exch_usca)
usgdp_t <- data.frame(to=us_gdp$State,
                      to_gdp=as.numeric(us_gdp$`DataValue_2019`)*exch_usca)
rm(us_gdp, exch_usca)
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
us_ca <- us_ca[us_ca$to != "NU" & us_ca$to != "NT" & us_ca$to != "YT" &
                 us_ca$from != "NU" & us_ca$from != "NT" & us_ca$from != "YT",]
#REGRESSION

coefnames <- c("Intercept","From GDP", "To GDP", "Distance (km)", "Domestic Dummy" )

reg1 <- feols(log(value)
              ~ log(from_gdp)
              + log(to_gdp)
              + log(km),
              data = us_ca,
              vcov = "hetero",
              subset = us_ca$domestic==1)

names(reg1$coefficients) <- coefnames[1:4]

reg2 <- feols(log(value)
              ~ log(from_gdp)
              + log(to_gdp) 
              + log(km) 
              + domestic, 
              data=us_ca,
              vcov = "hetero")

names(reg2$coefficients) <- coefnames

reg3 <- feols(log(value)
              ~ log(from_gdp)
              + log(to_gdp) 
              + log(km) 
              + domestic, 
              data=us_ca,
              vcov = "hetero", 
              subset=us_ca$from_gdp > 10^4 & us_ca$to_gdp > 10^4)

names(reg3$coefficients) <- coefnames

reg4 <- feols(log(value)
              ~ log(from_gdp)
              + log(to_gdp) 
              + log(km) 
              + domestic, 
              data=us_ca,
              vcov = "hetero",
              weights = us_ca$from_gdp + us_ca$to_gdp)

names(reg4$coefficients) <- coefnames

reg5 <- feols(log(value)
              ~ log(from_pop)
              + log(to_pop) 
              + log(km) 
              + domestic, 
              data=us_ca,
              vcov = "hetero")

names(reg5$coefficients) <- coefnames

reg6 <- feols(log(value)
              ~ log(from_gdp)
              + log(to_gdp) 
              + log(km), 
              data=us_ca,
              vcov = "hetero")

names(reg6$coefficients) <- coefnames[1:4]


regz <- list(reg1, 
             reg2, 
             reg3, 
             reg4, 
             reg5, 
             reg6)
#PLOT
plot ((us_ca$km), log(us_ca$value),
      pch = 20,
      cex = 1,
      main = "Distance vs Total Goods Imports",
      xlab = "Distance (km)",
      ylab = "log(Goods Imports)",
      col = alpha(us_ca$domestic+2,0.5),
      las = 1
)
legend("topright",
       pch = 20,
       bty = "n",
       cex = 0.75,
       horiz = FALSE,
       legend = c("International", "Interprovincial"),
       col = c(2, 3))


title <- "US Canada Trade Data"
frmla <- (`Imports` = value) +
  (`Distance (km)` = km) + 
  (`Export GDP` = from_gdp) +
  (`Import GDP` = to_gdp) ~ 
  (`N` = length) + 
  Mean + 
  (`St. Dev.` = sd) + 
  (`Min` = min) + 
  (`Max` = max)
frmlalog <- (`Imports` = log(value)) +
  (`Distance (km)` = log(km)) + 
  (`Export GDP` = log(from_gdp)) +
  (`Import GDP` = log(to_gdp)) ~ 
  (`N` = length) + 
  Mean + 
  (`St. Dev.` = sd) + 
  (`Min` = min) + 
  (`Max` = max)
#Output tables
datasummary(frmla, 
            data = us_ca, 
            title = paste(title, "- Summary Statistics"),
            fmt = fmt_significant(2))
datasummary(frmlalog, 
            data = us_ca, 
            title = paste(title, "- Summary Statistics - log"), 
            fmt = fmt_significant(2))
modelsummary(regz, 
             data = us_ca, 
             title = paste(title, "- Regression"),
             fmt = fmt_significant(3),
             statistic = c("SE = {std.error}", 
                           "t = {statistic}",
                           "p = {p.value}"),
             gof_map = c("nobs", "r.squared", "vcov.type"))
