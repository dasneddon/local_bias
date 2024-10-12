#######USING ONLY THE ORIGINAL TARGETED STATES FROM MCCALLUM
source("local_bias_script.R")
targetsf <- read.csv("abrs.csv")
targetst <- targetsf
colnames(targetsf) <- c("from", "targetf")
colnames(targetst) <- c("to", "targett")
us_ca <- merge(us_ca, targetsf, by="from")
us_ca <- merge(us_ca, targetst, by="to")
us_ca <- us_ca[(us_ca$targetf == 1) & (us_ca$targett == 1),]

reg7 <- lm(log(value[domestic==0])
           ~ log(from_gdp[domestic==0])
           + log(to_gdp[domestic==0])
           + log(km[domestic==0])
           + domestic[domestic==0],
           data = us_ca)

summary(reg7)

reg8 <- lm(log(value)
           ~ log(from_gdp)
           + log(to_gdp)
           + log(km)
           + domestic,
           data=us_ca)

summary(reg8)

reg9 <- lm(log(value[(from_gdp > 10^4) & (to_gdp > 10^4)])
           ~ log(from_gdp[(from_gdp > 10^4) & (to_gdp > 10^4)])
           + log(to_gdp[(from_gdp > 10^4) & (to_gdp > 10^4)])
           + log(km[(from_gdp > 10^4) & (to_gdp > 10^4)])
           + domestic[(from_gdp > 10^4) & (to_gdp > 10^4)],
           data=us_ca)

summary(reg9)

reg10 <- lm(log(value)
           ~ log(from_gdp)
           + log(to_gdp)
           + log(km)
           + domestic,
           data=us_ca,
           weights = from_gdp + to_gdp)

summary (reg10)

reg11 <- ivreg(log(value)
              ~ log(from_gdp)
              + log(to_gdp)
              + log(km)
              + domestic
              | log(from_pop)
              + log(to_pop)
              + log(km)
              + domestic,
              data=us_ca)

summary(reg11)

reg12 <- lm(log(value)
           ~ log(from_pop)
           + log(to_pop)
           + log(km)
           + domestic,
           data=us_ca)

summary(reg12)


plot ((us_ca$km), log(us_ca$value),
      pch = 20,
      cex = 0.5,
      main = "Distance vs Total Goods Imports",
      xlab = "Distance (km)",
      ylab = "arsinh(Goods Imports)",
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
