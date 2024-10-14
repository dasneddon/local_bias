##AGGREGATE AND WEIGHT US NATIONAL EXPORTS TO CANADA

usagg <- can_imports[can_imports$from =="US",]
stagg <- can_imports[can_imports$from !="US",]
can_imports <- can_imports[can_imports$from !="US",]
stagg$ptot <- 0
for (i in unique(stagg$to)){
  stagg$ptot[stagg$to == i] <- sum(stagg$value[stagg$to == i])
}

stagg$wt <- stagg$value/stagg$ptot
stagg <- merge(stagg,usagg, by="to")


stagg$ussplit <- stagg$wt * stagg$value.y

stagg <- data.frame(stagg$fttag.x, stagg$ussplit)
colnames(stagg) <- c("fttag", "valueu")
can_imports <- merge(can_imports, stagg, by="fttag")
can_imports$value <- can_imports$value + can_imports$valueu
can_imports$valueu <- NULL