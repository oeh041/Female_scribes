if(T){
  rm(list=ls(all=T))
  gc()
  options(stringsAsFactors = F)
  cat("\014")
} 

## Reading data
## Sheet1: ...
s1 <- read.csv2(file = "Kolofoner_sheet1.csv", stringsAsFactors = F, header = T)
s1 <- s1[,1:8]
names(s1) <- c("number","male","female","unknown","year","lat","vern","text")
s1[is.na(s1)] <- 0
s1[s1==""] <- 0
s1$name <- ""
s1$text[s1$text==0] <- ""
s1$colos <- 1
s1$scribes <- 1

## Sheet3: ...
s3 <- read.csv2(file = "Kolofoner_sheet3.csv", stringsAsFactors = F, header = T)
s3 <- s3[,1:8]
names(s3) <- c("number","male","female","unknown","year","lat","vern","text")
s3[is.na(s3)] <- 0
s3[s3==""] <- 0
s3$name <- ""
s3$text[s3$text==0] <- ""
s3$colos <- 1
s3$scribes <- 1

## Sheet2: Names
s2 <- read.csv2(file = "Kolofoner_sheet2.csv", stringsAsFactors = F, header = T)
s2 <- s2[!s2$Number%in%c("no","no no."),]
dim(s2)
# names(s2) <- c("number","name","colos","scribes","year","lat","vern")
names(s2) <- c("number","name","colos","scribes","year","lat","vern","no.mss","text")
table(s2$no.mss,s2$scribes,useNA="ifany")
s2$scribes[is.na(s2$scribes)] <- s2$no.mss[is.na(s2$scribes)]
s2$colos[is.na(s2$colos)] <- s2$no.mss[is.na(s2$colos)]
t2scribes <-table(s2$scribes);t2scribes
## Number of scribes
sum(as.numeric(names(t2scribes))*t2scribes)
## Number of colophones
sum(s2$colos,na.rm=T)
s2 <- data.frame(number=s2[,"number"], male=0, female=1,unknown=0,s2[,c("year","lat","vern","text","name","colos","scribes")])

## Full data
tot <- rbind(s2,s1,s3)
tot$to <- tot$from <- tot$year

## From
tot$from <- gsub(pattern = "00-t", replacement = "00", x = tot$from)
tot$from[tot$number=="3367"] <- 1000
tot$from[tot$number=="3369"] <- 1100
tot$from[tot$number=="3725"] <- 1466
tot$from[tot$number=="3734"] <- 1260
tot$from[tot$number=="3930"] <- 1169
tot$from[tot$from=="(14)77"] <- "1477"
tot$from[tot$from=="1058-85"] <- 1058
tot$from[tot$from=="1178-1191"] <- 1178
tot$from[tot$from=="1184-1205"] <- 1184
tot$from[tot$from=="11-1250"] <- 1100
tot$from[tot$from=="11-1200"] <- 1100
tot$from[tot$from=="12-1350"] <- 1200
tot$from[tot$from=="12-1300"] <- 1200
tot$from[tot$from=="1236-37"] <- 1236
tot$from[tot$from=="13-1450"] <- 1300
tot$from[tot$from=="13-1400"] <- 1300
tot$from[tot$from=="1330-40"] <- 1330
tot$from[tot$from=="14-1550"] <- 1400
tot$from[tot$from=="14-1500"] <- 1400
tot$from[tot$from=="14??"] <- "1400"
tot$from <- gsub(pattern = "1413[ (a-z0-9]*", replacement = "1413", x = tot$from)
tot$from[tot$from=="1420-33"] <- 1420
tot$from[tot$from=="1451 (1452(?)"] <- "1451"
tot$from[tot$from=="1458-70"] <- 1458
tot$from[tot$from=="1478/79"] <- "1478"
tot$from[tot$from=="15-1650"] <- 1500
tot$from[tot$from=="15-1600"] <- 1500
tot$from[tot$from=="1569-70 (?)"] <- 1569
tot$from[tot$from=="6-750"] <- 600
tot$from[tot$from=="6-700"] <- 600
tot$from[tot$from=="7-850"] <- 700
tot$from[tot$from=="7-800"] <- 700
tot$from[tot$from=="795-819"] <- 795
tot$from[tot$from=="8-950"] <- 800
tot$from[tot$from=="8-900"] <- 800
tot$from[tot$from=="9-1050"] <- 900
tot$from[tot$from=="9-1000"] <- 900
tot$from[tot$from=="before 1403"] <- "1353"
tot$from <- gsub(pattern = "c. ", replacement = "", x = tot$from)
tot$from <- gsub(pattern = "ca. ", replacement = "", x = tot$from)
tot$from[tot$from=="d.1506"] <- "1456"
tot$from <- gsub(pattern = "early 15. c", replacement = "1400", x = tot$from)
tot$from[tot$from=="s. 13-14"] <- "1200"
tot$from[tot$from=="s. 13 and 15"] <- 1200
tot$from[tot$from=="s. 14 2/2"] <- "1350"
tot$from[tot$from=="s. 14"] <- "1300"
tot$from[tot$from=="1462?"] <- "1462"
tot$from[tot$from=="s. 15"] <- "1400"
tot$from[tot$from=="s. 15-16"] <- "1400"
tot$from[tot$from=="s. 15 ex"] <- "1480"
tot$from[tot$from=="s. 15(1)"] <- "1400"
tot$from[tot$from=="s. 15in"] <- "1400"
tot$from[tot$from=="s. 15 2/2"] <- "1450"
tot$from[tot$from=="s. 16"] <- "1500"
tot$from[tot$from=="0"] <- ""
tot$from[tot$from==""] <- NA
tot$from <- gsub(pattern = "00s", replacement = "00", x = tot$from)
tot$from <- gsub(pattern = "0s", replacement = "0", x = tot$from)
tot$from <- as.numeric(tot$from)

## To
tot$to <- gsub(pattern = "00-t", replacement = "99", x = tot$to)
tot$to[tot$number=="3367"] <- 1099
tot$to[tot$number=="3369"] <- 1199
tot$to[tot$number=="3725"] <- 1503
tot$to[tot$number=="3734"] <- 1260
tot$to[tot$number=="3930"] <- 1200
tot$to[tot$to=="(14)77"] <- "1477"
tot$to[tot$to=="1058-85"] <- 1085
tot$to[tot$to=="1178-1191"] <- 1191
tot$to[tot$to=="1184-1205"] <- 1205
tot$to[tot$to=="11-1299"] <- 1299
tot$to[tot$to=="12-1399"] <- 1399
tot$to[tot$to=="1236-37"] <- 1237
tot$to[tot$to=="13-1499"] <- 1499
tot$to[tot$to=="1330-40"] <- 1340
tot$to[tot$to=="14-1599"] <- 1599
tot$to[tot$to=="14??"] <- "1499"
tot$to <- gsub(pattern = "1413[ (a-z0-9]*", replacement = "1413", x = tot$to)
tot$to[tot$to=="1420-33"] <- 1433
tot$to[tot$to=="1451 (1452(?)"] <- "1452"
tot$to[tot$to=="1458-70"] <- 1470
tot$to[tot$to=="1478/79"] <- "1479"
tot$to[tot$to=="15-1699"] <- 1699
tot$to[tot$to=="1569-70 (?)"] <- 1570
tot$to[tot$to=="6-799"] <- 799
tot$to[tot$to=="7-899"] <- 899
tot$to[tot$to=="795-819"] <- 819
tot$to[tot$to=="8-999"] <- 999
tot$to[tot$to=="9-1099"] <- 1099
tot$to[tot$to=="before 1403"] <- "1403"
tot$to <- gsub(pattern = "c. ", replacement = "", x = tot$to)
tot$to <- gsub(pattern = "ca. ", replacement = "", x = tot$to)
tot$to[tot$to=="d.1506"] <- "1506"
tot$to <- gsub(pattern = "early 15. c", replacement = "1425", x = tot$to)
tot$to[tot$to=="s. 13-14"] <- "1399"
tot$to[tot$to=="s. 13 and 15"] <- 1499
tot$to[tot$to=="s. 14 2/2"] <- "1399"
tot$to[tot$to=="s. 14"] <- "1399"
tot$to[tot$to=="1462?"] <- "1462"
tot$to[tot$to=="s. 15"] <- "1499"
tot$to[tot$to=="s. 15-16"] <- "1599"
tot$to[tot$to=="s. 15 ex"] <- "1499"
tot$to[tot$to=="s. 15(1)"] <- "1499"
tot$to[tot$to=="s. 15in"] <- "1499"
tot$to[tot$to=="s. 15 2/2"] <- "1499"
tot$to[tot$to=="s. 16"] <- "1599"
tot$to[tot$to=="0"] <- ""
tot$to[tot$to==""] <- NA
tot$to <- gsub(pattern = "00s", replacement = "99", x = tot$to)
tot$to <- gsub(pattern = "0s", replacement = "9", x = tot$to)
tot$to <- as.numeric(tot$to)


## Sorting out year
tot$year <- gsub(pattern = "00-t", replacement = "50", x = tot$year)
tot$year[tot$number=="3367"] <- round((1099+1000)/2)
tot$year[tot$number=="3369"] <- round((1199+1100)/2)
tot$year[tot$number=="3725"] <- round((1503+1466)/2)
tot$year[tot$number=="3734"] <- 1260
tot$year[tot$number=="3930"] <- round((1200+1169)/2)
tot$year[tot$year=="(14)77"] <- "1477"
tot$year[tot$year=="1058-85"] <- round((1058+1085)/2)
tot$year[tot$year=="1178-1191"] <- round((1178+1191)/2)
tot$year[tot$year=="1184-1205"] <- round((1184+1205)/2)
tot$year[tot$year=="11-1250"] <- round((1100+1299)/2)
tot$year[tot$year=="12-1350"] <- round((1200+1399)/2)
tot$year[tot$year=="1236-37"] <- round((1236+1237)/2)
tot$year[tot$year=="13-1450"] <- round((1300+1499)/2)
tot$year[tot$year=="1330-40"] <- round((1330+1340)/2)
tot$year[tot$year=="14-1550"] <- round((1400+1599)/2)
tot$year[tot$year=="14??"] <- "1450"
tot$year <- gsub(pattern = "1413[ (a-z0-9]*", replacement = "1413", x = tot$year)
tot$year[tot$year=="1420-33"] <- round((1420+1433)/2)
tot$year[tot$year=="1451 (1452(?)"] <- "1451"
tot$year[tot$year=="1458-70"] <- round((1458+1470)/2)
tot$year[tot$year=="1478/79"] <- "1478"
tot$year[tot$year=="15-1650"] <- round((1500+1699)/2)
tot$year[tot$year=="1569-70 (?)"] <- round((1569+1570)/2)
tot$year[tot$year=="6-750"] <- round((600+799)/2)
tot$year[tot$year=="7-850"] <- round((700+899)/2)
tot$year[tot$year=="795-819"] <- round((795+819)/2)
tot$year[tot$year=="8-950"] <- round((800+999)/2)
tot$year[tot$year=="9-1050"] <- round((900+1099)/2)
tot$year[tot$year=="before 1403"] <- "1402"
tot$year <- gsub(pattern = "c. ", replacement = "", x = tot$year)
tot$year <- gsub(pattern = "ca. ", replacement = "", x = tot$year)
tot$year[tot$year=="d.1506"] <- "1505"
tot$year <- gsub(pattern = "early 15. c", replacement = "1425", x = tot$year)
tot$year[tot$year=="s. 13-14"] <- "1300"
tot$year[tot$year=="s. 13 and 15"] <- round((1200+1499)/2)
tot$year[tot$year=="s. 14 2/2"] <- "1375"
tot$year[tot$year=="s. 14"] <- "1350"
tot$year[tot$year=="1462?"] <- "1462"
tot$year[tot$year=="s. 15"] <- "1450"
tot$year[tot$year=="s. 15-16"] <- "1500"
tot$year[tot$year=="s. 15 ex"] <- "1490"
tot$year[tot$year=="s. 15(1)"] <- "1450"
tot$year[tot$year=="s. 15in"] <- "1450"
tot$year[tot$year=="s. 16"] <- "1550"
tot$year[tot$year=="s. 15 2/2"] <- "1475"
tot$year[tot$year=="0"] <- ""
tot$year[tot$year==""] <- NA
tot$year <- gsub(pattern = "00s", replacement = "50", x = tot$year)
tot$year <- gsub(pattern = "0s", replacement = "5", x = tot$year)
tot$year <- as.numeric(tot$year)
## Vernacular to numeric
tot$vern[tot$vern==""] <- NA
tot$vern[tot$vern=="1(?)"] <- 1
tot$vern[tot$vern=="1 (gresk)"] <- 1
tot$vern <- as.numeric(tot$vern)
table(tot$vern, useNA = "ifany")

## Females only
ftot <- tot[tot$female==1,]
## Removed after individual inspection
ftot <- ftot[!ftot$number%in%c("5353b","22024","7513","3744","21213","7510","7512","21066","22209","20652","7514","20650","7511","3731","453","922","3961","12595","21755"),]
## Adding decade?
ftot$decade <- cut(x = ftot$year, breaks = seq(from = 800,to = 1700,by = 10))
## All women
xtot <- table(ftot$decade);xtot
## Latin
xtot.lat <- table(ftot$decade[ftot$lat==1]);xtot.lat
## Vernacular
xtot.vern <- table(ftot$decade[ftot$vern==1]);xtot.vern
## Years
x <- seq(from = 805, to = 1700, by = 10)
offset <- 2

## Number of manuscripts
N <- 23774
## Number of manuscripts written by women
n <- sum(ftot$scribes!=0, na.rm=T);n
p <- n/N;p
se <- sqrt(p*(1-p)/N);se
round(100*(p+1.96*c(-1,0,1)*se),1)

## Number of named manuscripts
sum((s2$colos==1)&(!s2$number%in%c("5353b","22024","7513","3744","21213","7510","7512","21066","22209","20652","7514","20650","7511","3731","453","922","3961","12595","21755")))
## Number of anonymous manuscripts
sum((s1$female==1)&(!s1$number%in%c("5353b","22024","7513","3744","21213","7510","7512","21066","22209","20652","7514","20650","7511","3731","453","922","3961","12595","21755"))) + 
  sum((s3$female==1)&(!s3$number%in%c("5353b","22024","7513","3744","21213","7510","7512","21066","22209","20652","7514","20650","7511","3731","453","922","3961","12595","21755")))

## Number of names
## Some names appear more than once
name <- read.csv2(file = "Kolofoner_sheet2.csv", stringsAsFactors = F, header = T)[,c("Name","Scribes.","Year","LAT","VERN","Number")];dim(name)
## Manually removed
name <- name[!name$Number%in%c("5353b","22024","7513","3744","21213","7510","7512","21066","22209","20652","7514","20650","7511","3731","453","922","3961","12595","21755"),]
name <- name[!name[,"Number"]%in%c("no","no no."),];dim(name)
name <- name[order(name$Scribes.,decreasing = T),];head(name);dim(name)
n.list <- strsplit(x = name$Name, split = ", ");length(n.list)
name2 <- matrix(NA, nrow = length(unlist(n.list)), 6)
colnames(name2) <- names(name);head(name2);dim(name2)
k <- 0
for(i in 1:nrow(name)){
  for(j in 1:length(n.list[[i]])){
    if(!identical(n.list[[i]],character(0))){
      k <- k+1
      name2[k,] <- unlist(c(n.list[[i]][[j]],name[i,2:6]))
    }
  }
}
name2 <- name2[order(name2[,1],name2[,3],name2[,4],name2[,5]),];head(name2,20);dim(name2)
name2scribes <- name2[name2[,"Scribes."]!="0"|is.na(name2[,"Scribes."]!="0"),];dim(name2scribes)

## Table with all females
tab.out <- with(data = ftot[ftot$colos==1,],data.frame(number,from,to,name,text));head(tab.out);dim(tab.out)
tab.out <- tab.out[order(tab.out$from),];head(tab.out);dim(tab.out)
tab.out$number <- gsub(pattern = "[ (?)]*",replacement = "",x = tab.out$number);tab.out$number
names(tab.out) <- c("Colophone number","From","To","Name(s) of scribe(s)","Colophone text")
head(tab.out)

## Fig 3
ftot <- ftot[order(ftot$from),]
ftot <- ftot[!is.na(ftot$scribes)&(ftot$scribes!=0)&!is.na(ftot$from),]
ftot$to <- ftot$to+1
cols <- rep("black",nrow(ftot))
cols[ftot$lat==1] <- "orange"
cols[ftot$vern==1] <- "purple"
pts <- (ftot$to-ftot$from)==1
xpts <- ftot$from[pts]
## Adding century
ftot$century <- cut(x = ftot$year, breaks = seq(from = 800,to = 1700,by = 100))
head(ftot);tail(ftot);dim(ftot)
## All women
xtot2 <- table(ftot$century);xtot2
## Latin
xtot.lat2 <- table(ftot$century[ftot$lat==1]);xtot.lat2
## Vernacular
xtot.vern2 <- table(ftot$century[ftot$vern==1]);xtot.vern2
x <- seq(from = 800, to = 1600, by = 100)-10
x.adj <- seq(from = 800, to = 1600, by = 100)+10
## Adjusted
## Loss rates 
## 20th - 0
## 19th - .01
## 18th - .05
## 18th through 20th - 1-(1-0.01)*(1-0.05) = 0.0595
## 17th - .30
## 16th - .31
## 15th - .32
## 14th - .32
## 13th - .26
## 12th - .24
## 11th - .19
## 10th - .31
##  9th - .11
## 5.95 is for 18th through 20th
shape1 <- c(5.95,30,31,32,32,26,24,19,31,11);shape1
shape2 <- 100-shape1;shape2
## Paper used nrow=100000
mloss <- matrix(NA,nrow = 100, ncol = length(shape1));dim(mloss)
colnames(mloss) <- c(paste0((8:16)*100,"s"),"17-1900s")
head(mloss)
mxtot.adj <- mloss[,-ncol(mloss)]
## Add 0.5 to xtot2 to aviod zeros. 
## Corresponds to finding half a book in the 10th century
set.seed(0)
for(i in 1:nrow(mloss)){
  p <- runif(n = length(shape1),min = shape1*0.5/100,max = shape1*1.5/100);p # Uniform
  mloss[i,] <- rev(cumprod(1-p));mloss[i,]
  mxtot.adj[i,] <- (xtot2+0.5)/mloss[i,-ncol(mloss)];mxtot.adj[i,]
}
## Based on Poisson only. No simulations.
loss <- rev((1-0.0595)*cumprod(1-c(30,31,32,32,26,24,19,31,11)/100));loss
xtot.adj <- (xtot2+.5)/loss;xtot.adj
## Error bars based on Poisson only. No simulations. 
## Use +/- 2 SDs
error.pois <- xtot.adj
error <- sqrt(error.pois);error
error.min <- xtot.adj-2*error;error.min[error.min<0] <- 0;error.min
error.max <- xtot.adj+2*error;error.max
##
rbind(error.min, xtot.adj, error.max)

## Error bars based on Poisson and simulations.
## Use +/- 2 SDs
## Var = Var.pois + Var.sim
xtot.adj.mean <- apply(X = mxtot.adj, MARGIN = 2, FUN = mean);xtot.adj.mean
error.sim <- apply(X = mxtot.adj, MARGIN = 2, FUN = var);error.sim
error.tot <- sqrt(error.pois+error.sim);error.tot
error.min <- xtot.adj-2*error.tot;error.min[error.min<xtot2] <- xtot2[error.min<xtot2];error.min
error.max <- xtot.adj+2*error.tot;error.max
##
rbind(error.min, xtot.adj, error.max)

## One line per manuscript
## Scale up by accuracy
## Account for number of female scribes per manuscript
# tiff(filename = "figA.tiff", width = 20, height = 8, res = 1000, compression = "lzw", units = "in")
tiff(filename = "fig1.tiff", width = 20, height = 8, res = 1000, compression = "lzw", units = "in")
par(mfrow = c(1,2))
plot(NA,xlim = range(c(ftot$from,ftot$to),na.rm = T), ylim = c(0,nrow(ftot)), xlab = "", ylab = "", xaxt = "n", yaxt = "n", main = "A", cex.main = 3)
axis(side = 1, at = seq(from = 800, to = 1700, by = 50), cex.axis = 1.5)
axis(side = 2, at = seq(from = 0, to = 220, by = 50), las = 1, cex.axis = 1.5)
abline(v = seq(from = 750, to = 1700, by = 10), h = seq(from = 0, to = nrow(ftot), by = 10), col = "grey90" ,lty = 3)
segments(x0 = xpts, x1 = xpts+1, y0 = which(pts), y1 = which(pts), col = cols[pts], lwd = 3)
segments(x0 = ftot$from[!pts], x1 = ftot$to[!pts], y0 = which(!pts), y1 = which(!pts), col = cols[which(!pts)], lwd = 2, lend = 3)
legend("topleft", col = c("black", "orange", "purple"), legend = c("Unknown", "Latin", "Vernacular"), lty = 1, bty = "n", cex = 1.5, lwd = 5)
## Plot
## Black: Observed manuscripts
## Grey : Estimated manuscripts (adjusted according to loss rate)
plot(NA,xlim = range(ftot$year,na.rm = T), ylim = c(0,max(error.max,na.rm=T)), xlab = "", ylab = "", xaxt = "n", main = "B", cex.main = 3, cex.axis = 1.5)
axis(side = 1, at = seq(from = 800, to = 1700, by = 100), labels = paste0(seq(from = 800, to = 1700, by = 100),"s"), cex.axis = 1.5)
abline(h = seq(from = 0, to = max(error.max,na.rm = T), by = 25), col = "grey90")
segments(x0 = x, x1 = x, y0 = 0, y1 = xtot2, lwd = 21, lend = 1)
segments(x0 = x.adj, x1 = x.adj, y0 = 0, y1 = xtot.adj, lwd = 21, lend = 1,col="grey")
arrows(x0 = x.adj, x1 = x.adj, y0 = error.min, y1 = error.max, lwd = 3, angle = 90, length = .06 ,col="grey35", lend = 3)
arrows(x0 = x.adj, x1 = x.adj, y1 = error.min, y0 = error.max, lwd = 3, angle = 90, length = .06 ,col="grey35", lend = 3)
legend("topleft", col = c("black", "grey"), legend = c("Observed number of manuscripts", "Estimated number of manuscripts"), pch = "-", cex = 1.5, pt.cex = 9, lwd = 1, lty = NA, bty="n")# box.lwd = 0, box.col = "white", bg = "white") ## replace bty="n" with this?
dev.off()
shell("start fig1.tiff")
