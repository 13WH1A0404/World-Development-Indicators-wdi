attach(Indicators)
newdata <- Indicators[order(IndicatorCode),]

## WORLD POLLUTION
co2 <- c(Indicators$IndicatorCode == "EN.ATM.CO2E.KT")
wrld_co2 <- Indicators[co2,]
View(wrld_co2)
summary(wrld_co2)
C <- wrld_co2[wrld_co2$Year > 1990,]
summary(C)
View(C)

vc1 <- C[C$Year < 1997,]
Vc1 = vc1$Value
VC1 = mean(Vc1)


vc2 <- C[C$Year == 1997:2001,]
Vc2 = vc2$Value
VC2 = mean(Vc2)

vc3 <- C[C$Year == 2002:2006,]
Vc3 = vc3$Value
VC3 = mean(Vc3)

vc4 <- C[C$Year == 2007:2011,]
Vc4 = vc4$Value
VC4 = mean(Vc4)

co2_vals <- c(VC1,VC2,VC3,VC4)
co2_yrs <-c("1991-1996","1997-2001","2002-2006","2007-2011")
barplot(co2_vals,main = "Air Pollution",xlab = "Years",ylab = "CO2 Emission",names.arg = c("1991-1996","1997-2001","2002-2006","2007-2011"),border = "blue")

## WORLD POPULATION
pop <- c(Indicators$IndicatorCode == "SP.POP.TOTL")
wrld_pop <- Indicators[pop,]
View(wrld_pop)
summary(wrld_pop)

P <- wrld_pop[wrld_pop$Year > 1990,]
Summary(P)
View(P)

vp1 <- P[P$Year < 1997,]
Vp1 = vp1$Value
VP1 = mean(vp1)

vp2 <- P[P$Year == 1997:2001,]
Vp2 = vp2$Value
VP2 = mean(Vp2)

vp3 <- P[P$Year == 2002:2006,]
Vp3 = vp3$Value
VP3 = mean(Vp3)

vp4 <- P[P$Year == 2007:2011,]
Vp4 = vp4$Value
VP4 = mean(Vp4)

vp5 <- P[P$Year == 2012:2014,]
Vp5 = vp5$Value
VP5 = mean(Vp5)

pop_vals <- c(VP1,VP2,VP3,VP4,VP5)
pop_yrs <-c(1996,2001,2006,2011,2014)
Population <- pop_vals
years <- pop_yrs

plot(years,Population,main = "WORLD POPULATION", type="o", col="blue")

## wORLD GDP PER CAPITA 

gdp <- c(Indicators$IndicatorCode == "NY.GDP.PCAP.CD")
g <- Indicators[gdp,]
View(g)
summary(g)

G <- wrld_gdp[wrld_gdp$Year > 1990,]
Summary(G)
View(G)

G1 <- G[G$Year == 2014,]
G12 <- G1[which(G1$CountryCode== "EAP" |  G1$CountryCode== "IND" | G1$CountryCode== "CEB" | G1$CountryCode== "CHN" | G1$CountryCode== "MYS" | G1$CountryCode== "AFG" | G1$CountryCode== "AUS" | G1$CountryCode== "JPN"), ] 
gdp_vals <- c(G12$Value)
barplot(gdp_vals,main = "GDP per Capita ",xlab = "Years",ylab = "GDP per capita",names.arg = c( "EMU","IND","CEB","CHN","MYS","AFG","AUS","JPN"),border = "blue")

## RANKING OF THE COUNTRIES ON THE BASIS OF ELECTRICITY CONSUMPTION

wrld_ele = sort(wrld_ele$CountryName)

E <- newdata[order(CountryCode),]
sor <- E[order(CountryName,Year),]

wrld_ele <- c(Indicators$IndicatorCode == "EG.USE.ELEC.KH.PC")
ele <- Indicators[wrld_ele,]

test <- aggregate(Value ~ CountryName + Value , sor, sum )
Ele_rank <- rank(test)

attach(Ele_rank)
Ele_rank = data.frame(Ele_rank)
View(Ele_rank)
Elec = data.frame(test,Ele_rank)
Elec1 <- Elec[order(Ele_rank),]
Elec2 <- Elec1[Elec1$Ele_rank < 215,]
View(Elec2)
 
## Covarience between Rural and Urban Population

reg <- lm(BIRTH~DEATH)
rp <- c(Indicators$IndicatorCode == "SP.RUR.TOTL")
Rp <- Indicators[rp,]
up <- c(Indicators$IndicatorCode == "SP.URB.TOTL")
Up <- Indicators[up,]
RP <- Rp$Value[1:247]
UP <- Up$Value[1:247]
RU_DF = data.frame(RP,UP)
cov(RU_DF)
