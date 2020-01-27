EPI_data <- read.csv("2010EPI_data.csv", skip=1, header=TRUE)

attach(EPI_data)



#EPI for landlocked nations
EPILand <- EPI[!Landlock]

Eland <- EPILand[!is.na(EPILand)]

summary(Eland)

fivenum(Eland,na.rm=TRUE)

stem(Eland)

hist(Eland)

hist(Eland, seq(30., 95., 1.0),prob=TRUE)

lines(density(Eland, na.rm=TRUE, bw=1.))

lines(density(Eland, na.rm=TRUE, bw="SJ"))

rug(Eland)

plot(ecdf(Eland),do.points=FALSE, verticals=TRUE)

par(pty="s")
qqnorm(Eland); qqline(Eland)

x <- seq(30,95,1)
qqplot(qt(ppoints(250), df=5), x, xlab = "Q-Q plot for tdsn")
qqline(x)


#EPI high population density
EPIpop <- EPI[!High_Population_Density]

Epop <- EPIpop[!is.na(EPIpop)]

summary(Epop)

fivenum(Epop,na.rm=TRUE)

stem(Epop)

hist(Epop)

hist(Epop, seq(30., 95., 1.0),prob=TRUE)

lines(density(Epop, na.rm=TRUE, bw=1.))

lines(density(Epop, na.rm=TRUE, bw="SJ"))

rug(Epop)

plot(ecdf(Epop),do.points=FALSE, verticals=TRUE)

par(pty="s")
qqnorm(Epop); qqline(Epop)

x <- seq(30,95,1)
qqplot(qt(ppoints(250), df=5), x, xlab = "Q-Q plot for tdsn")
qqline(x)

#EPI north america
EPINorth <- EPI[GEO_subregion == "North America"]

EpiNA <- EPINorth[!is.na(EPINorth)]

summary(EpiNA)

fivenum(EpiNA,na.rm=TRUE)

stem(EpiNA)

hist(EpiNA)

hist(EpiNA, seq(30., 95., 1.0),prob=TRUE)

lines(density(EpiNA, na.rm=TRUE, bw=1.))

lines(density(EpiNA, na.rm=TRUE, bw="SJ"))

rug(EpiNA)

plot(ecdf(EpiNA),do.points=FALSE, verticals=TRUE)

par(pty="s")
qqnorm(EpiNA); qqline(EpiNA)

x <- seq(30,95,1)
qqplot(qt(ppoints(250), df=5), x, xlab = "Q-Q plot for tdsn")
qqline(x)


# EPI western europe
EPIWest <- EPI[GEO_subregion == "Western Europe"]

EpiWe <- EPIWest[!is.na(EPIWest)]

summary(EpiWe)

fivenum(EpiWe,na.rm=TRUE)

stem(EpiWe)

hist(EpiWe)

hist(EpiWe, seq(30., 95., 1.0),prob=TRUE)

lines(density(EpiWe, na.rm=TRUE, bw=1.))

lines(density(EpiWe, na.rm=TRUE, bw="SJ"))

rug(EpiWe)

plot(ecdf(EpiWe),do.points=FALSE, verticals=TRUE)

par(pty="s")
qqnorm(EpiWe); qqline(EpiWe)

x <- seq(30,95,1)
qqplot(qt(ppoints(250), df=5), x, xlab = "Q-Q plot for tdsn")
qqline(x)

