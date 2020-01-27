EPI_data <- read.csv("2010EPI_data.csv", skip=1, header=TRUE)

attach(EPI_data)


View(EPI_data)

EPI

tf <- is.na(EPI)
E <- EPI[!tf]

E

#EPI column
summary(EPI)

fivenum(EPI,na.rm=TRUE)

stem(EPI)

hist(EPI)

hist(EPI, seq(30., 95., 1.0),prob=TRUE)

lines(density(EPI, na.rm=TRUE, bw=1.))

lines(density(EPI, na.rm=TRUE, bw="SJ"))

rug(EPI)

plot(ecdf(EPI),do.points=FALSE, verticals=TRUE)

par(pty="s")
qqnorm(EPI); qqline(EPI)

x <- seq(30,95,1)
qqplot(qt(ppoints(250), df=5), x, xlab = "Q-Q plot for tdsn")
qqline(x)

#ECOSYSTEM column
summary(ECOSYSTEM)

fivenum(ECOSYSTEM,na.rm=TRUE)

stem(ECOSYSTEM)

hist(ECOSYSTEM, seq(0., 100., 1.0),prob=TRUE)

lines(density(ECOSYSTEM, na.rm=TRUE, bw=1.))

lines(density(ECOSYSTEM, na.rm=TRUE, bw="SJ"))

rug(ECOSYSTEM)

plot(ecdf(ECOSYSTEM),do.points=FALSE, verticals=TRUE)

par(pty="s")
qqnorm(ECOSYSTEM); qqline(ECOSYSTEM)

x <- seq(0,100,1)
qqplot(qt(ppoints(250), df=5), x, xlab = "Q-Q plot for tdsn")
qqline(x)

#Water_E column
summary(WATER_E)

fivenum(WATER_E,na.rm=TRUE)

stem(WATER_E)

hist(WATER_E, seq(0., 100., 1.0),prob=TRUE)

lines(density(WATER_E, na.rm=TRUE, bw=1.))

lines(density(WATER_E, na.rm=TRUE, bw="SJ"))

rug(WATER_E)

plot(ecdf(WATER_E),do.points=FALSE, verticals=TRUE)

par(pty="s")
qqnorm(WATER_E); qqline(WATER_E)

x <- seq(0,100,1)
qqplot(qt(ppoints(250), df=5), x, xlab = "Q-Q plot for tdsn")
qqline(x)

#compare
boxplot(EPI,ECOSYSTEM)

qqplot(EPI, DALY)

boxplot(AIR_H,AIR_E)

qqplot(AIR_H, AIR_E)


