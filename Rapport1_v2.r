# Exercise 1. Read the data files into R as two data.frame’s objects.
#installing the foreign package, to read the xpt file
install.packages("foreign")

#read dataset 1, the csv file
d1 <- read.csv('/Users/ulrikkenystrup/Library/Mobile Documents/com~apple~CloudDocs/4.semester/Statistik/Project 1/albSerum.csv', header= FALSE)
#Julianes computer: /Users/julia/OneDrive/DTU/4. semester/statistik/projekt 1/albSerum.csv

#changing collumname 
colnames(d1, do.NULL = FALSE)
colnames(d1) <- c("albSerum")

#remove empty rows (containing NA)
dataSet1 <- na.omit(d1)
#print out dataset 1
dataSet1

#read dataset 2, the xpt file 
d2 <- foreign::read.xport('/Users/ulrikkenystrup/Library/Mobile Documents/com~apple~CloudDocs/4.semester/Statistik/Project 1/ALB_CR_F.xpt')
#Julianse computer: /Users/julia/OneDrive/DTU/4. semester/statistik/projekt 1/ALB_CR_F.xpt

#remove empty rows (containing NA)
dataSet2<- na.omit(d2)

#print out dataset 2
dataSet2


# Exercise 2. Make summary statistic and relevant plots to describe the three relevant variables (albSerum, URXUMA and URXUMA2).
#først laves summary
summary(dataSet1$albSerum)
summary(dataSet2$URXUMA)
summary(dataSet2$URXUMA2)

#dernæst plottes data
plot(dataSet1$albSerum, xlab="Sample number", ylab="Albumin serum value", main= "Plot af albumin serum")
boxplot(dataSet1$albSerum, xlab="Sample number", ylab="Albumin serum value", main= "Plot af albumin serum")
plot(dataSet2$URXUMA, xlab="Sample number", ylab="URXAMA value", main="Plot af URXAMA")
plot(dataSet2$URXUMA2, xlab="Sample number", ylab="URXAMA2 value", main="Plot af URXAMA2")

#Exercise 3.Examine if it is fair to assume that the three variables are normally distributed. 
#Use transformations if necessary.

#Laver et histogram over dataSet1
hist(dataSet1$albSerum, main="Histogram of albumin serum", xlab="albSerum value", density=20, breaks=20, prob=TRUE, ylim=c(0, .2))

#Beregner mean og sd af serum albumin
myAlbSerum <- mean(dataSet1$albSerum)
sigmaAlbSerum <- sd(dataSet1$albSerum)

#Tilføjer en normal density curve til histogrammet
curve(dnorm(x, mean = myAlbSerum, sd = sigmaAlbSerum), add = TRUE, col = "red", lwd = 2)

# Tilføjer en density curve til histogrammet
lines(density(dataSet1$albSerum), col = "blue")

#laver et QQ plot
qqnorm(dataSet1$albSerum, main="Normal QQ plot af albumin serum")
qqline(dataSet1$albSerum)

#Laver en test for vurdering om data er normalfordelt
shapiro.test(dataSet1$albSerum)
shapiro.test(dataSet2$URXUMA)
shapiro.test(dataSet2$URXUMA2)
