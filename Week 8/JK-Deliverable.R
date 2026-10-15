#GETTING STARTED

2 + 2 #simple arithmetic
10+(8/2) #PEMDAS example
log(42) #natural log of 42
sqrt(9) #Square root of 9
4^3 #exponents!
#Challenge Problem
(((3*7)+4)/(sqrt(pi+7.8)))

#Comment Example!

log(4, base = 10)
x <- 5
x + 5
x <- 3
weights <- x
c(1,3,5,7,9)

TemperatureF <- c(78,85,64,54,102,98.5)
TemperatureF

TemperatureC <- (TemperatureF-32) * (5/9)
TemperatureC[3]

TempDiff <- TemperatureF-TemperatureC
mean(TemperatureC)
sum(TemperatureC)
length(TemperatureC)

library(ggplot2)
titanicData <- read.csv("titanic.csv")
summary(titanicData)
titanicData$age
titanicData$logage <- log(titanicData$age)


femaleData <- titanicData[titanicData$sex == "female",]

#BASIC SUMMARY STATISTICS
mean(titanicData$age)
mean(titanicData$age, na.rm=TRUE)
median(titanicData$age)
median(titanicData$age, na.rm=TRUE)
summary(titanicData$age)
IQR(titanicData$age, na.rm=TRUE)

#ABOUT PLOTS
ggplot(data=titanicData)
ggplot(data=titanicData, aes(x= 1 , y= 1))
ggplot(data=titanicData, aes(x= 1 , y= 1)) #+ geom_type()

ggplot(data=titanicData, aes(x=age)) + geom_histogram()
ggplot(data=titanicData, aes(x=sex)) + geom_bar()

ggplot(data=titanicData, aes(x=sex,y=age)) + geom_boxplot()



#COMAPRING MEANS OF TWO GROUPS


t.test(titanicData$age ~ titanicData$sex)
library(car)
leveneTest(titanicData$age ~ titanicData$survive)
shapiro.test(titanicData$age)

ggplot(data=titanicData, aes(x=age)) + geom_histogram() + facet_wrap(~passenger_class, ncol = 1)


titanicData %>%
  group_by(passenger_class)

titanicData %>% 
  summarise(mean(age, na.rm=TRUE))

titanic_by_passenger_class <- group_by(titanicData,passenger_class)
summarise(titanic_by_passenger_class, group_mean = mean(age, na.rm=TRUE),group_sd = sd(age, na.rm=TRUE))


#ANOVA
titanicANOVA <- lm(titanicData$age ~ titanicData$passenger_class)
anova(titanicANOVA)

TukeyHSD(aov(titanicANOVA))

kruskal.test(titanicData$age ~ titanicData$passenger_class)

#REGRESSION

guppyData <- read.csv("guppydata.csv")

cor(x = guppyData$fatherOrnamentation, y = guppyData$sonAttractiveness)
cor.test(x = guppyData$fatherOrnamentation, y = guppyData$sonAttractiveness)

guppyLM <- lm(guppyData$sonAttractiveness ~ guppyData$fatherOrnamentation)
guppyLM
summary(guppyLM)

ggplot(data=guppyData, aes(x=fatherOrnamentation,y=sonAttractiveness)) + geom_point() + theme_minimal() + xlab("Father's Ornamentation") + ylab("Son's Attractiveness") + geom_smooth(method = lm)

residuals(guppyLM)

plot(residuals(guppyLM) ~ guppyData$fatherOrnamentation) + abline(h=0)


fishSizeData <- read.csv("FishSizeData.csv")
pairs(fishSizeData[, c("size", "temp", "depth", "algae")])
cor(fishSizeData[, c("size", "temp", "depth", "algae")])
hist(fishSizeData$size)

plot(fishSizeData, which=1, which=2)



#ASSIGNMENT

#Part 1

abalone <- read.csv("abalone_reserve.csv")
mean(abalone$inside_reserve) #INSIDE
mean(abalone$outside_reserve) #OUTSIDE

sd(abalone$inside_reserve)
sd(abalone$outside_reserve)

boxplot(abalone[,2:3])
ggplot(data=abalone, aes(x=outside_reserve)) + geom_histogram()
ggplot(data=abalone, aes(x=inside_reserve)) + geom_histogram()


t.test(abalone$inside_reserve, abalone$outside_reserve, paired = TRUE)


#Part 2
yellowfish <- read.csv("yellowfish.csv")
yellowfish$Population <- as.factor(yellowfish$Population)

yellowfish %>%
  group_by(Population) %>%
    summarize(mean_size = mean(Yellow.Fish.size), variances = var(Yellow.Fish.size))

library(car)
leveneTest(yellowfish$Yellow.Fish.size ~ yellowfish$Population)

ggplot(data = yellowfish, aes(x=Population, y=Yellow.Fish.size)) + geom_boxplot()

ggplot(data = yellowfish, aes(x=Yellow.Fish.size)) + geom_histogram() + facet_wrap(yellowfish$Population, ncol = 1)

yellowfish %>%
  group_by(Population) %>%
    summarize(shapiro_p = shapiro.test(yellowfish$Yellow.Fish.size)$p.value)

shapiro.test(yellowfish$Yellow.Fish.size)

t.test(yellowfish$Yellow.Fish.size ~ yellowfish$Population)


#ANOVA
bacteria <- read.csv("bacteriadata.csv")
bacteria$Beach <- as.factor(bacteria$Beach)

ggplot(data = bacteria, aes(x=Beach, y=Bacterial.Level)) + geom_boxplot()
bacteriaANOVA <- lm(bacteria$Bacterial.Level ~ bacteria$Beach)
anova(bacteriaANOVA)
TukeyHSD(aov(bacteriaANOVA))


#Regression

shark <- read.csv("SharkAge.csv")
cor(shark)
pairs(shark[, c("age", "length_cm", "vertebral_bands", "Gill.spots", "scars", "telomere.length")])

sharkLM <- lm(shark$age ~ shark$length_cm + shark$vertebral_bands)
summary(sharkLM)
plot(sharkLM, which = 1)
plot(sharkLM, which = 2)
vif(sharkLM)
ggplot(data=shark, aes(x=length_cm,y=age)) + geom_point() + theme_minimal() + xlab("Length CM") + ylab("Age") + geom_smooth(method = lm)


sharkLM2 <- lm(shark$age ~ shark$telomere.length + shark$Gill.spots)
summary(sharkLM2)
plot(sharkLM2, which = 1)
plot(sharkLM2, which = 2)
vif(sharkLM2)


AIC(sharkLM, sharkLM2)


ggplot(data = shark, aes(x=length_cm, y=age)) + geom_point() + theme_minimal() + xlab("Shark Length (cm)") + ylab("Shark Age (Years)") + geom_smooth(method = lm)






