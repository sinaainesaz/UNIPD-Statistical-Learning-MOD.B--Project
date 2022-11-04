library(ggplot2)
library(leaps)
library(boot)
library(Metrics)
#################################
# MIAMI HOUSE PRICE PREDICTION ##
#################################
getwd()
#### IMPORTING DATASET ####
getwd()
house = 'miami-housing.csv'
Miami_house <- read.csv(house)

head(Miami_house)
attach(Miami_house)
str(Miami_house)
#### OVERVIEW OF THE VARIABLES IN THE DATASET ####
# PARCELNO: unique identifier for each property. About 1% appear multiple times.
# SALE_PRC: sale price ($)
# LND_SQFOOT: land area (square feet)
# TOTLVGAREA: floor area (square feet)
# SPECFEATVAL: value of special features (e.g., swimming pools) ($)
# RAIL_DIST: distance to the nearest rail line (an indicator of noise) (feet)
# OCEAN_DIST: distance to the ocean (feet)
# WATER_DIST: distance to the nearest body of water (feet)
# CNTR_DIST: distance to the Miami central business district (feet)
# SUBCNTR_DI: distance to the nearest subcenter (feet)
# HWY_DIST: distance to the nearest highway (an indicator of noise) (feet)
# age: age of the structure
# avno60plus: dummy variable for airplane noise exceeding an acceptable level
# structure_quality: quality of the structure
# month_sold: sale month in 2016 (1 = jan)
# LATITUDE
# LONGITUDE

###################################
#### PREPROCESSING THE DATASET ####
###################################
length(which(Miami_house$SPEC_FEAT_VAL < 50))
length(which(Miami_house$WATER_DIST == 0))

summary(Miami_house)
summary(Miami_house$SALE_PRC)

# dimension of the dataset #
dim(Miami_house)

# checking to see if there is a missing value in the dataset #
sum(is.na(Miami_house))

# adding new column as logaritmic value of house price #
Miami_house$log10_SALE_PRC = log10(SALE_PRC)

# converting the dummy variable for airplane noise to a factor #
Miami_house$avno60plus = as.factor(Miami_house$avno60plus)

# converting the variable related to the sell month to a factor #
Miami_house$month_sold = as.factor(Miami_house$month_sold)

# converting the variable for the quality of structure to a factor #
Miami_house$structure_quality = as.factor(Miami_house$structure_quality)

#Adding a new column has_SPECFEAT indicating whether the house has a special feature #
Miami_house$has_SPECFEAT = as.factor(Miami_house$SPEC_FEAT_VAL!=0)

#Adding a new column has_BODYOFWATER indicating whether the house has a body of water #
Miami_house$has_BODYOFWATER = as.factor(Miami_house$WATER_DIST==0)

# Dropping the unique identifier column which has no information about the house prices #
Miami_house$PARCELNO = NULL

# DIMENSION OF THE DATASET WITH NEW FEATURES #
dim(Miami_house)
str(Miami_house)

######################################
########### EXPLORING DATA ###########
######################################

# Checking distribution of SALE PRICE variable #
ggplot(Miami_house, aes(x= SALE_PRC)) + geom_histogram(color = "green", bins=30) + labs(x ="price ($)", title = "Price of the House")

# Logaritmic distribution of SALE PRICE variable #
ggplot(Miami_house, aes(x= log10_SALE_PRC)) + geom_histogram(color = "green", bins=30) + labs(x ="logaritmic price ($)", title = "Price of the House (Log)")

# Checking distribution of LAND SIZE variable #
ggplot(Miami_house, aes(x = LND_SQFOOT)) + geom_histogram(color = "green", bins=30) + labs(x ="land area (square feet)", title = "Size of Land")

# Checking distribution of FLOOR SIZE variable #
ggplot(Miami_house, aes(x = TOT_LVG_AREA)) + geom_histogram(color = "green", bins=30) + labs(x ="floor area (square feet)", title = "Size of Floor")

# Checking distribution of SPECIFIC FEATURE variable #
ggplot(Miami_house, aes(x = SPEC_FEAT_VAL)) + geom_histogram(color = "green", bins=30) + labs(x ="value of special features ($)", title = "Special Feature")

# Checking distribution of DISTANCE from RAILWAY variable #
ggplot(Miami_house, aes(x = RAIL_DIST)) + geom_histogram(color = "green", bins=30) + labs(x ="distance to the nearest rail line (feet)", title = "Rail line Dist")

# Checking distribution of DISTANCE from OCEAN variable #
ggplot(Miami_house, aes(x = OCEAN_DIST)) + geom_histogram(color = "green", bins=30) + labs(x ="distance to the ocean (feet)", title = "Ocean Dist")

# Checking distribution of DISTANCE from WATER variable #
ggplot(Miami_house, aes(x = WATER_DIST)) + geom_histogram(color = "green", bins=30) + labs(x ="distance to the nearest body of water (feet)", title = "Water Dist")

# Checking distribution of DISTANCE from BUSINESS CENTER variable #
ggplot(Miami_house, aes(x = CNTR_DIST)) + geom_histogram(color = "green", bins=30) + labs(x ="distance to the Miami central business district (feet)", title = "Central Business Dist")

# Checking distribution of DISTANCE from SUBCENTER variable #
ggplot(Miami_house, aes(x = SUBCNTR_DI)) + geom_histogram(color = "green", bins=30) + labs(x ="distance to the nearest subcenter (feet)", title = "SUBCENTER Dist")

# Checking distribution of DISTANCE from HIGHWAY variable #
ggplot(Miami_house, aes(x = HWY_DIST)) + geom_histogram(color = "green", bins=30) + labs(x ="distance to the nearest highway (feet)", title = "HIGHWAY Dist")

# Borplot of the age variable #
ggplot(Miami_house, aes(x = age)) + geom_bar(color="yellow") + labs(x ="Age of house")

# Borplot of airplane noise variable #
ggplot(Miami_house, aes(x = avno60plus)) + geom_bar(color="yellow") + labs(x ="Aiplane noise")

# Boxplot of the price with respect to the noise of airplane #
ggplot(Miami_house, aes(x=avno60plus, y=log10_SALE_PRC, fill=factor(avno60plus))) + geom_boxplot(alpha=0.3) + theme(legend.position="none") + labs(x = 'airplane noise', y = 'Log(Price)')

# Borplot of SOLD MONTH variable #
ggplot(Miami_house, aes(x = month_sold)) + geom_bar(color="yellow") + labs(x ="Month",  title = "Sold Month")

# Boxplot of the price with respect to the sold month #
ggplot(Miami_house, aes(x=month_sold, y=log10_SALE_PRC, fill=factor(month_sold))) + geom_boxplot(alpha=0.3) + theme(legend.position="none") + labs(x = 'Month', y = 'Log(Price)')

# Borplot of STRUCTURE QUALITY variable #
ggplot(Miami_house, aes(x = structure_quality)) + geom_bar(color="yellow") + labs(x ="Quality",  title = "Quality of the Structure")

# Boxplot of the price with respect to the quality of the structure#
ggplot(Miami_house, aes(x=structure_quality, y=log10_SALE_PRC, fill=factor(structure_quality))) + geom_boxplot(alpha=0.3) + theme(legend.position="none") + labs(x = 'Quality', y = 'Log(Price)')

# Borplot of has specific feature variable #
ggplot(Miami_house, aes(x = has_SPECFEAT)) + geom_bar(color="yellow") + labs(x ="If there is specific feature")

# Boxplot of the price with respect to the fact that if there is specific feature #
ggplot(Miami_house, aes(x=has_SPECFEAT, y=log10_SALE_PRC, fill=factor(has_SPECFEAT))) + geom_boxplot(alpha=0.3) + theme(legend.position="none") + labs(x = 'There is specific feature', y = 'Log(Price)')

# Borplot of has BODYOFWATER variable #
ggplot(Miami_house, aes(x = has_BODYOFWATER)) + geom_bar(color="yellow") + labs(x ="If there is waterbody")

# Boxplot of the price with respect to the fact that if there is WATERBODY #
ggplot(Miami_house, aes(x=has_BODYOFWATER, y=log10_SALE_PRC, fill=factor(has_BODYOFWATER))) + geom_boxplot(alpha=0.3) + theme(legend.position="none") + labs(x = 'There is WATERBODY', y = 'Log(Price)')


##################################################
########### MODELING AND DATA ANALYSIS ###########
##################################################

# Checking the correlation between logaritmic value of price and other numerical variables #
numerical_variables <- subset(Miami_house, select = c(log10_SALE_PRC, LND_SQFOOT, TOT_LVG_AREA, SPEC_FEAT_VAL, RAIL_DIST, OCEAN_DIST, WATER_DIST, CNTR_DIST, SUBCNTR_DI, HWY_DIST, LATITUDE, LONGITUDE))
round(cor(numerical_variables[]), 3)

# Linear model #
model.num1 <- lm(data = Miami_house, log10_SALE_PRC ~ TOT_LVG_AREA)
summary(model.num1)
BIC(model.num1)

# Plotting model #
ggplot(Miami_house, aes(x = TOT_LVG_AREA, y = log10_SALE_PRC)) + geom_point() + stat_smooth(method = "lm", formula = y ~ x) + labs(x = 'floor area (square feet)', y = 'Log(Price) ($)')

# Diagnostic #
par(mfrow=c(2,2))
plot(model.num1)
par(mfrow=c(1,1))

# Polynomial model #
model.num2 <- lm(data=Miami_house, log10_SALE_PRC ~ TOT_LVG_AREA + I(TOT_LVG_AREA**2))
summary(model.num2)
BIC(model.num2)

# Plotting model #
ggplot(Miami_house, aes(x = TOT_LVG_AREA, y = log10_SALE_PRC)) + geom_point() + stat_smooth(method = "lm", formula = y ~ x + I(x**2)) + labs(x = 'floor area (square feet) + [floor area (square feet)]ˆ2 ', y = 'Log(Price) ($)')

# Diagnostic #

par(mfrow=c(2,2))
plot(model.num2)
par(mfrow=c(1,1))

# Using Regsubset() function #
regfit.num <- regsubsets(log10_SALE_PRC ~ LND_SQFOOT + TOT_LVG_AREA + SPEC_FEAT_VAL + RAIL_DIST + OCEAN_DIST + WATER_DIST + CNTR_DIST + SUBCNTR_DI + HWY_DIST + LATITUDE + LONGITUDE, data=Miami_house)
reg.summary <- summary(regfit.num)
reg.summary
names(reg.summary)
reg.summary$rsq

# Useful plots #
par(mfrow=c(2,2))

# residual sum of squares
plot(reg.summary$rss, xlab="Number of Variables", ylab="RSS", type="l")

# adjusted-Rˆ2 with its largest value
plot(reg.summary$adjr2, xlab="Number of Variables", ylab="Adjusted RSq", type="l")
which.max(reg.summary$adjr2)
points(8, reg.summary$adjr2[8], col="blue",cex=2,pch=20)

# Mallow's Cp with its smallest value
plot(reg.summary$cp, xlab="Number of Variables", ylab="Cp", type='l')
which.min(reg.summary$cp)
points(8, reg.summary$cp[8], col="blue", cex=2, pch=20)

# BIC with its smallest value
plot(reg.summary$bic, xlab="Number of Variables", ylab="BIC", type='l')
which.min(reg.summary$bic)
points(8, reg.summary$bic[8], col="blue", cex=2, pch=20)

par(mfrow=c(1,1))

plot(regfit.num,scale="r2")
plot(regfit.num,scale="adjr2")
plot(regfit.num,scale="Cp")
plot(regfit.num,scale="bic")

# Best model "BIC"
best.bic <- lm(log10_SALE_PRC~TOT_LVG_AREA+SPEC_FEAT_VAL+RAIL_DIST+WATER_DIST+SUBCNTR_DI+HWY_DIST, data=Miami_house)
summary(best.bic)

# Checking the degree of each 6 variable #

# log(price) vs Special Feature variable #
model.SPEC_FEAT_VAL <- lm(data=Miami_house, log10_SALE_PRC ~ SPEC_FEAT_VAL)
summary(model.SPEC_FEAT_VAL)
BIC(model.SPEC_FEAT_VAL)

ggplot(Miami_house, aes(x = SPEC_FEAT_VAL, y = log10_SALE_PRC)) + geom_point() + stat_smooth(method = "lm", formula = y ~ x) + labs(x = 'Value of special features ($)', y = 'Log(Price) ($)')


# Diagnostic
par(mfrow=c(2,2))
plot(model.SPEC_FEAT_VAL)
par(mfrow=c(1,1))

# log(price) vs SPEC_FEAT_VAL + SPEC_FEAT_VAL2
model.SPEC_FEAT_VAL2 <- lm(data=Miami_house, log10_SALE_PRC ~ SPEC_FEAT_VAL + I(SPEC_FEAT_VAL**2))
summary(model.SPEC_FEAT_VAL2)
BIC(model.SPEC_FEAT_VAL2)

ggplot(Miami_house, aes(x = SPEC_FEAT_VAL, y = log10_SALE_PRC)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x,2)) +
  labs(x = 'Value of special features ($)', y = 'Log(Price) ($)')

# Diagnostic
par(mfrow=c(2,2))
plot(model.SPEC_FEAT_VAL2)
par(mfrow=c(1,1))

# log(price) vs Distance to the nearest RAIL LINE variable #
model.RAIL_DIST <- lm(data=Miami_house, log10_SALE_PRC ~ RAIL_DIST)
summary(model.RAIL_DIST)
BIC(model.RAIL_DIST)

ggplot(Miami_house, aes(x = RAIL_DIST, y = log10_SALE_PRC)) + geom_point() + stat_smooth(method = "lm", formula = y ~ x) + labs(x = 'Distance to the nearest RAIL LINE (feet)', y = 'Log(Price) ($)')

# Diagnostic
par(mfrow=c(2,2))
plot(model.RAIL_DIST)

par(mfrow=c(1,1))

model.RAIL_DISTmulti <- lm(data=Miami_house, log10_SALE_PRC ~ RAIL_DIST + I(RAIL_DIST**2) + I(RAIL_DIST**3) + I(RAIL_DIST**4)+ I(RAIL_DIST**5))
summary(model.RAIL_DISTmulti)
anova(model.RAIL_DISTmulti)

# Degree 5
model.RAIL_DIST5 <- lm(data=Miami_house, log10_SALE_PRC ~ poly(RAIL_DIST, 5))
summary(model.RAIL_DIST5)
BIC(model.RAIL_DIST5)

ggplot(Miami_house, aes(x = RAIL_DIST, y = log10_SALE_PRC)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x, 5)) +
  labs(x = 'Distance to the nearest RAIL LINE (feet)', y = 'Log(Price) ($)')

# Diagnostic
par(mfrow=c(2,2))
plot(model.RAIL_DIST5)

# log(price) vs Distance to the nearest BODY OF WATER variable #
model.WATER_DIST <- lm(data=Miami_house, log10_SALE_PRC ~ WATER_DIST)
summary(model.WATER_DIST)
BIC(model.WATER_DIST)

ggplot(Miami_house, aes(x = WATER_DIST, y = log10_SALE_PRC)) + geom_point() + stat_smooth(method = "lm", formula = y ~ x) + labs(x = 'Distance to the nearest BODY OF WATER (feet)', y = 'Log(Price) ($)')

# Diagnostic
par(mfrow=c(2,2))
plot(model.WATER_DIST)

par(mfrow=c(1,1))

model.WATER_DISTmulti <- lm(data=Miami_house, log10_SALE_PRC ~ WATER_DIST + I(WATER_DIST**2) + I(WATER_DIST**3) + I(WATER_DIST**4)+ I(WATER_DIST**5))
summary(model.WATER_DISTmulti)
anova(model.WATER_DISTmulti)

# Degree 4 for Water Distance #
model.WATER_DIST4 <- lm(data=Miami_house, log10_SALE_PRC ~ poly(WATER_DIST, 4))
summary(model.WATER_DIST4)
BIC(model.WATER_DIST4)

ggplot(Miami_house, aes(x = WATER_DIST, y = log10_SALE_PRC)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x, 4)) +
  labs(x = 'Distance to the nearest BODY OF WATER (feet)', y = 'Log(Price) ($)')

# Diagnostic
par(mfrow=c(2,2))
plot(model.WATER_DIST4)

# log(price) vs Distance to the nearest SUBCENTER variable #
model.SUBCNTR_DI <- lm(data=Miami_house, log10_SALE_PRC ~ SUBCNTR_DI)
summary(model.SUBCNTR_DI)
BIC(model.SUBCNTR_DI)

ggplot(Miami_house, aes(x = SUBCNTR_DI, y = log10_SALE_PRC)) + geom_point() + stat_smooth(method = "lm", formula = y ~ x) + labs(x = 'Distance to the nearest Subcenter (feet)', y = 'Log(Price) ($)')

# Diagnostic
par(mfrow=c(2,2))
plot(model.SUBCNTR_DI)

par(mfrow=c(1,1))

model.SUBCNTR_DImulti <- lm(data=Miami_house, log10_SALE_PRC ~ SUBCNTR_DI + I(SUBCNTR_DI**2) + I(SUBCNTR_DI**3) + I(SUBCNTR_DI**4)+ I(SUBCNTR_DI**5))
summary(model.SUBCNTR_DImulti)
anova(model.SUBCNTR_DImulti)

# Degree 4 for Subcenter Distance #
model.SUBCNTR_DI4 <- lm(data=Miami_house, log10_SALE_PRC ~ poly(SUBCNTR_DI, 4))
summary(model.SUBCNTR_DI4)
BIC(model.SUBCNTR_DI4)

ggplot(Miami_house, aes(x = SUBCNTR_DI, y = log10_SALE_PRC)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x, 4)) +
  labs(x = 'Distance to the nearest subcenter (feet)', y = 'Log(Price) ($)')

# Diagnostic
par(mfrow=c(2,2))
plot(model.SUBCNTR_DI4)

# log(price) vs Distance to the nearest HIGHWAY variable #
model.HWY_DIST <- lm(data=Miami_house, log10_SALE_PRC ~ HWY_DIST)
summary(model.HWY_DIST)
BIC(model.HWY_DIST)

ggplot(Miami_house, aes(x = HWY_DIST, y = log10_SALE_PRC)) + geom_point() + stat_smooth(method = "lm", formula = y ~ x) + labs(x = 'Distance to the nearest Highway (feet)', y = 'Log(Price) ($)')

# Diagnostic
par(mfrow=c(2,2))
plot(model.HWY_DIST)

par(mfrow=c(1,1))

model.HWY_DISTmulti <- lm(data=Miami_house, log10_SALE_PRC ~ HWY_DIST + I(HWY_DIST**2) + I(HWY_DIST**3) + I(HWY_DIST**4)+ I(HWY_DIST**5))
summary(model.HWY_DISTmulti)
anova(model.HWY_DISTmulti)

# Degree 4 for HIGHWAY Distance #
model.HWY_DIST4 <- lm(data=Miami_house, log10_SALE_PRC ~ poly(HWY_DIST, 4))
summary(model.HWY_DIST4)
BIC(model.HWY_DIST4)

ggplot(Miami_house, aes(x = HWY_DIST, y = log10_SALE_PRC)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x, 4)) +
  labs(x = 'Distance to the nearest HIGHWAY (feet)', y = 'Log(Price) ($)')

# Diagnostic
par(mfrow=c(2,2))
plot(model.HWY_DIST4)

# Comparing all the models with each other #
# The first numerical model with one variable, (TOT_LVG_AREA) #
summary(model.num1)
# Diagnostic
par(mfrow=c(2,2))
plot(model.num1)
par(mfrow=c(1,1))
# The second numerical model with 2 variables, (TOT_LVG_AREA) and (TOT_LVG_AREA**2) #
summary(model.num2)
# Diagnostic
par(mfrow=c(2,2))
plot(model.num1)
par(mfrow=c(1,1))
# The third numerical model with 6 variables, all the 6 variables with degree 1 #
model.poly1 <- lm(log10_SALE_PRC ~ TOT_LVG_AREA + SPEC_FEAT_VAL + RAIL_DIST + WATER_DIST + SUBCNTR_DI + HWY_DIST,
             data=Miami_house)
summary(model.poly1)
# Diagnostic
par(mfrow=c(2,2))
plot(model.poly1)
par(mfrow=c(1,1))
# The fourth numerical model with 6 variables, all the 6 variables with degree 1 except TOT_LVG_AREA with degree 2 #
model.poly2 <- lm(log10_SALE_PRC ~ poly(TOT_LVG_AREA,2) + SPEC_FEAT_VAL + RAIL_DIST + WATER_DIST + SUBCNTR_DI + HWY_DIST,
                  data=Miami_house)
summary(model.poly2)
# Diagnostic
par(mfrow=c(2,2))
plot(model.poly2)
par(mfrow=c(1,1))
# The fifth numerical model with 6 variables, all 6 variables with their best degree #
model.poly3 <- lm(log10_SALE_PRC ~ poly(TOT_LVG_AREA,2) + SPEC_FEAT_VAL + RAIL_DIST + poly(WATER_DIST, 4) + poly(SUBCNTR_DI, 4) + HWY_DIST,
                  data=Miami_house)
summary(model.poly3)
# Diagnostic
par(mfrow=c(2,2))
plot(model.poly3)
par(mfrow=c(1,1))

# Comparing BIC #

BIC(model.num1)
BIC(model.num2)
BIC(model.poly1)
BIC(model.poly2)
BIC(model.poly3)
# CATEGORICAL VARIABLE SELECTION #
model.cat1 <- lm(log10_SALE_PRC ~ avno60plus + month_sold + structure_quality + has_SPECFEAT +
                 has_BODYOFWATER + age, data=Miami_house)
summary(model.cat1)
BIC(model.cat1)
# without month_sold #
model.cat2 <- lm(log10_SALE_PRC ~ avno60plus + structure_quality + has_SPECFEAT +
                   has_BODYOFWATER + age, data=Miami_house)
summary(model.cat2)
BIC(model.cat2)

# without month_sold and avno60plus #

model.cat3 <- lm(log10_SALE_PRC ~ structure_quality + has_SPECFEAT + has_BODYOFWATER + age, data=Miami_house)
summary(model.cat3)
BIC(model.cat3)

# without month_sold and structure #
model.cat4 <- lm(log10_SALE_PRC ~ avno60plus + has_SPECFEAT +
                   has_BODYOFWATER + age, data=Miami_house)
summary(model.cat4)
BIC(model.cat4)

# without month_sold and has feature #
model.cat5 <- lm(log10_SALE_PRC ~ avno60plus + structure_quality  +
                   has_BODYOFWATER + age, data=Miami_house)
summary(model.cat5)
BIC(model.cat5)

# without month_sold and water #
model.cat6 <- lm(log10_SALE_PRC ~ avno60plus + structure_quality + has_SPECFEAT + age, data=Miami_house)
summary(model.cat6)
BIC(model.cat6)
# Plotting categorical models #

par(mfrow=c(2,2))
plot(model.cat1)

par(mfrow=c(2,2))
plot(model.cat2)

par(mfrow=c(2,2))
plot(model.cat3)

par(mfrow=c(2,2))
plot(model.cat4)

par(mfrow=c(2,2))
plot(model.cat5)

par(mfrow=c(2,2))
plot(model.cat6)

# COMBINING NUMERICAL AND CATEGORICAL VARIABLES TO OBTAIN THE BEST FINAL MODEL #

# Poly1 + CAT2 #
FINmodel1.Pol1Cat2 <- lm(log10_SALE_PRC ~ TOT_LVG_AREA + SPEC_FEAT_VAL + RAIL_DIST + WATER_DIST + SUBCNTR_DI + HWY_DIST + avno60plus + structure_quality + has_SPECFEAT +
                         has_BODYOFWATER + age, data=Miami_house)
summary(FINmodel1.Pol1Cat2)
# Diagnostic
par(mfrow=c(2,2))
plot(FINmodel1.Pol1Cat2)
BIC(FINmodel1.Pol1Cat2)
par(mfrow=c(1,1))

# Poly2 + CAT2 #
FINmodel2.Pol2Cat2 <- lm(log10_SALE_PRC ~ poly(TOT_LVG_AREA,2) + SPEC_FEAT_VAL + RAIL_DIST + WATER_DIST + SUBCNTR_DI + HWY_DIST + avno60plus + structure_quality + has_SPECFEAT +
                         has_BODYOFWATER + age, data=Miami_house)
summary(FINmodel2.Pol2Cat2)
# Diagnostic
par(mfrow=c(2,2))
plot(FINmodel2.Pol2Cat2)
BIC(FINmodel2.Pol2Cat2)
par(mfrow=c(1,1))

# Poly3 + CAT2 #
FINmodel3.Pol3Cat2 <- lm(log10_SALE_PRC ~ poly(TOT_LVG_AREA,2) + SPEC_FEAT_VAL + RAIL_DIST + poly(WATER_DIST, 4) + poly(SUBCNTR_DI, 4) + HWY_DIST + avno60plus + structure_quality + has_SPECFEAT +
                         has_BODYOFWATER + age, data=Miami_house)
summary(FINmodel3.Pol3Cat2) #### THE BEST MODEL
# Diagnostic
par(mfrow=c(2,2))
plot(FINmodel3.Pol3Cat2)
BIC(FINmodel3.Pol3Cat2)
par(mfrow=c(1,1))

# Poly1 + CAT6 #
FINmodel4.Pol1Cat6 <- lm(log10_SALE_PRC ~ TOT_LVG_AREA + SPEC_FEAT_VAL + RAIL_DIST + WATER_DIST + SUBCNTR_DI + HWY_DIST + avno60plus + structure_quality + has_SPECFEAT + age, data=Miami_house)
summary(FINmodel4.Pol1Cat6)
# Diagnostic
par(mfrow=c(2,2))
plot(FINmodel4.Pol1Cat6)
BIC(FINmodel4.Pol1Cat6)
par(mfrow=c(1,1))

# Poly2 + CAT6 #
FINmodel5.Pol2Cat6 <- lm(log10_SALE_PRC ~ poly(TOT_LVG_AREA,2) + SPEC_FEAT_VAL + RAIL_DIST + WATER_DIST + SUBCNTR_DI + HWY_DIST + avno60plus + structure_quality + has_SPECFEAT + age, data=Miami_house)
summary(FINmodel5.Pol2Cat6)
# Diagnostic
par(mfrow=c(2,2))
plot(FINmodel5.Pol2Cat6)
BIC(FINmodel5.Pol2Cat6)
par(mfrow=c(1,1))

# Poly3 + CAT6 #
FINmodel6.Pol3Cat6 <- lm(log10_SALE_PRC ~ poly(TOT_LVG_AREA,2) + SPEC_FEAT_VAL + RAIL_DIST + poly(WATER_DIST, 4) + poly(SUBCNTR_DI, 4) + HWY_DIST + avno60plus + structure_quality + has_SPECFEAT + age, data=Miami_house)
summary(FINmodel6.Pol3Cat6)
# Diagnostic
par(mfrow=c(2,2))
plot(FINmodel6.Pol3Cat6)
BIC(FINmodel6.Pol3Cat6)
par(mfrow=c(1,1))

#######################################################
#######CROSS-VALIDATION OF THE FINAL BEST MODEL #######
#######################################################

finalbest.fit <- lm(log10_SALE_PRC ~ poly(TOT_LVG_AREA,2) + SPEC_FEAT_VAL + RAIL_DIST + poly(WATER_DIST, 4) + poly(SUBCNTR_DI, 4) + HWY_DIST + avno60plus + structure_quality + has_SPECFEAT +
                      has_BODYOFWATER + age, data=Miami_house)
model.glm <- glm(formula = log10_SALE_PRC ~ poly(TOT_LVG_AREA,2) + SPEC_FEAT_VAL + RAIL_DIST + poly(WATER_DIST, 4) + poly(SUBCNTR_DI, 4) + HWY_DIST + avno60plus + structure_quality + has_SPECFEAT +
                   has_BODYOFWATER + age, data=Miami_house)
summary(model.glm)
library(boot)
cv.err <- cv.glm(data = Miami_house, model.glm, K=10)
cv.err$delta[1]

mse(Miami_house$log10_SALE_PRC, finalbest.fit$fitted.values) 
