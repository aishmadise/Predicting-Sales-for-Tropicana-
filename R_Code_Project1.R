install.packages('data.table')

attach(tropic1)
str(tropic1)

#Log Transformation of Variables Price and Quant
log.price <-log(Price)
log.quant <- log (Quant)

#Quarter Variables Creation 
library(data.table)
qrt1 <- ifelse((between(Week,1,13) | between(Week,53,65)), 1,0)
qrt2 <- ifelse((between(Week,14,26) | between(Week,66,78)),1,0)
qrt3 <- ifelse( between(Week, 27,39) | between(Week,79,91), 1, 0)
qrt4 <- ifelse( between(Week, 40,52) | between(Week,92,104), 1, 0)

# End9 Variable Creation
End9 <- 1*(round(10*(10*Price-floor(10*Price)))==9)

#Create Factor Variables
Season=as.factor(qrt1+2*qrt2+3*qrt3+4*qrt4)
store=as.factor(Store)
end9=as.factor(End9)

# Frequency of deal and store
table(Deal,store)
table(end9,store)

# Descriptive Statistics of Sales and Price
summary(Quant)
summary(Price)

# Average Sales Volume by Store
tapply(Quant,store, mean)

# Correlation of Sales, Price and Week
cor(tropic1,method= "pearson")

#Regression 
#Linear Regression
reg <- lm( Quant ~ Price+Deal,data = tropic1)
summary(reg)
#Log-Log Model
reg1 <- lm(log.quant ~ log.price+Deal, data = tropic1)
summary(reg1)
#Log-Log Models
reg2 <- lm(log.quant ~ log.price+Deal+Season, data = tropic1)
summary(reg2)
#Semi-Log Models
reg3 <- lm(log.quant ~ Price+Deal, data = tropic1)
summary(reg3)
#Semi-Log Models
reg4 <- lm(log.quant ~ Price+Deal+Season, data = tropic1)
summary(reg4)


