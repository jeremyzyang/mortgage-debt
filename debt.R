                                                 # debt # 

library(lme4)
library(MASS)
library(car)

# read in data
demo <- read.csv('C:/Users/Jeremy/Desktop/Workspace/R/debt/county_demo.csv')
debt <- read.csv('C:/Users/Jeremy/Desktop/Workspace/R/debt/debt_06.csv')
house.price <- read.csv('C:/Users/Jeremy/Desktop/Workspace/R/debt/state_house_price.csv') # state level house price

# select house price for 2006 and average over 4 quarters
house.price <- house.price[which(house.price$Year.Quarter=='2006Q1'|house.price$Year.Quarter=='2006Q2'|house.price$Year.Quarter=='2006Q3'|house.price$Year.Quarter=='2006Q4'), ]
hp <- house.price[which(house.price$State!='US'),] # excluding national price

hp$Average.Price <- gsub('\\$|,','',hp$Average.Price) # remove the $ and , symbols
hp$Average.Price <- as.numeric(hp$Average.Price) # change back to numeric format

state <- unique(debt$state)
average <- tapply(hp$Average.Price,hp$State,mean) # calculate average
average <- average[-c(1,45)]
average <- data.frame(state,average)
colnames(average) <- c('state','house_price_average')
average

# merge 3 dataset into 1
colnames(demo);colnames(debt)
data0 <- merge(debt,demo,'FIPS'); colnames(data)
data['State_Name'] <- NULL; data['County_Name'] <- NULL
data2 <- merge(data,average,'state');head(data2)
data2[,22:28] <- data[,22:28]
data <- data2;head(data)
colnames(data)[32] <- 'State_house_price_average'
colnames(data)
head(data)

# we standadize the explanatory variables to normalize the scale, so it'd be easier to compare the magnitude of coefficients 
data.std <- scale(data[,c(10:28,32)]);head(data.std)
data2 <- data
data2[,c(10:28,32)]<-data.std
data.std <- data2;head(data.std)

# OLS on mortgage debt
ols.mortgage <- lm(Mortgage ~ Female_life_Growth + Male_life_Growth + Per_young + Per_old +
                     Per_white + Per_black + Per_hispanic + Per_single_mother + Per_NO_HighSchool + Per_college +
                     Rate_median_income + Percent_divorced + Per_Evangelical_adherent +
                     Per_Evangelical_congregations + Per_Obama + Per_Rural + Rate_Teen_Birth +
                     Rate_Violent_Crime, data.std)
summary(ols.mortgage)
coefficients(ols.mortgage)

with(data,scatterplot(Per_Evangelical_adherent,Per_Evangelical_congregations)) # these predictors are strongly correlated
with(data,scatterplot(Per_single_mother,Percent_divorced))
with(data,scatterplot(Per_white,Per_black))

vif(ols.mortgage) # test for multicollinearity: Per_single_mother, Per_Evangelical_adherent and Per_white/black are problematic 

ols.mortgage <- lm(Mortgage ~ Female_life_Growth + Male_life_Growth + Per_young + Per_old +
                     Per_black + Per_NO_HighSchool + Per_college +
                     Rate_median_income + Percent_divorced + Per_Evangelical_congregations + Per_Obama + Per_Rural + Rate_Teen_Birth +
                     Rate_Violent_Crime, data.std) # drop those two and run the model again
summary(ols.mortgage)
vif(ols.mortgage) # now it looks better

ols.log.mortgage <- lm(log(Mortgage) ~ Female_life_Growth + Male_life_Growth + Per_young + Per_old +
                     Per_black + Per_NO_HighSchool + Per_college + Rate_median_income + Percent_divorced +
                     Per_Evangelical_congregations + Per_Obama + Per_Rural + Rate_Teen_Birth +
                     Rate_Violent_Crime, data.std)
summary(ols.log.mortgage)
coefficients(ols.log.mortgage)

# multilevel model with state level predictor (state house price, we can also include group mean of couty level predictors)

lme.mortgage <- lmer(Mortgage ~ Female_life_Growth + Male_life_Growth + Per_young + Per_old +
                       Per_black + Per_NO_HighSchool + Per_college + Rate_median_income + Percent_divorced + 
                       Per_Evangelical_congregations + Per_Obama + Per_Rural + Rate_Teen_Birth + Rate_Violent_Crime +
                       State_house_price_average + (1|state), data.std) # we add in random intercept
summary(lme.mortgage) # state level house price is a significant predictor, counties within in high price state has higher intercept


q()
