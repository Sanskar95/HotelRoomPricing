

# NAME-SANSKAR GUPTA
#EMAIL snskr95@gmail.com









city.df <- read.csv(paste("Cities42.csv", sep=""))
attach(city.df)


factor((city.df$IsMetroCity))

View(city.df)

mean(city.df$Population) #calculating the mean population

boxplot(city.df$Population,horizotal=FALSE,ylab="number",xlab="population")

table(city.df$StarRating) #anaylsing the frequency of star ratingings

boxplot(city.df$HotelCapacity,horizotal=TRUE,ylab="number",xlab="hotel capacity")

plot(x=Population,y=StarRating,xlab="population",ylab="star rating",data=city.df)
#population versus star rating
plot(x=Population,y=HotelCapacity,xlab="population",ylab="hotel capacity",data=city.df)
#population versus hotel capacity
abline(lm(Population~HotelCapacity,data=city.df))
 

plot(x=RoomRent,y=StarRating,xlab="room rent",ylab="star rating",data=city.df)
#room rent versus star rating
 
pairs(~RoomRent+Airport+HotelCapacity+StarRating,data=city.df,main="Scatter plot matrix of 3 independent variables")
            #scatter plot of considered variables
            #here y is the room rent and x1 x2 x3  are Hotelcapacity,airport,star rating respectively 
#creating the subset of selected variables and storing them in my vars
myvars<-city.df[c(2,10,11,12)]
myvars

library(corrgram)
corrgram(myvars, order=NULL, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Corrgram of variables in consideration")
#analysisng the covariance between the independent variables


options(digits=2)
cor(myvars,use="complete.obs",method="kendall")

#hypothesis:

fit11<-lm(RoomRent~Airport+HotelCapacity+Population+StarRating,data=myvars)

summary(fit11)
 #regression model on the myvars







city.df$CityRank<-city.df$CityRank+1
factor(city.df$Date)

fit1<-lm(RoomRent~ CityRank+IsMetroCity+IsNewYearEve+IsTouristDestination+IsWeekend+StarRating+Airport+FreeWifi+FreeBreakfast+HotelCapacity+HasSwimmingPool,data=city.df)
summary(fit1)
library(leaps)
#regression on all the variables



model1<-RoomRent~ CityRank+IsMetroCity+IsNewYearEve+IsTouristDestination+IsWeekend+StarRating+Airport+FreeWifi+FreeBreakfast+HotelCapacity+HasSwimmingPool

leap1 <- regsubsets(model1, data = city.df, nbest=1)

plot(leap1)
 #checking for the best model that fits the data


#the best model will be without the airport ,isweekend,freebreakfast
#now runing the regressin model wthout the above metioned parameters
model2<-RoomRent~ CityRank+IsMetroCity+IsNewYearEve+IsTouristDestination+StarRating+FreeWifi+HotelCapacity+HasSwimmingPool
fit2<-lm(model2,data=city.df)
summary(fit2)


#now to analyse the beta coefficients we will use the coefplot
library(coefplot)
library(ggplot2)
coefplot(fit2,coefficients=c("Cityrank","ismetrocity", "isnewlibrarayyearseve", "istouristdestination", "starrrating", "Freewifi","hotelcapacity","hasswimingpool"))
fit1<-lm(RoomRent~Airport+HotelCapacity+Population+StarRating,data=myvars)

summary(fit11)
fitted(fit11)
fitted(fit2)

AIC(fit11)
AIC(fit2)
#aic of fit 2 is better than fit1 so fit 2 is selected as the best fit
predictedRent = data.frame(fitted(fit2))
actualRent = data.frame(city.df$RoomRent)
RentComparison = cbind(actualRent, predictedRent)
View(RentComparison)

#result:
#the rent of room is directly related to the parameters in model 2
