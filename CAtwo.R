#Loading the data
mydata<- read.csv("C:/Users/Dell/Desktop/car_train.csv")
mydata
head(mydata)
#Defining the variable
x1<- mydata$rownum
x3<- mydata$body_type
x4<- mydata$category
x6<- mydata$cylinders
x7<- mydata$economy
x8<- mydata$fuel
x9<- mydata$last_updated
x10<- mydata$litres
x11<- mydata$location
x12<- mydata$make
x13<- mydata$model
x14<- mydata$odometer
x15<- mydata$transmission
x16<- mydata$year
y<- mydata$price
dataset<- na.omit(data.frame(x1,x3,x4,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,y))
dim(mydata)
#fitiing the model
fit.glm<- glm(y~., dataset, family = 'gaussian')
summary(fit.glm)

#indexing the model and dividing it into 80/20 ratio
n=nrow(dataset) 

indexes = sample(n,n*(80/100)) 

trainset = dataset[indexes,] 

testset = dataset[-indexes,] 

dim(mydata)

# fitting the model using trainset 

trainset.glm <- glm(trainset$y ~.,trainset, family="gaussian")


# full model 

fit = glm(trainset$y ~., data= trainset, family='gaussian') 

#Predicting  the Model  
pred=predict(fit, testset) 

actual=testset$y 

rmse=sqrt((sum((pred-actual)^2))/nrow(testset)) 



# reduced model 

library(MASS) 

fit_red = stepAIC(fit)
# fitting reduced model 

pred_r=predict(fit_red, testset) 

rmse_r=sqrt((sum((pred_r - actual)^2))/nrow(testset)) 

#Accuracy

RMSE=c(0,0) 
RMSE=RMSE+c(rmse,rmse_r) 





