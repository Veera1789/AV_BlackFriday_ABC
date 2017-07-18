# Base Model for Black Friday Competition... 
# Competition URL : https://datahack.analyticsvidhya.com/contest/black-friday/

# Reading traindata from local repository 
traindata<-read.csv("C:\\Users\\user\\Dropbox\\temp\\Hackathons\\AV\\BlackFriday_ABC\\data\\train.csv",header=TRUE)
summary(traindata)
str(traindata)
traindata$User_ID<-as.character(traindata$User_ID)
traindata$Product_ID<-as.character(traindata$Product_ID)
traindata$Occupation<-as.factor(traindata$Occupation)
traindata$Marital_Status<-as.factor(traindata$Marital_Status)
traindata$Product_Category_1<-as.factor(traindata$Product_Category_1)
traindata$Product_Category_2<-as.factor(traindata$Product_Category_2)
traindata$Product_Category_3<-as.factor(traindata$Product_Category_3)

#trying city stay and Age as numeric 
traindata$Stay_In_Current_City_Years <-as.numeric(traindata$Stay_In_Current_City_Years)
traindata$Age<-as.numeric(traindata$Age)
traindata$Product_ID<-as.factor(traindata$Product_ID)

#
summary(traindata)
md.pattern(traindata[,-1])

#Reading testdata
testdata<-read.csv("C:\\Users\\user\\Dropbox\\temp\\Hackathons\\AV\\BlackFriday_ABC\\data\\test.csv",header = TRUE)
str(testdata)
testdata$User_ID<-as.character(testdata$User_ID)
testdata$Product_ID<-as.character(testdata$Product_ID)
testdata$Occupation<-as.factor(testdata$Occupation)
testdata$Marital_Status<-as.factor(testdata$Marital_Status)
testdata$Product_Category_1<-as.factor(testdata$Product_Category_1)
testdata$Product_Category_2<-as.factor(testdata$Product_Category_2)
testdata$Product_Category_3<-as.factor(testdata$Product_Category_3)

#trying city stay and Age as numeric 
testdata$Stay_In_Current_City_Years <-as.numeric(testdata$Stay_In_Current_City_Years)
testdata$Age<-as.numeric(testdata$Age)

 
# #Creating comb variable in traindata
# traindata$comb<-paste(as.character(traindata$User_ID),as.character(traindata$Product_ID),sep = "")
# traindata$comb<-as.character(traindata$comb)


model1<-lm(Purchase~Gender+Occupation+City_Category+Age+Stay_In_Current_City_Years+Marital_Status+Product_Category_1+Product_Category_2+Product_Category_3,data=traindata)
summary(model1)
out<-predict(model1,testdata[,-c(1,2)])




test1<-testdata[(testdata$Product_Category_1%in%c(7,17,18)),]

length(out)
final<-data.frame(testdata$User_ID,testdata$Product_ID,out)
write.csv(final,"Submission1.csv",row.names = F)

getwd()
