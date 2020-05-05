library(caret)
library(ROCR)
library(car)

# read csv file 
path="C:/Users/Tanmay Ambatkar/Documents/DataSets/social.csv"
social=read.csv(path,header = T)

str(social)
head(social)
# remove unwanted columns

social$User.ID = NULL

# convert the unfactor to factor

social$Purchased=as.factor(social$Purchased)

# split the columns into numeric and factors

numcols=colnames(social)[sapply(social, is.numeric)]
numcols
factcols=colnames(social)[sapply(social, is.factor)]
factcols

# EDA

checknull=function(x)return(any(is.na(social)))
null=colnames(social)[apply(social,2,checknull)] # nulls checks
if(length(null)==0)
  print("There are no NULLS") else
    print(paste("NULLS in column",null))

checkzero=function(x)return(any(x<=0)) #check zero
zero=colnames(social[numcols])[apply(social[numcols],2,checkzero)]
if(length(zero)==0)
  print("Rhere are no Zeros") else
    print(paste("Zeros in column:",zero))

library(corrplot) # check the multicollinearity
corr=cor(social[numcols])
corrplot(corr,type = 'lower',method = 'number')

# Check for the outliers by histogram and boxplot 

for(c in numcols)
{
  title=paste("histogram for",c)
  hist(unlist(social[c]),main = title,col = "red")  #unlist is imp for histogram
} 

for(a in numcols)
{
  title=paste("boxplot for",a)
  boxplot(social[a],main = title,col = "red",horizontal = T) #we dont unlist data in boxplot
} 

#check for Y-distribution
table(social$Purchased)

#check the number of levels for each variable
for(b in factcols)
{
  str(social[b])
}
#or
levels(social$Gender)

#split the data into train and test
total=nrow(social)
r=sample(seq(1,total),0.7*total)
train=social[r,]
test=social[-r,]
print(paste(dim(train), dim(test)))

for(f in factcols)
{
  count_train = length(levels(unlist(train[f])))
  count_test = length(levels(unlist(test[f])))
  
  if(count_train>=count_test)
    print("counts are OK")  else
      print(paste("counts not OK for factor",f))
}     #########Error?????


length(levels(unlist(train['Gender'])))
length(levels(unlist(test['Gender'])))


#build the logistic regression model

m1=glm(Purchased~.,data=train, binomial(link = "logit")) #model didn't converge. so you require to check and eleminate that column which is restricting the model from running. 
colnames(train)

#model 1

m1=glm(Purchased~.-Gender,data=train, binomial(link = "logit"))  
summary(m1)

#predictions
p1=predict(m1,test,type="response")

# actual vs predicted
table(ActualValue = test$Purchased , PredictedValue = p1>0.5)

table(ActualValue = test$Purchased , PredictedValue = p1>0.3)

# ploting the roc curve

ROCRPred = prediction(p1,test$Purchased)
ROCRPref = performance(ROCRPred,"tpr","fpr")

plot(ROCRPref,colorize = TRUE, print.cutoffs.at=seq(0.1,by=0.1 ))

#convert probabilities into classes
preds=ifelse(p1<0.5,0,1)

table(preds)
table(test$Purchased)

# confusion matrix

cm = confusionMatrix(test$Purchased,factor(preds),positive = "1")

print(cm)


