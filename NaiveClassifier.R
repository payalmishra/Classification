#install.packages("e1071")

library(e1071)

sample<-read.csv("sample1.csv")
class(sample)

# tell R where is the training data and test data
traindata <- sample[1:14,]
testdata<-sample[15,]


# create the model. cross check this with our 'enroll' Excel sheet on 14th Day
model <- naiveBayes(Enrolls~Age+Income+JobSatisfaction+Desire,traindata)  # naiveBayes ( the element we are predicting ~ based on what fields or 
                                                                          # independent fields , based on the training data)
model


#Predict using this model which was created out of training data and use it for test data
results <- predict(model,testdata)
results

# ################ end of Bayesian Classifier ###################################

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~      TABLE Function ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ( entire steps done in Excel, is now being done in R using TABLE )

tenroll <- table(traindata$Enrolls)
tenroll
tenroll <- tenroll/sum(tenroll)
tenroll  # check G6 row of 'Enroll' excel sheet practiced by me
         # note : R adds 0.000000000 at the beginning. nothing to worry about this. when we do the sum(tenroll) that 0.000 is handled well


ageCounts <- table(traindata$Enrolls,traindata$Age)
ageCounts
ageCounts["Yes","<=30"]

ageCounts <- ageCounts/rowSums(ageCounts) # sum of rows ( rowSums)
ageCounts                                 # same as that in excel. cross check ( enroll-practiced-by-me.xlsx)


incomeCounts <- table(traindata$Enrolls,traindata$Income) 
incomeCounts
incomeCounts <- incomeCounts/rowSums(incomeCounts)
incomeCounts

jdCounts <- table(traindata$Enrolls,traindata$JobSatisfaction) 
jdCounts
jdCounts <- jdCounts/rowSums(jdCounts)
jdCounts

desireCounts <- table(traindata$Enrolls,traindata$Desire) 
desireCounts
desireCounts <- desireCounts/rowSums(desireCounts)
desireCounts


probYes <- ageCounts["Yes","<=30"]*incomeCounts["Yes","Medium"]*jdCounts["Yes","Yes"]*desireCounts["Yes","Fair"]*tenroll["Yes"]
probNo <- ageCounts["No","<=30"]*incomeCounts["No","Medium"]*jdCounts["No","Yes"]*desireCounts["No","Fair"]*tenroll["No"] 

# finding whether the person will enroll or not
if(probYes>probNo){
    print("Yes")
  }else{
  print("No")
}


# Alternate Method of above approach

pYes <- ageCounts["Yes",testdata[,c("Age")]]*incomeCounts["Yes",testdata[,c("Income")]]*jdCounts["Yes",testdata[,c("JobSatisfaction")]]*desireCounts["Yes",testdata[,c("Desire")]]*tenroll["Yes"]
pYes







