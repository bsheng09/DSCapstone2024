#________________________________Step1: Clean Data________________________________ 
install.packages("MASS")
install.packages("scales")
#Read Data 
companyData <-read.csv(file =  "/Users/yutongsheng/Downloads/companies.csv", header = TRUE)
nrow(companyData)
ncol(companyData)
convert_k <- function(x) {
  if(grepl("k", x)) {
    return(as.numeric(sub("k", "e3", x)))
  } else {
    return(as.numeric(x))
  }
}
#clean total reviews
table(companyData$Total_reviews) #no -- 
companyData$ReviewClean <- sapply(companyData$Total_reviews, convert_k)
print(companyData$ReviewClean) #total reviews cleaned
#clean average salary
table(companyData$Avg_salary) #found 2 --
companyData$Avg_salary[companyData$Avg_salary == "--"] <- NA #convert -- to NA
companyData$SalaryClean <- sapply(companyData$Avg_salary, convert_k)
print(companyData$SalaryClean)
#clean interviews taken
table(companyData$Interviews_taken) #found 219 --
companyData$Interviews_taken[companyData$Interviews_taken == "--"] <- NA #convert -- to NA
companyData$InterviewsClean <- sapply(companyData$Interviews_taken, convert_k)
print(companyData$InterviewsClean)
#clean Total jobs available 
table(companyData$Total_jobs_available) #found 3923 --
companyData$Total_jobs_available[companyData$Total_jobs_available == "--"] <- NA #convert -- to NA
companyData$TotalJobsClean <- sapply(companyData$Total_jobs_available, convert_k)
print(companyData$TotalJobsClean)
#clean total benefits 
table(companyData$Total_benefits) #found 79 --
companyData$Total_benefits[companyData$Total_benefits == "--"] <- NA #convert -- to NA
companyData$BenefitsClean <- sapply(companyData$Total_benefits, convert_k)
print(companyData$BenefitsClean)

##############################Step1: Clean Data Ends##############################

#________________________________Step2: Split into Training and Testing________________________________ 
#create a subset with all the variables I want to investigate and drop NAs 
companySub <- companyData[, c("Ratings", "InterviewsClean", "SalaryClean", "BenefitsClean")]
dim(companySub)
companySubClean <- na.omit(companySub)
dim(companySubClean)

#split into training and testing 80/20
set.seed(123)  # For reproducibility
# calculate the size of the training set (80/20)
n <- floor(0.8 * nrow(companySubClean))
indices <- sample(seq_len(nrow(companySubClean)), size = n)

#split the data into training and testing sets
companySubTraining <- companySubClean[indices, ]
companySubTesting <- companySubClean[-indices, ]


##############################Step2: Split into Training and Testing Ends##############################

#________________________________Step3: Data Processing and Initial Visualization________________________________ 
#---data processing--#
#log transformation
library(scales)
companySubTraining$RatingsLog <- log(companySubTraining$Ratings)
companySubTraining$SalaryLog <- log(companySubTraining$SalaryClean)
companySubTraining$InterviewsLog <- log(companySubTraining$InterviewsClean)
companySubTraining$BenefitsLog <- log(companySubTraining$BenefitsClean)

###jitter 
salary.jit<-companySubTraining$SalaryLog+rnorm(nrow(companySubTraining), mean=0, sd=0.01) # sd determines how much jittering
ratings.jit<-companySubTraining$RatingsLog+rnorm(nrow(companySubTraining), mean=0, sd=0.01)
plot(salary.jit, ratings.jit, col=alpha("orange",.25), 
     #main ="Relationship between Company Ratings and Average Salary", 
     xlab ="Average Salary (Log)", 
     ylab ="Company Ratings (Log)")
spout<-smooth.spline(companySubTraining$RatingsLog~companySubTraining$SalaryLog)
points(spout$x, spout$y, col="blue",type="l")

interview.jit<-companySubTraining$InterviewsLog+rnorm(nrow(companySubTraining), mean=0, sd=0.01) # sd determines how much jittering
ratings.jit<-companySubTraining$RatingsLog+rnorm(nrow(companySubTraining), mean=0, sd=0.01)
plot(interview.jit, ratings.jit, col=alpha("green",.25),
     #main ="Relationship between Company Ratings and Total Number of Interviews Taken", 
     xlab ="Interviews (Log)", 
     ylab ="Company Ratings (Log)")
spout<-smooth.spline(companySubTraining$RatingsLog~companySubTraining$InterviewsLog)
points(spout$x, spout$y, col="blue",type="l")

benefits.jit<-companySubTraining$BenefitsLog+rnorm(nrow(companySubTraining), mean=0, sd=0.01) # sd determines how much jittering
ratings.jit<-companySubTraining$RatingsLog+rnorm(nrow(companySubTraining), mean=0, sd=0.01)
plot(benefits.jit, ratings.jit, col=alpha("pink",.25),
     #main ="Relationship between Company Ratings and Total Number of Benefits", 
     xlab ="Benefits (Log)", 
     ylab ="Company Ratings (Log)")
spout<-smooth.spline(companySubTraining$RatingsLog~companySubTraining$BenefitsLog)
points(spout$x, spout$y, col="blue",type="l")

#________________________________Step4: Data Modeling________________________________ 

#---run simple linear/multiple predictor models with log data--- 
#ratings and average salary
reg_salaryLog<-lm(RatingsLog~SalaryLog, data= companySubTraining)
plot(companySubTraining$RatingsLog, companySubTraining$SalaryLog, 
       pch=18, col="orange",
       xlab="Company Ratings Log",
       ylab="Average Salary Log",
       main="Company Ratings versus Average Salary (Log)")
abline(reg_salaryLog$coefficients,lty=2)
summary(reg_salaryLog)
BIC(reg_salaryLog)
plot(reg_salaryLog$fitted.values,reg_salaryLog$resid, 
     pch=18, col="orange",
     xlab="Fitted Values",
     ylab="Residuals",
     main="Residuals versus Fitted Values Average Salary (Log)")
abline(0,0)
#ratings and interviews taken
reg_interviewsLog<-lm(RatingsLog~InterviewsLog, data= companySubTraining)
plot(companySubTraining$RatingsLog, companySubTraining$InterviewsLog, 
     pch=18, col="green3",
     xlab="Company Ratings Log",
     ylab="Interviews Taken Log",
     main="Company Ratings versus Interviews Taken (Log)")
abline(reg_interviewsLog,lty=2)
summary(reg_interviewsLog)
BIC(reg_interviewsLog)
plot(reg_interviewsLog$fitted.values,reg_interviewsLog$resid, 
     pch=18, col="green3",
     xlab="Fitted Values",
     ylab="Residuals",
     main="Residuals versus Fitted Values Interviews Taken (Log)")
abline(0,0)
#ratings and total benefits
reg_benefitsLog<-lm(RatingsLog~BenefitsLog, data= companySubTraining)
plot(companySubTraining$RatingsLog, companySubTraining$BenefitsLog, 
     pch=18, col="lightblue",
     xlab="Company Ratings Log",
     ylab="Total Benefits Log",
     main="Company Ratings versus Total Benefits (Log)")
abline(reg_benefitsLog,lty=2)
summary(reg_benefitsLog)
BIC(reg_benefitsLog)
plot(reg_benefitsLog$fitted.values,reg_benefitsLog$resid, 
     pch=18, col="lightblue",
     xlab="Fitted Values",
     ylab="Residuals",
     main="Residuals versus Fitted Values Total Benefits (Log)")
abline(0,0)

#run multiple predictor models 
#benefit+salary
lm_BenefitSalaryLog <- lm(RatingsLog ~ BenefitsLog+SalaryLog, data=companySubTraining)
summary(lm_BenefitSalaryLog)
BIC(lm_BenefitSalaryLog)
plot(lm_BenefitSalaryLog$fitted.values,lm_BenefitSalaryLog$resid, 
     pch=18, col="purple",
     xlab="Fitted Values",
     ylab="Residuals",
     main="Residuals versus Fitted Values for Benefit+Salary (Log)")
abline(0,0)
#benefit+interview
lm_BenefitInterviewLog <- lm(RatingsLog ~ BenefitsLog+InterviewsLog, data=companySubTraining)
summary(lm_BenefitInterviewLog)
BIC(lm_BenefitInterviewLog)
plot(lm_BenefitInterviewLog$fitted.values,lm_BenefitInterviewLog$resid, 
     pch=18, col="pink",
     xlab="Fitted Values",
     ylab="Residuals",
     main="Residuals versus Fitted Values for Benefit+Interview (Log)")
abline(0,0)
#interview and salary
lm_SalaryInterviewLog <- lm(RatingsLog ~ SalaryLog+InterviewsLog, data=companySubTraining)
summary(lm_SalaryInterviewLog)
BIC(lm_SalaryInterviewLog)
#benefit+salary+interview
lm_all3Log <- lm(RatingsLog ~ BenefitsLog+SalaryLog+InterviewsLog, data=companySubTraining)
summary(lm_all3Log)
BIC(lm_all3Log)
plot(lm_all3Log$fitted.values,lm_all3Log$resid, 
     pch=18, col="blue",
     xlab="Fitted Values",
     ylab="Residuals",
     main="Residuals versus Fitted Values for all three predictors (Log)")
abline(0,0)

fit.jit<-lm_all3Log$fitted.values+rnorm(nrow(companySubTraining), mean=0, sd=0.01) # sd determines how much jittering
res.jit<-lm_all3Log$resid+rnorm(nrow(companySubTraining), mean=0, sd=0.01)
plot(fit.jit, res.jit, col=alpha("cornflower blue",.25),
     xlab="Fitted Values",
     ylab="Residuals",
    )
spout<-smooth.spline(lm_all3Log$resid~lm_all3Log$fitted.values)
points(spout$x, spout$y, col="blue",type="l")


#Testing data based on Benefits+Salary Model
#companySubTraining$RatingsLog <- log(companySubTesting$Ratings)
companySubTesting$SalaryLog <- log(companySubTesting$SalaryClean)
companySubTesting$InterviewsLog <- log(companySubTesting$InterviewsClean)
companySubTesting$BenefitsLog <- log(companySubTesting$BenefitsClean)

# Predict using the model
company_ratings <- companySubTesting$Ratings

company_predict <- exp(predict(lm_all3Log, newdata = companySubTesting))
company_predict


# calculate Mean Absolute Error (MAE)
MAE <- mean(abs(company_ratings - company_predict))
# calculate Mean Squared Error (MSE)
MSE <- mean((company_ratings - company_predict)^2)







