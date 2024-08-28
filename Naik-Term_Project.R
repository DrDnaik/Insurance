#MET CS 555 Term Project
#Deepa Naik

library(tidyverse)
library(corrplot)
library(ggcorrplot)
library(gridExtra)
library(car)


# Command to set working directory to the specified path 
setwd("C:/Deepa/Boston University/Assignments/MET CS 555/CS 555 Term Project Report")

# This command will read the data into a special data type named "data frame".
insurance_data<-read.csv("insurance_data.csv",header=TRUE,na.strings=c("","NA"))

#check NA values in the variables
colSums(is.na(insurance_data))

#remove NA values rows
insurance<-na.omit(insurance_data)

#check duplicates
sum(duplicated(insurance))

#check outliers
outlier_iqr <- function(x) {
  iqr <- IQR(x, na.rm = TRUE, type = 7)
  q <- quantile(x)
  upper_bound <- q[4] + (iqr * 1.5)
  lower_bound <- q[2] - (iqr * 1.5)
  outliers <- which((x > upper_bound) | (x < lower_bound))
  return(outliers)
}

outlier_age<-insurance[outlier_iqr(insurance$age),"age"]
outlier_bmi<-insurance[outlier_iqr(insurance$bmi),"bmi"]
outlier_bp<-insurance[outlier_iqr(insurance$bloodpressure),"bloodpressure"]
outlier_children <- insurance[outlier_iqr(insurance$children), "children"]

#number of outliers in data
length(outlier_age)
length(outlier_bmi)
length(outlier_bp)
length(outlier_children) 

#Boxplots for age,bmi and blood pressure
par(mfrow = c(2 ,2))
boxplot(insurance$age,main="Boxplot of age",xlab="Age",col="blue")
boxplot(insurance$bmi,main="Boxplot of bmi",xlab="BMI",col="red")
boxplot(insurance$bloodpressure,main="Boxplot of BP",xlab="Blood pressure",col="pink")
par(mfrow = c(1 ,1))

#summary of statistical values 
summary(insurance)

# Convert categorical variables to factors 
insurance$region<-as.factor(insurance$region)
insurance$gender <- as.factor(insurance$gender)
insurance$diabetic <- as.factor(insurance$diabetic)
insurance$smoker <- as.factor(insurance$smoker)

#frequency of categorical variables
table(insurance$gender)
table(insurance$region)
table(insurance$diabetic)
table(insurance$smoker)


#Histogram showing distribution of age,bmi,bp
# Set up the plotting area for a 2x2 grid
par(mfrow = c(2, 2))
# Create histograms for each variable
hist(insurance$age, 
     main = "Age Distribution", 
     xlab = "Age", 
     col = "pink", 
     border = "black")

hist(insurance$bmi, 
     main = "BMI Distribution", 
     xlab = "BMI", 
     col = "blue", 
     border = "black")

hist(insurance$bloodpressure, 
     main = "Blood Pressure Distribution", 
     xlab = "Blood Pressure", 
     col = "purple", 
     border = "black")
par(mfrow = c(1, 1))

#Boxplot for gender,children,region
#insurance claim gender wise
z<-ggplot(insurance, aes(x=gender, y=claim,fill=gender)) +
  geom_boxplot(outlier.color="red")   +
  theme_minimal()+
  scale_x_discrete(labels = c("Female", "Male")) +
  theme(legend.position="right")+
  ggtitle("Gender wise claim distribution ")+
  theme(plot.title = element_text(hjust=0.5))+
  labs(x = "Gender", y = "claim") 
z

#Region wise claim
n<-ggplot(insurance, aes(x=region, y=claim,fill=region)) +
  geom_boxplot(outlier.color="red")   +
  theme_minimal()+
  theme(legend.position="right")+
  ggtitle(" Region wise claim distribution ")+
  theme(plot.title = element_text(hjust=0.5))+
  labs(x = "region", y = "claim") 
n

#insurance Claim children wise
z1<-ggplot(insurance, aes(x = as.factor(children), y = claim, fill = as.factor(children))) +
  geom_boxplot(outlier.color = "red") +
  theme_minimal() +
  theme(legend.position = "right") +
  ggtitle(" Claims by Number of Children") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Number of Children", y = "Claim",fill='children')

z1

grid.arrange(z, n, z1, nrow = 2, ncol = 2)

#coorelation matrix-age bmi,blood pressure,claim
corr <- cor(insurance[, c(3, 5, 6,11)])
corr

# Create the correlation plot
corrplot(corr, 
         method = "color",   # Use color to represent correlation values
         type = "full",      # Show full correlation matrix
         tl.col = "black",   # Color of text labels
         tl.srt = 45,        # Angle of text labels
         addCoef.col = "red" # Add correlation coefficients in red
)


#correlation test for age, bmi and blood pressure
r<-cor.test(x =insurance$age,y = insurance$claim)
r

r1<-cor.test(x =insurance$bmi,y = insurance$claim)
r1

r2<-cor.test(x =insurance$bloodpressure,y = insurance$claim)
r2


#Multiple Regression model
#create linear model m
m<-lm(claim~age+bmi+bloodpressure+children+diabetic+region+smoker+gender,data=insurance)
m

summary(m) 


#Multiple Regression model with significant variables
m1<-lm(claim~bmi+bloodpressure+smoker+region+children,data=insurance)
m1

summary(m1)

#Assessing fit/Regression Diagnostics-Multiple linear Regression model m1
#Residual plots - Checking constant variance and linearity

par(mfrow = c(2 ,2))
plot(m1)
par(mfrow = c(1 ,1))

hist(resid(m1), 
     main = "Histogram of Residuals", 
     xlab = "Residuals", 
     col = "pink", 
     border = "black")


#influence points
cooks.dist <- cooks.distance(m1)
influence_points<-which(cooks.dist > (4/(nrow(insurance)-length(coefficients(m1)-1))))

#Cook's plot'
plot(m1, which=4, cook.levels=influence_points) #?? less points on plot

#Anova
model_a<-aov(claim~region,data=insurance)
summary(model_a)

#post hoc comparison
#tukey
TukeyHSD(model_a)


#Ancova
model_ac<-Anova(lm(claim~region +bloodpressure,data=insurance), type=3)
model_ac
