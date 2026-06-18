#Load in heart data
heart_data <- read.csv("Framingham.csv")

#Create subset
heart_data_new <- heart_data[,c(2:5,7,9:10,13,28)]
heart_data_clean <- na.omit(heart_data_new)

#Describe character variables
heart_data_clean$sex_label <- ifelse(heart_data_clean$SEX == 1,"Male","Female")
heart_data_clean$cursmoke_label <- ifelse(heart_data_clean$CURSMOKE == 0,"Doesn't Smoke","Smokes")
heart_data_clean$diabetes_label <- ifelse(heart_data_clean$DIABETES == 0,"No Diabetes","Diabetes")
heart_data_clean$anychd_label <- ifelse(heart_data_clean$ANYCHD == 0,"No Coronary Heart Disease","Coronary Heart Disease")

#Check structure
dim(heart_data_clean)
str(heart_data_clean)
head(heart_data_clean)

#Part 2: Create table and plot for anychd_label
chd_table <- table(heart_data_clean$anychd_label)
chd_bar_plot <- barplot(chd_table,main="Coronary Heart Disease Data",xlab="Disease Status",ylab="Frequency",col="blue")

#Task 6: Visualizations for key variables
par(mfrow = c(2,5))

hist(heart_data_clean$AGE,main="Distribution of Age for Participants",xlab="Age",ylab = "Frequency", col="purple")
boxplot(heart_data_clean$AGE,main="Distribution of Age for Participants",ylab="Age",col="grey",horizontal = TRUE)

hist(heart_data_clean$TOTCHOL,main="Distribution of Total Cholesterol for Participants",xlab="Cholesterol",col="purple")
boxplot(heart_data_clean$TOTCHOL,main="Distribution of Total Cholesterol for Participants",ylab="Cholesterol",col="grey",horizontal = TRUE)

hist(heart_data_clean$SYSBP,main="Distribution of Systolic Blood Pressure for Participants",xlab="Blood Pressure",col="purple")
boxplot(heart_data_clean$SYSBP,main="Distribution of Systolic Blood Pressure for Participants",ylab="Blood Pressure",col="grey",horizontal = TRUE)

hist(heart_data_clean$BMI,main="Distribution of Body Mass Index for Participants",xlab="BMI",col="purple")
boxplot(heart_data_clean$BMI,main="Distribution of Body Mass Index for Participants",ylab="BMI",col="grey",horizontal = TRUE)

hist(heart_data_clean$GLUCOSE,main="Distribution of Glucose for Participants",xlab="Glucose",col="purple")
boxplot(heart_data_clean$GLUCOSE,main="Distribution of Glucose for Participants",ylab="Glucose",col="grey",horizontal = TRUE)

#Task 7: Side by Side boxplots for key variables
boxplot(AGE ~ heart_data_clean$anychd_label, data = heart_data_clean,main="Distribution of Age in anychd_label Dataset",ylab="Age",col="pink",horizontal = TRUE)
boxplot(TOTCHOL ~ heart_data_clean$anychd_label, data = heart_data_clean,main="Distribution of Cholesterol in anychd_label Dataset",ylab="Age",col="pink",horizontal = TRUE)
boxplot(SYSBP ~ heart_data_clean$anychd_label, data = heart_data_clean,main="Distribution of Blood Pressure in anychd_label Dataset",ylab="Age",col="pink",horizontal = TRUE)
boxplot(BMI ~ heart_data_clean$anychd_label, data = heart_data_clean,main="Distribution of BMI in anychd_label Dataset",ylab="Age",col="pink",horizontal = TRUE)
boxplot(GLUCOSE ~ heart_data_clean$anychd_label,data = heart_data_clean,main="Distribution of GLUCOSE in anychd_label Dataset",ylab="Age",col="pink",horizontal = TRUE)

#Task 8: Stacked bar charts to visualize anychd_label
sex_table <- table(heart_data_clean$sex_label,heart_data_clean$anychd_label)
cursmoke_table <- table(heart_data_clean$cursmoke_label,heart_data_clean$anychd_label)
diabetes_table <- table(heart_data_clean$diabetes_label,heart_data_clean$anychd_label)

prop_sex_table <- prop.table(sex_table,margin = 1)
prop_cursmoke_table <- prop.table(cursmoke_table,margin = 1)
prop_diabetes_table <- prop.table(diabetes_table,margin = 1)

barplot(t(prop_sex_table),main="Distribution of CHD between males and females(Blue has CHD, Green does not)",xlab="Sex",ylab="Proportion",col=c("blue","green"))
barplot(t(prop_cursmoke_table),main="Distribution of CHD between smokers and non-smokers(Blue has CHD, Green does not)",xlab="Smoking Status",ylab="Proportion",col=c("blue","green"))
barplot(t(prop_diabetes_table),main="Distribution of CHD between diabetes and no diabetes(Blue has CHD, Green does not)",xlab="Diabetes Status",ylab="Proportion",col=c("blue","green"))

#LIAM: Everything looks correct here except it might be worth fixing the xlab (Google has a good solution for this) for clarity. Otherwise, it all looks right?

#Task 9: Perform two sample t-test
t.test(AGE ~ anychd_label,data = heart_data_clean)
t.test(TOTCHOL ~ anychd_label,data = heart_data_clean)
t.test(SYSBP ~ anychd_label,data = heart_data_clean)
t.test(BMI ~ anychd_label,data = heart_data_clean)
t.test(GLUCOSE ~ anychd_label,data = heart_data_clean)

#Task 10: Perform chi-squared test of independence
chisq.test(diabetes_table)

#Task 11: Fit a simple LPM to predict the likelihood of heart disease
fit <- lm(ANYCHD ~ TOTCHOL, data = heart_data_clean)
summary(fit)

#Task 12: Fit a multiple LPM to predict anychd_label
multi_fit <-  lm(ANYCHD ~ AGE + SEX + TOTCHOL + SYSBP + BMI + CURSMOKE + DIABETES + GLUCOSE, data = heart_data_clean)
summary(multi_fit)
