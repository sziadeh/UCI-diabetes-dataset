# import packages

library(dplyr)
library(ggplot2)

#load diabetic data
data <- read.csv("/Users/samiahziadeh/uci_diabetes_project/data/diabetic_data.csv")
head(data) #column names present
str(data) #looking at how the columns are characterized 
data[data == "?"] <- NA #convert "?" to NAs

#convert appropriate columns into factors using dplyr lib, 2 ways to do this
data$diag_1 <- as.factor(data$diag_1)
data$diag_2 <- as.factor(data$diag_2)
data$diag_3 <- as.factor(data$diag_3)
data <- data %>%
  mutate(across(race:admission_source_id, as.factor))
data$medical_specialty <- as.factor(data$medical_specialty)
data$payer_code <- as.factor(data$payer_code)
data <- data %>%
  mutate(across(max_glu_serum:readmitted, as.factor))
str(data) ## this shows that the correct columns are factors

# identify the amount of rows that have NAs in them
nrow(data[is.na(data$race) | is.na(data$readmitted),])
colSums(is.na(data)) #show how many missing values in each column
# not sure what to do with the NA values here since it seems like there's a lot of them
## first calcualte missingness rate for each feature

features <- c("race", "gender", "age", "weight", "time_in_hospital",
              "num_medications", "max_glu_serum", "A1Cresult", 
              "diabetesMed", "readmitted")

# filter to only those features that exist in the data
features <- features[features %in% colnames(data)]

missing_rates <- colMeans(is.na(data[ , features])) * 100
print(round(missing_rates, 2))
# race= 2.23%, weight=96.86%
# over 90% is NAs so dropping weight column
data$weight <- NULL  
## preserving race column and converting NA to "unknown" 
# adding "Unknown" as another possible value
levels(data$race) <- c(levels(data$race), "Unknown")
# replace NAs with "Unknown"
data$race[is.na(data$race)] <- "Unknown"

# frequency counts for categorical features
table(data$race)
table(data$gender)
table(data$age)
# remove table(data$weight)
table(data$max_glu_serum)
table(data$A1Cresult)
table(data$diabetesMed)
table(data$readmitted)

# to visualize 
ggplot(data, aes(x = race)) + geom_bar()
ggplot(data, aes(x = gender)) + geom_bar()
ggplot(data, aes(x = age)) + geom_bar()
# remove ggplot(data, aes(x = weight)) + geom_bar()
ggplot(data, aes(x = max_glu_serum)) + geom_bar()
ggplot(data, aes(x = A1Cresult)) + geom_bar()
ggplot(data, aes(x = diabetesMed)) + geom_bar()
ggplot(data, aes(x = readmitted)) + geom_bar()


## Evaluate the colinearity of max_glu_serum and A1Cresult
#checking data type
str(data$max_glu_serum)
str(data$A1Cresult)
#cross-tabulation
table(data$max_glu_serum, data$A1Cresult)

ggplot(data, aes(x = max_glu_serum, fill = A1Cresult)) +
  geom_bar(position = "fill") +
  labs(y = "Proportion", title = "Relationship Between Max Glucose Serum and A1C Result") +
  theme_minimal()
chisq.test(table(data$max_glu_serum, data$A1Cresult)) #statistical test

## Evaluate the colinearity of A1Cresult and readmitted in <30d
#checking data type
str(data$readmitted)
# create binary variable 
data$readmit_30 <- ifelse(data$readmitted == "<30", "Yes", "No")
data$readmit_30 <- as.factor(data$readmit_30)
# how often each combination occurs 
table(data$A1Cresult, data$readmit_30)

chisq.test(table(data$A1Cresult, data$readmit_30))

ggplot(data, aes(x = A1Cresult, fill = readmit_30)) +
  geom_bar(position = "fill") +
  labs(y = "Proportion", title = "A1C Result vs. Readmission in <30 Days") +
  theme_minimal()

