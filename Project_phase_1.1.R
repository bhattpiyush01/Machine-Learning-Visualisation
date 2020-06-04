#all required library
library(ggplot2)
library(car)
library(dplyr)
library(lattice)
library(tidyr)
library(caret)
library(MASS)
library(broom)
library(ROCR)

theme_set(theme_minimal())
rm(list = ls())
options(scipen = 999)
# Read the data
bank_data <- read.csv("C:/Users/bhatt/Desktop/Resume And Imp Documents/RMIT University/Semester 3/Machine Learning/Phase-1/bank/bank-full.csv", 
                      sep = ";")
head(bank_data)

# Data Pre-Processing
bank_data <- subset(bank_data, bank_data$poutcome != "other")

bank_data$education <- plyr::revalue(bank_data$education, c("unknown" = "other"))
bank_data$job <- plyr::revalue(bank_data$job, c("unknown" = "other"))
# Check missing value in Numeric columns
num_var <- select_if(bank_data, is.numeric)
colSums(sapply(num_var, is.na))

# Check missing values in Categorical Columns
cat_var <- select_if(bank_data, is.factor)
colSums(sapply(cat_var, is.na))

# Summarize the numerical variables
summary(num_var)

# Summarize the categorical variables
summary(cat_var$poutcome)

# Explore the target variable
table(bank_data$y)

# Visualize the balance to check the outliers and remove them if any
outliers <- boxplot(bank_data$balance, horizontal = TRUE, plot = FALSE)$out
bank_data <- bank_data[-which(bank_data$balance %in% outliers),]
boxplot(bank_data$balance, horizontal = TRUE)

# Remove the column contact as it has no impact on target variable y
bank_data$contact <- NULL 
# Keep records which has call duration of more than 5 seconds
bank_data <- subset.data.frame(bank_data, bank_data$duration > 5)
# Drop the records for customer with education as other
bank_data <- subset(bank_data, bank_data$education != "other")
cat_var <- select_if(bank_data, is.factor)
summary(cat_var)

# Rename the y variable as target
names(bank_data)[length(bank_data)] <- "Target"

# Data Exploration
# Distribution of age
p <- ggplot(bank_data, aes(x = age))
p + geom_bar(color = "white",
               fill  = "blue") + theme_minimal() + labs(title = "Distribution of Age")


# Distribution of Balance
hist(bank_data$balance, fill = "red", col = "red", 
                        main = "Distribution of Balance",
                        xlab = "Balance(in Euro)")

# Relationship between age and balance
d <- ggplot(bank_data, aes(x = age, y = balance)) 
d +  geom_point(color = "blue") + labs(title = "Relationship b/w Age and balance") + geom_smooth(method = "lm", se = F)

# Visualization of duration and campaign
boxplot(bank_data$duration, main = "Distribution of Duration of call",
                            ylab = "Duration in Seconds")
boxplot(bank_data$campaign, main = "Distribution of Campaign")

# Relationship b/w duration and campaign with response rate

ggplot(bank_data, aes(x = bank_data$duration, y = bank_data$campaign)) + 
      geom_point(aes(col = Target)) + labs(title = "Relationship b/w Duration and no of call",
                                                x = "Duration of call (in Seconds)",
                                                y = "No of calls made during Campaign")

# Correlation between all the numerical varibles with target variable
bank_data$Target <- ifelse(bank_data$Target == "yes", 1,0)
sub_data <- bank_data[,c("age","balance","duration", "campaign", "pdays","previous", "Target")]
library(psych)
pairs.panels(sub_data, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

# From the correlation matrix, duration has highest impact on Target variable
# whereas campaign( i.e. no of calls made during the campaign) has negative impact
# i.e. the more calls customers receives, more chances that the customer does not
# subscribe
range(bank_data$balance)
# Visualise target variable with respect to different different predictors
ggplot(data = bank_data, aes(x = age, y = bank_data$balance, fill = Target)) + geom_bar(stat = "identity", position = position_dodge()) +
   labs(title = "Customer Response on Age and Balance",
        y = "Balance(in Euro)") + scale_color_manual(values = c("#999999","##56B4E9"))
bank_data$Target <- ifelse(bank_data$Target == 1, "Subscribed","Not Subscribed")
# Boxplot between numerical variables
e <- ggplot(data = bank_data, aes(x = education, y = balance, fill = Target))
e + geom_boxplot() + labs(y = "Balance(in Euro)",
                          x = "Education",
                          title = "Analysis of Subscription ",
                          subtitle = "based on Education, balance and marital status") + 
  theme(legend.title = element_blank()) + facet_wrap( ~ marital)
# Barplot between loan and balacne with respect to target variable.
f <- ggplot(bank_data, aes(x = loan, y = balance, fill = Target))
f + geom_bar(stat = "identity", position = position_dodge()) + 
   labs(title = "Subscription rate",
         subtitle = "Based on Job, Balance, and loan") + facet_wrap(~ job)
