if (!require("pacman")) install.packages("pacman")
pacman::p_load(doParallel, ggplot2)

cores <- parallel::detectCores(logical = FALSE)
registerDoParallel(cores = cores)

Smoke_path <- 'https://raw.githubusercontent.com/Dforouzanfar/Statistical_Learning/master/Dataset/smoking.csv'
smoke <- read.csv(Smoke_path)

# EDA
str(smoke)
summary(smoke)

## gender
unique(smoke$gender)
smoke$gender <- as.factor(smoke$gender)

## age
unique(smoke$age)
any(is.na(smoke$age))

## marital_status
unique(smoke$marital_status)
any(is.na(smoke$marital_status))
smoke$marital_status <- factor(smoke$marital_status, levels = c("Single", "Married", "Divorced", "Widowed", "Separated"), labels = c("Single", "Married", "Separated", "Separated", "Separated"))

## highest_qualification
unique(smoke$highest_qualification)
any(is.na(smoke$highest_qualification))
smoke$highest_qualification <- factor(smoke$highest_qualification, levels = c("No Qualification", "Degree", "GCSE/O Level", "GCSE/CSE", "Other/Sub Degree", "Higher/Sub Degree", "ONC/BTEC", "A Levels"), labels = c("Without", "University", "GCSE", "GCSE", "Sub Degree", "Sub Degree", "ONC", "A Levels"))

## nationality
unique(smoke$nationality)
row_index <- smoke$nationality %in% c("Refused", "Unknown")
smoke$nationality[row_index] <- NA
row_index <- smoke$nationality %in% ("Irish")
smoke$nationality[row_index] <- "Other"
any(is.na(smoke$nationality))
sum(is.na(smoke$nationality))
smoke <- smoke[complete.cases(smoke$nationality), ]

smoke$nationality <- as.factor(smoke$nationality)

## ethnicity
unique(smoke$ethnicity)
(tes <- table(smoke$ethnicity, smoke$smoke))
smoke <- subset(smoke, select = -ethnicity)

## gross_income
unique(smoke$gross_income)
row_index <- smoke$gross_income %in% c("Refused", "Unknown")
smoke$gross_income[row_index] <- NA
any(is.na(smoke$gross_income))
sum(is.na(smoke$gross_income))
smoke <- smoke[complete.cases(smoke$gross_income), ]

smoke$gross_income <- factor(smoke$gross_income, levels = c("2,600 to 5,200", "Under 2,600", "28,600 to 36,400", "10,400 to 15,600", "15,600 to 20,800", "Above 36,400", "5,200 to 10,400", "20,800 to 28,600"), labels = c("Low", "Low", "High", "Moderate", "Moderate", "High", "Low", "Moderate"))

## region
unique(smoke$region)
any(is.na(smoke$region))
smoke$region <- as.factor(smoke$region)

## smoke
unique(smoke$smoke)
any(is.na(smoke$smoke))
smoke$smoke <- as.factor(smoke$smoke)

## amt_weekends
unique(smoke$amt_weekends)
any(is.na(smoke$amt_weekends))
sum(is.na(smoke$amt_weekends))
smoke <- subset(smoke, select = -amt_weekends)

## amt_weekdays
unique(smoke$amt_weekdays)
any(is.na(smoke$amt_weekdays))
sum(is.na(smoke$amt_weekdays))
smoke <- subset(smoke, select = -amt_weekdays)

## type
unique(smoke$type)
row_index <- smoke$type %in% ("")
smoke$type[row_index] <- NA
any(is.na(smoke$type))
sum(is.na(smoke$type))
smoke <- subset(smoke, select = -type)

# Visualization
## gender
ggplot(smoke, aes(x = gender, fill = factor(gender))) +
  geom_bar(stat = "count", position = "dodge") +
  ggtitle("Distribution of gender") +
  theme_classic()

tgs <- table(smoke$gender, smoke$smoke)
barplot(tgs, 
        beside = TRUE,
        legend = rownames(tgs),
        xlab = "Smoke", ylab = "Frequency",
        main = "Gender vs Smoke"
)

## age
hist(smoke$age,
     xlab = "Age",
     ylab = "Frequency",
     main = "Histogram of Age",
     col = "darkblue",
     border = "white"
)

ggplot(data = smoke, aes(x = smoke, y = age, group = smoke), fig(10,4)) +
  geom_boxplot() +
  theme(plot.caption = element_text(hjust = 0, size = 24, face = "bold"))

# Outlier
smokers <- smoke[smoke$smoke == "Yes", ]
oldest_smoker <- smokers[which.max(smokers$age), ]

smoke <- smoke[!(smoke$smoke == "Yes" & smoke$age == max(smoke[smoke$smoke == "Yes", ]$age)), ]

## marital_status
ggplot(smoke, aes(x = marital_status, fill = factor(marital_status))) +
  geom_bar(stat = "count", position = "dodge") +
  ggtitle("Distribution of marital_status") +
  theme_classic()

tms <- table(smoke$marital_status, smoke$smoke)
barplot(tms, 
        beside = TRUE,
        legend = rownames(tms),
        xlab = "Smoke", ylab = "Frequency",
        main = "Marital status vs Smoke"
)

## highest_qualification
ggplot(smoke, aes(x = highest_qualification, fill = factor(highest_qualification))) +
  geom_bar(stat = "count", position = "dodge") +
  ggtitle("Distribution of highest_qualification") +
  theme_classic()

ths <- table(smoke$highest_qualification, smoke$smoke)
barplot(ths, 
        beside = TRUE,
        legend = rownames(ths),
        xlab = "Smoke", ylab = "Frequency",
        main = "Highest qualification vs Smoke"
)

## nationality
ggplot(smoke, aes(x = nationality, fill = factor(nationality))) +
  geom_bar(stat = "count", position = "dodge") +
  ggtitle("Distribution of nationality") +
  theme_classic()

tns <- table(smoke$nationality, smoke$smoke)
barplot(tns, 
        beside = TRUE,
        legend = rownames(tns),
        xlab = "Smoke", ylab = "Frequency",
        main = "Nationality vs Smoke"
)

## gross_income
ggplot(smoke, aes(x = gross_income, fill = factor(gross_income))) +
  geom_bar(stat = "count", position = "dodge") +
  ggtitle("Distribution of Gross income") +
  theme_classic()

tgs <- table(smoke$gross_income, smoke$smoke)
barplot(tgs, 
        beside = TRUE,
        legend = rownames(tgs),
        xlab = "Smoke", ylab = "Frequency",
        main = "Gross income vs Smoke"
)

## region
ggplot(smoke, aes(x = region, fill = factor(region))) +
  geom_bar(stat = "count", position = "dodge") +
  ggtitle("Distribution of region") +
  theme_classic()

trs <- table(smoke$region, smoke$smoke)
barplot(trs, 
        beside = TRUE,
        legend = rownames(trs),
        xlab = "Smoke", ylab = "Frequency",
        main = "Region vs Smoke"
)

## smoke
table(smoke$smoke)
ggplot(smoke, aes(x = smoke, fill = factor(smoke))) +
  geom_bar(stat = "count", position = "dodge") +
  ggtitle("Distribution of smoke") +
  theme_classic()

# write.csv(smoke, "UK Smoking Data/Dataset/Preprocessed.csv", row.names = FALSE)
