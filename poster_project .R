library(tidyverse)

# READ IN THE DATA ####
Our_Subset <- read_csv("poster survey data.csv")

# DELETE THE INVALID VALUES ####
Our_Subset <- Our_Subset[c(-1, -86, -89),]


# ADJUST THE LIKERT SCALE QUESTIONS ####
Our_Subset$Arts_Study <- recode(Our_Subset$Arts_Study, 
                               "Strongly disagree" = 7, "Disagree" = 6, 
                               "Somewhat disagree" = 5,"Neither agree nor disagree" = 4, 
                               "Somewhat agree" = 3, "Agree" = 2, "Strongly agree" = 1)

Our_Subset$Art_Value <- recode(Our_Subset$Art_Value, 
                                "Strongly disagree" = 7, "Disagree" = 6, 
                                "Somewhat disagree" = 5,"Neither agree nor disagree" = 4, 
                                "Somewhat agree" = 3, "Agree" = 2, "Strongly agree" = 1)

Our_Subset$Arts_Participation <- recode(Our_Subset$Arts_Participation, 
                                "Strongly disagree" = 1, "Disagree" = 2, 
                                "Somewhat disagree" = 3,"Neither agree nor disagree" = 4, 
                                "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7)

Our_Subset$Art_Consumption <- recode(Our_Subset$Art_Consumption, 
                                "Strongly disagree" = 1, "Disagree" = 2, 
                                "Somewhat disagree" = 3,"Neither agree nor disagree" = 4, 
                                "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7)

Our_Subset$Art_Useful <- recode(Our_Subset$Art_Useful, 
                                "Strongly disagree" = 1, "Disagree" = 2, 
                                "Somewhat disagree" = 3,"Neither agree nor disagree" = 4, 
                                "Somewhat agree" = 5, "Agree" = 6, "Strongly agree" = 7)

# CREATING / ALTERING VARIABLES ####

Our_Subset$Overall <- (Our_Subset$Art_Useful + Our_Subset$Art_Consumption + 
  Our_Subset$Arts_Participation + Our_Subset$Art_Value + Our_Subset$Arts_Study) / 5

Our_Subset$Active <- (Our_Subset$Art_Consumption + Our_Subset$Arts_Participation) / 2

Our_Subset$Passive <- (Our_Subset$Art_Useful + Our_Subset$Art_Value + 
                         Our_Subset$Arts_Study) / 3

# ORDERING INCOME VARIABLE AND MAKING IT  A FACTOR 

Our_Subset$Income <- factor(Our_Subset$Income, 
                            level = c("<$50,000", "$50,000 - $100,000", 
                                      "$100,000 - $200,000", 
                                      "$200,000 - $250,000", ">$250,000"))


# VISUALIZING ####

ggplot(data = Our_Subset) +
  geom_boxplot(mapping = aes(Income, Overall, color = Income)) + 
  ggtitle("How Appreciation of the Arts Varies with Family Income") + 
  xlab("Family Income Group") + 
  ylab("Overall Appreciation Score") + 
  theme(axis.text.x = element_blank()) 

ggplot(data = Our_Subset) +
  geom_boxplot(aes(Income, Active, color = Income)) + 
  ggtitle("How Active Appreciation of the Arts Varies with Family Income") + 
  xlab("Income Group") + 
  ylab("Active Appreciation Score") + 
  theme(axis.text.x = element_blank())

ggplot(data = Our_Subset) +
  geom_boxplot(aes(Income, Passive, color = Income)) + 
  ggtitle("How Passive Appreciation of the Arts Varies with Family Income") + 
  xlab("Income Group") + 
  ylab("Passive Appreciation Score") + 
  theme(axis.text.x = element_blank())

# ANOVA ####

ANOVA1 <- aov(Overall ~ Income, data = Our_Subset)
summary(ANOVA1)
# Insignificant: F(4, 101) = 1.05, p = .385

ANOVA2 <- aov(Active ~ Income, data = Our_Subset)
summary(ANOVA2)
# Insignificant: F(4, 101) = 1.63, p = .174

ANOVA3 <- aov(Passive ~ Income, data = Our_Subset)
summary(ANOVA3)
# Insignificant: F(4, 101) = 0.75, p = .558

# REMOVING OUTLIERS ####

Overall_No_Outlier <- Our_Subset[-46,]
ANOVA4 <- aov(Overall ~ Income, data = Overall_No_Outlier)
summary(ANOVA4)
# Insignificant: F(4, 100) = 1.25, p = .296

Active_No_Outlier <- Our_Subset[-58,]
ANOVA5 <- aov(Active ~ Income, data = Active_No_Outlier)
summary(ANOVA5)
# Insignificant: F(4, 100) = 1.43, p = .229

Passive_No_Outlier <- Our_Subset[-c(34, 46, 56),]
ANOVA6 <- aov(Passive ~ Income, data = Passive_No_Outlier)
summary(ANOVA6)
#Insignificant: F(4, 98) = 1.63, p = .172

# CONCLUSION ####

# _____ No statistically significant differences in appreciation for art #### 
#       between income groups; not even when segmented for active and 
#       passive appreciation

