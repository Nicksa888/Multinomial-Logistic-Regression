#########################
#########################
#### Clear Workspace ####
#########################
#########################

rm(list = ls()) 
# clear global environment to remove all loaded data sets, functions and so on.

###################
###################
#### Libraries ####
###################
###################

library(easypackages) # enables the libraries function
suppressPackageStartupMessages(
  libraries("Hmisc", # enables loading of SAS software files
            "tidyverse",
            "tableone", # checking bivariate statistical associations
            "nnet", # for multinomial modelling
            "mlogit", # enables hmftest() function
            "reshape2", # for melt() function
            "jmv", # for frequency table
            "lmtest" # for likelihood ratio tests
  ))

###############################
###############################
#### Set Working Directory ####
###############################
###############################

setwd("C:/R Portfolio/Multinomial and Ordinal Logistic Regression")

stem <- Hmisc::sasxport.get(file = "stem-nsf-2017-ch11.xpt")
str(stem)
glimpse(stem)
summary(stem)

# make a subset
stem.cleaned <- stem %>%
  select(n2ocprmg, satadv, satsal, satsoc, gender, age)

# check the new data set
summary(object = stem.cleaned)

# Use the descriptive function to get the descriptive data
descriptives(stem.cleaned, vars = vars(n2ocprmg, satadv, satsal, satsoc, gender, age), freq = TRUE)

# Write a function to recode
RecSatis <- function(x){
  return(recode(x,
                "1" = "Very satisfied",
                "2" = "Somewhat satisfied",
                "3" = "Somewhat dissatisfied",
                "4" = "Very dissatisfied",
                "L" = NA_character_))
}         

# recode and rename
stem.cleaned <- stem %>%
  select(n2ocprmg, satadv, satsal, satsoc, gender, age) %>%
  mutate(job.cat = recode(.x = n2ocprmg,
                          "1" = "CS, Math, Eng",
                          "2" = "Other Sciences",
                          "3" = "Other Sciences",
                          "4" = "Other Sciences",
                          "5" = "CS, Math, Eng",
                          "6" = "CS, Math, Eng",
                          "7" = "Nonscience",
                          "8" = NA_character_)) %>%
  mutate(satis.advance = RecSatis(x = satadv)) %>%
  mutate(satis.salary = RecSatis(x = satsal)) %>%
  mutate(satis.contrib = RecSatis(x = satsoc)) %>%
  mutate(sex = recode(.x = gender, "M" = "Male", "F"= "Female")) %>%
  mutate(sex = fct_relevel(.f = sex, c("Male", "Female"))) %>%
  mutate(age = as.numeric(x = age)) %>%
  select(-n2ocprmg, -satadv, -satsal, -satsoc, -gender)

str(stem.cleaned)

# set a seed value to take a sample
set.seed(seed = 143)

# take a sample of 1500 cases
# 500 from each job.cat category
stem.samp <- stem.cleaned %>%
  drop_na(job.cat) %>%
  group_by(job.cat) %>%
  sample_n(size = 500)

# check work
summary(stem.samp)

# set a seed
set.seed(seed = 143)

# take 200 from each job.cat
stem.samp.200 <- stem.cleaned %>%
  group_by(job.cat) %>%
  sample_n(size = 200)

summary(stem.samp.200)

# take 200 from each job.cat
# subset first to remove NA from job.cat
set.seed(seed = 143)

stem.samp.200.noNA <- stem.cleaned %>%
  drop_na(job.cat) %>%
  group_by(job.cat) %>%
  sample_n(size = 200)

summary(stem.samp.200.noNA)

# sample 10% of each job.cat group
set.seed(seed = 143)
stem.samp.perc <- stem.cleaned %>%
  drop_na(job.cat) %>%
  group_by(job.cat) %>%
  sample_frac(size = .1)

summary(stem.samp.perc)

# plotting distribution of sex within job type 
stem.samp %>%
  ggplot(aes(x = sex, group = job.cat, y = ..prop..)) +
  geom_bar(fill = "#7463AC") +
  theme_classic() +
  labs(y = "Percent within job category", x = "Sex") +
  facet_grid(cols = vars(job.cat)) +
  scale_y_continuous(labels = scales::percent)

# plotting distribution of job type by sex 
stem.samp %>%
  ggplot(aes(x = job.cat, y = ..prop.., group = sex)) +
  geom_bar(fill = "#7463AC") +
  theme_classic() +
  labs(y = "Percent within sex category", x = "Job category") +
  facet_grid(cols = vars(sex)) +
  scale_y_continuous(labels = scales::percent)

# plotting distribution of job type and age 
stem.samp %>%
  ggplot(aes(y = age, x = job.cat)) +
  geom_jitter(aes(color = job.cat), alpha = .6) +
  geom_boxplot(aes(fill = job.cat), alpha = .4) +
  scale_fill_manual(values = c("dodgerblue2","#7463AC", "gray40"), guide =
                      "none") +
  scale_color_manual(values = c("dodgerblue2","#7463AC", "gray40"), guide =
                       "none") +
  theme_classic() + labs(x = "Job type", y = "Age in years")

# plotting distribution of job type, age, and sex 
stem.samp %>%
  ggplot(aes(y = age, x = job.cat, fill = sex)) +
  geom_jitter(aes(color = sex), alpha = .6) +
  geom_boxplot(aes (fill = sex), alpha = .4) +
  scale_fill_manual(values = c("gray", "#7463AC"), name = "Sex") +
  scale_color_manual(values = c("gray", "#7463AC"), guide = "none") +
  theme_classic() +
  labs(x = "Job type", y = "Age in years")

# plotting distribution of job type, sex, and age 
stem.samp %>%
  ggplot(aes(y = age, x = job.cat)) +
  geom_jitter(aes(color = sex), alpha = .6) +
  geom_boxplot(aes (fill = sex), alpha = .4) +
  scale_fill_manual(values = c("gray", "#7463AC"), guide = "none") +
  scale_color_manual(values = c("gray", "#7463AC"), guide = "none") +
  theme_classic() + labs(x = "Job type", y = "Age in years") +
  facet_grid(cols = vars(sex))

# plotting distribution of age 
stem.samp %>%
  ggplot(aes(x = age)) +
  geom_histogram(fill = "#7463AC", color = "white") +
  theme_classic() +
  labs(x = "Age in years", y = "Number of observations") +
  facet_grid(cols = vars(job.cat))

# The histograms are not normally distributed, so we can add nonnormal = age in the CreateTableOne() function, in order for kruskal wallis bivariate test to be used to compare age across the three groups rather than by ANOVA.

# make a table of statistics to examine job.cat
table.desc <- CreateTableOne(data = stem.samp, strata = 'job.cat',
                             vars = c('sex', 'age'))

print(table.desc, showAllLevels = TRUE, nonnormal = 'age')

# Since there is a significant Chi-squared test, we can obtain standardized residuals as a follow up.
sex.jobcat.chi <- chisq.test(x = stem.samp$sex,
                             y = stem.samp$job.cat)
sex.jobcat.chi$residuals
sex.jobcat.chi

# The Kruskal Wallis test is significant. We can follow up to learn where the difference is, since this is also an omnibus test
age.jobcat.dunn <- dunn.test::dunn.test(x = stem.samp$age, 
                                        g = stem.samp$job.cat,
                                        method = "bonferroni")

# A Chi-Squared test showed a significant association between sex and job category
# A Kruskal-Wallis test showed that age also differed between the job categories (KW(2) = 11.35, p = 0.003).

# get reference groups for job.cat and sex
levels(stem.samp$job.cat)
levels(stem.samp$sex)

# estimate the model and print its summary
job.type.mod <- multinom(job.cat ~ age + sex + age*sex,
                         stem.samp,
                         model = TRUE)

summary(job.type.mod)

# multinomial null model
job.type.mod.null <- multinom(job.cat ~ 1,
                              stem.samp,
                              model = TRUE)

summary(job.type.mod.null)

# get the job model chi-squared
job.chisq <- job.type.mod.null$deviance - job.type.mod$deviance

# get the degrees of freedom for chi-squared
job.df <- length(summary(job.type.mod)$coefficients) - 
  length(summary(job.type.mod.null)$coefficients)

# get the p-value for chi-squared
job.p <- pchisq(q = job.chisq, df = job.df, lower.tail = FALSE) # larger chi square values are found in upper tail only or right hand tail

# put together into a vector and round to 3 decimal places
modelsig <- round(x = c(job.chisq, job.df, job.p), 3)

# add names to the vector
names(x = modelsig) <- c("Chi-squared", "df", "p")

# print the vector
modelsig

# print first six rows of fitted values
head(job.type.mod$fitted.values) # indicates the predicted probability for each row for each predicted value

# Alternatively, fitted() can be used on the model object
fitted(job.type.mod)

# Test the goodness of fit
chisq.test(stem.samp$job.cat,predict(job.type.mod))

# We can examine the changes in predicted probability associated with one variable. To do this, we can create small datasets varying one variable while holding the other constant. We will first do this holding age at its mean and examining the predicted probabilities for each level of satis.salary.

# estimate the model and print its summary
job.type.mod_t <- multinom(job.cat ~ age + satis.salary,
                         stem.samp,
                         model = TRUE)

summary(job.type.mod_t)
dses <- data.frame(satis.salary = c("Very satisfied", "Somewhat satisfied", "Somewhat dissatisfied", "Very dissatisfied"), age = mean(stem.samp$age))
predict(job.type.mod_t, newdata = dses, "probs")

# Another way to understand the model using the predicted probabilities is to look at the averaged predicted probabilities for different values of the continuous predictor variable age within each level of satis.salary.

dwrite <- data.frame(satis.salary = rep(c("Very satisfied", "Somewhat satisfied", "Somewhat dissatisfied", "Very dissatisfied"), each = 41), 
                     age = rep(c(30:70), 4))

## store the predicted probabilities for each value of satis.salary and age
pp.write <- cbind(dwrite, predict(job.type.mod_t, newdata = dwrite, type = "probs", se = TRUE))

## calculate the mean probabilities within each level of satis.salary
by(pp.write[, 3:5], pp.write$satis.salary, colMeans)

# Sometimes, a couple of plots can convey a good deal amount of information. Using the predictions we generated for the pp.write object above, we can plot the predicted probabilities against the age by the level of satis.salary for different levels of the outcome variable.

## melt data set to long for ggplot2
lpp <- melt(pp.write, id.vars = c("satis.salary", "age"), value.name = "probability")
head(lpp)  # view first few rows

## plot predicted probabilities across age values for each level of satis.salary
## faceted by program type
ggplot(lpp, aes(x = age, y = probability, colour = satis.salary)) + geom_line() + facet_grid(variable ~., scales = "free") +
  theme_classic()

# predict job type
head(predict(job.type.mod))

# observed vs. predicted category for each observation
fit.n <- table(observed = job.type.mod$model$job.cat,
               predicted = predict(object = job.type.mod))
fit.n

# observed vs. predicted category for each observation
fit.perc <- prop.table(table(observed = job.type.mod$model$job.cat,
                             predicted = predict(object = job.type.mod)),
                       margin = 1)
fit.perc

##########################
# Likelihood Ratio Tests #
##########################

# Measures the importance of each variable to the model

lrtest(job.type.mod_t, "satis.salary") #  chi sq 0.001286

##############################################
##############################################
# Multinomial Model Predictor Interpretation #
##############################################
##############################################

##########################
# Predictor Significance #
##########################

# get odds ratios
exp(coef(job.type.mod))

# get odds ratios and transpose
t(exp(coef(job.type.mod)))

# For every year older a person is, the odds of having a career in other sciences is 1.02 times higher compared to the odds of being in computer, maths or engineering.

# confidence intervals for odds ratios
exp(confint(job.type.mod))

# The interaction between age and sexfemale is not statistically significant as the CI's include 1.

# get odds ratios for other sciences from the model object
oddsrat.other.sci <- t(exp(coef(job.type.mod)))[ , 1]

# get CI for other sciences
confint.other.sci <- exp(confint(job.type.mod))[ , 1:2, 1]

# put into a data frame and print
other.sci <- data.frame(OR.other = oddsrat.other.sci,
                        CI.other = confint.other.sci)
other.sci

# get odds ratios for nonscience
oddsrat.non.sci <- t(exp(coef(job.type.mod)))[ , 2]

# get CI for nonscience
confint.non.sci <- exp(confint(job.type.mod))[ , 1:2, 2]

# put into a data frame and print
non.sci <- data.frame(OR.non = oddsrat.non.sci,
                      CI.non = confint.non.sci)
non.sci

# all together
or.ci <- data.frame(other.sci, non.sci)
or.ci

############################
# Predictor Interpretation #
############################

# Compared to men, the odds of females being employed in other science are 3.04 times higher than being involved in computer science, maths or engineering jobs
# Compared to men, women are 2.8 times higher odds of nonscience employment compared to computer science, maths or engineering jobs

# The interaction between age and sex was not statistically significant for either other sciences or non science compared to computer science, maths or engineering jobs

#################################
#################################
# Multinomial Model Assumptions #
#################################
#################################

# There are two assumptions: independence of observations and independence of irrelevant alternatives

################################
# Independence of observations #
################################

# The data consist of a random sample of graduates, so this assumption is met.

###########################################
# independence of irrelevant alternatives #
###########################################

# the categories of the outcome must be independent from each other
# The test is the Hausman-McFadden Test

# reshape data to use with the mlogit function
stem.samp.4mlog <- mlogit.data(stem.samp,
                               choice = "job.cat",
                               shape = "wide")

# estimate the model and print its summary
mlogit.job <- mlogit(job.cat ~ 0 | age + sex + age*sex,
                     stem.samp.4mlog)

summary(mlogit.job)

# estimate the model with two outcome categories and print its summary
mlogit.job.alt <- mlogit(job.cat ~ 0 | age + sex + age*sex,
                         stem.samp.4mlog,
                         alt.subset = c("CS, Math, Eng", "Nonscience"))

summary(mlogit.job.alt)

# hmftest
hmftest(mlogit.job, mlogit.job.alt)

# The p value is 1, so thhe IIA is rejected, which means the assumption is met.

mlogit.job.no.int <- mlogit(job.cat ~ 0 | age + sex,
                            stem.samp.4mlog)
summary(mlogit.job.no.int)

#get confidence intervals and odds ratios
mlogit.job.no.int.confint <- exp(confint(mlogit.job.no.int))
mlogit.job.no.int.or <- exp(summary(mlogit.job.no.int)$coefficients[1:6])

#put the OR and 95% CI together
mlogit.job.no.int.all <- cbind(mlogit.job.no.int.or, mlogit.job.no.int.confint)

#look at the results
mlogit.job.no.int.all

#get df for LR x2
job.df.noint <- length(summary(object = mlogit.job.no.int)$coefficients) - 
  length(summary(job.type.mod.null)$coefficients)
