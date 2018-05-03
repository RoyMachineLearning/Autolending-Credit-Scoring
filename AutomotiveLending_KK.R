#KK Credit Scoring - Logistic Regression Example

install.packages("aod")
install.packages("mice")
install.packages("pscl")

# load package FactoMineR and ggplot2
require(FactoMineR)
require(ggplot2)
library (aod)
library(pscl)
library(mice)

# read cleaned data set
dd = read.csv("CreditScoring.csv", header=TRUE, stringsAsFactors=TRUE)

# Remove the 999999 and impute the missing values
# m=5 refers to the number of imputed datasets. Five is the default value.
# meth='norm' refers to the imputation method
new_dd <- mice(data = dd, m = 5, method = "cart", maxit = 10, seed = 500)

# derieve complete cases
derived_dd <- complete(new_dd)

# keep 2/3 of the data for learning, and 1/3 for testing
n = nrow(derived_dd)
learn = sample(1:n, size=round(0.67 * n))
nlearn = length(learn)
ntest = n - nlearn

# try Logistic Regression using all variables
glm1 = glm(Status ~ ., data=dd[learn,], family=binomial)


# Logistic Regression after removing variables
# new model
glm2 <- glm(formula = Status ~ Seniority + Age + Income + Debt + Amount + Home + Marital + Records + 
  Job + X1yr_duration + X2yr_duration + X3yr_duration + X4yr_duration, family = binomial, data = derived_dd[learn, ]) 


# final model using backward regression 
glm3 <- glm(formula = Status ~ Seniority + Income + Amount + Assets + Debt + Records + 
              Job + Price + X3yr_duration*Amount,  family = binomial, data = derived_dd[learn, ]) 


# check summary
summary(glm3)

# check coefficients
exp(glm3$coefficients)

# Anova
anova(glm3, test="Chisq")

# r square
pR2(glm3)

# Confidence intervals using log likelyhood
confint(glm3)

# Walds test to get the overall effect of rank
wald.test(b = coef(glm3), Sigma = vcov(glm3), Terms = 4:6)

# The chi-squared test statistic of 166.5, 
# with three degrees of freedom is associated with a p-value of 0.000 indicating that the overall effect of rank is statistically significant.

# since we are trying to justify our hypothesis,
# we are not assessing the predictive ability of the model

#odds ratio
exp(cbind(OR = coef(glm3), confint(glm3)))
