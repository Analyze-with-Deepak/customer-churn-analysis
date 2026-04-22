churn_df <- read.csv("telco_churn_cleaned.csv")
head(churn_df)
str(churn_df)
head(churn_df)
str(churn_df)
table(churn_df$Churn)
prop.table(table(churn_df$Churn))
aggregate(tenure ~ Churn, data = churn_df, mean)
aggregate(MonthlyCharges ~ Churn, data = churn_df, mean)
t.test(tenure ~ Churn, data = churn_df)
aggregate(tenure ~ Churn, data = churn_df, mean)
t.test(tenure ~ Churn, data = churn_df)
aggregate(MonthlyCharges ~ Churn, data = churn_df, mean)
t.test(MonthlyCharges ~ Churn, data = churn_df)
aggregate(MonthlyCharges ~ Churn, data = churn_df, mean)
t.test(MonthlyCharges ~ Churn, data = churn_df)
churn_df$ChurnFlag <- ifelse(churn_df$Churn == "Yes", 1, 0)
table(churn_df$ChurnFlag)
churn_df$Contract <- as.factor(churn_df$Contract)
logit_model <- glm(
  ChurnFlag ~ tenure + MonthlyCharges + Contract,
  data = churn_df,
  family = binomial
)
summary(logit_model)
exp(coef(logit_model))
churn_df$ChurnFlag <- ifelse(churn_df$Churn == "Yes", 1, 0)
table(churn_df$ChurnFlag)

churn_df$Contract <- as.factor(churn_df$Contract)

logit_model <- glm(
  ChurnFlag ~ tenure + MonthlyCharges + Contract,
  data = churn_df,
  family = binomial
)

summary(logit_model)
exp(coef(logit_model))
churn_df$ChurnFlag <- ifelse(churn_df$Churn == "Yes", 1, 0)
table(churn_df$ChurnFlag)

churn_df$Contract <- as.factor(churn_df$Contract)

logit_model <- glm(
  ChurnFlag ~ tenure + MonthlyCharges + Contract,
  data = churn_df,
  family = binomial
)

summary(logit_model)
exp(coef(logit_model))
install.packages("rpart")
library(rpart)
tree_model <- rpart(
  ChurnFlag ~ tenure + MonthlyCharges + Contract,
  data = churn_df,
  method = "class"
)

plot(tree_model)
text(tree_model, use.n = TRUE)

