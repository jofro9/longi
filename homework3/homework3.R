library(janitor)
library(kableExtra)
library(lme4)
library(MASS)

# data
data = read.csv('../data/Cereal2.csv', header=TRUE) %>% clean_names()
data = data[data$fam_mem == 3,]

## question 1

# part a
model1 = glm(c1 ~ cond + sex + wt1, family = poisson(link = 'log'), data = data)
summary1 = summary(model1)

# part b
model2 = glm(c1 ~ cond + sex + wt1, family=quasipoisson(link='log'), data=data)
summary2 = summary(model2)

sqrt(summary2$dispersion)*summary1$coefficients[2,2]
summary2$coefficients[2,2]

# part c
model3 = glmer(c1 ~ cond + sex + wt1 + (1 | fam_idno), family=poisson(link='log'), data=data)
summary3 = summary(model3)

# part d
model4 = glm.nb(c1 ~ cond + sex + wt1, link='log', data=data)
summary4 = summary(model4)

## question 2

# part a
alpha = 0.05
z = qnorm(1 - (alpha / 2))

table1 = data.frame(
  " " = c(
    paste(round(exp(summary1$coefficients[1, 1]), 3), " (", round(exp(summary1$coefficients[1, 1] - summary1$coefficients[1, 2]*z), 3), ", ", round(exp(summary1$coefficients[1, 1] + summary1$coefficients[1, 2]*z), 3), ")", sep=""),
    paste(round(exp(summary1$coefficients[2, 1]), 3), " (", round(exp(summary1$coefficients[2, 1] - summary1$coefficients[2, 2]*z), 3), ", ", round(exp(summary1$coefficients[2, 1] + summary1$coefficients[2, 2]*z), 3), ")", sep=""),
    paste(round(exp(summary1$coefficients[3, 1]), 3), " (", round(exp(summary1$coefficients[3, 1] - summary1$coefficients[3, 2]*z), 3), ", ", round(exp(summary1$coefficients[3, 1] + summary1$coefficients[3, 2]*z), 3), ")", sep=""),
    paste(round(exp(summary1$coefficients[4, 1]), 3), " (", round(exp(summary1$coefficients[4, 1] - summary1$coefficients[4, 2]*z), 3), ", ", round(exp(summary1$coefficients[4, 1] + summary1$coefficients[4, 2]*z), 3), ")", sep=""),
    NA
  ),
  " " = c(
    paste(round(exp(summary2$coefficients[1, 1]), 3), " (", round(exp(summary2$coefficients[1, 1] - summary2$coefficients[1, 2]*z), 3), ", ", round(exp(summary2$coefficients[1, 1] + summary2$coefficients[1, 2]*z), 3), ")", sep=""),
    paste(round(exp(summary2$coefficients[2, 1]), 3), " (", round(exp(summary2$coefficients[2, 1] - summary2$coefficients[2, 2]*z), 3), ", ", round(exp(summary2$coefficients[2, 1] + summary2$coefficients[2, 2]*z), 3), ")", sep=""),
    paste(round(exp(summary2$coefficients[3, 1]), 3), " (", round(exp(summary2$coefficients[3, 1] - summary2$coefficients[3, 2]*z), 3), ", ", round(exp(summary2$coefficients[3, 1] + summary2$coefficients[3, 2]*z), 3), ")", sep=""),
    paste(round(exp(summary2$coefficients[4, 1]), 3), " (", round(exp(summary2$coefficients[4, 1] - summary2$coefficients[4, 2]*z), 3), ", ", round(exp(summary2$coefficients[4, 1] + summary2$coefficients[4, 2]*z), 3), ")", sep=""),
    round(summary2$dispersion, 3)
  ),
  " " = c(
    paste(round(exp(summary3$coefficients[1, 1]), 3), " (", round(exp(summary3$coefficients[1, 1] - summary3$coefficients[1, 2]*z), 3), ", ", round(exp(summary3$coefficients[1, 1] + summary3$coefficients[1, 2]*z), 3), ")", sep=""),
    paste(round(exp(summary3$coefficients[2, 1]), 3), " (", round(exp(summary3$coefficients[2, 1] - summary3$coefficients[2, 2]*z), 3), ", ", round(exp(summary3$coefficients[2, 1] + summary3$coefficients[2, 2]*z), 3), ")", sep=""),
    paste(round(exp(summary3$coefficients[3, 1]), 3), " (", round(exp(summary3$coefficients[3, 1] - summary3$coefficients[3, 2]*z), 3), ", ", round(exp(summary3$coefficients[3, 1] + summary3$coefficients[3, 2]*z), 3), ")", sep=""),
    paste(round(exp(summary3$coefficients[4, 1]), 3), " (", round(exp(summary3$coefficients[4, 1] - summary3$coefficients[4, 2]*z), 3), ", ", round(exp(summary3$coefficients[4, 1] + summary3$coefficients[4, 2]*z), 3), ")", sep=""),
    round(sqrt(summary3$varcor$fam_idno[1]), 3)
  ),
  " " = c(
    paste(round(exp(summary4$coefficients[1, 1]), 3), " (", round(exp(summary4$coefficients[1, 1] - summary4$coefficients[1, 2]*z), 3), ", ", round(exp(summary4$coefficients[1, 1] + summary4$coefficients[1, 2]*z), 3), ")", sep=""),
    paste(round(exp(summary4$coefficients[2, 1]), 3), " (", round(exp(summary4$coefficients[2, 1] - summary4$coefficients[2, 2]*z), 3), ", ", round(exp(summary4$coefficients[2, 1] + summary4$coefficients[2, 2]*z), 3), ")", sep=""),
    paste(round(exp(summary4$coefficients[3, 1]), 3), " (", round(exp(summary4$coefficients[3, 1] - summary4$coefficients[3, 2]*z), 3), ", ", round(exp(summary4$coefficients[3, 1] + summary4$coefficients[3, 2]*z), 3), ")", sep=""),
    paste(round(exp(summary4$coefficients[4, 1]), 3), " (", round(exp(summary4$coefficients[4, 1] - summary4$coefficients[4, 2]*z), 3), ", ", round(exp(summary4$coefficients[4, 1] + summary4$coefficients[4, 2]*z), 3), ")", sep=""),
    round(summary4$theta, 3)
  )
)
rownames(table1) = c("Intercept", "Condition", "Sex", "Weight", "Other")
colnames(table1) = c("Poisson Regression", "Poisson QL", "Poisson + Normal error", "NB NLMIXED")
table1
kable(table1, booktabs=TRUE)
