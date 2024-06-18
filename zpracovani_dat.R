library(ggplot2)
library(car)
# Levene's test with one independent variable
leveneTest(weight ~ group, data = PlantGrowth)

all <- c(2.90967, 2.95292, 2.97261)
pml <- c(2.79521, 3.05231, 2.88549)
mye <- c(2.79617, 2.91124, 3.0235)
lym <- c(3.00581, 3.01066, 2.75894)
cml <- c(2.87706, 2.85542, 2.84549)
df <- data.frame(all, pml, mye, lym, cml)

data <- c(all,pml,mye,lym,cml)
names <- c(rep("all", times = 3),rep("pml", times = 3), rep("mye", times = 3), rep("lym", times = 3), rep("cml", times = 3))
exprese <- data.frame(names, data)

#Testovani normality
summary(df)

#ggplot(exprese, aes(x=names, y=data, color=data))

qqnorm(df$all, main ="ALL Q-Q Plot")
qqnorm(df$pml, main ="PML Q-Q Plot")
qqnorm(df$mye, main ="MYE Q-Q Plot")
qqnorm(df$lym, main ="LYM Q-Q Plot")
qqnorm(df$cml, main ="CML Q-Q Plot")

boxplot(df, xlab = colnames(df), ylab = "signal")

# Levene's test with one independent variable
# leveneTest(exprese$data ~ exprese$names, data = exprese)
leveneTest(exprese$data ~ exprese$names, data = exprese)

#Test rozptylu- Jednofaktorová ANOVA

# summary(aov(formula = values ~ ind, data = stack(df)))
summary(aov(exprese$data ~ exprese$names))
qf(.95, df1=2, df2=6)


