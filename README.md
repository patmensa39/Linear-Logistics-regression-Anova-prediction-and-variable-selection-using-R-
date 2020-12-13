# Linear-Logistics-regression-Anova-prediction-and-variable-selection-using-R-
Used Muscle data set to produce linear and logistics regression, analysis of variance, ggplots, and variable selection. 

### Reading the muscle data

```{r}
muscle<- read.table("Muscle.csv", header = TRUE, sep = ",")
```

```{r}
### 1 Fitted Linear model. 
#### Pairs by selecting the third to 8th column
Philant_muscle<-muscle[, 3:8]
pairs(Philant_muscle)
```


```{r}
### 2 Linear model.This include all the other variables. 
philant.model1<-lm(Ankle_stance_even~., data = Philant_muscle)
summary(philant.model1)
```


```{r}
### 3 Analysis of variance for the model above
anova(philant.model1)
```


```{r}
### 4 The coefficient estimates give the effect on the response of each covariate taking into account the presence of the other covariate. For only one covarite.
philant.model2<-lm(Ankle_stance_even~Ankle_swing_even, data = Philant_muscle)
summary(philant.model2)

```

```{r}
###5 Anova for the above model
anova(philant.model2)
### The Analysis of Variance take the data go step by step. It can be realized that the sum of squares.
```


```{r}
### Residual to check linearity assumption, constant variance, and independence assumption
resid(philant.model1)
resid(philant.model2)
```


```{r}
### Fitted model
fitted(philant.model)
fitted(philant.model2)
```

```{r}
### Plotting the model
plot(philant.model1)
plot(philant.model2)
```


```{r}
### Prediction 
predict(philant.model2, newdata = data.frame(Ankle_swing_even=c(30:50)))

predict(philant.model2, newdata = data.frame(Ankle_swing_even=c(30:50)), se.fit = T)
predict(philant.model2, newdata = data.frame(Ankle_swing_even=c(30:50)), interval = "confidence")
predict(philant.model2, newdata = data.frame(Ankle_swing_even=c(30:50)), interval = "prediction")
```

```{r}
### Boxplot for Ankle-stance-even grouped by Gender.
library(ggplot2)
ggplot(data = muscle, mapping = aes(x=Gender, y=Ankle_stance_even))+geom_boxplot()+ labs(y="Ankle-Stance-Even")
### Boxplot for Ankle-stance-uneven grouped by Gender.
ggplot(data = muscle, mapping = aes(x=Gender, y=Ankle_stance_uneven))+geom_boxplot() + labs(y="Ankle-Stance-uneven")
### Boxplot for Ankle-stance-even grouped by Age.
ggplot(data = muscle, mapping = aes(x=Age, y=Ankle_stance_even))+geom_boxplot() + labs(y="Ankle-Stance-even")
### Boxplot for Ankle-stance-uneven grouped by Age.
ggplot(data = muscle, mapping = aes(x=Age, y=Ankle_stance_uneven))+geom_boxplot() + labs(y="Ankle-Stance-uneven")

## Knee stance measurements
### Boxplot for Knee-stance-even grouped by Gender
ggplot(data = muscle, mapping = aes(x=Gender, Knee_stance_even))+geom_boxplot() + labs(y="Knee-Stance-even")
### Boxplot for Knee-stance-uneven grouped by Gender.
ggplot(data = muscle, mapping = aes(x=Gender, Knee_stance_uneven))+geom_boxplot() + labs(y="Knee-Stance-uneven")
### Boxplot for Knee-stance-even grouped by Age.
ggplot(data = muscle, mapping = aes(x=Age, Knee_stance_even))+geom_boxplot() + labs(y="Knee-Stance-even")
### Boxplot for Knee-stance-uneven grouped by Age
ggplot(data = muscle, mapping = aes(x=Age, Knee_stance_uneven))+geom_boxplot() + labs(y="Knee-Stance-uneven")
```

```{r}
### Variable selection. 
  

### Best subset regression. This selects the best subset of predictors that's do the best at meeting some well-defined objects criterion, Such as having the best our largest all square value or the smallest MSE, Mallow CP all  AIC 
ols_step_best_subset(philant.model1, details=TRUE)
philant1<-ols_step_best_subset(philant.model1)
plot(philant1)

### Step-wise forward regression. This put in one variable at a time and check which one is the best, and later keep on adding the variables and check which one is the best till they finish.  
ols_step_forward_p(philant.model1, details = TRUE)
philant3<-ols_step_forward_p(philant.model1)
plot(philant3)

### Backwards elimination. this starts with all the variable, check the worst one and remove it till we get a good model 
ols_step_backward_p(philant.model1, details = TRUE)
philant4<-ols_step_backward_p(philant.model1)
plot(philant4)
### Step-wise regression. In this regression there are no variables left to enter or remove any more. The model includes all the predictor variables.
ols_step_both_p(philant.model1, details = TRUE)
philant5<-ols_step_both_p(philant.model1)
plot(philant5)
### Stepwise AIC forward regression
ols_step_forward_aic(philant.model1, details = TRUE)
philant6<-ols_step_forward_aic(philant.model1)
plot(philant6)

### Stepwise AIC backwards regression
ols_step_backward_aic(philant.model1, details = TRUE)
philant7<-ols_step_backward_aic(philant.model1)
plot(philant7)

### Stepwise AIC regression
ols_step_both_aic(philant.model1, details = TRUE)
philant8<-ols_step_both_aic(philant.model1)
plot(philant8)
```

```{r}

### 2 Using the Gender response variable. Building a logistics regression.
code.gender<-as.factor(muscle$Gender)
pat.model<-glm(code.gender~Ankle_stance_even+Ankle_swing_even+Knee_stance_even+Knee_swing_even+Ankle_stance_uneven+Ankle_swing_uneven+Knee_stance_uneven+Knee_swing_uneven, data = muscle, family = "binomial")
summary(pat.model)

### One with one main effect only.
pat.model1<-glm(code.gender~Ankle_stance_even, family = "binomial", data = muscle)
summary(pat.model1)
pat.model2<-glm(code.gender~Ankle_stance_even, family = binomial(link = probit), data = muscle)
summary(pat.model2)

### One with two main effect
pat.model3<-glm(code.gender~Ankle_stance_even+Ankle_swing_even, family = "binomial", data = muscle)
summary(pat.model3)
pat.model4<-glm(code.gender~Ankle_stance_even+Ankle_swing_even, family =binomial(link = probit) , data = muscle)
summary(pat.model4)

### One with one three main effects 
pat.model5<-glm(code.gender~Ankle_stance_even+Ankle_swing_even+Knee_stance_even+Knee_swing_even, family = "binomial", data = muscle)
summary(pat.model5)
pat.model6<-glm(code.gender~Ankle_stance_even+Ankle_swing_even+Knee_stance_even+Knee_swing_even, family = binomial(link = cloglog), data = muscle)
summary(pat.model6)

```


```{r}
### ANOVA for Generalized linear model
anova(pat.model, test = "LRT")
```

```{r}
### Anova Using F test
anova(pat.model, test = "F")
```

```{r}
### Dropping the variables using the F test
drop1(pat.model, test = "F")
```

```{r}
### From a poisson example.
philant.count<-c(10,30,45,65,12,69,90,12,10)
philant.outcome<-gl(3,1,9)
philant.treatment<-gl(3,3)
### Making it a data frame.
philant.dataframe<-data.frame(philant.treatment, philant.outcome, philant.count)
print(philant.dataframe)

### Assume philant.count is the response and making a model on treatment and count on poisson family
new.model<-glm(philant.count~philant.outcome+philant.treatment, family = poisson())
summary(new.model)
### You can write a fitted equation from the summary

#### Making Anova for the poisson model
anova(new.model)
```
