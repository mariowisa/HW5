Mario,cassidy,Zack





load ("/Users/mariowisa/Desktop/B2000/acs2017_ny/acs2017_ny_data.RData")
attach(acs2017_ny)
require(AER)
install.packages("yhat")
famincome <- (FTOTINC - INCWAGE)
use_varb <- (AGE >= 25) & (AGE <= 65) & (LABFORCE == 2) & (WKSWORK2 > 4) & (UHRSWORK >= 35) & (famincome) & ((educ_college) | (educ_advdeg)) 
dat_use <- subset(acs2017_ny,use_varb)
summary(dat_use)
model_temp1 <- lm(INCWAGE ~ AGE + AfAm + Asian + Amindian + race_oth + Hispanic + educ_hs + educ_somecoll + educ_college + educ_advdeg + CITIZEN, data=dat_use)
summary(model_temp1)
plot(model_temp1)

#2. Carefully explain the hypothesis tests of each coefficient and all of the coefficients jointly. For each coefficient, explain the t-stat, p-value, and confidence interval.
#First, we made a subset that looked at the full-time workforce between the ages of 25-65, their independent income (opposed to their family or household), and if they were college educated. Following, we ran a regression using the data and looked at the individual’s income compared with their age, race, and citizenship status. 
#Re age: we found that as one gets older, their income increases. The t-value and p-value demonstrate that these results are significant, and thus we reject the null hypothesis. 
#Re race: we found that identifying as African American had a negative correlation with wages. The p-value demonstrated that it was very statistically significant and the t-value was -11.055 which is significant but less than the t-value of age. 
#Re citizenship: we found that there was a somewhat positive correlation, however, the t-value and p-value state that it is not statistically significant.  

detach()
attach(acs2017_ny)
to_be_predicted2 <- data.frame(AGE = 25:65, female = 1, AfAm = 0, Asian = 0, Amindian = 1, race_oth = 1, Hispanic = 1, educ_hs = 0, educ_somecoll = 0, educ_college = 1, educ_advdeg = 0)
model_temp2 <- lm(INCWAGE ~ AGE + AfAm + Asian + Amindian + race_oth + Hispanic + educ_hs + educ_somecoll + educ_college + educ_advdeg, data=dat_use)
to_be_predicted2$yhat <- predict(model_temp2, newdata = to_be_predicted2)
plot(to_be_predicted2)
summary(to_be_predicted2$yhat)

#3. Explain your results, giving details about the estimation and providing any relevant graphics. What are the changes from what you’d previously found (with k-nn or averages) and why might this be so? How do changes in specification (e.g. logs) change the estimated coefficients? What are some relevant predicted values? Do those seem sensible? What additional information would be useful?
#To be honest, I was not able to figure out how to use the predicted model. When I tried running a summary, I just got various tables with  the mean and was unable to see the hypothesis tests. Looking at your examples, research and some of my peer’s work, by using log, it minimizes how wide of a variation there is. In theory, the summary statistics should be much closer together and thus the data should be scaled smaller. The coefficient and p-value are useful values in demonstrating how varying and significant the outputs are. For this data, I think it would be helpful to have a follow up survey conducted as well as more indepth questions regarding wages such as, how much they pay in taxes, are they supporting anyone (not including children), etc..  
