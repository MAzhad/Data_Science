str(Churn_Train)

#check missing value
is.na(Churn_Train)

#replace missing value
Churn_Train <- Churn_Train %>%
  mutate(Total.Charges = replace(Total.Charges,is.na(Total.Charges),mean(Total.Charges,na.rm=T)))

#check back missing value
is.na(Churn_Train$Total.Charges)

describe(Churn_Train)
describe(Churn_Train,Tenure,Monthly.Charges,Total.Charges)
describe(Churn_Train,Senior.Citizen:Total.Charges)
describe(Churn_Train,-(Tenure))

normality(Churn_Train)
normality(Churn_Train,Tenure,Monthly.Charges,Total.Charges)
normality(Churn_Train,Senior.Citizen:Total.Charges)
normality(Churn_Train,-(Tenure))

plot_normality(Churn_Train,Monthly.Charges,Total.Charges)

correlate(Churn_Train)
correlate(Churn_Train,Tenure,Monthly.Charges,Total.Charges)
correlate(Churn_Train,Senior.Citizen:Total.Charges)
correlate(Churn_Train,-(Tenure))

describe(Churn_Train,Churn)

Churn_Train%>%
  correlate()%>%
  plot()

correlate(Churn_Train,Monthly.Charges,Total.Charges)%>%
  plot()

categ <- target_by(Churn_Train,Churn)

cat_num <- relate(categ,Total.Charges)
cat_num
summary(cat_num)
plot(cat_num)

cat_cat <- relate(categ,Internet.Service)
cat_cat
summary(cat_cat)
plot(cat_cat)

num <- target_by(Churn_Train,Monthly.Charges)
num_num <- relate(num,Total.Charges)
num_num
summary(num_num)
plot(num_num)