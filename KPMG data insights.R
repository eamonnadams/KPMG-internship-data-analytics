#Install and load all necessary packages and libraries
install.packages("DataExplorer")
install.packages("magrittr")
install.packages("lubridate")
install.packages("e1071")
install.packages("rfm")
install.packages("caret")
install.packages("pROC")
library(readxl)
library(dplyr)
library(ggplot2)
library(DataExplorer)
library(magrittr)
library(lubridate)
library(e1071)
library(rfm)
library(caret)
library(pROC)
getwd()
#Import data from excel 
transactions <- read_excel("KPMG_VI_New_raw_data_update_final_Insights.xlsx",2)
CustomerDemographics <- read_excel("KPMG_VI_New_raw_data_update_final_Insights.xlsx",4)
CustomerAddress <- read_excel("KPMG_VI_New_raw_data_update_final_Insights.xlsx", 5)
New_customers <- read_excel("KPMG_VI_New_raw_data_update_final_Insights.xlsx", 3)


df1 <- merge(x=transactions,CustomerDemographics, by = "customer_id")
df <- merge(x=df1,CustomerAddress,by="customer_id")
distinct_customers <- distinct(df,customer_id, .keep_all = TRUE )
#Review data structure
str(df)
summary_df <- summary(df)
introduce(df) #introduction of data
plot_intro(df) #Metrics

#Replacing missing data
plot_missing(df)
final_df <- set_missing(df, list(0L, "unknown"))
final_df <- final_df %>% 
  filter(gender %in% c("Male","Female") & brand != "unknown"
         & job_industry_category != "unknown" & job_title != "unknown")
plot_missing(final_df)

#EDA analysis
#Bar plots to visualise frquency distributions for all discrete features
plot_bar(final_df)

#Histograms to visualize distributions for all continuous features
plot_histogram(final_df)

##Categorical data vs continuous data
#total number of purchase in 3 years per gender
final_df %>%
  select(gender,past_3_years_bike_related_purchases) %>%
  group_by(gender) %>%
  summarize(total_past_purchases = sum(past_3_years_bike_related_purchases)) %>%
  ggplot(aes(gender,total_past_purchases,fill=gender)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label =total_past_purchases)) +
  ggtitle("Total Past 3 year purchase by Gender") 
#percentage number of purchase in 3 years per gender
final_df %>%
  select(gender,past_3_years_bike_related_purchases) %>%
  group_by(gender) %>%
  summarize(total_past_purchases = sum(past_3_years_bike_related_purchases)) %>%
  mutate(percent = signif(total_past_purchases/sum(total_past_purchases)*100,4))%>%
  ggplot(aes(gender,percent,fill=gender)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label =percent))+
  ggtitle("Percentage Past 3 year purchase by Gender")
#percentage sales per gender
final_df %>%
  select(gender,list_price) %>%
  group_by(gender) %>%
  summarize(total_sales = sum(list_price)) %>%
  mutate(percent_sales = signif(total_sales/sum(total_sales)*100,4))%>%
  ggplot(aes(gender,percent_sales,fill=gender)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label =percent_sales))+
  ggtitle("Percentage sales by Gender")
#Tenure by age
final_df %>%
  select(Age,tenure) %>%
  ggplot(aes(Age,tenure)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Tenure by Age") 
#Cars owned per State
final_df %>%
  select(owns_car,state) %>%
  ggplot(aes(state,stat_count = owns_car,fill = owns_car)) +
  geom_bar(position = "dodge") +
  ggtitle("Cars owned by state")
#Brands amounts sold per gender
final_df %>%
  select(gender,brand) %>%
  ggplot(aes(brand,stat_count = gender,fill = gender)) +
  geom_bar(position = "dodge") +
  ggtitle("Brand amounts by gender")
#wealth segment per state
final_df %>%
  select(wealth_segment,state) %>%
  ggplot(aes(state,stat_count = wealth_segment,fill = wealth_segment)) +
  geom_bar(position = "dodge") +
  ggtitle("Wealth segment per state")



##feature engineering
#checking the skewness of Age distribution
skewness(final_df$Age)
hist(final_df$Age) #distribution of Age
#checking the skewness of the list_price
skewness(final_df$list_price)  
hist(final_df$list_price) #distribution of list_price
#checking the skewness of standard_cost 
skewness(final_df$standard_cost) 
hist(final_df$standard_cost)
#checking the skewness of past_3_years_bike_related_purchases
skewness(final_df$past_3_years_bike_related_purchases)
hist(final_df$past_3_years_bike_related_purchases)

##RFM analysis 
analysis_date <- lubridate::as_date('2018-01-01')
rfm_recencydate <- final_df %>% 
  mutate(analysis_date) %>% 
  mutate(recency_days = (analysis_date)-as.Date(transaction_date)) %>%
  select(customer_id,recency_days)%>%
  group_by(customer_id)%>% 
  summarize(recency_days=min(as.numeric(recency_days))) #Recent date calculation
rfm_orders <- final_df %>% 
  group_by(customer_id) %>% 
  summarise(number_of_orders = as.numeric(n())) #number of orders calculation
rfm_recentvisit <- final_df %>%
  select(customer_id,transaction_date) %>%
  group_by(customer_id) %>%
  summarize(most_recent_visit = max((transaction_date))) %>%
  mutate(most_recent_visit = as.Date(most_recent_visit)) #recent visit calculation
class(rfm_recentvisit$most_recent_visit)
rfm_revenue <- final_df %>%
  group_by(customer_id) %>%
  summarize(revenue=sum(list_price)) #revenue calculation
#rfm customer data table using merging of tables
rfm_data_consumer1 <- merge(x=rfm_revenue,rfm_recentvisit,by = "customer_id")
rfm_data_consumer2 <- merge(x=rfm_data_consumer1,rfm_orders,by = "customer_id")
rfm_data_consumer_final <- merge(x=rfm_data_consumer2,rfm_recencydate,by = "customer_id") #rfm data for distinct customer ids

class(rfm_data_consumer_final$customer_id)
class(rfm_data_consumer_final$revenue)
class(rfm_data_consumer_final$most_recent_visit)
class(rfm_data_consumer_final$number_of_orders)
class(rfm_data_consumer_final$recency_days)

analysis_date <- lubridate::as_date("2018-01-01") #Define analysis date 
options(max.print = 1000000)
rfm_table <- rfm_table_customer(rfm_data_consumer_final,
                                customer_id, number_of_orders,
                                recency_days,revenue, 
                                analysis_date) #rfm table formation 

#RFM visualization
rfm_heatmap(rfm_table)
rfm_bar_chart(rfm_table) #distributions of RFM score combinations
rfm_histograms(rfm_table) #rfm distribution 
rfm_order_dist(rfm_table) #distribution of customers across orders
rfm_rm_plot(rfm_table) #Recency vs Monetary comparison
rfm_fm_plot(rfm_table) #Frequency vs Monetary comparison
rfm_rf_plot(rfm_table) #Recency vs Frequency comparison

#segmentation categories
segment_names <- c("Champions","Loyal Customers", "Potential Loyalists",
                   "New Customers", "Promising","Need Attention",
                   "About to Sleep", "At Risk", "Can't Lose Them",
                   "Hibernating", "Lost")
recency_lower   <- c(4,2,3,4,3,3,2,1,1,2,1)
recency_upper   <- c(5,4,5,5,4,4,3,2,1,2,2)
frequency_lower <- c(4,3,1,1,1,2,1,2,4,1,1)
frequency_upper <- c(5,5,3,1,1,3,2,5,5,2,2)
monetary_lower  <- c(4,3,1,1,1,2,1,2,4,1,1)
monetary_upper  <- c(5,5,3,1,1,3,2,5,5,2,2)
#segments table with the RFM scores and segments
segments <- rfm_segment(rfm_table,segment_names,recency_lower,
                        recency_upper,frequency_lower,
                        frequency_upper,monetary_lower,
                        monetary_upper)
head(segments)
#distribution of customers across the segments
segments %>% 
  count(segment) %>%
  arrange(desc(n)) %>%
  rename(Segment = segment, Count = n)

rfm_plot_median_recency(segments) #median recency
rfm_plot_median_frequency(segments) #median frequency
rfm_plot_median_monetary(segments) #median monetary 

#Hypothesis test using a t-test
#Ho: mu > 3
#one-sided 95% confidence interval for mu
X_r <- sample(rfm_table$recency_bins, 1000, replace = TRUE)#sample size 1000 of recency score
mean(X_r)
sd(X_r)
#t test of X_r against a null hypothesis that population mean mu_r is 3
t.test(X_r, mu = 3, alternative = "two.sided")

X_f <- sample(rfm_table$frequency_bins, 1000, replace = TRUE)#sample size 1000 of recency score
mean(X_f)
sd(X_f)
#t test of X_r against a null hypothesis that population mean mu_r is 3
t.test(X_f, mu = 3, alternative = "two.sided")

X_f <- sample(rfm_table$monetary_bins, 1000, replace = TRUE)#sample size 1000 of recency score
mean(X_f)
sd(X_f)
#t test of X_r against a null hypothesis that population mean mu_r is 3
t.test(X_f, mu = 3, alternative = "two.sided")

#Converting segments to binomial variables 1 and 0, 1 for target and 0 for not target
segment_new <- segments %>% 
  mutate(recency_s = ifelse(recency_score > 3, "HIGH", "LOW"),
         frequency_s = ifelse(frequency_score > 3, "FREQUENT", "INFREQUENT"),
         monetary_s = ifelse(monetary_score > 3,"HIGH", "MEDIUM"),
         segment_s = ifelse(segment %in% c("Champions","Loyal Customers","Potential Loyalists",
                                           "New Customers", "Promising", "Need Attention", 
                                          "Can't Lose Them"),1,0))

#Split data into training and test set
set.seed(123)
final_table <- merge(x=segment_new, final_df,by = "customer_id")
final_table <- final_table %>%
  filter(gender %in% c("Male","Female") & brand %in% c("Norco Bicycles",
                                                       "Trek Bicycles", "OHM Cycles",
                                                        "WeareA2B","Giant Bicycles",
                                                       "Solex"))
data2 = sort(sample(nrow(final_table), nrow(final_table)*.7))
#creating training data set by selecting the output row values
train <- final_table[data2,]
#creating test data set by not selecting the output row values
test <- final_table[-data2,]
test_f <- test %>%
  filter(gender %in% c("Male","Female"))
train_f <-  train %>%
  filter(gender %in% c("Male","Female"))
dim(train)
dim(test)

##multiple logistic regression model
logistics_model <- glm(segment_s ~ recency_s + frequency_s+monetary_s + gender + cubic_Age + wealth_segment + 
                         past_3_years_bike_related_purchases, data=train_f, family = "binomial")
# to predict using the logistics regression model, probabilities obtained
test_f[1:10,]
logistics_model_prob <- predict(logistics_model, test_f, type = "response")
head(logistics_model_prob,20)

#convert probabilities to binomial answers
prediction <- ifelse(logistics_model_prob > 0.5, 1,0)
head(prediction,10)
head(test_f$segment_s,10)

#test of model 
summary(logistics_model)
ROC_2 <- roc(test_f$segment_s, logistics_model_prob)
plot(ROC_2, col = "blue")
auc(ROC_2)
mean(prediction == test_f$segment_s)

#export final_table to excel
write.table(final_table, file="FinalCustomerTable.csv",row.names = FALSE,sep=",")

#Most valuable new customers
Most_valuable_new_customers <- New_customers %>%
  filter(wealth_segment %in% c("Mass Customer","High Net Worth") &
           job_industry_category %in% c("Financial Services","Manufacturing",
                                        "Health","Retail", "Property"))
write.table(Most_valuable_new_customers, file="TargetCustomerTable.csv",row.names = FALSE,sep=",")