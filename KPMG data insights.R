install.packages("DataExplorer")
install.packages("magrittr")
install.packages("lubridate")
install.packages("e1071")
install.packages("rfm")
install.packages("caret")
library(readxl)
library(dplyr)
library(ggplot2)
library(DataExplorer)
library(magrittr)
library(lubridate)
library(e1071)
library(rfm)
library(caret)
getwd()
transactions <- read_excel("KPMG_VI_New_raw_data_update_final_Insights.xlsx",2)
CustomerDemographics <- read_excel("KPMG_VI_New_raw_data_update_final_Insights.xlsx",4)
CustomerAddress <- read_excel("KPMG_VI_New_raw_data_update_final_Insights.xlsx", 5)
df1 <- merge(x=transactions,CustomerDemographics, by = "customer_id")
df <- merge(x=df1,CustomerAddress,by="customer_id")
distinct_customers <- distinct(df,customer_id, .keep_all = TRUE )
#Review data structure
str(df)

#Review data
summary_df <- summary(df)
introduce(df) #introduction of data
plot_intro(df) #Metrics

#Replacing missing data
plot_missing(df)
final_df <- set_missing(df, list(0L, "unknown"))
plot_missing(final_df)

#Bar plots to visualise frquency distributions for all discrete features
plot_bar(final_df)

#Histograms to visualize distributions for all continuous features
plot_histogram(final_df)

#feature engineering 
 
skewness(final_df$Age) #checking the skewness
skewness(final_df$list_price) #checking the skewness
skewness(final_df$standard_cost) #checking the skewness
skewness(final_df$postcode)
#RFM analysis 
analysis_date <- lubridate::as_date('2018-01-01')
rfm_recencydate <- final_df %>% #Recency
  mutate(analysis_date) %>% 
  mutate(recency_days = (analysis_date)-as.Date(transaction_date)) %>%
  select(customer_id,recency_days) %>% group_by(customer_id)%>% summarize(recency_days=min(as.numeric(recency_days)))
rfm_orders <- final_df %>% #frequency
  group_by(customer_id) %>% 
  summarise(number_of_orders = as.numeric(n()))
rfm_recentvisit <- final_df %>%
  select(customer_id,transaction_date) %>%
  group_by(customer_id) %>%
  summarize(most_recent_visit = max((transaction_date))) %>%
  mutate(most_recent_visit = as.Date(most_recent_visit))
class(rfm_recentvisit$most_recent_visit)
rfm_revenue <- final_df %>%
  group_by(customer_id) %>%
  summarize(revenue=sum(list_price))
#rfm customer data table using merging of tables
rfm_data_consumer1 <- merge(x=rfm_revenue,rfm_recentvisit,by = "customer_id")
rfm_data_consumer2 <- merge(x=rfm_data_consumer1,rfm_orders,by = "customer_id")
rfm_data_consumer_final <- merge(x=rfm_data_consumer2,rfm_recencydate,by = "customer_id") #rfm data for distinct customer ids

class(rfm_data_consumer_final$customer_id)
class(rfm_data_consumer_final$revenue)
class(rfm_data_consumer_final$most_recent_visit)
class(rfm_data_consumer_final$number_of_orders)
class(rfm_data_consumer_final$recency_days)

analysis_date <- lubridate::as_date("2018-01-01")
options(max.print = 1000000)
rfm_table <- rfm_table_customer(rfm_data_consumer_final, customer_id, number_of_orders,recency_days,revenue, analysis_date)
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

#RFM model and ROC test
segment_new <- segments %>% 
  mutate(recency_s = ifelse(recency_score >= 3, "HIGH", "LOW"),
         frequency_s = ifelse(frequency_score >= 3, "FREQUENT", "INFREQUENT"),
         monetary_s = ifelse(monetary_score >= 3,"HIGH", "MEDIUM"),
         segment_s = ifelse(segment %in% c("Champions","Loyal","Potential Loyalists",
                                           "New Customers", "Promising", "Need Attention", 
                                           "About To Sleep", "At Risk", "Can't Lose Them"),1,0))


#Split data into training and test set
set.seed(123)
final_table <- merge(x=segment_new, final_df,by = "customer_id")
data2 = sort(sample(nrow(final_table), nrow(final_table)*.7))
#creating training data set by selecting the output row values
train <- final_table[data2,]
#creating test data set by not selecting the output row values
test <- final_table[-data2,]
dim(train)
dim(test)

#multiple logistic regression
logistics_model <- glm(segment_s ~ recency_s + frequency_s+monetary_s + gender + Age + wealth_segment + 
                         past_3_years_bike_related_purchases, data=train, family = "binomial")
# to predict using the logistics regression model, probabilities obtained
test[1:10,]
logistics_model_prob <- predict(logistics_model, test, type = "response")
head(logistics_model_prob,20)

#prediction 
prediction <- ifelse(logistics_model_prob > 0.5, 1,0)
head(prediction,10)
head(test$segment_s,10)

#test of model 
summary(logistics_model)
ROC_2 <- roc(test$segment_s, logistics_model_prob)
plot(ROC_2, col = "blue")
auc(ROC_2)
mean(prediction == test$segment_s)


