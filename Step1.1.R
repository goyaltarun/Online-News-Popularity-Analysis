rm(list=ls())
library(data.table)
library(Boruta)
library(caret)
library(ggplot2)
library(ROCR)

OnlineNewsPopularity <-read.csv('OnlineNewsPopularity.csv',header = TRUE)
### Divide all variables into 10 sorts
links <- c('num_hrefs','num_self_hrefs','self_reference_min_shares','self_reference_max_shares',
           'self_reference_avg_sharess')
news_vars <- c('kw_min_min','kw_max_min','kw_avg_min','kw_min_max','kw_max_max','kw_avg_max',
               'kw_min_avg','kw_max_avg','kw_avg_avg')
news_data <- c('data_channel_is_lifestyle','data_channel_is_entertainment','data_channel_is_bus',
               'data_channel_is_socmed','data_channel_is_tech','data_channel_is_world')
news_lda <- c('LDA_00','LDA_01','LDA_02','LDA_03','LDA_04','url')
news_language <- c('global_subjectivity','global_sentiment_polarity',
                   'global_rate_positive_words','global_rate_negative_words',
                   'rate_positive_words','rate_negative_words',
                   'avg_positive_polarity','min_positive_polarity',
                   'max_positive_polarity','avg_negative_polarity',
                   'min_negative_polarity','max_negative_polarity',
                   'title_subjectivity', 'title_sentiment_polarity',
                   'abs_title_subjectivity', 'abs_title_sentiment_polarity')
news_words <- c('n_tokens_title','n_tokens_content','n_unique_tokens','n_non_stop_words',
                'n_non_stop_unique_tokens','average_token_length')
news_media <- c('num_imgs','num_videos')
news_time_weekday<- c('weekday_is_monday','weekday_is_tuesday','weekday_is_wednesday','weekday_is_thursday',
                      'weekday_is_friday','weekday_is_saturday') #categorical
news_time_weekend <- c('weekday_is_sunday',"is_weekend")
### Detect whether exist missing values in origin data
anyNA(OnlineNewsPopularity)
# FALSE Attach (OnlineNewsPopularity) summary(shares) # Min. 1st Qu. Median Mean 3rd Qu. Max. # 1 946 1400 3395 2800 843300
# plotting the y-variable to check for the distribution
par(mfrow=c (1,3))
plot(OnlineNewsPopularity$shares,xlab="")
boxplot(OnlineNewsPopularity$shares)
hist(OnlineNewsPopularity$shares,prob=T)
lines(density(OnlineNewsPopularity$shares), col="blue")
#Now we try to get rid of non-predictive first two variables:
news <- OnlineNewsPopularity[,-c(1,2)]
# Next, divide the data set on the basis of the categorical variables:
news_categorical <- c(12:17,30:37)
# Classify the y-variable as binomial based on the median value i.e. 1400 as was calculated in the
# summary section i.e. if y>1400 we denote it as 1 else 0
y <- ifelse(news$shares>1400,1,0)
# change the variable to factor
y <- as.factor(y)
# Data Scaling
# scale all the continuous variables except for the categorical variables:
news_scale <- scale(news[,-c(59,news_categorical)],center=T,scale=T)
# merge the datasets i.e. categorical dataset and scaled dataset
news_final <- data.frame(y,news[,news_categorical],news_scale)


summary(OnlineNewsPopularity$shares);

library(h2o);
h2o.init();

iris.hex <- as.h2o(news_final)

