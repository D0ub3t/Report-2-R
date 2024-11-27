library(readr)
user <- read_csv("C:/Users/Bubbl/Downloads/OA 11.6 - yelp_academic_dataset_user.json.csv")
business <- read_csv("C:/Users/Bubbl/Downloads/OA 11.7 - yelp_academic_dataset_business.json.csv")
View(user)
View(business)

head(business) #first 5 rows of data frame

install.packages("ggplot2")
library(ggplot2)

#Bar graph of businesses in each state
ggplot(business)+geom_bar(aes(x = state), fill = "pink") +labs(x = "States", y = "# of Businesses", title = "Businesses Per State")

pie_table <- table(business$stars)
#Pie chart of star rating for all the businesses
pie(pie_table, col = cm.colors(9), main = "Business Ratings")

stars_values <- as.factor(business$stars)
#reviews by star rating box plot
ggplot(business, aes(x = stars_values, y = review_count, fill = stars_values))+geom_boxplot()+ labs(x = "Star Values", y = "Review Count", title = "Reviews per Star Value")

#Chi Squared Test
star_cont_table <- table(business$stars == 1, business$stars == 5)
chisq.test(star_cont_table)

#Users Data
nrow(user)
colnames(user)
str(user)
#Correlation Values
corr_score <- cor(user[c("cool_votes", "funny_votes", "useful_votes")])
print(corr_score)

#Linear Regression
ggplot(user) + 
  geom_point(aes(x = cool_votes, y = useful_votes)) + 
  geom_smooth(aes(x = cool_votes, y = useful_votes), method = "lm", se = FALSE) +labs( x = "Cool Votes", y = "Useful Votes", title = "Cool v Useful")

#Writing reviews and fan count relationship

ggplot(user, aes(x = review_count, y = fans)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Review Count", y = "# of Fans", title = "Reviews v Fans")
# the two columns are positively correlated and have a positive slope. This means 
# that a higher review count results in more fans.

#Variable that brings MORE fans
corr_score_df <- cor(user[c("cool_votes", "funny_votes", "useful_votes","fans","review_count")])
print(corr_score_df)

#Useful Votes are better correlated with fan numbers than review counts. 
#Thus useful votes bring in more fans.This is determined with its correlation score.

useful_cont_table <- table(user$useful_votes, user$fans)
chisq.test(fan_cont_table)

count_cont_table <- table(user$review_count, user$fans)
chisq.test(count_cont_table)

#based on a Chi squared test we can see that the useful votes is better related to the fan count.

#K-means (review Count)
length(user$review_count)
review_count_x <- user$review_count [1:12]
wcss <- function(k){
  kmeans(review_count_x, centers = k)$tot.withinss}
k_values<- 1:10

wcss_values<- sapply(k_values,wcss)
print(wcss_values)

elbow_plot_1 <- data.frame(k=k_values, wcss = wcss_values)
View(elbow_plot_1)

ggplot(elbow_plot_1, aes( x = k, y=wcss))+geom_line()+geom_point()

count_data <- user[, c("review_count", "fans")]
kmeans_result <- kmeans(count_data, centers = 2)

user$cluster <- kmeans_result$cluster

ggplot(user, aes(x = review_count, y = fans, color = as.factor(cluster))) +
  geom_point() +
  geom_point(data = as.data.frame(kmeans_result$centers), aes(x = review_count, y = fans), 
             color = "pink", size = 3, shape = 8) +
  labs(title = "K-Means Clustering", x = "Review Count", y = "# of Fans") +
  scale_color_discrete(name = "Cluster")

#K-means (useful votes)
useful_vote_x <- user$useful_votes [1:12]
wcss <- function(k){
  kmeans(useful_vote_x, centers = k)$tot.withinss}
k_values<- 1:10

wcss_values<- sapply(k_values,wcss)
print(wcss_values)

elbow_plot_2 <- data.frame(k=k_values, wcss = wcss_values)
View(elbow_plot_1)

ggplot(elbow_plot_2, aes( x = k, y=wcss))+geom_line()+geom_point()

count_data <- user[, c("review_count", "fans")]
kmeans_result <- kmeans(count_data, centers = 2)

user$cluster <- kmeans_result$cluster

ggplot(user, aes(x = useful_votes, y = fans, color = as.factor(cluster))) +
  geom_point() +
  geom_point(data = as.data.frame(kmeans_result$centers), aes(x = review_count, y = fans), 
             color = "purple", size = 3, shape = 8) +
  labs(title = "K-Means Clustering", x = "Useful Votes", y = "# of Fans") +
  scale_color_discrete(name = "Cluster")

#I chose 2 clusters for each cluster graph. I did this based on the elbow graphs and the consistency around the second point. 
#The clusters for the useful votes data is better split up in to clusters. This backs the information previously in the code.