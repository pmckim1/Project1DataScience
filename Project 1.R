#### Be sure to have the packages below installed using install.packages() function ###

require(tidyverse) # Package used for data manipulation
require(ggplot2) # Package for data visualization
require(pander) # Package to make 
require(reshape2) # Additional package for data manipulation

# Read in AirBnB listing dataset (might need to change file path/working directory)
listing <- read.csv("listings.csv") 

# Function to remove outliers written by Dr. Lo
outlierKD <- function(dt, var) { 
  var_name <- eval(substitute(var),eval(dt))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(var_name, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(dt))
  } else{
    cat("Nothing changed", "n")
    return(invisible(var_name))
  }
}

# Use function to remove outliers from "listing" dataset
# Enter Y when prompted in the console
outlierKD(listing, price)
outlierKD(listing, number_of_reviews)

# Question 1
# Plot boxplot of number of reviews by room type
room_to_reviews <- ggplot(listing, aes(x = as.factor(room_type), y = number_of_reviews, color = room_type)) +
  geom_boxplot() + 
  theme_bw() + 
  labs(x = "Room Type",
       y = "Number of Reviews",
       color = "Room Type") 
print(room_to_reviews + ggtitle("Boxplot of Number of Reviews by Room Type")) # Print boxplot with title

# ANOVA test to see if mean number of reviews is different between room types
# Conclusion: statistically significant, reject null hypothesis
anova_room_review <- aov(number_of_reviews ~ as.factor(room_type), data = listing)
pander(anova_room_review)

# Tukey HSD test to determine which of the room types are pairwise considered different
# Conclusion: Private to entire home  different
# Conclusion: Shared to entire home and shared to private room not different
tukey_room_review <- TukeyHSD(anova_room_review)
tukey_room_review

# Question 2
# Boxplot of price by room type
price_to_room_type <- ggplot(listing, aes(x = factor(room_type), y = price, color = room_type)) +
  geom_boxplot() + 
  theme_bw() +
  labs(x = "Room Type",
       y = "Price",
       color = "Room Type")
print(price_to_room_type + ggtitle("Boxplot of Price by Room Type")) # Print boxpot with title

# One-way ANOVA test to see if mean prices are different between room types
# Conclusion: statistically significant, reject null hypothesis
anova_room_price <- aov(price ~ as.factor(room_type), data = listing)
pander(anova_room_price) # Print formatted table

# Tukey HSD test to determine which of the room types are pairwise considered different
# Conclusion: all different from one another 
tukey_room_price <- TukeyHSD(anova_room_price)
tukey_room_price

# Question 3 
# Separate neighborhoods by unit id
neighborhood <- 
  as.data.frame(listing) %>%
  select(id, price, neighbourhood)

neighborhood <- separate(neighborhood, neighbourhood, into = c("N1", "N2", "N3"), sep = ",")
neighborhood <-melt(neighborhood, 
       id.vars = c("id", "price"),
       variable.name = "neighborhood",
       na.rm=T) %>%
  arrange(id) 
names(neighborhood) <- c("id", "price", "number", "neighborhood")

# Histogram of number of AirBnBs by neighborhood
freq_table <- 
  neighborhood %>%
  group_by(neighborhood) %>%
  count() %>%
  arrange(-n)

neigh_bar <- ggplot(subset(freq_table, n > quantile(freq_table$n, 0.85)), aes(x = neighborhood, y = n)) +
  geom_bar(aes(fill=neighborhood), stat="identity") + 
  theme_bw() + 
  labs(x = "Neighborhood",
       y = "Number of AirBnbs",
       fill = "Neighborhood") + 
  theme(axis.ticks.x = element_blank()) + 
  scale_x_discrete(labels = c("Blmn.", 
                              "K St.",
                              "Kingman",
                              "Lincoln",
                              "Logan",
                              "Mt. Pleasant",
                              "Pleasant",
                              "Stanton",
                              "Truxton",
                              "Hill",
                              "Columbia Hgts",
                              "Dupont",
                              "Edgewood",
                              "Shaw",
                              "Union Station"))
print(neigh_bar + ggtitle("Top 15 Most Popular AirBnB Neighborhoods")) # Print bar chart

# Scatterplot of mean price by neighborhood
price_table <-
  as.factor(neighborhood$neighborhood)
  group_by(neighborhood$neighborhood) %>%
  mutate(mean_price = mean(price, na.rm=T))




# Question 5
# Bin the number of reviews by 25 review bins
listing$bins <- cut(listing$number_of_reviews, breaks = seq(0, 750, 25))
listing <- subset(listing, number_of_reviews > 0) 
listing <- 
  listing %>%
  group_by(bins) %>%
  mutate(mean_price_bins = mean(price, na.rm=T)) # Take mean price by review bin

# Scatterplot of average price (by bin) vs. number of reviews
price_to_reviews <- ggplot(listing, aes(x = as.factor(bins), y = mean_price_bins)) + 
  geom_point() +
  theme_bw() +
  labs(x = "Number of Reviews",
       y = "Mean Price (by bin)") + 
  theme(axis.text.x = element_text(size = 7)) 
print(price_to_reviews + ggtitle("Mean Price (by bin) vs. Number of Reviews")) # Print scatterplot with title

# Scatterplot of price by number of reviews by room type
price_to_reviews_to_room <- ggplot(listing, aes(x = number_of_reviews, y = price, color = room_type)) +
  geom_point() + 
  theme_bw() +
  labs(x = "Number of Reviews",
       y = "Price",
       color = "Room Type")
print(price_to_reviews_to_room + facet_grid(.~room_type) + ggtitle("Scatterplot of Price by Number of Reviews")) # Print scatterplot with title
