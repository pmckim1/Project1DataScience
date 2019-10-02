library(readr)
library(readxl)
library(tidyverse)
library(ggplot2)
library(pander)
library(reshape2)
library(plyr)

listing <- read.csv("/Users/Jeffrey/Desktop/Math/DATS 6101/Project 1/listings.csv")

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

outlierKD(listing, price)
price_to_room_type <- ggplot(listing, aes(x = factor(room_type), y = price)) +
  geom_boxplot() + 
  theme_bw() +
  labs(x = "Room Type",
       y = "Price")
print(price_to_room_type + ggtitle("Boxplot of Price by Room Type"))

listing$bins <- cut(listing$number_of_reviews, breaks = seq(0, 750, 25))
listing <- subset(listing, number_of_reviews > 0) 
listing <- 
  listing %>%
  group_by(bins) %>%
  mutate(mean_price_bins = mean(price, na.rm=T))

price_to_reviews <- ggplot(listing, aes(x = as.factor(bins), y = mean_price_bins)) + 
  geom_point() +
  theme_bw() +
  labs(x = "Number of Reviews",
       y = "Mean Price (by bin)") + 
  theme(axis.text.x = element_text(size = 7)) 
print(price_to_reviews + ggtitle("Mean Price (by bin) vs. Number of Reviews"))

room_to_reviews <- ggplot(listing, aes(x = as.factor(room_type), y = number_of_reviews, color = room_type)) +
  geom_boxplot() + 
  theme_bw() + 
  labs(x = "Room Type",
       y = "Number of Reviews",
       color = "Room Type") 
print(room_to_reviews + ggtitle("Boxplot of Number of Reviews by Room Type"))

neighborhood_subset <- 
  listing %>%
  select(id, neighbourhood) %>%
  separate(neighbourhood, into = c("N1", "N2", "N3"), sep = ",") 
neighborhood_subset$bins <- NULL
neighborhood_subset <- na.omit(neighborhood_subset) 
neighborhood_subset <- melt(neighborhood_subset, id.vars = "id")
neighborhood_subset$variable <- NULL

price <- as.data.frame(cbind(listing$id, listing$price))
names(price) <- c("id", "price")
neighborhood_subset <- join(neighborhood_subset, price, by = id, type = "left", match = "all")



### ANOVA and Tukey Tests
anova_room_price <- aov(price ~ as.factor(room_type), data = listing)
pander(anova_room_price)

tukey_room_review <- TukeyHSD(anova_room_price)
tukey_room_review

anova_room_review <- aov(number_of_reviews ~ as.factor(room_type), data = listing)
pander(anova_room_review)

tukey_room_price <- TukeyHSD(anova_room_price)
tukey_room_price