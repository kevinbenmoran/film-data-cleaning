set.seed(123)
library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(ggplot2)
options(scipen = 500)


# Set Working Directory.
setwd("~/Desktop/R Projects/film-data-cleaning")
# Read in initial data, set type as double to avoid error due to huge budgets in yen.
raw <- read_csv("raw.csv", col_types = cols(budget = col_double()))

# Arrange by year, create unique film identifier.
raw <- arrange(raw, title_year, movie_title)
raw$film_id <- c(1:nrow(raw))
raw <- select(raw, film_id, 1:28)
raw$movie_title <- str_trim(raw$movie_title)

# -------------Functions-------------------


get_unique <- function (x, col_nums) {
  # Create function to get list of unique values over multiple specified columns, and assign unique ids.
  #
  # Args:
  #   x: The dataset containing the columns the operation is to be performed on
  #   col_nums: A vector of the column numbers to find unique values in: i.e c(1,4,8)
  #
  # Returns:
  #   A dataframe containing two columns, one containing the unique values, and one with unique ids    
  #
  
  full_list <- unlist(map(x[col_nums], unlist))
  unique_values <-na.omit(unique(full_list))
  l <- length(unique_values)
  
  df <- data_frame(unique_values)
  df <- arrange(df, unique_values)
  id <- c(1:l)
  
  df$id <- id
  df <- select(df, id, unique_values)
  names(df)[2] <- "name"
  df
}

# Function to prepare actor/director columns for binding.
get_list <- function(x, col_name, r) {
  d <- select(x, film_id, col_name) %>%
    mutate(role = r)
  names(d)[2] <- "name"
  d
}


# Create people table and assign unique id.
people <- get_unique(raw, c(3, 8, 12, 16))
names(people)[1] <- "person_id"





# ---------------Roles---------------

# Form individual lists with necessary information for each column, then combine them, specifying role.
# Come back and do this using map function.
actor_1 <- get_list(raw, 12, "actor")
actor_2 <- get_list(raw, 8, "actor")
actor_3 <- get_list(raw, 16, "actor")
actors <- bind_rows(actor_1, actor_2, actor_3)

directors <- get_list(raw, 3, "director")


# Merge with film dataframe, and people dataframe to get person_ids.
actors <- merge(x = actors, y = people)
directors <- merge(x = directors, y = people)

# Combine actors and directors into one roles table, assign unique role id, and remove redundant columns.
roles <- rbind(actors, directors)
roles <- arrange(roles, film_id, person_id, role)
roles$id <- c(1:nrow(roles))
roles <- select(roles, id, film_id, person_id, role)



# ----------------Reviews---------------------

# Create reviews table.
reviews <- select(raw, 
                  film_id,
                  num_user = num_user_for_reviews,
                  num_critic = num_critic_for_reviews, 
                  imdb_score, 
                  num_votes = num_voted_users,
                  facebook_likes = movie_facebook_likes)

# Assign ids in random order so they don't match up exactly with film ids.
reviews$review_id <- sample(nrow(reviews))
reviews <- select(reviews, review_id, 1:7) %>%
  arrange(review_id)


# Leave review_id and film_id as is for merging with films table.


# ----------------Films------------------------ 

# Rename columns appropriately
films <- select(raw, 
                id = film_id,
                title = movie_title,
                release_year = title_year,
                duration,
                gross,
                budget,
                country,
                certification = content_rating,
                language,
                color)

films <- merge(x = films, y = reviews[,1:2], by.x = "id", by.y = "film_id")
films <- select(films, 
                id,
                title,
                release_year,
                country,
                duration,
                color,
                language,
                certification,
                gross,
                budget)


# Remove redundant columns and rename for desired output.
reviews <- select(reviews, id = review_id, 2:7)
names(people)[1] <- "id"
rm(actor_1, actor_2, actor_3, actors, directors)

films$title <- trimws((films$title))

# Write tables to csv.
write_csv(roles, "roles.csv", na = "")
write_csv(people, "people.csv", na = "")
write_csv(reviews, "reviews.csv", na = "")

# Use write.csv to avoid scientific notation in output.
write.csv(films, "films.csv", na = "", row.names = F)


#-------------Exploratory----------

glimpse(films)

profit <- select(films, id, title, gross, budget)
profit <- profit[complete.cases(profit),]

profit <- mutate(profit, difference = gross - budget, multiplier = gross/budget)
arrange(profit, desc(difference))
arrange(profit, desc(multiplier))

top_10_multiplier <- arrange(profit, desc(multiplier))[1:10,]
top_10_difference <- arrange(profit, desc(difference))[1:10,]

diff_plot <- ggplot(top_10_multiplier, aes(x = factor(title), y = multiplier, fill = title)) +
  geom_col() +
  coord_flip() +
  guides(fill = FALSE) +
  xlab("Movie") +
  ylab("Markup: Gross/Budget") +
  ggtitle("Top 10 movie markups") +
  theme(axis.text=element_text(size=12),
        title = element_text(size=14,face="bold")) +
  theme(plot.margin=unit(c(0.5,1,0.5,0.5),"cm"))

multiplier_plot <- ggplot(top_10_difference, aes(x = factor(title), y = difference, fill = title)) +
  geom_col() +
  coord_flip() +
  guides(fill = FALSE) +
  xlab("Movie") +
  ylab("Margin: Gross - Budget") +
  ggtitle("Top 10 movie margins") +
  theme(axis.text=element_text(size=12),
        title = element_text(size=14,face="bold")) +
  theme(plot.margin=unit(c(0.5,1,0.5,0.5),"cm"))

ggsave("difference.png", plot = diff_plot)
ggsave("multiplier.png", plot = multiplier_plot)
