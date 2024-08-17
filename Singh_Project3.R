# Singh_Project3.R
# Sneha Singh
# 07/30/2024
# ALY 6000

# Load necessary libraries
library(tidyverse)
library(janitor)
library(lubridate)

# Read the dataset
books <- read.csv("/mnt/data/books.csv") 
books <- read_csv("books.csv")

# Step 1: Clean the column names
books <- clean_names(books)

# Step 2: Convert `first_publish_date` to date
books$first_publish_date <- mdy(books$first_publish_date)

# Step 3: Extract the year from `first_publish_date`
books <- books %>%
  mutate(year = year(first_publish_date))

# Step 4: Filter books published between 1990 and 2020
books <- books %>%
  filter(year >= 1990 & year <= 2020)

# Step 5: Remove specified columns
books <- books %>%
  select(-publish_date, -edition, -characters, -price, -genres, -setting, -isbn)

# Step 6: Keep only books with fewer than 700 pages
books <- books %>%
  filter(pages < 700)

# Step 7: Remove rows with NAs
books <- books %>%
  drop_na()

# Create avg_rating_by_year data frame
avg_rating_by_year <- books %>%
  group_by(year) %>%
  summarize(avg_rating = mean(rating))

ggplot(avg_rating_by_year, aes(x = year, y = avg_rating)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Average Book Rating by Year", x = "Year", y = "Average Rating")

avg_rating_by_year <- books %>%
  group_by(year) %>%
  summarize(avg_rating = mean(rating))

ggplot(avg_rating_by_year, aes(x = year, y = avg_rating)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Average Book Rating by Year", x = "Year", y = "Average Rating")

# Create the by_year data frame
by_year <- books %>%
  count(year, name = "total_books")

# Check the contents of by_year
head(by_year)
str(by_year)

# Plot the total number of books per year
ggplot(by_year, aes(x = year, y = total_books)) +
  geom_line() +
  geom_point() +
  labs(title = "Total Number of Books Rated Per Year", x = "Year", y = "Total Books")


# Loading dataset
head(books)
str(books)

# Create the book_publisher data frame
book_publisher <- books %>%
  count(publisher, name = "book_count") %>%
  filter(book_count >= 125) %>%
  arrange(desc(book_count)) %>%
  mutate(cum_counts = cumsum(book_count),
         rel_freq = book_count / sum(book_count),
         cum_freq = cumsum(rel_freq)) %>%
  mutate(publisher = factor(publisher, levels = publisher))

# Check the contents of book_publisher
head(book_publisher)
str(book_publisher)





# Step 8: Use `glimpse` to produce a long view of the dataset
glimpse(books)

# Step 9: Use `summary` to produce a breakdown of the statistics
summary(books)

# Step 10: Create a rating histogram
ggplot(books, aes(x = rating)) +
  geom_histogram(binwidth = 0.25, fill = "red") +
  labs(title = "Histogram of Book Ratings", x = "Rating", y = "Number of Books")

# Step 11: Create a boxplot of the number of pages per book
ggplot(books, aes(x = pages)) +
  geom_boxplot(fill = "red") +
  coord_flip() +
  labs(title = "Box Plot of Page Counts", x = "Pages")

# Step 12: Create a data frame `by_year` with a count of books by year
by_year <- books %>%
  count(year, name = "total_books")

# Step 13: Create a line plot with points for the counts per year
ggplot(by_year, aes(x = year, y = total_books)) +
  geom_line() +
  geom_point() +
  labs(title = "Total Number of Books Rated Per Year", x = "Year", y = "Total Books")

# Step 14: Create a data frame `book_publisher` with count of titles per publisher
book_publisher <- books %>%
  count(publisher, name = "book_count")

# Step 15: Remove publishers with fewer than 125 books
book_publisher <- book_publisher %>%
  filter(book_count >= 125)

# Step 16: Order `book_publisher` by total number of books in descending order
book_publisher <- book_publisher %>%
  arrange(desc(book_count))

# Step 17: Add a column `cum_counts` with the cumulative sum of `book_count`
book_publisher <- book_publisher %>%
  mutate(cum_counts = cumsum(book_count))

# Step 18: Add a column `rel_freq` with the relative frequency of `book_count`
book_publisher <- book_publisher %>%
  mutate(rel_freq = book_count / sum(book_count))

# Step 19: Add a column `cum_freq` with the cumulative sum of `rel_freq`
book_publisher <- book_publisher %>%
  mutate(cum_freq = cumsum(rel_freq))

# Step 20: Make the `publisher` column into a factor with levels defined by current ordering
book_publisher$publisher <- factor(book_publisher$publisher, levels = book_publisher$publisher)

# Step 21: Create a Pareto Chart with an ogive of cumulative counts
ggplot(book_publisher, aes(x = publisher, y = book_count)) +
  geom_bar(stat = "identity", fill = "cyan") +
  geom_line(aes(y = cum_counts), group = 1, color = "red") +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Cumulative Count")) +
  labs(title = "Book Counts (1990 - 2020)", x = "Publisher", y = "Number of Books") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Step 22: Create an additional visualization
# Example: Bar plot of average rating by year
avg_rating_by_year <- books %>%
  group_by(year) %>%
  summarize(avg_rating = mean(rating))

ggplot(avg_rating_by_year, aes(x = year, y = avg_rating)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Average Book Rating by Year", x = "Year", y = "Average Rating")

