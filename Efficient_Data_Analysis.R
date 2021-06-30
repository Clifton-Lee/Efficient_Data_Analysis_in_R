
library(Amelia)
library(tidyverse)


## Data Understanding

dataset <- read_csv("book_reviews.csv") # Import the dataset

dim(dataset)

colnames(dataset)

for (name in colnames(dataset)){
  print(paste(name, " column is of type ", typeof(dataset[[name]])))
}


list_of_columns <- list()
for (name in colnames(dataset)){
  list_of_columns[[name]] <- unique(dataset[[name]])
}
list_of_columns


## Data Preprocessing 

### Missing Data 


missmap(dataset, col = c("black", "grey"))

new_dataset <- dataset %>% 
               filter(!is.na(review))

dim(new_dataset)

### Noise Handling


list_of_columns$state

new_dataset <- new_dataset %>% 
               mutate(state_name = case_when(state == "TX" ~ "Texas",
                                             state == "NY" ~ "New York",
                                             state == "FL" ~ "Florida",
                                             state == "CA" ~ "California",
                                             TRUE ~ state)) %>% 
               select(-state)

head(new_dataset, 10) # Examine the modifications to confirm they were made appropriately.

### Feature Modification


new_dataset <- new_dataset %>% 
               mutate(review_num = case_when(review == "Poor" ~ 1,
                                             review == "Fair" ~ 2,
                                             review == "Good" ~ 3,
                                             review == "Great" ~ 4,
                                             review == "Excellent" ~ 5),
                      is_high_review = if_else(review_num >= 4, TRUE, FALSE)) %>% 
              select(-review)

new_dataset

## Data Analysis 


most_profitable <- new_dataset %>% 
                   group_by(book) %>% 
                   summarize(amount_sold = n(),
                             total_sales_value = sum(price),
                             typical_rating = median(review_num)) %>% 
                   arrange(desc(total_sales_value))

most_profitable

