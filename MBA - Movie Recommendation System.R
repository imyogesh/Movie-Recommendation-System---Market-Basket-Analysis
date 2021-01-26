movie_subset <- read.csv("Movie_subset.csv")

# Have a glimpse at the dataset
head(movie_subset)

# Calculate the number of distinct users and movies
n_distinct(movie_subset$userId)
n_distinct(movie_subset$movieId)

# Distribution of the number of movies watched by users
movie_subset %>%
  group_by(userId) %>% 
  summarise(nb_movies = n_distinct(movieId)) %>%
  ggplot(aes(x = nb_movies)) +
  geom_bar() + 
  ggtitle("Distribution of number of movies watched")

# Split dataset into movies and users
data_list = split(movie_subset$title, movie_subset$userId)

library(arules)

# Transform data into a transactional dataset
movie_trx = as(data_list, "transactions")

# Plot of the item matrix
image(movie_trx[1:100,1:100])

# Plot the absolute item frequency plot
itemFrequencyPlot(movie_trx,
                  type = "absolute",
                  topN = 10,
                  horiz = TRUE,
                  main = 'Absolute item frequency')

# Plot the 10 least popular items
barplot(sort(table(unlist(LIST(movie_trx))))[1:10],
        horiz = TRUE,
        las = 1,
        main = 'Least popular items')

# Extract the set of most frequent itemsets
itemsets = apriori(movie_trx,
                   parameter = list(support = 0.4,
                                    target = 'frequent'
                   ))

# Inspect the five most popular items
inspect(sort(itemsets, by='support', decreasing = T)[1:5])

# Set of confidence levels
confidenceLevels = seq(from=0.95, to=0.5, by=-0.05)

# Create empty vector
rules_sup04 = NULL

# Apriori algorithm with a support level of 40%
for (i in 1:length(confidenceLevels)) {
  rules_sup04[i] = 
    length(apriori(movie_trx,
                   parameter=list(sup=0.4, 
                                  conf=confidenceLevels[i],
                                  target="rules")))
}

# Number of rules found with a support level of 40%
qplot(confidenceLevels,rules_sup04 , 
      geom=c("point", "line"),xlab="Confidence level",
      ylab="Number of rules found", 
      main="Apriori with a support level of 40%") +
  theme_bw()

# Extract rules with the apriori
rules_movies = apriori(movie_trx,
                       parameter = list(supp = 0.3,
                                        conf = 0.9,
                                        minlen = 2, 
                                        target = "rules"))

# Summary of extracted rules
summary(rules_movies)


# Create redudant rules and filter from extracted rules
rules_red = is.redundant(rules_movies)
rules.pruned = rules_movies[!rules_red]
# Inspect the non-redundant rules with highest confidence
inspect(head(sort(rules.pruned, by="confidence")))

library(arulesViz)

# Plot movie rules as a graph
plot(rules_movies,
     method = "graph",
     engine = "htmlwidget")

