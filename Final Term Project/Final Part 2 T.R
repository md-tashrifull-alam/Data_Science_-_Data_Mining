install.packages(c("tm", "topicmodels", "ggplot2", "tidytext", "dplyr", "readr", "tidyverse"))
library(tm)
library(topicmodels)
library(ggplot2)
library(tidytext)
library(dplyr)
library(readr)
library(tidyverse)


ittefaq_data <- read_csv("ittefaq_news_cleaned.csv")


corpus <- Corpus(VectorSource(ittefaq_data$processed_description))


corpus <- corpus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("en")) %>%
  tm_map(stripWhitespace)


dtm <- DocumentTermMatrix(corpus)
dtm <- removeSparseTerms(dtm, 0.99) 


num_topics <- 10
lda_model <- LDA(dtm, k = num_topics, control = list(seed = 1234))


term_probs <- tidy(lda_model)

View(term_probs)


top_terms <- term_probs %>%
  group_by(topic) %>%
  slice_max(beta, n = 5) %>%
  ungroup() %>%
  mutate(term = reorder_within(term, beta, topic))

View(top_terms)


top_words_per_topic <- top_terms %>%
  group_by(topic) %>%
  summarise(top_words = paste(term, collapse = ", ")) %>%
  arrange(topic)


print(top_words_per_topic)
View(top_words_per_topic)


ggplot(top_terms, aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free", ncol = 2) +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "Top Terms per Topic (Ittefaq News)",
       x = "Terms", y = "Beta (Probability)") +
  theme_minimal()


doc_topics <- tidy(lda_model, matrix = "gamma")  


doc_max_topic <- doc_topics %>%
  group_by(document) %>%
  slice_max(gamma, n = 1) %>%
  ungroup()

View(doc_topics)
View(doc_max_topic)


doc_max_topic$document <- as.integer(doc_max_topic$document)


ittefaq_data_with_topics <- ittefaq_data %>%
  mutate(document = row_number()) %>%
  left_join(doc_max_topic, by = "document")


topic_labels <- data.frame(
  topic = 1:10,
  topic_name = c(
    "Politics", "Sports", "Entertainment", "Economy", "Crime",
    "Health", "Education", "Weather", "Technology", "International"
  )
)

ittefaq_data_named <- ittefaq_data_with_topics %>%
  left_join(topic_labels, by = "topic")


write_csv(ittefaq_data_named, "ittefaq_news_with_named_topics.csv")
View(ittefaq_data_named)
