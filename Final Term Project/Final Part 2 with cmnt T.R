# === Step 1: Install and Load Required Libraries ===
install.packages(c("tm", "topicmodels", "ggplot2", "tidytext", "dplyr", "readr", "tidyverse"))
library(tm)
library(topicmodels)
library(ggplot2)
library(tidytext)
library(dplyr)
library(readr)
library(tidyverse)

# === Step 2: Load Cleaned CSV File ===
ittefaq_data <- read_csv("ittefaq_news_cleaned.csv")

# === Step 3: Create Corpus from Processed Description ===
corpus <- Corpus(VectorSource(ittefaq_data$processed_description))

# Preprocess: lower, remove punctuation, numbers, stop words, whitespace
corpus <- corpus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("en")) %>%
  tm_map(stripWhitespace)

# === Step 4: Create Document-Term Matrix ===
dtm <- DocumentTermMatrix(corpus)
dtm <- removeSparseTerms(dtm, 0.99)  # Optional: Reduce sparsity


# === Step 5: Apply LDA Topic Modeling ===
num_topics <- 10
lda_model <- LDA(dtm, k = num_topics, control = list(seed = 1234))

# === Step 6: Extract Term Probabilities Per Topic ===
term_probs <- tidy(lda_model)

View(term_probs)

# === Step 7: View Top Terms Per Topic ===
top_terms <- term_probs %>%
  group_by(topic) %>%
  slice_max(beta, n = 5) %>%
  ungroup() %>%
  mutate(term = reorder_within(term, beta, topic))

View(top_terms)

# === Step 7.1: Summarize top 5 words per topic as a single string ===
top_words_per_topic <- top_terms %>%
  group_by(topic) %>%
  summarise(top_words = paste(term, collapse = ", ")) %>%
  arrange(topic)

# Print or View the summary table
print(top_words_per_topic)
View(top_words_per_topic)


# === Step 8 (Alternative): Show Top Words with Beta per Topic ===
top_terms_table <- term_probs %>%
  group_by(topic) %>%
  arrange(desc(beta)) %>%       # Sort by beta within each topic
  slice_max(beta, n = 10) %>%   # Get top 10 terms (you can change this number)
  ungroup()

# View in RStudio viewer or print in console
View(top_terms_table)
print(top_terms_table)


# === Step 9: Get Dominant Topic for Each Document ===
doc_topics <- tidy(lda_model, matrix = "gamma")  # Document-topic probabilities

# Assign most likely topic per document
doc_max_topic <- doc_topics %>%
  group_by(document) %>%
  slice_max(gamma, n = 1) %>%
  ungroup()

View(doc_topics)
View(doc_max_topic)

# Convert document ID to numeric
doc_max_topic$document <- as.integer(doc_max_topic$document)

# === Step 10: Join Topic Back to Data ===
ittefaq_data_with_topics <- ittefaq_data %>%
  mutate(document = row_number()) %>%
  left_join(doc_max_topic, by = "document")

# === Step 11: (Optional) Assign Human-Readable Topic Names ===
topic_labels <- data.frame(
  topic = 1:10,
  topic_name = c(
    "Politics", "Sports", "Entertainment", "Economy", "Crime",
    "Health", "Education", "Weather", "Technology", "International"
  )
)

ittefaq_data_named <- ittefaq_data_with_topics %>%
  left_join(topic_labels, by = "topic")

# === Step 12: Save Final Dataset and View ===
write_csv(ittefaq_data_named, "ittefaq_news_with_named_topics.csv")
View(ittefaq_data_named)
