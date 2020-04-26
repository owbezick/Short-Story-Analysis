source("libraries.R", local = TRUE)

# Import Leader pdf file ----
raw_lines <- pdf_text("stories/ernest_hemmingway.pdf") %>%
  read_lines()

# Text Preprocessing ----
# Extract interview Lines
raw_story <- raw_lines[3:144]

# Append into single string
story_str= ""
for(line in raw_story) {
  story_str = paste(story_str, line)
}

# Make corpus of answers
v_source <- VectorSource(story_str)
v_corpus <- VCorpus(v_source)

# Clean Corpus Function
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removePunctuation) 
  corpus <- tm_map(corpus, content_transformer(replace_contraction))
  corpus <- tm_map(corpus, content_transformer(replace_number))
  return(corpus)
}

# Clean Answers
cleaned_story <- clean_corpus(v_corpus)

# Create term-doc matrix
tdm <- TermDocumentMatrix(cleaned_story)
tdm_matrix <- as.matrix(tdm)

# # Term frequency
q_term_freq <- sort(rowSums(tdm_matrix), decreasing = T)

# # Show top 100
q_term_freq[1:100]

# Create dataframe
story_df <- data.frame(text=unlist(sapply(cleaned_story, `[`, "content")), 
                            stringsAsFactors=F) 
# Pull just the text
story_text <- story_df %>% 
  select(text) %>%
  pull()

# Get NRC sentiment
story_sentiment <- get_nrc_sentiment(story_text)


# Sentence by Sentence Sentiment
sentences <- tokenize_sentences(story_str)

sentences_df <- data_frame(
  sentences = unlist(sentences)
)

sentence_char_vector <- sentences_df %>%
  select(sentences) %>%
  pull()

sentence_sentiment <- get_nrc_sentiment(sentence_char_vector)

merged <- merge(sentences_df, sentence_sentiment, by = 0) %>%
  mutate(sentence = as.numeric(Row.names))

merged <- merged %>%
  mutate(total_sentiment = negative+anger+disgust+fear+sadness+anticipation+joy+surprise+trust+positive)

sentences_by_sentiment <- merged[order(merged$total_sentiment, decreasing= T),]
sentences_by_story <- merged[order(merged$sentence),]

# Sentiment Bar Chart Function
sentimentBar <- function(df, title){
  df %>%
    e_chart(chart) %>%
    e_bar("negative", name = "Negative", color = "#543005") %>%
    e_bar("anger", name = "Anger", color = "#8c510a") %>%
    e_bar("disgust", name = "Disgust", color = "#bf812d") %>%
    e_bar("fear", name = "Fear", color = "#dfc27d") %>%
    e_bar("sadness", name = "Sadness", color = "#f6e8c3") %>%
    e_bar("anticipation", name= "Anticipation", color = "#c7eae5") %>%
    e_bar("joy", name = "Joy", color = "#80cdc1") %>%
    e_bar("surprise", name = "Surprise", color = "#35978f") %>%
    e_bar("trust", name = "Trust", color = "#01665e") %>%
    e_bar("positive", name = "Positive", color = "#003c30") %>%
    e_axis_labels(x = "Emotion", y = "Number of Tags") %>%
    e_title(title) %>%
    e_legend(bottom = 0) %>%
    e_tooltip()
}
