# source: Stanford's Big Local News twitter data
# https://biglocalnews.org/

library(tidyverse)
library(janitor)
library(lubridate)
library(tidytext)
library(tidycensus)

#load saved data from step 00
twdata_all <- readRDS("archived_data/twitterdata_governors.rds")


#some quick groupings explore

twdata_all %>% 
  count(party_affiliation)

twdata_all %>% 
  count(state)

#date ranges
twdata_all %>% 
  arrange(created_at) %>% 
  head(10)

twdata_all %>% 
  arrange(desc(created_at)) %>% 
  head(10)


## try to extract  the urls within the tweet text ####
# test <- twdata_all %>% filter(state == "Alabama")
# 
# test %>% 
#   select(text)
# 
# test %>% 
#   mutate(
#     urltext = str_trim(str_extract(text, "http.*"))
#   ) %>% 
#   select(urltext, text) %>% 
#   View()

# hmm, this is a little trickier because the links aren't always the last thing in the text
# ***will return to this



### filter for just original tweets - no retweets
twdata_all_originalonly <- twdata_all %>% 
  filter(is.na(retweet_text))



###### TEXTUAL ANALYSIS ####### --------------------------------


selectedcols <- twdata_all_originalonly %>%
  select(speaker, text)


#list the individual speakers
selectedcols %>% 
  count(speaker)



# SINGLE WORDS #### ----------------------

speaker_words <- selectedcols %>%
  unnest_tokens(word, text) %>%
  count(speaker, word, sort = TRUE)

total_words <- speaker_words %>% 
  group_by(speaker) %>% 
  summarize(total = sum(n))

speaker_words <- left_join(speaker_words, total_words)

speaker_words

# remove stop words
data(stop_words)

speaker_words <- speaker_words %>%
  anti_join(stop_words) %>% 
  filter(!str_detect(word, "[0-9]"), #remove numbers
         !str_detect(word, "http*"), 
         !str_detect(word, "t.co")) 

#aiming to remove references to state names or abbreviations
#since many tweets include references to their own states
#first-- create lookup tables from tidycensus' fips table
statelookup <- fips_codes %>% 
  distinct(state, state_name) %>% 
  mutate(state = str_to_lower(state),
         state_name = str_to_lower(state_name))

#create dfs of our values
state_abbs <- statelookup %>% select(word = state)

state_names <- statelookup %>% 
  separate_rows(state_name) %>% 
  distinct(state_name) %>% 
  select(word = state_name)

#run the anti-joins
speaker_words <- speaker_words %>%
  anti_join(state_abbs)
speaker_words <- speaker_words %>%
  anti_join(state_names)

#compile governors' own names and remove those words as well
govnames <- twdata_all_originalonly %>% 
  mutate(
    governor = str_trim(str_to_lower(governor))
  ) %>% 
  separate_rows(governor) %>% 
  select(word = governor) %>% 
  distinct(word)

speaker_words <- speaker_words %>%
  anti_join(govnames)





### BI-GRAMS #### -------------------

speaker_bigrams <- selectedcols %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  count(speaker, bigram, sort = TRUE)

total_words <- speaker_bigrams %>% 
  group_by(speaker) %>% 
  summarize(total = sum(n))

speaker_bigrams <- left_join(speaker_bigrams, total_words)

speaker_bigrams

#remove stop words as either of the two words
bigrams_separated <- speaker_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  filter(!str_detect(word1, "[0-9]")) %>% 
  filter(!str_detect(word2, "[0-9]")) %>% # remove numbers
  filter(!str_detect(word1, "http*")) %>% 
  filter(!str_detect(word2, "http*")) %>%
  filter(!str_detect(word1, "t.co")) %>% 
  filter(!str_detect(word2, "t.co")) 


# remove state and governor names using dfs created in previous section

#run the anti-joins on both word1 and word2 to capture both placements
bigrams_filtered <- bigrams_filtered %>%
  anti_join(state_abbs, by = c("word1" = "word"))

bigrams_filtered <- bigrams_filtered %>%
  anti_join(state_abbs, by = c("word2" = "word"))

bigrams_filtered <- bigrams_filtered %>%
  anti_join(state_names, by = c("word1" = "word"))

bigrams_filtered <- bigrams_filtered %>%
  anti_join(state_names, by = c("word2" = "word"))

bigrams_filtered <- bigrams_filtered %>%
  anti_join(govnames, by = c("word1" = "word"))

bigrams_filtered <- bigrams_filtered %>%
  anti_join(govnames, by = c("word2" = "word"))



# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(speaker, word1, word2, sort = TRUE)

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_united

#saved back to original name
speaker_bigrams <- bigrams_united



################################################################################


#### FREQUENCY COUNTS #######

#pull top SINGLE words for each speaker into table
top_word_per_speaker <- speaker_words %>%
  select(-total) %>% 
  group_by(speaker) %>%
  top_n(15) %>%
  slice(1:10) %>% #added to limit to 10 records per cand, even with ties 
  ungroup

top_word_per_speaker

#save to file
write_csv(top_word_per_speaker, "output/top_singleword_perspeaker.csv")



#pull top BIGRAMS for each speaker into table
top_bigrams_per_speaker <- speaker_bigrams %>%
  group_by(speaker) %>%
  top_n(15) %>%
  slice(1:10) %>% #added to limit to 10 records per cand, even with ties 
  ungroup

top_bigrams_per_speaker

#save to file
write_csv(top_bigrams_per_speaker, "output/top_bigrams_perspeaker.csv")





#####  TD-IDF ANALYSIS #############

# idf and thus tf-idf are zero for these extremely common words, so the idf term (which will then be the
# natural log of 1) is zero. The inverse document frequency (and thus tf-idf) is very low (near zero) for words that 
# occur in many of the documents in a collection; this is how this approach decreases the weight for common words. 
# The inverse document frequency will be a higher number for words that occur in fewer of the documents in the 
# collection.


### TF-IDF SINGLE WORDS ####

speaker_words <- speaker_words %>%
  bind_tf_idf(word, speaker, n)

speaker_words


speaker_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))


#pull top ones for each speaker into table
top_tfidf_per_speaker <- speaker_words %>%
  group_by(speaker) %>%
  top_n(15) %>%
  slice(1:10) %>% #added to limit to 10 records per cand, even with ties 
  ungroup

top_tfidf_per_speaker

#save to file
write_csv(top_tfidf_per_speaker, "output/tfidf_singleword_perspeaker.csv")


#visualizing 
speaker_tfidf_chart <- speaker_words %>%
  group_by(speaker) %>%
  top_n(15) %>%
  slice(1:10) %>% #added to limit to 10 records per cand, even with ties 
  ungroup %>%
  mutate(speaker = as.factor(speaker),
         word = reorder_within(word, tf_idf, speaker)) %>% #use the new reorder_within() func
  ggplot(aes(word, tf_idf, fill = speaker)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~speaker, ncol = 2, scales = "free") +
  coord_flip() +
  scale_x_reordered() # +
# scale_y_continuous(expand = c(0,0))

speaker_tfidf_chart 

# ggsave("img/speaker_tfidf_chart.jpg", speaker_tfidf_chart)
# ggsave("img/speaker_tfidf_chart.pdf", speaker_tfidf_chart)


#-----------


### TF-IDF BIGRAMS ######

speaker_bigrams_tfidf <- speaker_bigrams %>%
  bind_tf_idf(bigram, speaker, n)

speaker_bigrams_tfidf

speaker_bigrams_tfidf %>%
  select(-total) %>%
  arrange(desc(tf_idf)) 


#pull top ones for each speaker into table
top_tfidf_per_speaker_bigrams <- speaker_bigrams_tfidf %>%
  group_by(speaker) %>%
  top_n(15) %>%
  slice(1:10) %>% #added to limit to 10 records per cand, even with ties 
  ungroup

top_tfidf_per_speaker_bigrams

#save to file
write_csv(top_tfidf_per_speaker_bigrams, "output/tfidf_bigrams_speaker.csv")


#visualizing TF-IDF bi-grams ####
speaker_tfidf_chart_bigrams <- speaker_bigrams_tfidf %>%
  group_by(speaker) %>%
  top_n(10) %>%
  slice(1:10) %>% #added to limit to 10 records per cand, even with ties 
  ungroup %>%
  mutate(speaker = as.factor(speaker),
         bigram = reorder_within(bigram, tf_idf, speaker)) %>% #use the new reorder_within() func
  ggplot(aes(bigram, tf_idf, fill = speaker)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~speaker, ncol = 2, scales = "free") +
  coord_flip() +
  scale_x_reordered() # +
# scale_y_continuous(expand = c(0,0))

speaker_tfidf_chart_bigrams 

#save chart images to file
# ggsave("img/speaker_tfidf_chart_bigrams.jpg", speaker_tfidf_chart_bigrams)
# ggsave("img/speaker_tfidf_chart_bigrams.pdf", speaker_tfidf_chart_bigrams)

