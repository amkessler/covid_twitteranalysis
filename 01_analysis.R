# source: Stanford's Big Local News twitter data
# https://biglocalnews.org/

# import code below pulls data directly from the Stanford BLS site 
# an alternative if need be is archived verion stored in archived_data directory

library(tidyverse)
library(janitor)
library(lubridate)
library(tidytext)

#pull from live site:
raw <- read_csv("https://storage.googleapis.com/bln_prod/project/850c9bfc-7c02-4d48-b631-898a81ff4144/governors_20200328.csv?X-Goog-Algorithm=GOOG4-RSA-SHA256&X-Goog-Credential=bln-storage%40big-local-news-267923.iam.gserviceaccount.com%2F20200330%2Fauto%2Fstorage%2Fgoog4_request&X-Goog-Date=20200330T211143Z&X-Goog-Expires=86400&X-Goog-SignedHeaders=host&X-Goog-Signature=32f102ae691018635cdcee442b2b6aee8cc7e1fe73f3ea00fbf7dd96ee23c3fc807f1622fbd38f1ffa047474a521bd80938f7118333e891f957b5cbe4acc045287ad3b30ab0a85816225972912010fdf6fb18760ae6f017ba6b53de342c31420eafc56c66290dfac748f75e367b3e7e258b083ab85a8000a856765831f064292703db47d742ad876bf5affabb8f164dedded32197e303728b65525c8bc6464272bc4dadfe344c9795cdf2b48de75d58cfa49834f1fceadc15cb104dc99dee5049784baab37e8cff7a7bc5ae832ce31244d5f404c3698a864569d017e6f02a08ced9680f1f7be57dd0eeefc0bc04957e561d4f4a91487adeba80aa2a3f11e16d1",
                 col_types = cols(.default = "c"))

#convert date/time column
twdata_all <- raw %>% 
  mutate(
    created_at = ymd_hms(created_at)
  )

#-----

#pull from archived file
# twdata_all <- readRDS("archived_data/twitterdata.rds")

#----


glimpse(twdata_all)

#create party abbreviation and speaker column for identification
twdata_all <- twdata_all %>% 
  mutate(
    party_abbrev = str_sub(party_affiliation, 1L, 1L),
    speaker = paste0(governor, " (", abbreviation, "-", party_abbrev, ")")
  )

#missing parties?
twdata_all %>% 
  filter(is.na(party_abbrev)) %>% 
  count(state) %>% 
  View()

twdata_all %>% 
  filter(state == "Ohio") %>% 
  count(party_affiliation)

## This clearly is a shortcoming in the data - we'll have to fix this
# Will turn to:
# https://ballotpedia.org/Partisan_composition_of_governors

# Import processed governors table from saved Gsheet:
governorslist <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vR612UAkJ68pjn6Wh-ndohpUiugtT1SyTkqw8PBegzwuphh9Efyp711frrmdD7nLOnxnw778iTY3mpw/pub?gid=0&single=true&output=csv")



#some quick gropuings explore

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




###### TEXTUAL ANALYSIS #######


selectedcols <- twdata_all %>%
  select(speaker, text)


#list the individual speakers
selectedcols %>% 
  count(speaker)




#begin the text analysis - FREQUENCY COUNTS ---------------------------------------------

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
  filter(!str_detect(word, "[0-9]")) # remove numbers


#pull top ones for each speaker into table
top_word_per_speaker <- speaker_words %>%
  select(-total) %>% 
  group_by(speaker) %>%
  top_n(15) %>%
  slice(1:10) %>% #added to limit to 10 records per cand, even with ties 
  ungroup

top_word_per_speaker

#save to file
write_csv(top_word_per_speaker, "output/top_singleword_perspeaker.csv")



### NOW BI-GRAMS VERSION -------------------------------

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
  filter(!str_detect(word2, "[0-9]")) # remove numbers# remove numbers

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(speaker, word1, word2, sort = TRUE)

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_united

#saved back to original name
speaker_bigrams <- bigrams_united


#pull top ones for each speaker into table
top_bigrams_per_speaker <- speaker_bigrams %>%
  group_by(speaker) %>%
  top_n(15) %>%
  slice(1:10) %>% #added to limit to 10 records per cand, even with ties 
  ungroup

top_bigrams_per_speaker

#save to file
write_csv(top_bigrams_per_speaker, "output/top_bigrams_perspeaker.csv")





### TD-IDF ANALYSIS --------------------------------------------------------

speaker_words <- speaker_words %>%
  bind_tf_idf(word, speaker, n)

speaker_words

# idf and thus tf-idf are zero for these extremely common words, so the idf term (which will then be the
# natural log of 1) is zero. The inverse document frequency (and thus tf-idf) is very low (near zero) for words that 
# occur in many of the documents in a collection; this is how this approach decreases the weight for common words. 
# The inverse document frequency will be a higher number for words that occur in fewer of the documents in the 
# collection.

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




### NOW BI-GRAMS VERSION -------------------------------

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
  filter(!str_detect(word2, "[0-9]")) # remove numbers# remove numbers

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(speaker, word1, word2, sort = TRUE)

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_united

#saved back to original name
speaker_bigrams <- bigrams_united


# TF-IDF #### ---

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


#visualizing bi-grams ####
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

