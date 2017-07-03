library(gutenbergr)
library(tidytext)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggrepel)

get_book_summary <- function(id){
  book <- gutenberg_download(id, meta_fields = c("title", "author"))
  print(book$title[1])
  words<- unnest_tokens(book, word, text)
  nwords <- nrow(words)
  avg_word_len <-  mean(nchar(words$word))
  var_word_len <- var(nchar(words$word))
  distinct_words <- length(unique(words$word))
  sample_1k <- sample_n(words, 1000)
  avg_word_len_1k_smpl <- mean(nchar(sample_1k$word))
  distinct_words_1k <- length(unique(sample_1k$word))
  sample_10k <- sample_n(words, 10000)
  avg_word_len_10k_smpl <- mean(nchar(sample_10k$word))
  distinct_words_10k <- length(unique(sample_10k$word))
  sentences <- book %>% 
    unnest_tokens(sentence, text, token = "sentences")%>%
    mutate(sen_length = sapply(sentence,
                               function(x) strsplit(x, " ") %>%
                                 unlist %>% length))
  nsentences <- nrow(sentences)
  avg_wps <- mean(sentences$sen_length)
  var_wps <- var(sentences$sen_length)
  out <- list(title=book$title[1], author=book$author[1], 
              nwords=nwords, avg_word_len=avg_word_len, 
              var_word_len=var_word_len,
              distinct_words=distinct_words, 
              avg_word_len_1k = avg_word_len_1k_smpl,
              distinct_words_1k = distinct_words_1k,
              avg_word_len_10k = avg_word_len_10k_smpl,
              distinct_words_10k = distinct_words_10k,
              nsentences=nsentences, 
              avg_wps=avg_wps, var_wps=var_wps)
  rm(book, words, sentences, sample_1k, sample_10k)
  gc()
  out
}

replace_title <- function(table, main_title){
  mutate(table, title = ifelse(str_detect(title, main_title),
                               main_title, title))
}

###################
bbc2015_top100 <- read.csv("~/bbc2015_top100.csv")

found_works <- gutenberg_works(title %in% bbc2015_top100$Title, 
                               language=="en") 
ids <- found_works %>% 
  select(gutenberg_id)

system.time(summary_table <- lapply(ids$gutenberg_id, get_book_summary) %>%
              bind_rows())

summary_table <- left_join(summary_table, bbc2015_top100, by=c("title"="Title"))
summary_table <- mutate(summary_table, Year = as.character(Year) %>% as.numeric)
summary_table <- filter(summary_table, Year < 1925)

write.csv(summary_table, "bbc2015_summary.csv")

###################
summary_table <- read.csv("bbc2015_summary.csv")
summary_table <- mutate(summary_table, title=as.character(title))
summary_table <- mutate(summary_table, ttratio = distinct_words/nwords,
                        ttratio_1k = distinct_words_1k/1000,
                        ttratio_10k = distinct_words_10k/10000)
for(main_title in c("Robinson Crusoe", "Moll Flanders", "Tristram Shandy",
                    "Gulliver's Travels", "Frankenstein", "Nostromo",
                    "Jane Eyre", "Tess of the d'Urbervilles", "Jane Eyre")){
  summary_table <- replace_title(summary_table, main_title)
}

ggplot(summary_table, aes(x=nwords, y=avg_wps, label=title)) + 
  geom_point(aes(col=avg_word_len), size=4) + 
  geom_label_repel(size=5) +
  scale_color_distiller(palette="Spectral")+
  labs(x="Total word count", y="Average number of words per sentence",
       col="Average word length") +
  theme(axis.text=element_text(size=14), 
        axis.title=element_text(size=14),
        legend.title = element_text(size=14),
        legend.text = element_text(size=14))
