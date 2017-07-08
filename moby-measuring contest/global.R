library(gutenbergr)
library(stringr)
library(tidytext)
library(dplyr)
library(stringi)

get_relevant_works <- function(search.field, search.query){
  search_regex <- regex(search.query, ignore_case=TRUE)
  if(search.field == 'Title'){
    results <- filter(gutenberg_metadata, 
                      str_detect(title, search_regex),
                      language=='en', has_text)
  }else{
    results <- filter(gutenberg_metadata, 
                      str_detect(author, search_regex),
                      language=='en', has_text)
  }
  if(nrow(results) == 0){
    c(`No results found` = NA)
  }else{
    id <- results$gutenberg_id
    out_names <- paste0(results$title, " (", results$author, ")")
    setNames(id, out_names)
  }
}

get_book_summary <- function(id){
  book <- gutenberg_download(id, meta_fields = c("title", "author"))
  if(!is.na(book$title[1])){
    book <- mutate(book, text=stri_enc_toutf8(text, TRUE, TRUE))
    words <- unnest_tokens(book, word, text)
    nwords <- nrow(words)
    avg_word_len <-  mean(nchar(words$word))
    var_word_len <- var(nchar(words$word))
    distinct_words <- length(unique(words$word))
    if(nrow(words) > 1000){
      start <- sample(1:(nrow(words)-1000), 1)
      sample_1k_cont <- words[start:(start+1000),]
      distinct_words_1k_cont <- length(unique(sample_1k_cont$word))
      rm(sample_1k_cont)
    }else{
      distinct_words_1k_cont <- NA
    }
    sentences <- book %>% 
      unnest_tokens(sentence, text, token = "sentences")%>%
      mutate(sen_length = sapply(sentence,
                                 function(x) strsplit(x, " ") %>%
                                   unlist %>% length))
    nsentences <- nrow(sentences)
    avg_wps <- mean(sentences$sen_length)
    var_wps <- var(sentences$sen_length)
    out.names <- c("Total words", 
                  "Distinct words",
                  "Mean (word length)",
                  "Variance (word length)",
                  "Mean (words per sentence)",
                  "Variance (words per sentence)",
                  "Sampled type-token ratio")
    out.values <- c(nwords, distinct_words, avg_word_len, var_word_len,
                    avg_wps, var_wps, distinct_words_1k_cont/1000)
    rm(book, words, sentences)
    gc()
    data.frame(out.values, row.names=out.names)
  }
}