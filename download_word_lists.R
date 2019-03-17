# get long, medium and short word lists from internet
library(tidyverse)

# -----------------------------------------------------
long_word_list_raw <- tibble(word=read_lines("http://norvig.com/ngrams/word.list"))
# filter out all words with duplicate consecutive letters since they can never be in puzzle
long_word_list <-anti_join(long_word_list_raw, 
                      bind_rows(map(letters,
                                    function(x) filter(long_word_list_raw,
                                                       str_detect(word,paste0(x,x)))))) %>% 
  pull(word)

save(long_word_list,file="data/long_word_list.rdata")
# -----------------------------------------------------
medium_word_list_raw <- read_delim("http://norvig.com/ngrams/count_1w100k.txt",
                                   delim = "\t",
                                   col_names = c("word","popularity")) %>% 
  mutate(word=tolower(word))

medium_word_list <-anti_join(medium_word_list_raw, 
                      bind_rows(map(letters,
                                    function(x) filter(medium_word_list_raw,
                                                       str_detect(word,paste0(x,x)))))) %>% 
  pull(word)
save(medium_word_list,file = "data/medium_word_list.rdata")

# ----------------------------------------------------------------------------
short_word_list_raw <-readChar("https://xkcd.com/simplewriter/words.js",nchars=100000)
short_word_list <- short_word_list_raw %>% str_split("\\|") %>% unlist() %>% tibble(word=.) 
# filter out all words with duplicate consecutive letters since they can never be in puzzle
short_word_list <-anti_join(short_word_list, 
                            bind_rows(map(letters,
                                          function(x) filter(short_word_list,
                                                             str_detect(word,paste0(x,x)))))) %>% 
  pull(word)
save(short_word_list,file="data/short_word_list.rdata")
