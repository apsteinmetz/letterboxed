# compute word values

source("data\wordle_official2.r")

# use the smaller, 2315 word list from which the winners are drawn
# note the allowable guess list is much bigger
words <- wordle_dict_Aa

#calculate the naive letter score

#unordered letter frequencies
letter_freq <-words %>% str_split(pattern="") %>% 
  unlist() %>% 
  enframe() %>% 
  count(value) %>% 
  arrange(desc(n))

letter_vec <- function(string){
  unlist(str_split(string,""))
}

bigram_vec <- function(string){
  vec <- letter_vec(string)
  c(paste0(vec[1:2],collapse = ""),
    paste0(vec[2:3],collapse = ""),
    paste0(vec[3:4],collapse = ""),
    paste0(vec[4:5],collapse = ""))
}

score_word_naive <- Vectorize(function(word){
  word_vec<-letter_vec(word)
  letter_freq %>% filter(value %in% word_vec) %>% 
    tally(n) %>% 
    pull(n)
})

word_scores_1 <- words %>% score_word_naive()
word_scores_1 <- word_scores_1/max(word_scores_1) *100
scored_words <- tibble(word = words,naive_score=word_scores_1)

# bigram score  
#  tally the most popular bigrams at each position
# many bigrams never appear in the word list even though the letters in
# adjacent positions might be individually common, e.g. "tl".
bigrams = tibble()
pos_range = 1:4
for (pos in pos_range){
    bigram = str_sub(words,pos,pos+1)
    bigrams <- bigrams %>% bind_rows(tibble(position=pos,bigram=bigram))
  }

bigram_freq <- bigrams %>% 
  group_by(position) %>% 
  count(bigram) %>% 
  mutate(probability = n/length(words)) %>% 
  select(bigram,position,probability) %>% 
  complete(position,bigram,fill=list(probability=0)) %>% 
  unique() %>% 
  arrange(position,desc(probability)) %>% 
  {.}  

#show top 10 at each position
bigram_freq %>% 
  group_by(position) %>% 
  mutate(bigram = toupper(bigram)) %>% 
  slice_max(probability,n=5) %>%  
  mutate(position = as.factor(position)) %>% 
  mutate(bigram = as.factor(bigram)) %>% 
#  mutate(bigram = as.factor(as.character(bigram))) %>% 
#  complete(position,bigram,fill=list(probability=0)) %>% 
  ggplot(aes(position,bigram,fill=probability)) + 
  geom_tile() + 
  labs(title = "Probability of Most Frequent Wordle Bigrams") +
  scale_fill_distiller(palette = "Greens",
                       values = scales::rescale(1.2^(1:9)),
                       direction = 1)


score_word_bigram <- Vectorize(function(word){
  enframe(bigram_vec(word),name="position",value="bigram") %>% 
    left_join(bigram_freq,by =c("position", "bigram")) %>% 
    tally(probability) %>% 
    unlist()
})

# add bigram scores to naive scores data frame
word_scores_2 <- score_word_bigram(words)
word_scores_2 <- word_scores_2/max(word_scores_2) * 100
scored_words <- scored_words %>% mutate(bigram_score = word_scores_2)


# calculate the Simmering score
# https://jacobsimmering.com/post/wordle/


