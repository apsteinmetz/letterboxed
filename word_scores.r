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

score_word_bigram <- Vectorize(function(word){
  enframe(bigram_vec(word),name="position",value="bigram") %>% 
    left_join(bigram_freq,by =c("position", "bigram")) %>% 
    tally(probability) %>% 
    unlist()
})

# add bigram scores to naive scores data frame
word_scores_2 <- score_word_bigram(words)
word_scores_2 <- word_scores_2/max(word_scores_2) * 100

# calculate the Simmering score
# https://jacobsimmering.com/post/wordle/

letter_matches <- vector("list", 26)
for (letter_index in 1:26) {
  letter <- letters[letter_index]
  matches <- vector("list", 5)
  for (character_index in 1:5) {
    matches[[character_index]] <- tibble(
      letter = letter,
      yellow_or_green = stringr::str_detect(words, letter),
      green = stringr::str_sub(words, 
                               character_index, 
                               character_index) == letter
    ) %>%
      mutate(
        yellow = yellow_or_green & !green
      ) %>%
      select(letter, yellow, green) %>%
      mutate(
        character_index = character_index
      )
  }
  letter_matches[[letter_index]] <- matches %>%
    bind_rows()
}
naive_prob <- letter_matches %>% 
  bind_rows() %>%
  group_by(
    letter, character_index
  ) %>%
  summarize(
    yellow = mean(yellow),
    green = mean(green)
  )

# ----------------------------------------------
word_values <- vector("list", length(words))
for (i in 1:length(words)) {
  word <- words[[i]]
  word_values[[i]] <- tibble(
    `1` = stringr::str_sub(word, 1, 1),
    `2` = stringr::str_sub(word, 2, 2),
    `3` = stringr::str_sub(word, 3, 3),
    `4` = stringr::str_sub(word, 4, 4),
    `5` = stringr::str_sub(word, 5, 5),
  ) %>%
    gather(character_index, letter) %>%
    mutate(character_index = as.numeric(character_index)) %>%
    inner_join(
      naive_prob,
      by = c("character_index", "letter")
    ) %>%
    summarize(
      expected_yellow = sum(yellow),
      expected_green = sum(green)
    )
}

word_scores_3 <- word_values %>%
  bind_rows() %>% 
  mutate(word = words) %>% 
  mutate(simmering_score = expected_yellow + expected_green) %>% 
  # weight green values twice as high a yellow
  mutate(simmering_wgt_score = expected_yellow + 2* expected_green)

# normalize 0-100
word_scores_3 <- word_scores_3 %>%
  mutate(simmering_score = simmering_score/max(simmering_score)*100) %>% 
  mutate(simmering_wgt_score = simmering_wgt_score/max(simmering_wgt_score)*100) %>% 
  select(!contains("expected"))

# combine everything
scored_words <- tibble(word = words,naive_score=word_scores_1) %>% 
  mutate(bigram_score = word_scores_2) %>% 
  left_join(word_scores_3)

save(scored_words,file="data/scored_words_aa.rdata")

#Naive Plot
letter_freq %>% 
  arrange(value) %>% 
  mutate(n = n/length(words)) %>% 
  ggplot(aes(value,n,fill=n)) + 
  geom_col() + 
  scale_fill_distiller(palette = "Greens",
                       values = scales::rescale(1.2^(1:9)),
                       direction = 1) +
  theme(legend.position = "none") +
  labs(y="Probability That Letter Occurs in Word",
       x = "Letter")


# Bigram Plot
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


# Simmering Plot
letter_matches %>%
  bind_rows() %>%
  group_by(
    letter, character_index
  ) %>%
  summarize(
    `Yellow` = mean(yellow),
    `Green` = mean(green),
    `Green or Yellow` = mean(green | yellow),
    .groups = "drop"
  ) %>%
  gather(color, prob, -letter, -character_index) %>%
  mutate(color = forcats::fct_relevel(color, "Green or Yellow")) %>%
  ggplot(aes(x = letter, 
             y = character_index,
             fill = prob)) + 
  geom_tile() + 
  facet_grid(rows = vars(color)) + 
  labs(x = "", 
       y = "Letter Position", 
       fill = "Probability of Match") + 
  scale_fill_distiller(palette = "Spectral") + 
  theme(legend.position = "bottom")

# --------------------------------------------
naive_prob <- letter_matches %>% bind_rows() %>%
  group_by(
    letter, character_index
  ) %>%
  summarize(
    yellow = mean(yellow),
    green = mean(green)
  )


# ggplot(scored_words,aes(naive_score,bigram_score)) + geom_point()
# ggplot(scored_words,aes(simmering_score,simmering_wgt_score)) + geom_point()
# ggplot(scored_words,aes(simmering_score,naive_score)) + geom_point()
# ggplot(scored_words,aes(simmering_score,bigram_score)) + geom_point()
