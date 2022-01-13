# plot various wordle word scoring methods
# assumes variables from word_scores.r are already in the environment
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

ggplot(scored_words,aes(naive_score,bigram_score)) + geom_point()
ggplot(scored_words,aes(simmering_score,simmering_wgt_score)) + geom_point()
ggplot(scored_words,aes(simmering_score,naive_score)) + geom_point()
ggplot(scored_words,aes(simmering_score,bigram_score)) + geom_point()
