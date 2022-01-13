# https://jacobsimmering.com/post/wordle/

library(tidyverse)
source("data/wordle_official2.r")

# use short list of words in game
five_letter_words <-c(wordle_dict_Aa)

letter_matches <- vector("list", 26)
for (letter_index in 1:26) {
  letter <- letters[letter_index]
  matches <- vector("list", 5)
  for (character_index in 1:5) {
    matches[[character_index]] <- tibble(
      letter = letter,
      yellow_or_green = stringr::str_detect(five_letter_words, letter),
      green = stringr::str_sub(five_letter_words, 
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

# --------------------------------------------
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

# ----------------------------------------------
word_values <- vector("list", length(five_letter_words))
for (i in 1:length(five_letter_words)) {
  word <- five_letter_words[[i]]
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

word_values <- word_values %>%
  bind_rows() %>%
  mutate(word = five_letter_words)


# ---------------------------------
density_yellow <- word_values %>%
  bind_rows() %>%
  ggplot(aes(x = expected_yellow)) + 
  geom_density() + 
  labs(x = "Expected Number of Yellow Squares", 
       y = "")

density_green <- word_values %>%
  bind_rows() %>%
  ggplot(aes(x = expected_green)) + 
  geom_density() + 
  labs(x = "Expected Number of Green Squares", 
       y = "")

density_either <- word_values %>%
  bind_rows() %>%
  ggplot(aes(x = expected_green + expected_yellow)) + 
  geom_density() + 
  labs(x = "Expected Number of Yellow or Green Squares",
       y = "")

density_yellow
density_green
density_either

# (density_yellow | density_green) / (density_either)

# ----------------------------
word_values %>%
  bind_rows() %>%
  mutate(word = five_letter_words) %>%
  top_n(5, expected_yellow) %>%
  arrange(desc(expected_yellow)) %>%
  select(word, expected_yellow)

word_values %>%
  bind_rows() %>%
  mutate(word = five_letter_words) %>%
  top_n(5, desc(expected_yellow)) %>%
  arrange(expected_yellow) %>%
  select(word, expected_yellow)
# ----------------------------

word_values %>%
  bind_rows() %>%
  mutate(word = five_letter_words) %>%
  top_n(5, expected_green) %>%
  arrange(desc(expected_green)) %>%
  select(word, expected_green)


# ----------------------------
word_values %>%
  bind_rows() %>%
  mutate(word = five_letter_words) %>%
  top_n(5, desc(expected_green)) %>%
  arrange(expected_green) %>%
  select(word, expected_green)

# ----------------------------
# the best words

word_values %>%
  bind_rows() %>%
  mutate(word = five_letter_words,
         combined_performance = expected_yellow + expected_green) %>%
  top_n(5, combined_performance) %>%
  arrange(desc(combined_performance)) %>%
  select(word, combined_performance, expected_yellow, expected_green) %>% 
  pull(word)
  
# ----------------------------

word_values %>%
  bind_rows() %>%
  mutate(word = five_letter_words,
         combined_performance = expected_yellow + expected_green) %>%
  arrange(desc(combined_performance)) %>%
  mutate(
    rank = row_number(), 
    pct = (1 - rank / n()) * 100
  ) %>%
  filter(word %in% c("erase","irate", "weird", "arise")) %>%
  select(word, rank, percentile = pct, combined_performance)

## # A tibble: 4 × 4
##   word   rank percentile combined_performance
##   <chr> <int>      <dbl>                <dbl>
## 1 areae     1      100.                  2.06
## 2 heals  1160       92.7                 1.63
## 3 adieu  2354       85.2                 1.55
## 4 weird  9728       38.9                 1.27
