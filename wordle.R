# wordle game
library(tidyverse)
library(wfindr)
library(tidytext)
# get a list of 5-letter words
# we don't know what list the game uses but this is an abridged
# english list of similar size.

if (file.exists("wordle.rdata")){
  load("wordle.rdata")
} else {
  wordle_list <- read_lines("http://www-cs-faculty.stanford.edu/~uno/sgb-words.txt")
  save(wordle_list,file="wordle.rdata")
}

# use smaller word list from game site
source("data/wordle_official2.r")
worldle_list <- wordle_dict_Aa

# get letter frequencies
letter_freq <-wordle_list %>% str_split(pattern="") %>% 
  unlist() %>% 
  enframe() %>% 
  count(value) %>% 
  arrange(desc(n))

letter_vec <- function(string){
  unlist(str_split(string,""))
}

score_word <- Vectorize(function(word){
  word_vec<-letter_vec(word)
  letter_freq %>% filter(value %in% word_vec) %>% 
    tally(n) %>% unlist()
})

# # pre-score words?  I opt not to since it's time consuming
# # and most words will never be candidates.  I score on demand
# wordle_scores <- wordle_list %>% score_word
# wordle_list_df <- tibble(word = wordle_list,score=wordle_scores) %>% 
#    arrange(desc(score))
#  
#  
# #  tally the most popular biggrams at each position
# bigrams = tibble()
# pos_range = 1:4
#  for (pos in pos_range){
#    bigram=str_sub(wordle_list_df$word,pos,pos+1)
#    bigrams <- bigrams %>% bind_rows(tibble(position=pos,bigram=bigram))
#  }
# bigram_freq <- bigrams %>% 
#    group_by(position) %>% 
#    count(bigram) %>%  
#    slice_max(n=10,order_by = n)

# choose a 5-letter word with the most popular letters from the list
# it's "raise" or "arise"
start_word <- scrabble(letter_freq$value[1:5],words = wordle_list)[1]

next_best_words <- function(yes_letters,no_letters,yes_pos){
  # yes_pos is known positions of the form "y...." if y is in the first pos
  first_cut<-find_word(model=yes_pos,ban=no_letters,words = wordle_list)
  yes_regex <- paste0(rep_len(paste0("[",yes_letters,"]"),
                              str_length(yes_letters)),collapse="")
  
  # now get the subset of most frequent letters to choose the "best" word
  next_cut <- enframe(grep(yes_regex,first_cut,value = T,perl=T)) %>% 
    mutate(score = score_word(value)) %>% 
    arrange(desc(score))
  return(next_cut$value[1:10])
}

no_letters <- ""
yes_letters <- ""
yes_pos = "....."

cat("WORDLE HELPER\n")
cat("This will suggest words to try in WORDLE using English letter frequencies to rank them.\n")
cat("If using RStudio, you should turn off auto-complete.\n")
cat("Uncheck Menu/Tools/Global Options/Code/Completion/Allow auto completions in console.\n")
cat('I suggest starting with "AROSE,RAISE or LASER".\n')
repeat {
  answer <- readline(paste("What letters do you know are in the word?",yes_letters))
  if (answer == "quit") break
  yes_letters <- paste0(yes_letters,answer,collapse = "")
  no_letters <- paste0(no_letters,
                       readline(paste("What letters do you know are NOT in the word?",no_letters)),
                       collapse = "")
  cat("Enter a template for letters that you know the position of..\n")
  cat("Type a string of 5 characters. Use '.'where the letter is unknown.")
  cat("Example: If ÿ is the first letter and l is the fourth type 'y..l.' ")
  yes_pos <- readline("Use '.....' if you know the position of no letters:")
  cat('Next 10 best words are:\n')
  writeLines("\n")
  result <- next_best_words(yes_letters,no_letters,yes_pos)
  cat(toupper(result))
  writeLines("\n")
  cat("NOTE: This doesn't allow you to exclude known letters from certain positions yet\n")
  cat("Eliminate suggestions that include incorrect positions of known letters.\n")
  writeLines("\n")
}



