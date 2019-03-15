# letterboxed game
library(tidyverse)

vowels <- c("a","e","i","o","u")
consonants <- letters[!(letters %in% vowels)]

#word_list_raw <- tibble(word=readLines("http://norvig.com/ngrams/word.list"))
#save(word_list_raw,file="data/word_list_raw.rdata")
load("data/word_list_raw.rdata")
#set.seed(1234)
#test <- tibble(word=sample(word_list_raw,1000))

# filter out all words with duplicate consecutive letters since they can never be in puzzle
word_list <-anti_join(word_list_raw, 
                      bind_rows(map(letters,
                                    function(x) filter(word_list_raw,str_detect(word,paste0(x,x))))))


#new game
generate_board <- function(sides=4,letters_per_side=3,vowel_count=4){
  use_vowels <- sample(vowels,vowel_count)
  use_consonants <- sample(consonants,letters_per_side*sides-vowel_count)
  # deal out the letters
  letter = NULL
  vowels_used = 1
  consonants_used = 1
  spot = 1
  for (i in 1:letters_per_side){
    for(j in 1:sides){
      #don't put vowel at edge of side but mathematically, it doesn't matter where
      if (i == 2 & vowels_used <= vowel_count){
        letter[spot] <- use_vowels[vowels_used]
        vowels_used <- vowels_used + 1
        spot <- spot + 1
      } else{
        letter[spot] <- use_consonants[consonants_used]
        consonants_used <- consonants_used + 1      
        spot <- spot + 1
        
      }
    }
  }
  puzzle <- tibble(side=rep(1:sides,letters_per_side),
                   spot=unlist(map(1:letters_per_side,rep,sides)), 
                   letter=letter) %>% arrange(side,spot)
  return(puzzle)
}

puzzle <- generate_board()

ggplot(puzzle,aes(side,spot)) + 
  geom_line() + 
  # geom_point() + 
  geom_text(aes(label=letter),size=10) 
