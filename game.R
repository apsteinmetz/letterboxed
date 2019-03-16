# letterboxed game
library(tidyverse)
library(wfindr)
library(gtools)

findsides <- 5
letters_per_side <- 2
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


#
# ------------------------------------------------------------
generate_puzzle <- function(sides=4,letters_per_side=3,vowel_count=4,replacement = FALSE){
  if(sides < 4){
    print("Minimum Side is 4, changing to 4")
    sides = 4
  }
  if (vowel_count < sides) replacement=TRUE
  if (vowel_count > length(vowels)) replacement=TRUE
  use_vowels <- sample(vowels,vowel_count,replace = replacement)
  use_consonants <- sample(consonants,letters_per_side*sides-vowel_count,replace = replacement)
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

# -------------------------------------------------------------
get_polygon <- function(sides=4){
  x_center <- 0
  y_center <- 0
  radius <- 5
  y <- NULL
  x <- NULL
  angle = 3.925
  angle_increment <-  2 * pi / sides
  for (i in 1:sides){
    x[i] = x_center + radius * cos(angle)
    y[i] = y_center + radius * sin(angle)
    angle = angle + angle_increment
  }
  #close figure
  x[i+1] <- x[1]
  y[i+1] <- y[1]
  return(data.frame(x=x,y=y))
}

# -------------------------------------------------------------
get_points_on_segment <- function(end_points,num_points){
  # pointdistance is fraction of segment length
  a <- as.numeric(end_points[1,])
  b <- as.numeric(end_points[2,])

  # Use atan2!
  th = atan2( b[2]-a[2] , b[1]-a[1] )
  
  # length of segment AB
  AB = sqrt( (b[2]-a[2])^2 + (b[1]-a[1])^2 )
  AB_fraction <- AB / (num_points +1 )
  # points equidistant on the line
  AP = sapply(1:(num_points),function(x) x * AB_fraction)
  
  # The points of interest
  c = sapply(AP,function(d) c(x = a[1] + d*cos( th ),
                              y = a[2] + d*sin( th ))) %>% 
    t() %>%
    as.data.frame()
  return(c)
}
# -------------------------------------------------------------
draw_puzzle <-function(puzzle,sides=4,letters_per_side=3){
  
puzzle_shape <- get_polygon(sides)
letter_pos<-lapply(1:(nrow(puzzle_shape)-1),
                    function(p) get_points_on_segment(puzzle_shape[p:(p+1),],letters_per_side)) %>% 
  bind_rows() %>% 
  bind_cols(puzzle)


puzzle_shape %>% ggplot(aes(x,y)) + geom_path() + coord_fixed() +
  geom_point(data=letter_pos,aes(x,y),size=20,color="white") + 
  geom_text(data=letter_pos,aes(x,y,label = letter),size=10) + 
  theme_void() + 
  theme(panel.background = element_rect(fill="pink")) + 
  NULL

}
sides <- 4
letters_per_side <- 3
vowel_count <- sides
puzzle <- generate_puzzle(sides=sides,letters_per_side = letters_per_side,vowel_count = vowel_count)
draw_puzzle(puzzle,sides=sides,letters_per_side = letters_per_side)

# ------------------------------------------------------
# dplyr chain-friendly permuatations
d_permute <- function(v, n, r,  set, repeats.allowed){
  return(permutations(n, r, v, set, repeats.allowed))
}
# -----------------------------------------------------
get_line_combos <- function(a_side,puzzle){
  combos <- puzzle %>% filter(side==a_side) %>% 
  pull(letter) %>% 
  d_permute(n=3,r=2,set=F,repeats.allowed = T) %>% 
  apply(1,paste0,collapse="")
  return(combos)
}
# -----------------------------------------------------
# get all letter combos that are invalid because they lie on the same line segment
bans <- map(1:sides,get_line_combos,puzzle=puzzle) %>% unlist()

#get all possible words
puzzle_words <- scrabble(paste0(puzzle$letter,collapse = ""))
length(puzzle_words)
#winnow out illegal ones
puzzle_words <- puzzle_words[str_detect(puzzle_words,bans,negate = TRUE)]
length(puzzle_words)
