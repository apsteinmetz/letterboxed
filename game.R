# letterboxed game
library(tidyverse)
library(wfindr)
library(gtools)

sides <- 4
letters_per_side <- 3
vowels <- c("a","e","i","o","u")
consonants <- letters[!(letters %in% vowels)]

# load list of word candidates
# load("data/medium_word_list.rdata")
# load("data/short_word_list.rdata")
word_list <- medium_word_list
#rm(medium_word_list)
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

get_letter_coords <- function(puzzle,sides=4,letters_per_side=3){
  
  puzzle_shape <- get_polygon(sides)
  puzzle<-lapply(1:(nrow(puzzle_shape)-1),
                     function(p) get_points_on_segment(puzzle_shape[p:(p+1),],letters_per_side)) %>% 
    bind_rows() %>% 
    bind_cols(puzzle)
  return(puzzle)
}

# -------------------------------------------------------------
draw_puzzle <-function(puzzle,sides=4,letters_per_side=3){
  
puzzle_shape <- get_polygon(sides)


gg <- puzzle_shape %>% ggplot(aes(x,y)) + geom_path() + coord_fixed() +
  geom_point(data = puzzle,aes(x,y),size=20,color="white") + 
  geom_text(data = puzzle,aes(x,y,label = letter),size=10) + 
  theme_void() + 
  theme(panel.background = element_rect(fill="pink")) + 
  NULL 
print(gg)
return(gg)
}

# ---------------------------------------------------------
draw_solution <- function(puzzle, solution){
  gg <- draw_puzzle(puzzle)
  gg <- gg + annotate("text",x=0,y=0.9,label=paste(solution, collapse = "\n"), size = 6)
  print (gg)
  
}

# ------------------------------------------------------
# dplyr chain-friendly permuatations
d_permute <- function(v, n, r,  set, repeats.allowed){
  return(permutations(n, r, v, set, repeats.allowed))
}



# ------------------------------------------------------
# SOLVING SECTION
# -----------------------------------------------------
get_line_combos <- function(a_side,puzzle){
  combos <- puzzle %>% filter(side==a_side) %>% 
    pull(letter) %>% 
    as.character() %>% 
    d_permute(n=3,r=2,set=F,repeats.allowed = T) %>% 
    apply(1,paste0,collapse="")
  return(combos)
}

# -----------------------------------------------------
find_next_words <- function(w,needed_letters){
  # find words that start with last letter of w
  next_words<-puzzle_words[str_starts(puzzle_words,str_sub(w,-1))]
  # prefer larger words that touch more letters
  # OR prioritize words by greatest overlap with unused letters
  next_word_chars <-  map(next_words,strsplit,split="") %>% unlist(recursive = F)
  #temp <- map(next_word_chars,function(x) length(setdiff(needed_letters,x)))
  #next_words[which.min(unlist(temp))]
    #return minimum words by unique last letter
  last_letters <- str_sub(next_words,-1L) %>% unique()
  next_words <- map(last_letters,function(x) next_words[match(TRUE,endsWith(next_words,x))]) %>%
    unlist()
  
  return(next_words)
}

# -----------------------------------------------------
find_next_best_words <- function(w,needed_letters,max_return=1){
  # the higher max_return is the more words will be traversed.  Careful,
  # computation times will geometrically increase.
  # puzzle_words is global
  # find words that start with last letter of w
  next_words<-puzzle_words[str_starts(puzzle_words,str_sub(w,-1))]
  # prioritize words by greatest overlap with unused letters
  next_word_chars <-  map(next_words,strsplit,split="") %>% unlist(recursive = F)
  temp <- map(next_word_chars,function(x) length(setdiff(needed_letters,x))) %>% unlist()
  if (is.vector(temp)){
    next_words <- next_words[order(temp)]
    max_return <- min(length(next_words),max_return)
    return(next_words[1:max_return])  
  } else{
    return()
  }
  
}
# -----------------------------------------------------
test_needed_letters <- function(word_chain){
  word_chain_chars <-  paste0(word_chain,collapse = "") %>% 
    strsplit(split="") %>%
    unlist() %>% 
    unique()
  return(setdiff(all_puzzle_letters,
                     word_chain_chars))
}

# make word chains
# ---------------------------------------------------------------------
make_chain <- function(word_chain,used_last_letters){
  needed_letters <- test_needed_letters(word_chain)
  if (length(word_chain)>6){
    # come on, if you can't solve in 5, you suck!
    return()
  }
  if (length(needed_letters)==0) {
    # print(c("SOLVED",word_chain))
    # solved = TRUE
    solution_list[length(solution_list)+1] <<-  list(word_chain)
    return(list(word_chain))
  }
  else {
    last_word <- tail(word_chain,1)
    last_letter <-str_sub(last_word,-1L)
    if (str_detect(used_last_letters,last_letter,negate=T)){
      used_last_letters <- paste0(last_letter,used_last_letters,collapse = "")
      next_word<-find_next_best_words(last_word,needed_letters,max_return=1)
#      next_words<-find_next_best_words(last_word,needed_letters,max_return=1)
#      for (next_word in next_words) {
       if (length(next_word)>0){
        word_chain <- make_chain(c(word_chain,next_word),used_last_letters)
        } else {
          return()
        }
    } else{
      return()
    }
  }
}  
# ------------------------------------------------------------------------------

solve_puzzle <- function (puzzle) {
  # get all letter combos that are invalid because they lie on the same line segment
  bans <- map(1:sides,get_line_combos,puzzle=puzzle) %>% unlist()
  #get all possible words
  #  puzzle_words <<- scrabble(paste0(puzzle$letter,collapse = ""),words=word_list)
  puzzle_words <<- scrabble(paste0(puzzle$letter,collapse = ""))
  length(puzzle_words)
  #winnow out illegal ones
  banned_words <- map(bans,function(x) puzzle_words[str_which(puzzle_words,x)]) %>% 
    unlist()
  puzzle_words <<- puzzle_words[!(puzzle_words %in% banned_words)]
  length(puzzle_words)
  puzzle_words <<-puzzle_words[order(nchar(puzzle_words),decreasing = TRUE, puzzle_words)]
  
  
  all_puzzle_letters <<- puzzle$letter %>% as.vector()
  
  solutions <- map(puzzle_words,make_chain,"") %>% unlist(recursive = F)
  return(solutions)
}
# ------------------------------------------------------------------------------


vowel_count <- sides
# global variables
all_puzzle_letters <- NULL
puzzle_words <- NULL
solution_list <- NULL
puzzle <- generate_puzzle(sides=sides,letters_per_side = letters_per_side,vowel_count = vowel_count)
#puzzle <- sample_puzzle
puzzle <- get_letter_coords(puzzle,sides=sides,letters_per_side = letters_per_side)
draw_puzzle(puzzle)
solutions <- solve_puzzle(puzzle)
if (is.null(solutions)) {
  solution <- "No Solution"
} else {
  ideal <- map(solutions,length) %>% unlist() %>% which.min()
  solution <- c(solutions[[ideal]],paste(length(solutions)-1,"other solutions")) 
}
draw_solution(puzzle, solution)
