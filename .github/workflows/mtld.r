library(tidytext)
library(dplyr)
sample_times <- 1000     # change this number to change how many times you want to sample


# The code to measure MTLD was adapted from lexical-diversity 0.1.1 
# by Kristopher Kyle - re-written in R. Note: This may give a different MTLD from 
# Textinspector as it will checks the factor lenght and will not add a new factor until
# the current factor is at least 10 tokens long.

# Makes sure you are not dividing by 0
safe_divide <- function(numerator, denominator){
  if (denominator == 0 || denominator == 0.0){
    index <- 0
  }
  
  else{
    index <- numerator/denominator
  } 
  return(index)
}


# Finds the TTR of the text
ttr <-function(text){
  ntokens <- length(text)
  ntypes <- unique(text) %>% length()
  return(safe_divide(ntypes,ntokens))
}


# Finds the Root TTR of the text
root_ttr <-function(text){
  ntokens <- length(text)
  ntypes <- unique(text) %>% length()
  return(safe_divide(ntypes,sqrt(ntokens)))
}


# Processes the information necessary to calculate MTLD
cal_mtld <- function(text){
  text_reversed <- rev(text)
  f_mtld <- mtlder(text)
  b_mtld <- mtlder(text_reversed)
  t_mtld<- c(f_mtld, b_mtld) %>% mean()
  return(t_mtld)
}


# Does the actual MTLD calculations
mtlder <- function(text){
  factor <- 0
  factor_lengths <- 0
  start <- 1
  total_tokens <- length(text)
  for (x in 1:total_tokens){
    factor_text <- text[start:x]
    if (x == total_tokens){
      factor <- factor + (safe_divide((1 - ttr(factor_text)),(1 - .72)))
      factor_lengths <- total_tokens       
    }
    else{
      if (ttr(factor_text) < .720 && length(factor_text) >= 10){
        factor <- factor + 1
        factor_lengths <- factor_lengths + length(factor_text)
        start = x+1
      }
      else{
        next
      }
    }
  }
  s_mtld <- safe_divide(factor_lengths,factor)
  return(s_mtld)
} 


# Tokenizes text with Tidytext (It may be worthwhile switching to a library that lemmatizes)
tok_text <- function(file){
  text<- readLines(file, warn=FALSE)
  text_tidy <- tibble(text) %>% unnest_tokens(word, text)
  return(text_tidy)
}


# Gets a random sample and calculates TTR
rand_sample<- function(tok_text){
  rs <- sample_n(tok_text, 100)$word
  rs_ttr <- ttr(rs)
  return(rs_ttr)
}


# Gets multiple random samples and finds the average TTR
rand_samples <- function(tok_text) {
  rs_list <- c()
  s_times <- 1:sample_times
  for(i in s_times){
    rs_ttr <- rand_sample(tok_text)
    rs_list <- c(rs_list, rs_ttr)
  }
  rs_av <- mean(rs_list)
  return(rs_av)
}


# Calls all the appropriate LD functions for each of the files and returns the output
# Note: if there are less than 100 tokens in the text, it will return 0 for random samples and MTLD
cal_ld <- function(file){
  tok_text <- tok_text(file)
  t_tokens <- as.list(tok_text$word) %>% length()
  t_types <- as.list(tok_text$word) %>% unique() %>% length()
  text_ttr <- ttr(tok_text$word)
  guiraud <- root_ttr(tok_text$word)
  if (t_tokens > 100){
    s_ttr <- rand_sample(tok_text)
    av_ttr <- rand_samples(tok_text)
    mtld <- cal_mtld(tok_text$word)
  }
  else{
    s_ttr <- 0
    av_ttr <- 0
    mtld <- 0
  }
  output <- c(file, t_tokens, t_types, format(round(s_ttr, 3), nsmall = 3), sample_times, format(round(av_ttr, 3), nsmall = 3), format(round(text_ttr, 4), nsmall = 4), format(round(guiraud, 4), nsmall = 4), format(round(mtld, 4), nsmall = 4))
}


# Initial setup -> empty df, choose file, sets directory
df <- data.frame()
file <- file.choose() # choose any text file in the directory
directory <- dirname(file)
setwd(directory)


# Gets a list of all the text files in that directory
files <- as.list(list.files(path = directory, pattern = ".*.txt"))


# Gets the lexical diversity of each file and adds it to the data frame
for (file in files){ 
  output <- cal_ld(file)
  df <- rbind(df, output)
}


# Shows the final df and writes it to a csv file called output.csv
colnames(df) <- c("Text", "Tokens", "Types", "LD Single sample", "Sample times", "LD Average", "TTR", "Guiraud", "mtld")
write.csv(df,"output.csv")
df
