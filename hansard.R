# File: hansard.R
# Description: Generate plot and publish to twitter
# Author: Noel Dempsey
# Date: 2018-12-02

# Imports ---------------------------------------------------------------------

library(tidyverse)
library(rvest)
library(tidytext)
library(tm)
library(ggplot2)
library(ggwordcloud)
library(twitteR)
library(here)

source(here("utility/hansard_functions.R"))
# source("utility/auth.R")

# Main ------------------------------------------------------------------------

# Twitter API authentication
# setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# Check if latest sitting day is new
x <- enframe(hansard_link(), name = NULL)
hansData <- read_html(paste0("https://www.parliament.nz/", deframe(x)))
hansText <- hans %>% html_nodes('.body-text .section') %>% html_text() %>% enframe(name=NULL)
label <- x %>% str_split('/') %>% first %>% last()

# Load stop words
data(stop_words)
source(here("utility/hansard_words.R"))
words <- hansText %>% unnest_tokens(word, value) %>%
  filter(!word %in% stop_words$word) %>%
  filter(!word %in% mp_first_stop_words$word) %>%
  filter(!word %in% mp_last_stop_words$word) %>%
  filter(!word %in% parly_stop_words$word) %>%
  filter(!word %in% party_stop_words$word) %>%
  filter(!word %in% month_stop_words$word) %>%
  mutate(word=str_trim(str_replace_all(word, "[^[:alpha:]]", " "))) %>%
  filter(word != '') %>%
  count(word, sort = TRUE)

# Top 100
data <- words[1:100,]

# Create word cloud
plot <- hansard_plot()

# Save file
ggsave(paste0(label, '.png'), width = 15.87, height = 8.86, units = "cm")



y <- read_tsv("check.txt", col_names = "value",
              col_types = cols(.default = "c"))

if (str_detect(x$value, y$value) == FALSE) { 
  
  # If new run script
  source("utility/hansard_plot.R") 

# Tweet plot
updateStatus(paste("Word cloud for", x),
              mediaPath = paste("output/", x, ".png", sep = ""))
    
}
