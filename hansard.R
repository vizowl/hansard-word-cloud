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
library(rtweet)
library(here)

source(here("utility/hansard_functions.R"))
# source("utility/auth.R")

# Main ------------------------------------------------------------------------

# Check if latest sitting day is new
hansard_a <- read_html("https://www.parliament.nz/en/pb/hansard-debates/rhr/") %>%
     html_node("h2.hansard__heading a")
x <- hansard_a %>% html_attr('href')
processed <- read_csv(here('processed.csv'))
if (!(x %in% processed$Processed)) {
  hansardDay <- hansard_a %>% html_text(trim=T) %>% str_split(' - ') %>% first() %>% first()
  hansData <- read_html(paste0("https://www.parliament.nz/", x))
  hansText <- hansData %>% html_nodes('.body-text .section') %>% html_text() %>% enframe(name=NULL)
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
  filename <- here(paste0("images/", label, '.png'))
  ggsave(filename, width = 15.87, height = 8.86, units = "cm")

  post_tweet(status = paste("Top 100 words spoken in the New Zealand parliament on", hansardDay, "#nzpol"), media = filename)
  write_csv(bind_rows(processed, enframe(x, name=NULL, value="Processed")), 'processed.csv')

}
