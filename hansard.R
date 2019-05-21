# File: hansard.R
# Description: Generate plot and publish to twitter

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
data(stop_words)
source(here("utility/hansard_words.R"))
options(warn = 1)
# source("utility/auth.R")

# Main ------------------------------------------------------------------------

# Check if latest sitting day is new
processed <- read_csv(here('processed.csv'),
                      col_types=cols( Processed = col_character()
                                     ))
run <- function(hansard_a) {
  x <- hansard_a %>% html_attr('href')
  hansardDay <- hansard_a %>% html_text(trim=T) %>% str_split(' - ') %>% first() %>% first()
  print(x)
  print(hansardDay)
  if (!(x %in% processed$Processed)) {
    hansData <- read_html(paste0("https://www.parliament.nz/", x))
    hansText <- hansData %>% html_nodes('.body-text .section') %>% html_text() %>% enframe(name=NULL)
    label <- x %>% str_split('/') %>% first %>% last()

    # Load stop words
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

    runPlot <- function(d, size) {
      # Create word cloud
      plot <- ggplot(d, aes(label = word, size = n, colour = word)) +
        geom_text_wordcloud_area(shape = "square", rm_outside=F ) +
        scale_size_area(max_size=size) +
        theme_minimal() +
        theme(plot.margin=grid::unit(c(0,0,0,0), "mm")) +
        scale_colour_manual(values = rep(hansard_hexs(), 20))
      filename <- here(paste0("images/--", size, label, '.png'))
      # This is pretty hacky - I could not work out a better way of triggering the warnings
      # I want to detect when the words do not fit and reduce the font size and try again.
      withCallingHandlers(
                          ggsave(filename, width = 15.87, height = 8.86, units = "cm"),
                          warning = function(w) { 
                            print (paste(size, "too large"))
                            runPlot(d, size-2)
                          })
    }
    runPlot(data, 20)
    filename <- here(paste0("images/full-", label, '.png'))
    ggsave(filename, width = 15.87, height = 8.86, units = "cm")

    post_tweet(status = paste("Top 100 words spoken in the New Zealand parliament on", hansardDay, "#nzpol"), media = filename)
  }
  return(x)
}

latests <- read_html("https://www.parliament.nz/en/pb/hansard-debates/rhr/") %>%
  html_nodes("h2.hansard__heading a") %>% rev()

runs <- map_chr(latests, run)
write_csv(enframe(runs, name=NULL, value="Processed"), 'processed.csv')

