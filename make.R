
##----------------------------------------------------------------------------##
##                               DATA COLLECTION                              ##
##----------------------------------------------------------------------------##

## load rtweet, dplyr, and tidyverse
library(rtweet)
library(dplyr)
library(ggplot2)
## also need installed
#pkgs <- c("ggrepel", "purrr", "tfse", "hms")


## define function for getting CSPAN Twitter lists data
get_cspan_list <- function(slug) {
  ## get users data of list members
  x <- lists_members(slug = slug, owner_user = "CSPAN")
  ## document slug
  x$cspan_list <- slug
  ## timestamp observations
  x$timestamp <- Sys.time()
  ## return data
  x
}

## cspan lists
cspan_lists <- c("members-of-congress", "the-cabinet", "governors")

## members of congress
cspan_data <- purrr::map(cspan_lists, get_cspan_list)

## merge into single data frame
cspan_data <- dplyr::bind_rows(cspan_data)

## file name to save as (timestamped)
save_as <- sprintf("data/twitter_cspan_lists%s.rds",
  stringr::str_replace_all(Sys.time(), " |:", "-"))

## save file
saveRDS(cspan_data, save_as)

##----------------------------------------------------------------------------##
##                                  #DATAVIZ                                  ##
##----------------------------------------------------------------------------##

## load tidyverse
suppressPackageStartupMessages(library(tidyverse))

## read timestamped followers count file
data <- readRDS("data/timestamped-followers-data.rds")

## merge with running data set
cspan_data <- bind_rows(data,
  select(cspan_data, user_id, screen_name,
    followers_count, timestamp, cspan_list))

## save minimal form for public data
saveRDS(cspan_data, "data/timestamped-followers-data.rds")

## fast unique
funique <- function(x) {
  ## identify date-time, date, and character columns
  psx <- vapply(x, inherits, "POSIXct",
    FUN.VALUE = logical(1), USE.NAMES = FALSE)
  ## convert those columns to factors
  x[psx] <- lapply(x[psx], as.integer)
  ## return only unique rows
  x <- unique(x)
  ## convert columns back to date-time, date, and character
  x[psx] <- lapply(x[psx], as.POSIXct, origin = "1970-01-01")
  ## return data
  x
}

## round by hours
round2hour <- function(x) {
  n <- 60 * 60
  as.POSIXct(hms::hms(as.numeric(x) %/% n * n))
}

## save trimmed down CSV version for public data
cspan_data %>%
  select(screen_name, followers_count, timestamp) %>%
  mutate(timestamp = round2hour(timestamp),
    timestamp = as.integer(timestamp)) %>%
  readr::write_csv("data/timestamped-followers-data.csv")

## shortcuts for subsetting into data sets
congress_data <- function(cspan_data) filter(
  cspan_data, cspan_list == "members-of-congress")
cabinet_data <- function(cspan_data) filter(
  cspan_data, cspan_list == "the-cabinet")
governors_data <- function(cspan_data) filter(
  cspan_data, cspan_list == "governors")

## plot most popular congress accounts

## hacky function for labels
timestamp_range <- function(timestamp) {
  n <- length(unique(timestamp))
  x <- seq(min(timestamp), max(timestamp),
    length.out = (length(timestamp) / n))
  nas <- rep(as.POSIXct(NA_character_), length(x))
  c(x, rep(nas, n - 1L))
}

## member of congress
cspan_data %>%
  filter(followers_count > 6e5) %>%
  congress_data() %>%
  mutate(followers_count = sqrt(followers_count)) %>%
  group_by(screen_name) %>%
  mutate(mean = mean(followers_count)) %>%
  ungroup() %>%
  arrange(rev(timestamp)) %>%
  mutate(lab = ifelse(duplicated(screen_name), NA, screen_name)) %>%
  ggplot(aes(x = timestamp, y = followers_count,
    colour = screen_name, label = lab)) +
  theme_mwk(base_family = "Roboto Condensed") +
  theme(legend.position = "none",
    axis.title = element_text(hjust = .95, face = "italic")) +
  geom_line(size = .6) +
  ggrepel::geom_label_repel(family = "Roboto Condensed", size = 2.5,
    label.size = .05, label.padding = .05, fill = "#ffffffbb") +
  labs(title = "Tracking follower counts for members of Congress on Twitter",
    subtitle = "Tracking the number of Twitter followers of members of the Congress over time",
    x = NULL, y = "Number of followers (squared)",
    caption = "\nSource: Data collected via Twitter's REST API using rtweet (http://rtweet.info)") +
  ggsave("plots/members-of-congress.png",
    width = 9, height = 11, units = "in")

## cabinet members
cspan_data %>%
  filter(followers_count > 6e5) %>%
  cabinet_data() %>%
  mutate(followers_count = sqrt(followers_count)) %>%
  group_by(screen_name) %>%
  mutate(mean = mean(followers_count)) %>%
  ungroup() %>%
  arrange(rev(timestamp)) %>%
  mutate(lab = ifelse(duplicated(screen_name), NA, screen_name)) %>%
  ggplot(aes(x = timestamp, y = followers_count,
    colour = screen_name, label = lab)) +
  theme_mwk(base_family = "Roboto Condensed") +
  theme(legend.position = "none",
    axis.title = element_text(hjust = .95, face = "italic")) +
  geom_line(size = .6) +
  ggrepel::geom_label_repel(family = "Roboto Condensed", size = 2.5,
    label.size = .05, label.padding = .05, fill = "#ffffffbb") +
  labs(title = "Tracking follower counts for Cabinet members on Twitter",
    subtitle = "Tracking the number of Twitter followers of members of the Cabinet over time",
    x = NULL, y = "Number of followers (squared)",
    caption = "\nSource: Data collected via Twitter's REST API using rtweet (http://rtweet.info)") +
  ggsave("plots/the-cabinet.png",
    width = 9, height = 11, units = "in")

## governors
cspan_data %>%
  filter(followers_count > 100000) %>%
  governors_data() %>%
  mutate(followers_count = sqrt(followers_count)) %>%
  group_by(screen_name) %>%
  mutate(mean = mean(followers_count)) %>%
  ungroup() %>%
  arrange(rev(timestamp)) %>%
  mutate(lab = ifelse(duplicated(screen_name), NA, screen_name)) %>%
  ggplot(aes(x = timestamp, y = followers_count,
    colour = screen_name, label = lab)) +
  theme_mwk(base_family = "Roboto Condensed") +
  theme(legend.position = "none",
    axis.title = element_text(hjust = .95, face = "italic")) +
  geom_line(size = .6) +
  ggrepel::geom_label_repel(family = "Roboto Condensed", size = 2.5,
    label.size = .05, label.padding = .05, fill = "#ffffffbb") +
  labs(title = "Tracking follower counts for U.S. Governors on Twitter",
    subtitle = "Tracking the number of Twitter followers of Governors over time",
    x = NULL, y = "Number of followers (squared)",
    caption = "\nSource: Data collected via Twitter's REST API using rtweet (http://rtweet.info)") +
  ggsave("plots/governors.png", width = 9, height = 11, units = "in")

## add, commit, and push to git
tfse::add_to_git("auto update", pull = FALSE, interactive = FALSE)
