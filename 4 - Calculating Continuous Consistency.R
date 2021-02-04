<<<<<<< HEAD

# DY note:

# This script should be run fourth

# Requires: cses_cv_data.csv

# Outputs: cv_data_analysed.csv

# This script calculates the continuous consistency scores for each participant



library(magrittr)
library(reshape2)
library(dplyr)
library(psych)
library(ggplot2)

rm(list = ls())

data <- read.csv("cses_cv_data.csv")

parties <- LETTERS[1:9]

# FUNCTIONS

# directional difference calculation of ideologies:
directional_differences <- function(x, y) {
  ((x - 5)*(y - 5)) %>% divide_by(50) %>% add(0.5) # scaling between 0 and 1
}

proximity <- function(x, y) {
  Mod(x - y) %>% subtract(10, .) %>% divide_by(10)
}

# for calculating ideological utility:
ideo <- function(respondent_data_prefix, party_data, weight, func) {
  df <- parties %>% 
    sapply(function(P) 
      func(pull(data, paste0(respondent_data_prefix, P)), 
           party_data)) %>% 
      multiply_by(weight)
  colnames(df) %<>% paste0(., "_", respondent_data_prefix, "util")
  df
}

# for calculating evaluation and closeness utilities:
evcl <- function(respondent_data_prefix, weight) {
  df <- parties %>% sapply(function(P) pull(data, paste0(respondent_data_prefix, P)) * weight)
  colnames(df) %<>% paste0(., "_", respondent_data_prefix, "util")
  df
}

# averaging across dimensions for each party * participant, and applying the mask which hides disallowed
# parties where regional parties are found:
mask <- dplyr::select(data, contains("mask"))
conglomerate <- function(utilities) {
  df <- parties %>% sapply(function(P) dplyr::select(utilities, contains(paste0(P, "_"))) %>% 
                             apply(1, function(x) mean(x, na.rm=T))) * mask 
  colnames(df) <- parties
  df
}


# calculating the analytic variables:
analyse <- function(utilities) {
  analysis <- data.frame(
    max = apply(utilities, 1, function(x) max(x,na.rm=T)),
    min = apply(utilities, 1, function(x) min(x,na.rm=T)),
    voted = mapply(function(r, c) utilities[r, c], 
                   r = c(1:nrow(utilities)),
                   c = match(data$votes, colnames(utilities))) %>% 
      sapply(function(x) ifelse(is.null(x), NA, x)))
  
  max_parties <- parties %>% sapply(function(P) ifelse(analysis$max == pull(utilities, P), 1, 0))
  colnames(max_parties) %<>% paste0("max_", .)
  analysis %<>% cbind(max_parties)
  
  analysis %>% mutate(
    binary = ifelse(analysis$voted == analysis$max, 1, 0),
    continuous = 1 - ((max - voted)/(max - min))
  )
}

correct <- function(x) {
  x$all_tied <- 0                 
  x$all_tied[which(x$max == x$min)] <- 1 # coding for when all the party utilities are tied
  x$continuous[which(x$all_tied == 1)] <- 1 # assigning a continuous score of 1 in this case...
  x$continuous[which(is.na(x$voted) == TRUE)] <- NA # unless the participant didn't actually vote
  x
}

pipe <- function(x) {
  x %>% conglomerate %>% analyse %>% correct
}

# aggregating utility scores for each respondent * party * dimension (weighted and unweighted):
weighted <- cbind.data.frame(
  ideo("ideo_", data$IDEOLOGY, data$ideorange, directional_differences),
  evcl("eval_", data$GDPdiff),
  evcl("close_", data$demsatis)
  ) %>% pipe

weighted_unprejudiced <- cbind.data.frame(
  ideo("ideo_unprejudiced_", data$IDEOLOGY, data$ideorange_unprejudiced, directional_differences),
  evcl("eval_", data$GDPdiff),
  evcl("close_", data$demsatis)
) %>% pipe

colnames(weighted_unprejudiced) %<>% paste0(., "_unprejudiced")

unweighted <- cbind.data.frame(
  ideo("ideo_", c(1), data$ideorange, directional_differences),
  evcl("eval_", c(1)),
  evcl("close_", c(1))
) %>% pipe

colnames(unweighted) %<>% paste0(., "_unweighted")

proximal <- cbind.data.frame(
  ideo("ideo_", data$IDEOLOGY, data$ideorange, proximity),
  evcl("eval_", data$GDPdiff),
  evcl("close_", data$demsatis)
) %>% pipe

colnames(proximal) %<>% paste0(., "_proximal")


full <- cbind(data, weighted, unweighted, weighted_unprejudiced, proximal)



integrate_alliances <- function(additional) {
  
  vars <- c("max_A", "max_B", "max_C", "max_D", 
            "max_E", "max_F", "max_G", "max_H", "max_I")
  
  vars %<>% sapply(function(x) paste0(x, additional))
  
  maxes <- dplyr::select(
    full, all_of(vars))
  
  votes <- data$votes %>% dummy.code()
  
  alliances <- dplyr::select(full, contains("alliance"), -contains("correct_by"))
  
  booled_maxes <- alliances * maxes
  booled_votes <- alliances * votes
  
  all1_maxes <- booled_maxes %>% apply(1, function(x) any(x == 1))
  all2_maxes <- booled_maxes %>% apply(1, function(x) any(x == 2))
  all3_maxes <- booled_maxes %>% apply(1, function(x) any(x == 3))
  
  all1_votes <- booled_votes %>% apply(1, function(x) any(x == 1))
  all2_votes <- booled_votes %>% apply(1, function(x) any(x == 2))
  all3_votes <- booled_votes %>% apply(1, function(x) any(x == 3))
  
  correct_by_alliance <- rep(0, times = nrow(full))
  correct_by_alliance[which(all1_maxes == TRUE & all1_votes == TRUE)] <- 1
  correct_by_alliance[which(all2_maxes == TRUE & all2_votes == TRUE)] <- 1
  correct_by_alliance[which(all3_maxes == TRUE & all3_votes == TRUE)] <- 1
  
  correct_by_alliance
}


full$correct_by_alliances <- integrate_alliances("")
full$correct_by_alliances_unprejudiced <- integrate_alliances("_unprejudiced")
full$correct_by_alliances_unweighted <- integrate_alliances("_unweighted")
full$correct_by_alliances_proximal <- integrate_alliances("_proximal")

full %>% filter(binary == 0) %>% nrow
full %>% filter(binary == 0, correct_by_alliances == 1) %>% nrow
4314/80804 * 100 # about 5% of incorrect votes are cast for an allied party

write.csv(full, "cv_data_analysed.csv")
=======

# DY note:

# This script should be run fourth

# Requires: cses_cv_data.csv

# Outputs: cv_data_analysed.csv

# This script calculates the continuous consistency scores for each participant



library(magrittr)
library(reshape2)
library(dplyr)
library(psych)
library(ggplot2)

rm(list = ls())

data <- read.csv("cses_cv_data.csv")

parties <- LETTERS[1:9]

# FUNCTIONS

# directional difference calculation of ideologies:
directional_differences <- function(x, y) {
  ((x - 5)*(y - 5)) %>% divide_by(50) %>% add(0.5) # scaling between 0 and 1
}

proximity <- function(x, y) {
  Mod(x - y) %>% subtract(10, .) %>% divide_by(10)
}

# for calculating ideological utility:
ideo <- function(respondent_data_prefix, party_data, weight, func) {
  df <- parties %>% 
    sapply(function(P) 
      func(pull(data, paste0(respondent_data_prefix, P)), 
           party_data)) %>% 
      multiply_by(weight)
  colnames(df) %<>% paste0(., "_", respondent_data_prefix, "util")
  df
}

# for calculating evaluation and closeness utilities:
evcl <- function(respondent_data_prefix, weight) {
  df <- parties %>% sapply(function(P) pull(data, paste0(respondent_data_prefix, P)) * weight)
  colnames(df) %<>% paste0(., "_", respondent_data_prefix, "util")
  df
}

# averaging across dimensions for each party * participant, and applying the mask which hides disallowed
# parties where regional parties are found:
mask <- dplyr::select(data, contains("mask"))
conglomerate <- function(utilities) {
  df <- parties %>% sapply(function(P) dplyr::select(utilities, contains(paste0(P, "_"))) %>% 
                             apply(1, function(x) mean(x, na.rm=T))) * mask 
  colnames(df) <- parties
  df
}

# calculating the discriminability of the options:
discrim <- function(x) {
  y <- x[is.na(x) == FALSE]
  z <- y/max(y,na.rm=T)
  1 - ((sum(z) - 1)/(length(y) - 1))
}

# calculating the analytic variables:
analyse <- function(utilities) {
  analysis <- data.frame(
    max = apply(utilities, 1, function(x) max(x,na.rm=T)),
    min = apply(utilities, 1, function(x) min(x,na.rm=T)),
    discriminability = apply(utilities, 1, function(x) discrim(x)),
    voted = mapply(function(r, c) utilities[r, c], 
                   r = c(1:nrow(utilities)),
                   c = match(data$votes, colnames(utilities))) %>% 
      sapply(function(x) ifelse(is.null(x), NA, x)))
  
  max_parties <- parties %>% sapply(function(P) ifelse(analysis$max == pull(utilities, P), 1, 0))
  colnames(max_parties) %<>% paste0("max_", .)
  analysis %<>% cbind(max_parties)
  
  analysis %>% mutate(
    binary = ifelse(analysis$voted == analysis$max, 1, 0),
    continuous = 1 - ((max - voted)/(max - min)),
    weighted = 1 - (discriminability*((max - voted)/(max - min)))
  )
}

correct <- function(x) {
  x$all_tied <- 0
  x$all_tied[which(x$max == x$min)] <- 1
  x$continuous[which(x$all_tied == 1)] <- 1
  x$weighted[which(x$all_tied == 1)] <- 1
  x
}

pipe <- function(x) {
  x %>% conglomerate %>% analyse %>% correct
}

# aggregating utility scores for each respondent * party * dimension (weighted and unweighted):
weighted <- cbind.data.frame(
  ideo("ideo_", data$IDEOLOGY, data$ideorange, directional_differences),
  evcl("eval_", data$GDPdiff),
  evcl("close_", data$demsatis)
  ) %>% pipe

unweighted <- cbind.data.frame(
  ideo("ideo_", c(1), data$ideorange, directional_differences),
  evcl("eval_", c(1)),
  evcl("close_", c(1))
) %>% pipe

colnames(unweighted) %<>% paste0(., "_unweighted")

proximal_weighted <- cbind.data.frame(
  ideo("ideo_", data$IDEOLOGY, data$ideorange, proximity),
  evcl("eval_", data$GDPdiff),
  evcl("close_", data$demsatis)
) %>% pipe

colnames(proximal_weighted) %<>% paste0(., "_proximal_weighted")

proximal_unweighted <- cbind.data.frame(
  ideo("ideo_", c(1), data$ideorange, proximity),
  evcl("eval_", c(1)),
  evcl("close_", c(1))
) %>% pipe

colnames(proximal_unweighted) %<>% paste0(., "_proximal_unweighted")


full <- cbind(data, weighted, unweighted, proximal_weighted, proximal_unweighted)


integrate_alliances <- function(additional) {
  
  vars <- c("max_A", "max_B", "max_C", "max_D", 
            "max_E", "max_F", "max_G", "max_H", "max_I")
  
  vars %<>% sapply(function(x) paste0(x, additional))
  
  maxes <- dplyr::select(
    full, all_of(vars))
  
  votes <- data$votes %>% dummy.code()
  
  alliances <- dplyr::select(full, contains("alliance"), -contains("correct_by"))
  
  booled_maxes <- alliances * maxes
  booled_votes <- alliances * votes
  
  all1_maxes <- booled_maxes %>% apply(1, function(x) any(x == 1))
  all2_maxes <- booled_maxes %>% apply(1, function(x) any(x == 2))
  all3_maxes <- booled_maxes %>% apply(1, function(x) any(x == 3))
  
  all1_votes <- booled_votes %>% apply(1, function(x) any(x == 1))
  all2_votes <- booled_votes %>% apply(1, function(x) any(x == 2))
  all3_votes <- booled_votes %>% apply(1, function(x) any(x == 3))
  
  correct_by_alliance <- rep(0, times = nrow(full))
  correct_by_alliance[which(all1_maxes == TRUE & all1_votes == TRUE)] <- 1
  correct_by_alliance[which(all2_maxes == TRUE & all2_votes == TRUE)] <- 1
  correct_by_alliance[which(all3_maxes == TRUE & all3_votes == TRUE)] <- 1
  
  correct_by_alliance
}


full$correct_by_alliances <- integrate_alliances("")
full$correct_by_alliances_unweighted <- integrate_alliances("_unweighted")
full$correct_by_alliances_proximal_weighted <- integrate_alliances("_proximal_weighted")
full$correct_by_alliances_proximal_unweighted <- integrate_alliances("_proximal_unweighted")

full %>% filter(binary == 0) %>% nrow
full %>% filter(binary == 0, correct_by_alliances == 1) %>% nrow
4314/80804 * 100 # about 5% of incorrect votes are cast for an allied party

full %>% filter(binary_unweighted == 0) %>% nrow
full %>% filter(binary_unweighted == 0, correct_by_alliances_unweighted == 1) %>% nrow
3116/106943 * 100 # about 3% of incorrect votes are cast for an allied party

full %>% filter(binary_proximal_weighted == 0) %>% nrow
full %>% filter(binary_proximal_weighted == 0, correct_by_alliances_proximal_weighted == 1) %>% nrow
4591/86876 * 100 # about 5% of incorrect votes are cast for an allied party

full %>% filter(binary_proximal_unweighted == 0) %>% nrow
full %>% filter(binary_proximal_unweighted == 0, correct_by_alliances_proximal_unweighted == 1) %>% nrow
3083/109199 * 100 # about 3% of incorrect votes are cast for an allied party


data.frame(x = rep(c(1:10), times = 10),
           y = rep(c(1:10), each = 10)) %>%
  mutate(dd = mapply(directional_differences, x = x, y = y)) %>%
  ggplot(aes(x = x, y = y, colour = dd)) + geom_point() +
  scale_colour_gradient(high = "green", low = "red") 

data.frame(x = rep(c(1:10), times = 10),
           y = rep(c(1:10), each = 10)) %>%
  mutate(p = mapply(proximity, x = x, y = y)) %>%
  ggplot(aes(x = x, y = y, colour = p)) + geom_point() +
  scale_colour_gradient(high = "green", low = "red")

write.csv(full, "cv_data_analysed.csv")
>>>>>>> 7dfd48aff9080e84e5bf37914150baa32b3f585c
