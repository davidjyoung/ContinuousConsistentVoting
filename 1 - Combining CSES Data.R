<<<<<<< HEAD

# DY note:

# This script should be run first.

# In this script, I gather together variables from across the CSES 1-5 modules
# into one dataset, converting variables into a common scale when required.

# Requires: cses1, cses2, cses3, cses4, cses5

# Outputs: CSES_all.csv

library(readxl)
library(haven)
library(readxl)
library(haven)
library(ggplot2)
library(reshape2)
library(varhandle)
library(magrittr)
library(dplyr)

rm(list = ls())

CSES1 <- read_sav("cses1.sav")
CSES2 <- read_sav("cses2.sav")
CSES3 <- read_sav("cses3.sav")
CSES4 <- read_sav("cses4.sav")
CSES5 <- read_sav("cses5.sav")

PPMATRIX <- read.csv("PPMATRIX_trim.csv")

permittedcodes <- PPMATRIX$Num %>% unique

labels1 <- CSES1 %>% colnames %>% sapply(function(x) pull(CSES1, x) %>% attr(., "label"))
labels2 <- CSES2 %>% colnames %>% sapply(function(x) pull(CSES2, x) %>% attr(., "label"))
labels3 <- CSES3 %>% colnames %>% sapply(function(x) pull(CSES3, x) %>% attr(., "label"))
labels4 <- CSES4 %>% colnames %>% sapply(function(x) pull(CSES4, x) %>% attr(., "label"))
labels5 <- CSES5 %>% colnames %>% sapply(function(x) pull(CSES5, x) %>% attr(., "label"))

search <- function(TERM) {
  list(labels1[grep(TERM, labels1)],
       labels2[grep(TERM, labels2)],
       labels3[grep(TERM, labels3)],
       labels4[grep(TERM, labels4)],
       labels5[grep(TERM, labels5)])
}

Sample_weights <- c(CSES1$A1012_1,
                    CSES2$B1012_1,
                    CSES3$C1012_1,
                    CSES4$D1012_1,
                    CSES5$E1012_1)

# Efficacy questions

# makes a difference who is in power (external)
# 5 = low agreement CSES1-2, high agreement CSES3-5

Efficacy1 <- c(CSES1$A3028 %>% ifelse(between(., 1, 5), ., NA) %>% (function(x) ((5 - x) * 0.25)),
               CSES2$B3013 %>% ifelse(between(., 1, 5), ., NA) %>% (function(x) ((5 - x) * 0.25)),
               CSES3$C3004 %>% ifelse(between(., 1, 5), ., NA) %>% (function(x) ((x - 1)/4)),
               CSES4$D3009 %>% ifelse(between(., 1, 5), ., NA) %>% (function(x) ((x - 1)/4)),
               CSES5$E3016_1 %>% ifelse(between(., 1, 5), ., NA) %>% (function(x) ((x - 1)/4)))

# makes a difference who you vote for (external)
# 5 = high agreement CSES1-5

Efficacy2 <- c(CSES1$A3029 %>% ifelse(between(., 1, 5), ., NA) %>% (function(x) ((x - 1)/4)),
               CSES2$B3014 %>% ifelse(between(., 1, 5), ., NA) %>% (function(x) ((x - 1)/4)),
               CSES3$C3005 %>% ifelse(between(., 1, 5), ., NA) %>% (function(x) ((x - 1)/4)),
               CSES4$D3010 %>% ifelse(between(., 1, 5), ., NA) %>% (function(x) ((x - 1)/4)),
               CSES5$E3016_2 %>% ifelse(between(., 1, 5), ., NA) %>% (function(x) ((x - 1)/4)))



#Respondents' ideology scores:

Resp_Ideology <- c(CSES1$A3031 %>% as.numeric,
                   CSES2$B3045 %>% as.numeric,
                   CSES3$C3013 %>% as.numeric,
                   CSES4$D3014 %>% as.numeric,
                   CSES5$E3020 %>% as.numeric) %>% 
  ifelse(between(., 0, 10) == FALSE, NA, .)


#Respondents' evaluations of the government 
# I create evaluation variables scaled between 0 and 1 and 'directional' versions scaled between -1 and 1

# CSES1:

# whether economy got better or worse
# combining scores to the three questions and rescaling:
Eval_Econ_1_CSES1 <- ifelse(CSES1$A3023 == 3, 3, 0) + 
  sapply(CSES1$A3024, function(x) ifelse(x %in% c(8, 9), 2, x) %>% ifelse(. %in% c(1, 2), ., 0)) +
  sapply(CSES1$A3025, function(x) ifelse(x %in% c(8, 9), 4, x) %>% ifelse(. %in% c(4, 5), ., 0))
Eval_Econ_1_CSES1 %<>% ifelse(. %in% c(1:5), ., NA) %>% sapply(function(x) 0.25*(5 - x))

# economy improvement in last year
Eval_Econ_2_CSES1 <- CSES1$A3022 %>% sapply(function(x) ifelse(between(x, 1, 5) == FALSE, NA, x)) %>% 
  sapply(function(x) 0.25*(5 - x))

# taking the mean, ignoring missing values
Eval_CSES1 <- mapply(function(x, y) mean(c(x,y), na.rm=T), x = Eval_Econ_1_CSES1, y = Eval_Econ_2_CSES1) 
Eval_CSES1_directional <- Eval_CSES1 %>% sapply(function(x) (2*x) - 1)


# CSES2:

# gov performance on most important issue
Eval_CSES2 <- CSES2$B3011 %>% sapply(function(x) ifelse(between(x, 1, 4) == FALSE, NA, x)) %>% 
  sapply(function(x) 1/3*(4 - x))
Eval_CSES2_directional <- Eval_CSES2 %>% sapply(function(x) (2*x) - 1)

cbind(Eval_CSES2, CSES2$B3011) %>% unique

# CSES3:

# gov performance on most important issue
Eval_CSES3 <- CSES3$C3006 %>% sapply(function(x) ifelse(between(x, 1, 4) == FALSE, NA, x)) %>% 
  sapply(function(x) 1/3*(4 - x))
Eval_CSES3_directional <- Eval_CSES3 %>% sapply(function(x) (2*x) - 1)

# CSES4:

# state of the economy
Eval_CSES4 <- ifelse(CSES4$D3003_1 == 3, 3, 0) + 
  sapply(CSES4$D3003_2, function(x) ifelse(x %in% c(7, 8), 2, x) %>% ifelse(. %in% c(1, 2), ., 0)) +
  sapply(CSES4$D3003_3, function(x) ifelse(x %in% c(7, 8), 4, x) %>% ifelse(. %in% c(4, 5), ., 0))
Eval_CSES4 %<>% ifelse(. %in% c(1:5), ., NA) %>% sapply(function(x) 0.25*(5 - x))
Eval_CSES4_directional <- Eval_CSES4 %>% sapply(function(x) (2*x) - 1)


# CSES5:

# state of the economy
Eval_Econ_1_CSES5 <- CSES5$E3011 %>% sapply(function(x) ifelse(between(x, 1, 5) == FALSE, NA, x)) %>% 
  sapply(function(x) 0.25*(5 - x))

# general performance of gov
Eval_Gov_General_CSES5 <- CSES5$E3009 %>% sapply(function(x) ifelse(between(x, 1, 4) == FALSE, NA, x)) %>% 
  sapply(function(x) 1/3*(4 - x))
Eval_CSES5 <- mapply(function(x, y) mean(c(x,y), na.rm=T), x = Eval_Econ_1_CSES5, y = Eval_Gov_General_CSES5) 
Eval_CSES5_directional <- Eval_CSES5 %>% sapply(function(x) (2*x) - 1)

Evaluations <- c(Eval_CSES1, Eval_CSES2, Eval_CSES3, Eval_CSES4, Eval_CSES5)
Evaluations_directional <- c(Eval_CSES1_directional, Eval_CSES2_directional, 
                             Eval_CSES3_directional, Eval_CSES4_directional, 
                             Eval_CSES5_directional)


# Respondents' closeness evaluations

# There are five variables that code for information related to party closeness,
# however two of these are only available in CSES1 and CSES2

close_cses1 <- cbind(CSES1$A3009, CSES1$A3005_1, CSES1$A3007_1, CSES1$A3011) %>% 
    apply(1, function(x) x[which(between(x, 1, 98)== TRUE)][1] %>% ifelse(length(.) == 0, NA, .)) %>% 
    unlist

close_cses2 <- cbind(CSES2$B3033, CSES2$B3029_1, CSES2$B3031_1, CSES2$B3035) %>% 
  apply(1, function(x) x[which(between(x, 1, 98)== TRUE)][1] %>% ifelse(length(.) == 0, NA, .)) %>% 
  unlist

  
CLOSEST <- c(close_cses1,
             close_cses2,
             CSES3$C3020_3,
             CSES4$D3018_3,
             CSES5$E3024_3) %>% ifelse(. %in% permittedcodes == TRUE, ., NA)

DOC_raw <- c(CSES1$A3012 %>% as.numeric,
             CSES2$B3036 %>% as.numeric,
             CSES3$C3020_4 %>% as.numeric,
             CSES4$D3018_4 %>% as.numeric,
             CSES5$E3024_4 %>% as.numeric)


DOC <- DOC_raw %>% sapply(function(x) ifelse(x %in% c(1:3) == FALSE, NA, x)) %>% 
  sapply(function(x) 1/3*(4 - x))
DOC[which(is.na(DOC) == TRUE)] <- 2/3 
# replacing NAs with the modal answer for Ss who say they are close but don't provide a closeness score 



# Region of residence

Region <- c(CSES1$A2019 %>% as.numeric,
            CSES2$B2027 %>% as.numeric,
            CSES3$C2027 %>% as.numeric,
            CSES4$D2028 %>% as.numeric,
            CSES5$E2020 %>% as.numeric)

# Judged ideologies 

judged_ideologies <- data.frame(
  JI_A =                c(CSES1$A3032_A %>% as.numeric,
                       CSES2$B3038_A %>% as.numeric,
                       CSES3$C3011_A %>% as.numeric,
                       CSES4$D3013_A %>% as.numeric,
                       CSES5$E3019_A %>% as.numeric),
  JI_B =                c(CSES1$A3032_B %>% as.numeric,
                       CSES2$B3038_B %>% as.numeric,
                       CSES3$C3011_B %>% as.numeric,
                       CSES4$D3013_B %>% as.numeric,
                       CSES5$E3019_B %>% as.numeric),
  JI_C =                c(CSES1$A3032_C %>% as.numeric,
                       CSES2$B3038_C %>% as.numeric,
                       CSES3$C3011_C %>% as.numeric,
                       CSES4$D3013_C %>% as.numeric,
                       CSES5$E3019_C %>% as.numeric),
  JI_D =                c(CSES1$A3032_D %>% as.numeric,
                       CSES2$B3038_D %>% as.numeric,
                       CSES3$C3011_D %>% as.numeric,
                       CSES4$D3013_D %>% as.numeric,
                       CSES5$E3019_D %>% as.numeric),
  JI_E =                c(CSES1$A3032_E %>% as.numeric,
                       CSES2$B3038_E %>% as.numeric,
                       CSES3$C3011_E %>% as.numeric,
                       CSES4$D3013_E %>% as.numeric,
                       CSES5$E3019_E %>% as.numeric),
  JI_F =                c(CSES1$A3032_F %>% as.numeric,
                       CSES2$B3038_F %>% as.numeric,
                       CSES3$C3011_F %>% as.numeric,
                       CSES4$D3013_F %>% as.numeric,
                       CSES5$E3019_F %>% as.numeric),
  JI_G =                c(CSES1$A3032_G %>% as.numeric,
                       CSES2$B3038_G %>% as.numeric,
                       CSES3$C3011_G %>% as.numeric,
                       CSES4$D3013_G %>% as.numeric,
                       CSES5$E3019_G %>% as.numeric),
  JI_H =                c(CSES1$A3032_H %>% as.numeric,
                       CSES2$B3038_H %>% as.numeric,
                       CSES3$C3011_H %>% as.numeric,
                       CSES4$D3013_H %>% as.numeric,
                       CSES5$E3019_H %>% as.numeric),
  JI_I =                c(CSES1$A3032_I %>% as.numeric,
                       CSES2$B3038_I %>% as.numeric,
                       CSES3$C3011_I %>% as.numeric,
                       CSES4$D3013_I %>% as.numeric,
                       CSES5$E3019_I %>% as.numeric)) %>% transmute_all(function(x) ifelse(between(x, 0, 10) == FALSE, NA, x))


# Expert ideologies

expert_ideologies <- data.frame(
  ExI_A = c(CSES1$A5004_A,
       CSES2$B5018_A,
       CSES3$C5017_A,
       CSES4$D5017_A,
       CSES5$E5018_A),
  ExI_B = c(CSES1$A5004_B,
       CSES2$B5018_B,
       CSES3$C5017_B,
       CSES4$D5017_B,
       CSES5$E5018_B),
  ExI_C = c(CSES1$A5004_C,
       CSES2$B5018_C,
       CSES3$C5017_C,
       CSES4$D5017_C,
       CSES5$E5018_C),
  ExI_D = c(CSES1$A5004_D,
       CSES2$B5018_D,
       CSES3$C5017_D,
       CSES4$D5017_D,
       CSES5$E5018_D),
  ExI_E = c(CSES1$A5004_E,
       CSES2$B5018_E,
       CSES3$C5017_E,
       CSES4$D5017_E,
       CSES5$E5018_E),
  ExI_F = c(CSES1$A5004_F,
       CSES2$B5018_F,
       CSES3$C5017_F,
       CSES4$D5017_F,
       CSES5$E5018_F),
  ExI_G = c(rep(NA, times = nrow(CSES1)),
       CSES2$B5018_G,
       CSES3$C5017_G,
       CSES4$D5017_G,
       CSES5$E5018_G),
  ExI_H = c(rep(NA, times = nrow(CSES1)),
       CSES2$B5018_H,
       CSES3$C5017_H,
       CSES4$D5017_H,
       CSES5$E5018_H),
  ExI_I = c(rep(NA, times = nrow(CSES1)),
       CSES2$B5018_I,
       CSES3$C5017_I,
       CSES4$D5017_I,
       CSES5$E5018_I)
) %>% transmute_all(function(x) ifelse(between(x, 0, 10) == FALSE, NA, x))


# Education level

# need to convert CSES1 and CSES2 education variables onto ISCED scale used in CSES3-5

ISCEDconvert <- function(x) {case_when(
                             x == 1 ~ 96,
                             x == 2 ~ 1,
                             x == 3 ~ 2,
                             x == 4 ~ 3,
                             x == 5 ~ 4,
                             x == 6 ~ 5,
                             x == 7 ~ 6,
                             x == 8 ~ 7) }

Education_level <- c(CSES1$A2003 %>% ISCEDconvert,
                       CSES2$B2003 %>% ISCEDconvert,
                       CSES3$C2003 %>% ISCEDconvert,
                       CSES4$D2003,
                       CSES5$E2003)

# converting to a one_point scale

Education <- Education_level %>% (function(x) ifelse(x > 9, NA, x)) %>% 
  (function(x) ifelse(x %in% c(8, 9), 7, x)) %>% 
  subtract(1) %>% divide_by(6)

# Political Knowledge

# new approach - all codes other than "CORRECT" are treated as incorrect
# all participants scored out of three - n(CORRECT)/3
# if no questions are asked for an election, everyone scores NA

recode_with_labels <- function(x) {
  x %>% factor(levels = attr(x, "labels") %>% as.numeric,
               labels = attr(x, "labels") %>% attr(., "names"))
}

convert <- function(x) {
  recode_with_labels(x) %>% sapply(function(x) as.character(x) %>% strsplit(., ". ") %>% (function(x) x[[1]][2]))
}

mark <- function(x) {
  x %>% apply(1, function(x) length(which(x == "CORRECT"))/3)
}

# determining the elections where no knowledge questions were asked:

c1 <- CSES1 %>% select(A2023:A2025) %>% transmute_all(convert) %>%
  mutate(election = CSES1$A1004)

c1 %<>% mutate(allmissing = ifelse(A2023 == "MISSING" &
                                     A2024 == "MISSING" &
                                     A2025 == "MISSING", 1, 0))

no.k <- c1 %>% group_by(election) %>% summarise(A = mean(allmissing)) %>%
  filter(A == 1) %>% pull(election)


c2 <- CSES2 %>% select(B3047_1:B3047_3) %>% transmute_all(convert) %>%
  mutate(election = CSES2$B1004)

c2 %<>% mutate(
  allmissing = ifelse(
    B3047_1 == "MISSING" &
    B3047_2 == "MISSING" &
    B3047_3 == "MISSING", 1, 0))

no.k <- c2 %>% group_by(election) %>% summarise(A = mean(allmissing)) %>%
  filter(A == 1) %>% pull(election) %>% c(no.k, .)


c3 <- CSES3 %>% select(C3036_1:C3036_3) %>% transmute_all(convert) %>%
  mutate(election = CSES3$C1004)

c3 %<>% mutate(
  allmissing = ifelse(
    C3036_1 == "MISSING" &
      C3036_2 == "MISSING" &
      C3036_3 == "MISSING", 1, 0))

no.k <- c3 %>% group_by(election) %>% summarise(A = mean(allmissing)) %>%
  filter(A == 1) %>% pull(election) %>% c(no.k, .)


c4 <- CSES4 %>% select(D3025_1_A:D3025_4_A_PT) %>% transmute_all(convert) %>%
  mutate(election = CSES4$D1004)

c4$allmissing = c4 %>% select(-election) %>% apply(1, function(x) all(x == "MISSING")) %>% 
  (function(x) ifelse(x == "FALSE", 0, 1))

no.k <- c4 %>% group_by(election) %>% summarise(A = mean(allmissing)) %>%
  filter(A == 1) %>% pull(election) %>% c(no.k, .)

know_cses1 <- cbind(convert(CSES1$A2023), convert(CSES1$A2024), convert(CSES1$A2025)) %>% mark
know_cses2 <- cbind(convert(CSES2$B3047_1), convert(CSES2$B3047_2), convert(CSES2$B3047_3)) %>% mark
know_cses3 <- cbind(convert(CSES3$C3036_1), convert(CSES3$C3036_2), convert(CSES3$C3036_3)) %>% mark
know_cses4 <- cbind(convert(CSES4$D3025_1_A), convert(CSES4$D3025_2_A), convert(CSES4$D3025_3_A), 
                    convert(CSES4$D3025_4_A), convert(CSES4$D3025_2_A_PT), convert(CSES4$D3025_4_A_PT)) %>% mark


NAs <- which(c(CSES1$A1004, CSES2$B1004, CSES3$C1004, CSES4$D1004) %in% no.k)

polknow = c(know_cses1, know_cses2, know_cses3, know_cses4, rep(NA, times = nrow(CSES5)))

polknow[NAs] <- NA

polsoph_raw <- cbind(Education, polknow) %>% apply(1, function(x) mean(x, na.rm=T))


# like-dislike

like_ratings <- data.frame(
  LRP_A = c(CSES1$A3020_A %>% as.numeric,
        CSES2$B3037_A %>% as.numeric,
        CSES3$C3009_A %>% as.numeric,
        CSES4$D3011_A %>% as.numeric,
        CSES5$E3017_A %>% as.numeric),
  LRP_B = c(CSES1$A3020_B %>% as.numeric,
        CSES2$B3037_B %>% as.numeric,
        CSES3$C3009_B %>% as.numeric,
        CSES4$D3011_B %>% as.numeric,
        CSES5$E3017_B %>% as.numeric),
  LRP_C = c(CSES1$A3020_C %>% as.numeric,
        CSES2$B3037_C %>% as.numeric,
        CSES3$C3009_C %>% as.numeric,
        CSES4$D3011_C %>% as.numeric,
        CSES5$E3017_C %>% as.numeric),
  LRP_D = c(CSES1$A3020_D %>% as.numeric,
        CSES2$B3037_D %>% as.numeric,
        CSES3$C3009_D %>% as.numeric,
        CSES4$D3011_D %>% as.numeric,
        CSES5$E3017_D %>% as.numeric),
  LRP_E = c(CSES1$A3020_E %>% as.numeric,
        CSES2$B3037_E %>% as.numeric,
        CSES3$C3009_E %>% as.numeric,
        CSES4$D3011_E %>% as.numeric,
        CSES5$E3017_E %>% as.numeric),
  LRP_F = c(CSES1$A3020_F %>% as.numeric,
        CSES2$B3037_F %>% as.numeric,
        CSES3$C3009_F %>% as.numeric,
        CSES4$D3011_F %>% as.numeric,
        CSES5$E3017_F %>% as.numeric),
  LRP_G = c(rep(NA, times = nrow(CSES1)),
        CSES2$B3037_G %>% as.numeric,
        CSES3$C3009_G %>% as.numeric,
        CSES4$D3011_G %>% as.numeric,
        CSES5$E3017_G %>% as.numeric),
  LRP_H = c(rep(NA, times = nrow(CSES1)),
        CSES2$B3037_H %>% as.numeric,
        CSES3$C3009_H %>% as.numeric,
        CSES4$D3011_H %>% as.numeric,
        CSES5$E3017_H %>% as.numeric),
  LRP_I = c(rep(NA, times = nrow(CSES1)),
        CSES2$B3037_I %>% as.numeric,
        CSES3$C3009_I %>% as.numeric,
        CSES4$D3011_I %>% as.numeric,
        CSES5$E3017_I %>% as.numeric)
) %>% transmute_all(function(x) ifelse(between(x, 0, 10) == FALSE, NA, x) %>% divide_by(10))

# election name

Election_Name <- c(CSES1$A1004,
                   CSES2$B1004,
                   CSES3$C1004,
                   CSES4$D1004,
                   CSES5$E1004)

# dataset

Dataset <- c(CSES1$A1001,
             CSES2$B1001,
             CSES3$C1001,
             CSES4$D1001,
             CSES5$E1001)

# votes

define_filler <- function(dataset, code) {
  if (code != "filler") { 
    col <- c(dataset[ ,which(colnames(dataset) == code), drop = TRUE])
  } else {
    col <- c(rep(NA, times = nrow(dataset)))}
}

CON_FROM_EACH <- function(code1, code2, code3, code4, code5) {
  col1 <- define_filler(CSES1, code1)
  col2 <- define_filler(CSES2, code2)
  col3 <- define_filler(CSES3, code3)
  col4 <- define_filler(CSES4, code4)
  col5 <- define_filler(CSES5, code5)
  return(c(col1, col2, col3, col4, col5))
}

CON_FROM_EACH_NUMERIC <- function(code1, code2, code3, code4, code5) {
  col1 <- define_filler(CSES1, code1) %>% as.numeric
  col2 <- define_filler(CSES2, code2) %>% as.numeric
  col3 <- define_filler(CSES3, code3) %>% as.numeric
  col4 <- define_filler(CSES4, code4) %>% as.numeric
  col5 <- define_filler(CSES5, code5) %>% as.numeric
  return(c(col1, col2, col3, col4, col5))
}



Vote_District_List <- CON_FROM_EACH(
  "A2030", "filler", "filler", "filler", "filler") %>% ifelse(. %in% permittedcodes == TRUE, ., NA)

Vote_District_Candidate <- CON_FROM_EACH(
  "A2031", "filler", "filler", "filler", "filler") %>% ifelse(. %in% permittedcodes == TRUE, ., NA)

Vote_President_1 <- CON_FROM_EACH_NUMERIC(
  "A2029", "B3005_1", "C3023_PR_1", "D3006_PR_1", "E3013_PR_1") %>% ifelse(. %in% permittedcodes == TRUE, ., NA)

Vote_President_2 <- CON_FROM_EACH(
  "filler", "B3005_2", "C3023_PR_2", "D3006_PR_2", "E3013_PR_2") %>% ifelse(. %in% permittedcodes == TRUE, ., NA)

Vote_Lower_House_1 <- CON_FROM_EACH(
  "filler", "B3006_1", "filler", "filler", "filler") %>% ifelse(. %in% permittedcodes == TRUE, ., NA)

Vote_Lower_House_2 <- CON_FROM_EACH(
  "filler", "B3006_2", "filler", "filler", "filler") %>% ifelse(. %in% permittedcodes == TRUE, ., NA)

Vote_Upper_House_1 <- CON_FROM_EACH(
  "filler", "B3007_1", "filler", "filler", "filler") %>% ifelse(. %in% permittedcodes == TRUE, ., NA)

Vote_Upper_House_2 <- CON_FROM_EACH(
  "filler", "B3007_2", "filler", "filler", "filler") %>% ifelse(. %in% permittedcodes == TRUE, ., NA)

Vote_Lower_House_List <- CON_FROM_EACH(
  "filler", "filler", "C3023_LH_PL", "D3006_LH_PL", "E3013_LH_PL") %>% ifelse(. %in% permittedcodes == TRUE, ., NA)

Vote_Lower_House_Candidate <- CON_FROM_EACH(
  "filler", "filler", "C3023_LH_DC", "D3006_LH_DC", "E3013_LH_DC") %>% ifelse(. %in% permittedcodes == TRUE, ., NA)

Vote_Upper_House_List <- CON_FROM_EACH(
  "filler", "filler", "C3023_UH_PL", "D3006_UH_PL", "E3013_UH_PL") %>% ifelse(. %in% permittedcodes == TRUE, ., NA)

Vote_Upper_House_Candidate_1 <- CON_FROM_EACH(
  "filler", "filler", "C3023_UH_DC_1", "D3006_UH_DC", "E3013_UH_DC_1") %>% ifelse(. %in% permittedcodes == TRUE, ., NA)

Vote_Upper_House_Candidate_2 <- CON_FROM_EACH(
  "filler", "filler", "C3023_UH_DC_2", "filler", "E3013_UH_DC_2") %>% ifelse(. %in% permittedcodes == TRUE, ., NA)

Vote_Upper_House_Candidate_3 <- CON_FROM_EACH(
  "filler", "filler", "C3023_UH_DC_3", "filler", "filler") %>% ifelse(. %in% permittedcodes == TRUE, ., NA)

Vote_Upper_House_Candidate_4 <- CON_FROM_EACH(
  "filler", "filler", "C3023_UH_DC_4", "filler", "filler") %>% ifelse(. %in% permittedcodes == TRUE, ., NA)

Respondent_ID <- CON_FROM_EACH(
  "A1005", "B1005", "C1005", "D1005", "E1005")

Region <- c(CSES1$A2019,
            CSES2$B2027,
            CSES3$C2027,
            CSES4$D2028,
            CSES5$E2020)

# INDIVIDUAL-LEVEL PREDICTORS

# Age

CSES1_age <- pull(CSES1, A2001)

# need to correct for Peruvian data being categorically-coded
# I look up the mean age for each category in the next-closest Peruvian sample
# from CSES2, and replace each category code with the appropriate mean

Peru_ages <- filter(CSES2, B1004 == "PER_2006") %>% pull(., B2001) %>% as.numeric(.)
PERU_L1_rep <- which(between(Peru_ages, 18, 25) == TRUE) %>% Peru_ages[.] %>% mean(.) %>% round(.)
PERU_L2_rep <- which(between(Peru_ages, 26, 35) == TRUE) %>% Peru_ages[.] %>% mean(.) %>% round(.)
PERU_L3_rep <- which(between(Peru_ages, 36, 45) == TRUE) %>% Peru_ages[.] %>% mean(.) %>% round(.)
PERU_L4_rep <- which(between(Peru_ages, 46, 65) == TRUE) %>% Peru_ages[.] %>% mean(.) %>% round(.)

Peru_1 <- as.factor(CSES1_age[which(CSES1$A1004 == "PER_2000")])
Peru_2 <- as.factor(CSES1_age[which(CSES1$A1004 == "PER_2001")])


Peru_1 %<>% dplyr::recode("1" = PERU_L1_rep,
                          "2" = PERU_L2_rep,
                          "3" = PERU_L3_rep,
                          "4" = PERU_L4_rep)
Peru_2 %<>% dplyr::recode(., "1" = PERU_L1_rep,
                          "2" = PERU_L2_rep,
                          "3" = PERU_L3_rep,
                          "4" = PERU_L4_rep)
CSES1_age %<>% replace(., which(CSES1$A1004 == "PER_2000"), Peru_1)
CSES1_age %<>% replace(., which(CSES1$A1004 == "PER_2001"), Peru_2)

CSES2_age <- pull(CSES2, B2001)

# Kyrgyzstan needs de-categorising in CSES2 but we don't investigate 
# this election owing to a low level of democracy in the country

CSES3_age <- pull(CSES3, C2001)

# the code booklet implies a similar re-coding to Peru needs to be done 
# for Taiwan 2008, however, the ages appear to be coded appropriately already
# but those over 90 in the US were recorded as 1, so need to change them

grep("USA", CSES2$B1004) %>% .[1] %>% CSES2$B1004[.]

USA_ages <- filter(CSES2, B1004 == "USA_2004") %>% pull(., B2001) %>% as.numeric(.)
USA_90plus <- which(between(USA_ages, 90, max(USA_ages)) == TRUE) %>% USA_ages[.] %>% mean(.) %>% round(.)

CSES3_age %<>% replace(., which(CSES3_age == 1), USA_90plus)

CSES4_yobs <- as.numeric(CSES4$D2001_Y)
CSES4_yobs %<>% replace(., which(CSES4_yobs > 1998), NA)

CSES5_yobs <- as.numeric(CSES5$E2001_Y)
CSES5_yobs %<>% replace(., which(CSES5_yobs > 2001), NA)


CSES4_age <- as.numeric(CSES4$D5024_3) - CSES4_yobs
CSES5_age <- as.numeric(CSES5$E5026_3) - CSES5_yobs

ages <- c(CSES1_age,
          CSES2_age,
          CSES3_age,
          CSES4_age,
          CSES5_age)

ages <- replace(ages, which(ages > 150), NA)


# Political Sophistication







# Efficacy

clean_efficacy <- function(x) {
  y <- replace(x, which(x > 5), NA)
  (y - 1)/4 %>% return(.)
}

CSES1_eff1 <- clean_efficacy(CSES1$A3028)
CSES1_eff2 <- clean_efficacy(CSES1$A3029)
CSES2_eff1 <- clean_efficacy(CSES2$B3013)
CSES2_eff2 <- clean_efficacy(CSES2$B3014)
CSES3_eff1 <- clean_efficacy(CSES3$C3004)
CSES3_eff2 <- clean_efficacy(CSES3$C3005)
CSES4_eff1 <- clean_efficacy(CSES4$D3009)
CSES4_eff2 <- clean_efficacy(CSES4$D3010)
CSES5_eff1 <- clean_efficacy(CSES5$E3016_1)
CSES5_eff2 <- clean_efficacy(CSES5$E3016_2)

# In CSES1 and CSES2, these questions both initially have opposing
# correspondences between answer score and degree of agreement compared to the other datasets

CSES1_eff1 <- 1 - CSES1_eff1  
CSES2_eff1 <- 1 - CSES2_eff1 

eff1 <- c(CSES1_eff1,
          CSES2_eff1,
          CSES3_eff1,
          CSES4_eff1,
          CSES5_eff1)

eff2 <- c(CSES1_eff2,
          CSES2_eff2,
          CSES3_eff2,
          CSES4_eff2,
          CSES5_eff2)

efficacy <- cbind(eff1, eff2) %>% apply(1, function(x) sum(x)/2)



# education

# political knowledge

# efficacy

# vote shares - lower house and presidential ~ enep and party polarization index



efrors <- data.frame(efficacy, Election_Name) %>% group_by(Election_Name) %>% 
  dplyr::summarise(n = n(), x = length(which(is.na(efficacy) == FALSE)), perc = 100 * x/n)
efrors$perc %>% unique %>% sort

polsop <- data.frame(polsoph_raw, Election_Name) %>% 
  group_by(Election_Name) %>% 
  dplyr::summarise(n = n(), 
            x = length(which(is.na(polsoph_raw) == FALSE)), 
            perc = 100 * x/n)

arrange(polsop, perc)




agesss <- data.frame(ages, Election_Name) %>% 
  group_by(Election_Name) %>% 
  dplyr::summarise(n = n(), 
            x = length(which(is.na(ages) == FALSE)), 
            perc = 100 * x/n)
agesss %>% arrange(perc) %>% ggplot(aes(x = perc)) + geom_density()

agesss$perc %>% unique %>% sort


ideoss <- data.frame(Resp_Ideology, Election_Name) %>% 
  group_by(Election_Name) %>% 
  dplyr::summarise(n = n(), 
            x = length(which(is.na(Resp_Ideology) == FALSE)), 
            perc = 100 * x/n)
ideoss %>% arrange(perc) %>% ggplot(aes(x = perc)) + geom_density()

ideoss$perc %>% unique %>% sort


evals <- data.frame(Evaluations, Election_Name) %>% 
  group_by(Election_Name) %>% 
  dplyr::summarise(n = n(), 
            x = length(which(is.na(Evaluations) == FALSE)), 
            perc = 100 * x/n)
evals %>% arrange(perc) %>% ggplot(aes(x = perc)) + geom_density()

evals$perc %>% unique %>% sort


close1 <- data.frame(CLOSEST, Election_Name) %>% 
  group_by(Election_Name) %>% 
  dplyr::summarise(n = n(), 
            x = length(which(is.na(CLOSEST) == FALSE)), 
            perc = 100 * x/n)
close1 %>% arrange(perc) %>% ggplot(aes(x = perc)) + geom_density()

  
efrors %>% filter(perc < 50) %>% arrange(perc)
agesss %>% filter(perc < 50) %>% arrange(perc)
ideoss %>% filter(perc < 50) %>% arrange(perc)
evals %>% filter(perc < 50) %>% arrange(perc)
close1 %>% filter(perc < 50) %>% arrange(perc)

CSES_all <- data.frame(
  RESPONDENT_ID = as.factor(Respondent_ID),
  DATASET = Dataset,
  ELECTION = Election_Name,
  SAMPLE_WEIGHTS = Sample_weights,
  POLKNOW = polsoph_raw,
  EFFICACY = efficacy,
  AGE = ages,
  IDEOLOGY = Resp_Ideology,
  EDUCATION = Education,
  EVALUATION = Evaluations,
  EVALUATION_DIRECTIONAL = Evaluations_directional,
  CLOSEST_PARTY = CLOSEST,
  DEGREE_OF_CLOSENESS = DOC,
  like_ratings,
  expert_ideologies,
  judged_ideologies,
  Vote_District_Candidate,
  Vote_District_List,
  Vote_Lower_House_1,
  Vote_Lower_House_2,
  Vote_Lower_House_Candidate,
  Vote_Lower_House_List,
  Vote_President_1,
  Vote_President_2,
  Vote_Upper_House_1,
  Vote_Upper_House_2,
  Vote_Upper_House_Candidate_1,
  Vote_Upper_House_Candidate_2,
  Vote_Upper_House_Candidate_3,
  Vote_Upper_House_Candidate_4,
  Vote_Upper_House_List,
  REGION_OF_RESIDENCE = Region
)

polsoph <- vector()

for (m in 1:5) {
  filt <- filter(CSES_all, DATASET == paste0("CSES-MODULE-", m))
  for (e in 1:length(unique(filt$ELECTION))) {
    elec <- unique(filt$ELECTION)[e]
    polsoph[which(CSES_all$ELECTION == elec & CSES_all$DATASET == paste0("CSES-MODULE-", m))] <-
      filter(filt, ELECTION == elec) %>% pull(POLKNOW) %>% scale %>% as.numeric
  }
}


CSES_all$polsoph <- polsoph

CSES_all %>% group_by(ELECTION) %>% dplyr::summarise(M = mean(polsoph, na.rm=T) %>% round(10)) %>% View

write.csv(CSES_all, "CSES_all.csv")






=======

# DY note:

# This script should be run first.

# In this script, I gather together variables from across the CSES 1-5 modules
# into one dataset, converting variables into a common scale when required.

# Requires: cses1, cses2, cses3, cses4, cses5

# Outputs: CSES_all.csv

library(readxl)
library(haven)
library(readxl)
library(haven)
library(ggplot2)
library(reshape2)
library(varhandle)
library(magrittr)
library(dplyr)

rm(list = ls())

CSES1 <- read_sav("cses1.sav")
CSES2 <- read_sav("cses2.sav")
CSES3 <- read_sav("cses3.sav")
CSES4 <- read_sav("cses4.sav")
CSES5 <- read_sav("cses5.sav")

PPMATRIX <- read.csv("PPMATRIX_trim.csv")

permittedcodes <- PPMATRIX$Num %>% unique

labels1 <- CSES1 %>% colnames %>% sapply(function(x) pull(CSES1, x) %>% attr(., "label"))
labels2 <- CSES2 %>% colnames %>% sapply(function(x) pull(CSES2, x) %>% attr(., "label"))
labels3 <- CSES3 %>% colnames %>% sapply(function(x) pull(CSES3, x) %>% attr(., "label"))
labels4 <- CSES4 %>% colnames %>% sapply(function(x) pull(CSES4, x) %>% attr(., "label"))
labels5 <- CSES5 %>% colnames %>% sapply(function(x) pull(CSES5, x) %>% attr(., "label"))

search <- function(TERM) {
  list(labels1[grep(TERM, labels1)],
       labels2[grep(TERM, labels2)],
       labels3[grep(TERM, labels3)],
       labels4[grep(TERM, labels4)],
       labels5[grep(TERM, labels5)])
}

Sample_weights <- c(CSES1$A1012_1,
                    CSES2$B1012_1,
                    CSES3$C1012_1,
                    CSES4$D1012_1,
                    CSES5$E1012_1)

# Efficacy questions

# makes a difference who is in power (external)
# 5 = low agreement CSES1-2, high agreement CSES3-5

Efficacy1 <- c(CSES1$A3028 %>% ifelse(between(., 1, 5), ., NA) %>% (function(x) ((5 - x) * 0.25)),
               CSES2$B3013 %>% ifelse(between(., 1, 5), ., NA) %>% (function(x) ((5 - x) * 0.25)),
               CSES3$C3004 %>% ifelse(between(., 1, 5), ., NA) %>% (function(x) ((x - 1)/4)),
               CSES4$D3009 %>% ifelse(between(., 1, 5), ., NA) %>% (function(x) ((x - 1)/4)),
               CSES5$E3016_1 %>% ifelse(between(., 1, 5), ., NA) %>% (function(x) ((x - 1)/4)))

# makes a difference who you vote for (external)
# 5 = high agreement CSES1-5

Efficacy2 <- c(CSES1$A3029 %>% ifelse(between(., 1, 5), ., NA) %>% (function(x) ((x - 1)/4)),
               CSES2$B3014 %>% ifelse(between(., 1, 5), ., NA) %>% (function(x) ((x - 1)/4)),
               CSES3$C3005 %>% ifelse(between(., 1, 5), ., NA) %>% (function(x) ((x - 1)/4)),
               CSES4$D3010 %>% ifelse(between(., 1, 5), ., NA) %>% (function(x) ((x - 1)/4)),
               CSES5$E3016_2 %>% ifelse(between(., 1, 5), ., NA) %>% (function(x) ((x - 1)/4)))



#Respondents' ideology scores:

Resp_Ideology <- c(CSES1$A3031 %>% as.numeric,
                   CSES2$B3045 %>% as.numeric,
                   CSES3$C3013 %>% as.numeric,
                   CSES4$D3014 %>% as.numeric,
                   CSES5$E3020 %>% as.numeric) %>% 
  ifelse(between(., 0, 10) == FALSE, NA, .)


#Respondents' evaluations of the government 
# I create evaluation variables scaled between 0 and 1 and 'directional' versions scaled between -1 and 1

# CSES1:

# whether economy got better or worse
# combining scores to the three questions and rescaling:
Eval_Econ_1_CSES1 <- ifelse(CSES1$A3023 == 3, 3, 0) + 
  sapply(CSES1$A3024, function(x) ifelse(x %in% c(8, 9), 2, x) %>% ifelse(. %in% c(1, 2), ., 0)) +
  sapply(CSES1$A3025, function(x) ifelse(x %in% c(8, 9), 4, x) %>% ifelse(. %in% c(4, 5), ., 0))
Eval_Econ_1_CSES1 %<>% ifelse(. %in% c(1:5), ., NA) %>% sapply(function(x) 0.25*(5 - x))

# economy improvement in last year
Eval_Econ_2_CSES1 <- CSES1$A3022 %>% sapply(function(x) ifelse(between(x, 1, 5) == FALSE, NA, x)) %>% 
  sapply(function(x) 0.25*(5 - x))

# taking the mean, ignoring missing values
Eval_CSES1 <- mapply(function(x, y) mean(c(x,y), na.rm=T), x = Eval_Econ_1_CSES1, y = Eval_Econ_2_CSES1) 
Eval_CSES1_directional <- Eval_CSES1 %>% sapply(function(x) (2*x) - 1)


# CSES2:

# gov performance on most important issue
Eval_CSES2 <- CSES2$B3011 %>% sapply(function(x) ifelse(between(x, 1, 4) == FALSE, NA, x)) %>% 
  sapply(function(x) 1/3*(4 - x))
Eval_CSES2_directional <- Eval_CSES2 %>% sapply(function(x) (2*x) - 1)

cbind(Eval_CSES2, CSES2$B3011) %>% unique

# CSES3:

# gov performance on most important issue
Eval_CSES3 <- CSES3$C3006 %>% sapply(function(x) ifelse(between(x, 1, 4) == FALSE, NA, x)) %>% 
  sapply(function(x) 1/3*(4 - x))
Eval_CSES3_directional <- Eval_CSES3 %>% sapply(function(x) (2*x) - 1)

# CSES4:

# state of the economy
Eval_CSES4 <- ifelse(CSES4$D3003_1 == 3, 3, 0) + 
  sapply(CSES4$D3003_2, function(x) ifelse(x %in% c(7, 8), 2, x) %>% ifelse(. %in% c(1, 2), ., 0)) +
  sapply(CSES4$D3003_3, function(x) ifelse(x %in% c(7, 8), 4, x) %>% ifelse(. %in% c(4, 5), ., 0))
Eval_CSES4 %<>% ifelse(. %in% c(1:5), ., NA) %>% sapply(function(x) 0.25*(5 - x))
Eval_CSES4_directional <- Eval_CSES4 %>% sapply(function(x) (2*x) - 1)


# CSES5:

# state of the economy
Eval_Econ_1_CSES5 <- CSES5$E3011 %>% sapply(function(x) ifelse(between(x, 1, 5) == FALSE, NA, x)) %>% 
  sapply(function(x) 0.25*(5 - x))

# general performance of gov
Eval_Gov_General_CSES5 <- CSES5$E3009 %>% sapply(function(x) ifelse(between(x, 1, 4) == FALSE, NA, x)) %>% 
  sapply(function(x) 1/3*(4 - x))
Eval_CSES5 <- mapply(function(x, y) mean(c(x,y), na.rm=T), x = Eval_Econ_1_CSES5, y = Eval_Gov_General_CSES5) 
Eval_CSES5_directional <- Eval_CSES5 %>% sapply(function(x) (2*x) - 1)

Evaluations <- c(Eval_CSES1, Eval_CSES2, Eval_CSES3, Eval_CSES4, Eval_CSES5)
Evaluations_directional <- c(Eval_CSES1_directional, Eval_CSES2_directional, 
                             Eval_CSES3_directional, Eval_CSES4_directional, 
                             Eval_CSES5_directional)


# Respondents' closeness evaluations

# There are five variables that code for information related to party closeness,
# however two of these are only available in CSES1 and CSES2

close_cses1 <- cbind(CSES1$A3009, CSES1$A3005_1, CSES1$A3007_1, CSES1$A3011) %>% 
    apply(1, function(x) x[which(between(x, 1, 98)== TRUE)][1] %>% ifelse(length(.) == 0, NA, .)) %>% 
    unlist

close_cses2 <- cbind(CSES2$B3033, CSES2$B3029_1, CSES2$B3031_1, CSES2$B3035) %>% 
  apply(1, function(x) x[which(between(x, 1, 98)== TRUE)][1] %>% ifelse(length(.) == 0, NA, .)) %>% 
  unlist

  
CLOSEST <- c(close_cses1,
             close_cses2,
             CSES3$C3020_3,
             CSES4$D3018_3,
             CSES5$E3024_3) %>% ifelse(. %in% permittedcodes == TRUE, ., NA)

DOC_raw <- c(CSES1$A3012 %>% as.numeric,
             CSES2$B3036 %>% as.numeric,
             CSES3$C3020_4 %>% as.numeric,
             CSES4$D3018_4 %>% as.numeric,
             CSES5$E3024_4 %>% as.numeric)


DOC <- DOC_raw %>% sapply(function(x) ifelse(x %in% c(1:3) == FALSE, NA, x)) %>% 
  sapply(function(x) 1/3*(4 - x))
DOC[which(is.na(DOC) == TRUE)] <- 2/3 
# replacing NAs with the modal answer for Ss who say they are close but don't provide a closeness score 



# Region of residence

Region <- c(CSES1$A2019 %>% as.numeric,
            CSES2$B2027 %>% as.numeric,
            CSES3$C2027 %>% as.numeric,
            CSES4$D2028 %>% as.numeric,
            CSES5$E2020 %>% as.numeric)

# Judged ideologies 

judged_ideologies <- data.frame(
  JI_A =                c(CSES1$A3032_A %>% as.numeric,
                       CSES2$B3038_A %>% as.numeric,
                       CSES3$C3011_A %>% as.numeric,
                       CSES4$D3013_A %>% as.numeric,
                       CSES5$E3019_A %>% as.numeric),
  JI_B =                c(CSES1$A3032_B %>% as.numeric,
                       CSES2$B3038_B %>% as.numeric,
                       CSES3$C3011_B %>% as.numeric,
                       CSES4$D3013_B %>% as.numeric,
                       CSES5$E3019_B %>% as.numeric),
  JI_C =                c(CSES1$A3032_C %>% as.numeric,
                       CSES2$B3038_C %>% as.numeric,
                       CSES3$C3011_C %>% as.numeric,
                       CSES4$D3013_C %>% as.numeric,
                       CSES5$E3019_C %>% as.numeric),
  JI_D =                c(CSES1$A3032_D %>% as.numeric,
                       CSES2$B3038_D %>% as.numeric,
                       CSES3$C3011_D %>% as.numeric,
                       CSES4$D3013_D %>% as.numeric,
                       CSES5$E3019_D %>% as.numeric),
  JI_E =                c(CSES1$A3032_E %>% as.numeric,
                       CSES2$B3038_E %>% as.numeric,
                       CSES3$C3011_E %>% as.numeric,
                       CSES4$D3013_E %>% as.numeric,
                       CSES5$E3019_E %>% as.numeric),
  JI_F =                c(CSES1$A3032_F %>% as.numeric,
                       CSES2$B3038_F %>% as.numeric,
                       CSES3$C3011_F %>% as.numeric,
                       CSES4$D3013_F %>% as.numeric,
                       CSES5$E3019_F %>% as.numeric),
  JI_G =                c(CSES1$A3032_G %>% as.numeric,
                       CSES2$B3038_G %>% as.numeric,
                       CSES3$C3011_G %>% as.numeric,
                       CSES4$D3013_G %>% as.numeric,
                       CSES5$E3019_G %>% as.numeric),
  JI_H =                c(CSES1$A3032_H %>% as.numeric,
                       CSES2$B3038_H %>% as.numeric,
                       CSES3$C3011_H %>% as.numeric,
                       CSES4$D3013_H %>% as.numeric,
                       CSES5$E3019_H %>% as.numeric),
  JI_I =                c(CSES1$A3032_I %>% as.numeric,
                       CSES2$B3038_I %>% as.numeric,
                       CSES3$C3011_I %>% as.numeric,
                       CSES4$D3013_I %>% as.numeric,
                       CSES5$E3019_I %>% as.numeric)) %>% transmute_all(function(x) ifelse(between(x, 0, 10) == FALSE, NA, x))


# Expert ideologies

expert_ideologies <- data.frame(
  ExI_A = c(CSES1$A5004_A,
       CSES2$B5018_A,
       CSES3$C5017_A,
       CSES4$D5017_A,
       CSES5$E5018_A),
  ExI_B = c(CSES1$A5004_B,
       CSES2$B5018_B,
       CSES3$C5017_B,
       CSES4$D5017_B,
       CSES5$E5018_B),
  ExI_C = c(CSES1$A5004_C,
       CSES2$B5018_C,
       CSES3$C5017_C,
       CSES4$D5017_C,
       CSES5$E5018_C),
  ExI_D = c(CSES1$A5004_D,
       CSES2$B5018_D,
       CSES3$C5017_D,
       CSES4$D5017_D,
       CSES5$E5018_D),
  ExI_E = c(CSES1$A5004_E,
       CSES2$B5018_E,
       CSES3$C5017_E,
       CSES4$D5017_E,
       CSES5$E5018_E),
  ExI_F = c(CSES1$A5004_F,
       CSES2$B5018_F,
       CSES3$C5017_F,
       CSES4$D5017_F,
       CSES5$E5018_F),
  ExI_G = c(rep(NA, times = nrow(CSES1)),
       CSES2$B5018_G,
       CSES3$C5017_G,
       CSES4$D5017_G,
       CSES5$E5018_G),
  ExI_H = c(rep(NA, times = nrow(CSES1)),
       CSES2$B5018_H,
       CSES3$C5017_H,
       CSES4$D5017_H,
       CSES5$E5018_H),
  ExI_I = c(rep(NA, times = nrow(CSES1)),
       CSES2$B5018_I,
       CSES3$C5017_I,
       CSES4$D5017_I,
       CSES5$E5018_I)
) %>% transmute_all(function(x) ifelse(between(x, 0, 10) == FALSE, NA, x))


# Education level

# need to convert CSES1 and CSES2 education variables onto ISCED scale used in CSES3-5

ISCEDconvert <- function(x) {case_when(
                             x == 1 ~ 96,
                             x == 2 ~ 1,
                             x == 3 ~ 2,
                             x == 4 ~ 3,
                             x == 5 ~ 4,
                             x == 6 ~ 5,
                             x == 7 ~ 6,
                             x == 8 ~ 7) }

Education_level <- c(CSES1$A2003 %>% ISCEDconvert,
                       CSES2$B2003 %>% ISCEDconvert,
                       CSES3$C2003 %>% ISCEDconvert,
                       CSES4$D2003,
                       CSES5$E2003)

# converting to a one_point scale

Education <- Education_level %>% (function(x) ifelse(x > 9, NA, x)) %>% 
  (function(x) ifelse(x %in% c(8, 9), 7, x)) %>% 
  subtract(1) %>% divide_by(6)


# Political Knowledge

# new approach - all codes other than "CORRECT" are treated as incorrect
# all participants scored out of three - n(CORRECT)/3
# if no questions are asked for an election, everyone scores NA

recode_with_labels <- function(x) {
  x %>% factor(levels = attr(x, "labels") %>% as.numeric,
               labels = attr(x, "labels") %>% attr(., "names"))
}

convert <- function(x) {
  recode_with_labels(x) %>% sapply(function(x) as.character(x) %>% strsplit(., ". ") %>% (function(x) x[[1]][2]))
}

mark <- function(x) {
  x %>% apply(1, function(x) length(which(x == "CORRECT"))/3)
}

# determining the elections where no knowledge questions were asked:

c1 <- CSES1 %>% select(A2023:A2025) %>% transmute_all(convert) %>%
  mutate(election = CSES1$A1004)

c1 %<>% mutate(allmissing = ifelse(A2023 == "MISSING" &
                                     A2024 == "MISSING" &
                                     A2025 == "MISSING", 1, 0))

no.k <- c1 %>% group_by(election) %>% summarise(A = mean(allmissing)) %>%
  filter(A == 1) %>% pull(election)


c2 <- CSES2 %>% select(B3047_1:B3047_3) %>% transmute_all(convert) %>%
  mutate(election = CSES2$B1004)

c2 %<>% mutate(
  allmissing = ifelse(
    B3047_1 == "MISSING" &
    B3047_2 == "MISSING" &
    B3047_3 == "MISSING", 1, 0))

no.k <- c2 %>% group_by(election) %>% summarise(A = mean(allmissing)) %>%
  filter(A == 1) %>% pull(election) %>% c(no.k, .)


c3 <- CSES3 %>% select(C3036_1:C3036_3) %>% transmute_all(convert) %>%
  mutate(election = CSES3$C1004)

c3 %<>% mutate(
  allmissing = ifelse(
    C3036_1 == "MISSING" &
      C3036_2 == "MISSING" &
      C3036_3 == "MISSING", 1, 0))

no.k <- c3 %>% group_by(election) %>% summarise(A = mean(allmissing)) %>%
  filter(A == 1) %>% pull(election) %>% c(no.k, .)


c4 <- CSES4 %>% select(D3025_1_A:D3025_4_A_PT) %>% transmute_all(convert) %>%
  mutate(election = CSES4$D1004)

c4$allmissing = c4 %>% select(-election) %>% apply(1, function(x) all(x == "MISSING")) %>% 
  (function(x) ifelse(x == "FALSE", 0, 1))

no.k <- c4 %>% group_by(election) %>% summarise(A = mean(allmissing)) %>%
  filter(A == 1) %>% pull(election) %>% c(no.k, .)

know_cses1 <- cbind(convert(CSES1$A2023), convert(CSES1$A2024), convert(CSES1$A2025)) %>% mark
know_cses2 <- cbind(convert(CSES2$B3047_1), convert(CSES2$B3047_2), convert(CSES2$B3047_3)) %>% mark
know_cses3 <- cbind(convert(CSES3$C3036_1), convert(CSES3$C3036_2), convert(CSES3$C3036_3)) %>% mark
know_cses4 <- cbind(convert(CSES4$D3025_1_A), convert(CSES4$D3025_2_A), convert(CSES4$D3025_3_A), 
                    convert(CSES4$D3025_4_A), convert(CSES4$D3025_2_A_PT), convert(CSES4$D3025_4_A_PT)) %>% mark


NAs <- which(c(CSES1$A1004, CSES2$B1004, CSES3$C1004, CSES4$D1004) %in% no.k)

polknow = c(know_cses1, know_cses2, know_cses3, know_cses4, rep(NA, times = nrow(CSES5)))

polknow[NAs] <- NA

polsoph_raw <- cbind(Education, polknow) %>% apply(1, function(x) mean(x, na.rm=T))


# like-dislike

like_ratings <- data.frame(
  LRP_A = c(CSES1$A3020_A %>% as.numeric,
        CSES2$B3037_A %>% as.numeric,
        CSES3$C3009_A %>% as.numeric,
        CSES4$D3011_A %>% as.numeric,
        CSES5$E3017_A %>% as.numeric),
  LRP_B = c(CSES1$A3020_B %>% as.numeric,
        CSES2$B3037_B %>% as.numeric,
        CSES3$C3009_B %>% as.numeric,
        CSES4$D3011_B %>% as.numeric,
        CSES5$E3017_B %>% as.numeric),
  LRP_C = c(CSES1$A3020_C %>% as.numeric,
        CSES2$B3037_C %>% as.numeric,
        CSES3$C3009_C %>% as.numeric,
        CSES4$D3011_C %>% as.numeric,
        CSES5$E3017_C %>% as.numeric),
  LRP_D = c(CSES1$A3020_D %>% as.numeric,
        CSES2$B3037_D %>% as.numeric,
        CSES3$C3009_D %>% as.numeric,
        CSES4$D3011_D %>% as.numeric,
        CSES5$E3017_D %>% as.numeric),
  LRP_E = c(CSES1$A3020_E %>% as.numeric,
        CSES2$B3037_E %>% as.numeric,
        CSES3$C3009_E %>% as.numeric,
        CSES4$D3011_E %>% as.numeric,
        CSES5$E3017_E %>% as.numeric),
  LRP_F = c(CSES1$A3020_F %>% as.numeric,
        CSES2$B3037_F %>% as.numeric,
        CSES3$C3009_F %>% as.numeric,
        CSES4$D3011_F %>% as.numeric,
        CSES5$E3017_F %>% as.numeric),
  LRP_G = c(rep(NA, times = nrow(CSES1)),
        CSES2$B3037_G %>% as.numeric,
        CSES3$C3009_G %>% as.numeric,
        CSES4$D3011_G %>% as.numeric,
        CSES5$E3017_G %>% as.numeric),
  LRP_H = c(rep(NA, times = nrow(CSES1)),
        CSES2$B3037_H %>% as.numeric,
        CSES3$C3009_H %>% as.numeric,
        CSES4$D3011_H %>% as.numeric,
        CSES5$E3017_H %>% as.numeric),
  LRP_I = c(rep(NA, times = nrow(CSES1)),
        CSES2$B3037_I %>% as.numeric,
        CSES3$C3009_I %>% as.numeric,
        CSES4$D3011_I %>% as.numeric,
        CSES5$E3017_I %>% as.numeric)
) %>% transmute_all(function(x) ifelse(between(x, 0, 10) == FALSE, NA, x) %>% divide_by(10))

# election name

Election_Name <- c(CSES1$A1004,
                   CSES2$B1004,
                   CSES3$C1004,
                   CSES4$D1004,
                   CSES5$E1004)

# dataset

Dataset <- c(CSES1$A1001,
             CSES2$B1001,
             CSES3$C1001,
             CSES4$D1001,
             CSES5$E1001)

# votes

define_filler <- function(dataset, code) {
  if (code != "filler") { 
    col <- c(dataset[ ,which(colnames(dataset) == code), drop = TRUE])
  } else {
    col <- c(rep(NA, times = nrow(dataset)))}
}

CON_FROM_EACH <- function(code1, code2, code3, code4, code5) {
  col1 <- define_filler(CSES1, code1)
  col2 <- define_filler(CSES2, code2)
  col3 <- define_filler(CSES3, code3)
  col4 <- define_filler(CSES4, code4)
  col5 <- define_filler(CSES5, code5)
  return(c(col1, col2, col3, col4, col5))
}

CON_FROM_EACH_NUMERIC <- function(code1, code2, code3, code4, code5) {
  col1 <- define_filler(CSES1, code1) %>% as.numeric
  col2 <- define_filler(CSES2, code2) %>% as.numeric
  col3 <- define_filler(CSES3, code3) %>% as.numeric
  col4 <- define_filler(CSES4, code4) %>% as.numeric
  col5 <- define_filler(CSES5, code5) %>% as.numeric
  return(c(col1, col2, col3, col4, col5))
}



Vote_District_List <- CON_FROM_EACH(
  "A2030", "filler", "filler", "filler", "filler") %>% ifelse(. %in% permittedcodes == TRUE, ., NA)

Vote_District_Candidate <- CON_FROM_EACH(
  "A2031", "filler", "filler", "filler", "filler") %>% ifelse(. %in% permittedcodes == TRUE, ., NA)

Vote_President_1 <- CON_FROM_EACH_NUMERIC(
  "A2029", "B3005_1", "C3023_PR_1", "D3006_PR_1", "E3013_PR_1") %>% ifelse(. %in% permittedcodes == TRUE, ., NA)

Vote_President_2 <- CON_FROM_EACH(
  "filler", "B3005_2", "C3023_PR_2", "D3006_PR_2", "E3013_PR_2") %>% ifelse(. %in% permittedcodes == TRUE, ., NA)

Vote_Lower_House_1 <- CON_FROM_EACH(
  "filler", "B3006_1", "filler", "filler", "filler") %>% ifelse(. %in% permittedcodes == TRUE, ., NA)

Vote_Lower_House_2 <- CON_FROM_EACH(
  "filler", "B3006_2", "filler", "filler", "filler") %>% ifelse(. %in% permittedcodes == TRUE, ., NA)

Vote_Upper_House_1 <- CON_FROM_EACH(
  "filler", "B3007_1", "filler", "filler", "filler") %>% ifelse(. %in% permittedcodes == TRUE, ., NA)

Vote_Upper_House_2 <- CON_FROM_EACH(
  "filler", "B3007_2", "filler", "filler", "filler") %>% ifelse(. %in% permittedcodes == TRUE, ., NA)

Vote_Lower_House_List <- CON_FROM_EACH(
  "filler", "filler", "C3023_LH_PL", "D3006_LH_PL", "E3013_LH_PL") %>% ifelse(. %in% permittedcodes == TRUE, ., NA)

Vote_Lower_House_Candidate <- CON_FROM_EACH(
  "filler", "filler", "C3023_LH_DC", "D3006_LH_DC", "E3013_LH_DC") %>% ifelse(. %in% permittedcodes == TRUE, ., NA)

Vote_Upper_House_List <- CON_FROM_EACH(
  "filler", "filler", "C3023_UH_PL", "D3006_UH_PL", "E3013_UH_PL") %>% ifelse(. %in% permittedcodes == TRUE, ., NA)

Vote_Upper_House_Candidate_1 <- CON_FROM_EACH(
  "filler", "filler", "C3023_UH_DC_1", "D3006_UH_DC", "E3013_UH_DC_1") %>% ifelse(. %in% permittedcodes == TRUE, ., NA)

Vote_Upper_House_Candidate_2 <- CON_FROM_EACH(
  "filler", "filler", "C3023_UH_DC_2", "filler", "E3013_UH_DC_2") %>% ifelse(. %in% permittedcodes == TRUE, ., NA)

Vote_Upper_House_Candidate_3 <- CON_FROM_EACH(
  "filler", "filler", "C3023_UH_DC_3", "filler", "filler") %>% ifelse(. %in% permittedcodes == TRUE, ., NA)

Vote_Upper_House_Candidate_4 <- CON_FROM_EACH(
  "filler", "filler", "C3023_UH_DC_4", "filler", "filler") %>% ifelse(. %in% permittedcodes == TRUE, ., NA)

Respondent_ID <- CON_FROM_EACH(
  "A1005", "B1005", "C1005", "D1005", "E1005")

Region <- c(CSES1$A2019,
            CSES2$B2027,
            CSES3$C2027,
            CSES4$D2028,
            CSES5$E2020)

# INDIVIDUAL-LEVEL PREDICTORS

# Age

CSES1_age <- pull(CSES1, A2001)

# need to correct for Peruvian data being categorically-coded
# I look up the mean age for each category in the next-closest Peruvian sample
# from CSES2, and replace each category code with the appropriate mean

Peru_ages <- filter(CSES2, B1004 == "PER_2006") %>% pull(., B2001) %>% as.numeric(.)
PERU_L1_rep <- which(between(Peru_ages, 18, 25) == TRUE) %>% Peru_ages[.] %>% mean(.) %>% round(.)
PERU_L2_rep <- which(between(Peru_ages, 26, 35) == TRUE) %>% Peru_ages[.] %>% mean(.) %>% round(.)
PERU_L3_rep <- which(between(Peru_ages, 36, 45) == TRUE) %>% Peru_ages[.] %>% mean(.) %>% round(.)
PERU_L4_rep <- which(between(Peru_ages, 46, 65) == TRUE) %>% Peru_ages[.] %>% mean(.) %>% round(.)

Peru_1 <- as.factor(CSES1_age[which(CSES1$A1004 == "PER_2000")])
Peru_2 <- as.factor(CSES1_age[which(CSES1$A1004 == "PER_2001")])


Peru_1 %<>% dplyr::recode("1" = PERU_L1_rep,
                          "2" = PERU_L2_rep,
                          "3" = PERU_L3_rep,
                          "4" = PERU_L4_rep)
Peru_2 %<>% dplyr::recode(., "1" = PERU_L1_rep,
                          "2" = PERU_L2_rep,
                          "3" = PERU_L3_rep,
                          "4" = PERU_L4_rep)
CSES1_age %<>% replace(., which(CSES1$A1004 == "PER_2000"), Peru_1)
CSES1_age %<>% replace(., which(CSES1$A1004 == "PER_2001"), Peru_2)

CSES2_age <- pull(CSES2, B2001)

# Kyrgyzstan needs de-categorising in CSES2 but we don't investigate 
# this election owing to a low level of democracy in the country

CSES3_age <- pull(CSES3, C2001)

# the code booklet implies a similar re-coding to Peru needs to be done 
# for Taiwan 2008, however, the ages appear to be coded appropriately already
# but those over 90 in the US were recorded as 1, so need to change them

grep("USA", CSES2$B1004) %>% .[1] %>% CSES2$B1004[.]

USA_ages <- filter(CSES2, B1004 == "USA_2004") %>% pull(., B2001) %>% as.numeric(.)
USA_90plus <- which(between(USA_ages, 90, max(USA_ages)) == TRUE) %>% USA_ages[.] %>% mean(.) %>% round(.)

CSES3_age %<>% replace(., which(CSES3_age == 1), USA_90plus)

CSES4_yobs <- as.numeric(CSES4$D2001_Y)
CSES4_yobs %<>% replace(., which(CSES4_yobs > 1998), NA)

CSES5_yobs <- as.numeric(CSES5$E2001_Y)
CSES5_yobs %<>% replace(., which(CSES5_yobs > 2001), NA)


CSES4_age <- as.numeric(CSES4$D5024_3) - CSES4_yobs
CSES5_age <- as.numeric(CSES5$E5026_3) - CSES5_yobs

ages <- c(CSES1_age,
          CSES2_age,
          CSES3_age,
          CSES4_age,
          CSES5_age)

ages <- replace(ages, which(ages > 150), NA)


# Political Sophistication







# Efficacy

clean_efficacy <- function(x) {
  y <- replace(x, which(x > 5), NA)
  (y - 1)/4 %>% return(.)
}

CSES1_eff1 <- clean_efficacy(CSES1$A3028)
CSES1_eff2 <- clean_efficacy(CSES1$A3029)
CSES2_eff1 <- clean_efficacy(CSES2$B3013)
CSES2_eff2 <- clean_efficacy(CSES2$B3014)
CSES3_eff1 <- clean_efficacy(CSES3$C3004)
CSES3_eff2 <- clean_efficacy(CSES3$C3005)
CSES4_eff1 <- clean_efficacy(CSES4$D3009)
CSES4_eff2 <- clean_efficacy(CSES4$D3010)
CSES5_eff1 <- clean_efficacy(CSES5$E3016_1)
CSES5_eff2 <- clean_efficacy(CSES5$E3016_2)

# In CSES1 and CSES2, these questions both initially have opposing
# correspondences between answer score and degree of agreement compared to the other datasets

CSES1_eff1 <- 1 - CSES1_eff1  
CSES2_eff1 <- 1 - CSES2_eff1 

eff1 <- c(CSES1_eff1,
          CSES2_eff1,
          CSES3_eff1,
          CSES4_eff1,
          CSES5_eff1)

eff2 <- c(CSES1_eff2,
          CSES2_eff2,
          CSES3_eff2,
          CSES4_eff2,
          CSES5_eff2)

efficacy <- cbind(eff1, eff2) %>% apply(1, function(x) sum(x)/2)



# education

# political knowledge

# efficacy

# vote shares - lower house and presidential ~ enep and party polarization index



efrors <- data.frame(efficacy, Election_Name) %>% group_by(Election_Name) %>% 
  dplyr::summarise(n = n(), x = length(which(is.na(efficacy) == FALSE)), perc = 100 * x/n)
efrors$perc %>% unique %>% sort

polsop <- data.frame(polsoph_raw, Election_Name) %>% 
  group_by(Election_Name) %>% 
  dplyr::summarise(n = n(), 
            x = length(which(is.na(polsoph_raw) == FALSE)), 
            perc = 100 * x/n)

arrange(polsop, perc)




agesss <- data.frame(ages, Election_Name) %>% 
  group_by(Election_Name) %>% 
  dplyr::summarise(n = n(), 
            x = length(which(is.na(ages) == FALSE)), 
            perc = 100 * x/n)
agesss %>% arrange(perc) %>% ggplot(aes(x = perc)) + geom_density()

agesss$perc %>% unique %>% sort


ideoss <- data.frame(Resp_Ideology, Election_Name) %>% 
  group_by(Election_Name) %>% 
  dplyr::summarise(n = n(), 
            x = length(which(is.na(Resp_Ideology) == FALSE)), 
            perc = 100 * x/n)
ideoss %>% arrange(perc) %>% ggplot(aes(x = perc)) + geom_density()

ideoss$perc %>% unique %>% sort


evals <- data.frame(Evaluations, Election_Name) %>% 
  group_by(Election_Name) %>% 
  dplyr::summarise(n = n(), 
            x = length(which(is.na(Evaluations) == FALSE)), 
            perc = 100 * x/n)
evals %>% arrange(perc) %>% ggplot(aes(x = perc)) + geom_density()

evals$perc %>% unique %>% sort


close1 <- data.frame(CLOSEST, Election_Name) %>% 
  group_by(Election_Name) %>% 
  dplyr::summarise(n = n(), 
            x = length(which(is.na(CLOSEST) == FALSE)), 
            perc = 100 * x/n)
close1 %>% arrange(perc) %>% ggplot(aes(x = perc)) + geom_density()

  
efrors %>% filter(perc < 50) %>% arrange(perc)
agesss %>% filter(perc < 50) %>% arrange(perc)
ideoss %>% filter(perc < 50) %>% arrange(perc)
evals %>% filter(perc < 50) %>% arrange(perc)
close1 %>% filter(perc < 50) %>% arrange(perc)

CSES_all <- data.frame(
  RESPONDENT_ID = as.factor(Respondent_ID),
  DATASET = Dataset,
  ELECTION = Election_Name,
  SAMPLE_WEIGHTS = Sample_weights,
  POLKNOW = polsoph_raw,
  EFFICACY = efficacy,
  AGE = ages,
  IDEOLOGY = Resp_Ideology,
  EDUCATION = Education,
  EVALUATION = Evaluations,
  EVALUATION_DIRECTIONAL = Evaluations_directional,
  CLOSEST_PARTY = CLOSEST,
  DEGREE_OF_CLOSENESS = DOC,
  like_ratings,
  expert_ideologies,
  judged_ideologies,
  Vote_District_Candidate,
  Vote_District_List,
  Vote_Lower_House_1,
  Vote_Lower_House_2,
  Vote_Lower_House_Candidate,
  Vote_Lower_House_List,
  Vote_President_1,
  Vote_President_2,
  Vote_Upper_House_1,
  Vote_Upper_House_2,
  Vote_Upper_House_Candidate_1,
  Vote_Upper_House_Candidate_2,
  Vote_Upper_House_Candidate_3,
  Vote_Upper_House_Candidate_4,
  Vote_Upper_House_List,
  REGION_OF_RESIDENCE = Region
)

polsoph <- vector()

for (m in 1:5) {
  filt <- filter(CSES_all, DATASET == paste0("CSES-MODULE-", m))
  for (e in 1:length(unique(filt$ELECTION))) {
    elec <- unique(filt$ELECTION)[e]
    polsoph[which(CSES_all$ELECTION == elec & CSES_all$DATASET == paste0("CSES-MODULE-", m))] <-
      filter(filt, ELECTION == elec) %>% pull(POLKNOW) %>% scale %>% as.numeric
  }
}


CSES_all$polsoph <- polsoph

CSES_all %>% group_by(ELECTION) %>% dplyr::summarise(M = mean(polsoph, na.rm=T) %>% round(10)) %>% View

write.csv(CSES_all, "CSES_all.csv")






>>>>>>> 7dfd48aff9080e84e5bf37914150baa32b3f585c
