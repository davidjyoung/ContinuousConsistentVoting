<<<<<<< HEAD

# DY Note

# This script should be run third

# This script tidies up a lot of loose ends before getting the data into its 
# final format before running the actual Consistency Calculations in the next script
# the loose ends include: 
# identifying which of the 
# several variables coding participants' votes is the appropriate one for each election,
# and splitting this by MMD and SMD tier where necessary.
# filling in missing ideology data in PPMatrix by calculating the average ideology
# attributed to parties by those with Bachelor degrees
# This script also performs the exclusion of some elections for missing data

# Requires: CSES_all, PPMATRIX_trim, hetweights

# Outputs: cses_cv_data.csv


library(readxl)
library(haven)
library(ggplot2)
library(varhandle)
library(magrittr)
library(reshape2)
library(dplyr)
library(psych)


rm(list = ls())

options(scipen = 999)

CSES_all <- read.csv("CSES_all.csv")
PPMATRIX <- read.csv("PPMATRIX_trim.csv")
hetweights <- read.csv("hetweights.csv", stringsAsFactors = FALSE)

CSES_all %>% filter(ELECTION == "FRA_2007") %>% 
  dplyr::select(contains("Vote_")) %>% unique

CSES_all %<>% mutate(
  MODULE = sapply(as.character(CSES_all$DATASET), function(x) strsplit(x, "CSES-MODULE-")[[1]][2]) %>% as.numeric,
  RESPONDENT_ID <- c(1:nrow(CSES_all)),
  group = group_indices(., ELECTION, DATASET)
)

# excluding Japan 2007 and 2013 because these are not head-of-state elections
PPMATRIX %<>% filter(Alpha %in% c("JPN_2007", "JPN_2013") == FALSE)
PPMATRIX %<>% mutate(group = group_indices(., Alpha, Module))

# filling in party ideologies
judged_means <- CSES_all %>% filter(EDUCATION > 0.8) %>% group_by(ELECTION, MODULE) %>% 
  dplyr::select(contains("JI_")) %>% summarise_all(function(x) mean(x,na.rm=T))
# Ss with Bachelor's degrees score at least 0.8

judged_means_unprejudiced <- CSES_all %>% group_by(ELECTION, MODULE) %>% 
  dplyr::select(contains("JI_")) %>% summarise_all(function(x) mean(x,na.rm=T))

expert_means <- CSES_all %>% group_by(ELECTION, MODULE) %>% 
  dplyr::select(contains("ExI_")) %>% summarise_all(function(x) x[1])

cbind.data.frame(
  judged_means %>% select(-MODULE) %>% melt %>% pull(value),
  expert_means %>% filter(ELECTION != "IRL_2007") %>% select(-MODULE) %>% melt %>% pull(value)
) %>% cor(use = "pairwise.complete.obs") # r = 0.8278637

cbind.data.frame(
  judged_means_unprejudiced %>% select(-MODULE) %>% melt %>% pull(value),
  expert_means %>% select(-MODULE) %>% melt %>% pull(value)
) %>% cor(use = "pairwise.complete.obs") # r = 0.8065133


setdiff(expert_means$ELECTION, judged_means$ELECTION)

PPMATRIX$IdeoPosUnprejudiced <- c()

for (i in 1:nrow(PPMATRIX)) {

  key <- PPMATRIX$Letter[i] %>% as.character %>% paste0("_",. ) 
  
  if (key == "_NA") {
    
    ideo <- NA
    
  } else {
    
    filt <- filter(expert_means, MODULE == PPMATRIX$Module[i], ELECTION == as.character(PPMATRIX$Alpha[i])) %>% data.frame
    ideo <- ideo2 <- filt %>% dplyr::select(contains(key)) %>% as.numeric
  
    if (is.na(ideo) == TRUE) {
     
      filt <- filter(judged_means, MODULE == PPMATRIX$Module[i], ELECTION == as.character(PPMATRIX$Alpha[i])) %>% data.frame
      ideo <- filt %>% dplyr::select(contains(key)) %>% as.numeric
      
      filt2 <- filter(judged_means_unprejudiced, MODULE == PPMATRIX$Module[i], ELECTION == as.character(PPMATRIX$Alpha[i])) %>% data.frame
      ideo2 <- filt2 %>% dplyr::select(contains(key)) %>% as.numeric
      
    }
    
  }
  
  PPMATRIX$IdeoPos[i] <- ideo
  PPMATRIX$IdeoPosUnprejudiced[i] <- ideo2
}

# cutting parties with no ideology data
PPMATRIX <- PPMATRIX[-which(is.na(PPMATRIX$IdeoPos)), ]

# how many parties'/elections' need participant-estimates of ideology?
p <- PPMATRIX %>% filter(Alpha %in% c("MNE_2012", "DEU22002", "MNE_2016", "ROU_2012", "FRA_2007", "PER_2016",
                                      "THA_2007", "THA_2011", "MEX_2009", "BRA_2014") == FALSE) # these are excluded later on (except DEU22002, which is merged with DEU12002)
missings<- p$Alpha[which(p$IdeoPos != p$IdeoPosUnprejudiced)] %>% unique


p$Alpha %>% unique %>% length #146 elections 
p$Alpha[which(p$IdeoPos != p$IdeoPosUnprejudiced)] %>% unique %>% length # 25 with missing data
p$Alpha %>% length # 970 parties
p$Alpha[which(p$IdeoPos != p$IdeoPosUnprejudiced)] %>% length # 50 with missing data 







# cutting other tables to match elections in PPMATRIX
CSES_filt <- filter(CSES_all, ELECTION %in% PPMATRIX$Alpha == TRUE)
hetweights %<>% filter(code %in% PPMATRIX$Alpha)

# In the Romanian and Mexican elections of modules 1 and 2, candidates standing for the Presidency
# were typically supported by alliances of parties. The collaborators assigned these alliances
# new numerical codes. For simplicity, and so that I can have a clean alignment between party
# letters and party codes, I recode the alliance code into the code for its first-listed 
# constituent party. 

CSES_filt$Vote_President_1 <- which(CSES_filt$ELECTION == "ROU_1996" & CSES_filt$Vote_President_1 == 33) %>% replace(CSES_filt$Vote_President_1, ., 4)
CSES_filt$Vote_President_1 <- which(CSES_filt$ELECTION == "ROU_1996" & CSES_filt$Vote_President_2 == 34) %>% replace(CSES_filt$Vote_President_1, ., 1)
CSES_filt$Vote_President_2 <- which(CSES_filt$ELECTION == "ROU_1996" & CSES_filt$Vote_President_2 == 33) %>% replace(CSES_filt$Vote_President_2, ., 4)
CSES_filt$Vote_President_2 <- which(CSES_filt$ELECTION == "ROU_1996" & CSES_filt$Vote_President_2 == 34) %>% replace(CSES_filt$Vote_President_2, ., 1)

CSES_filt$Vote_President_1 <- which(CSES_filt$ELECTION == "ROU_2004" & CSES_filt$Vote_President_1 == 1) %>% replace(CSES_filt$Vote_President_1, ., 9)
CSES_filt$Vote_President_1 <- which(CSES_filt$ELECTION == "ROU_2004" & CSES_filt$Vote_President_1 == 2) %>% replace(CSES_filt$Vote_President_1, ., 6)
CSES_filt$Vote_President_2 <- which(CSES_filt$ELECTION == "ROU_2004" & CSES_filt$Vote_President_2 == 1) %>% replace(CSES_filt$Vote_President_2, ., 9)
CSES_filt$Vote_President_2 <- which(CSES_filt$ELECTION == "ROU_2004" & CSES_filt$Vote_President_2 == 2) %>% replace(CSES_filt$Vote_President_2, ., 6)

CSES_filt$Vote_President_1 <- which(CSES_filt$ELECTION == "MEX_2000" & CSES_filt$Vote_President_1 == 7) %>% replace(CSES_filt$Vote_President_1, ., 1)
CSES_filt$Vote_President_1 <- which(CSES_filt$ELECTION == "MEX_2000" & CSES_filt$Vote_President_1 == 8) %>% replace(CSES_filt$Vote_President_1, ., 3)


CSES_filt$ELECTION %<>% as.character
PPMATRIX$Alpha %<>% as.character

# sorting out which variable contains the votes for each election, including where I look at multiple
# elections within one (i.e. MMD and SMD, round 1 and round 2)

votecols <- CSES_filt %>% group_by(MODULE, ELECTION) %>% dplyr::select(contains("VOTE_")) %>% summarise_all(
  function(x) length(which(is.na(x) == FALSE))/length(x)) %>% 
mutate_at(c("Vote_District_Candidate",     
               "Vote_District_List",          
               "Vote_Lower_House_1",  
               "Vote_Lower_House_2",
               "Vote_Lower_House_Candidate",  
               "Vote_Lower_House_List",       
               "Vote_President_1", "Vote_President_2"), function(x) ifelse(x > 0, 1, 0)) %>% 
  dplyr::select("MODULE", "ELECTION", "Vote_District_Candidate", "Vote_District_List", "Vote_Lower_House_1", "Vote_Lower_House_2", 
         "Vote_Lower_House_Candidate", "Vote_Lower_House_List", "Vote_President_1", "Vote_President_2") %>% ungroup
votecols %<>% mutate(n = votecols %>% dplyr::select(contains("Vote_")) %>% rowSums())

votecolsCSES5 <- CSES_filt %>% filter(MODULE == 5) %>% group_by(ELECTION, MODULE) %>% dplyr::select(
  contains("Vote_District"), contains("Vote_Lower"), contains("Vote_President"),
  ) %>% summarise_all(function(x) ifelse(length(unique(x)) == 1, 0, 1)) %>% ungroup
votecolsCSES5 %<>% mutate(n = votecolsCSES5 %>% dplyr::select(contains("Vote_")) %>% rowSums())

votecols %<>% rbind(votecolsCSES5)

lh1_1 <- filter(votecols, n == 1, Vote_President_1 == 0) %>% mutate(name = ELECTION)
pres1_1 <- filter(votecols, n == 1, Vote_President_1 == 1) %>% mutate(name = ELECTION)
pres1_2 <- filter(votecols, n == 2, Vote_President_1 == 1, Vote_President_2 == 0) %>% mutate(name = ELECTION)
  pres1_2[,3:8] <- c(0)
pres1_3 <- filter(votecols, n == 2, Vote_President_1 == 1, Vote_President_2 == 1) %>% mutate(name = ELECTION) %>% mutate(name = paste0(ELECTION, "_R1")) 
  pres1_3$Vote_President_2 <- c(0)
pres2_1 <- filter(votecols, n == 2, Vote_President_1 == 1, Vote_President_2 == 1) %>% mutate(name = ELECTION) %>% mutate(name = paste0(ELECTION, "_R2")) 
  pres2_1$Vote_President_1 <- c(0)
lh1_2 <- filter(votecols, n == 2, Vote_Lower_House_1 == 1, Vote_Lower_House_2 == 1) %>% mutate(name = paste0(ELECTION, "_SMD"))
  lh1_2$Vote_Lower_House_2 <- c(0)
lh2_1 <- filter(votecols, n == 2, Vote_Lower_House_1 == 1, Vote_Lower_House_2 == 1) %>% mutate(name = paste0(ELECTION, "_MMD"))
  lh2_1$Vote_Lower_House_1 <- c(0)
lh1_3 <- filter(votecols, n == 2, Vote_Lower_House_Candidate == 1, Vote_Lower_House_List == 1) %>% mutate(name = paste0(ELECTION, "_SMD"))
  lh1_3$Vote_Lower_House_List <- c(0)
lh2_2 <- filter(votecols, n == 2, Vote_Lower_House_Candidate == 1, Vote_Lower_House_List == 1) %>% mutate(name = paste0(ELECTION, "_MMD"))
  lh2_2$Vote_Lower_House_Candidate <- c(0)
lh1_4 <- filter(votecols, n == 2, Vote_District_Candidate == 1, Vote_District_List == 1) %>% mutate(name = paste0(ELECTION, "_SMD"))
  lh1_4$Vote_District_List <- c(0)
lh2_3 <- filter(votecols, n == 2, Vote_District_Candidate == 1, Vote_District_List == 1) %>% mutate(name = paste0(ELECTION, "_MMD"))
  lh2_3$Vote_District_Candidate <- c(0)
pres1_4 <- filter(votecols, n == 3, Vote_President_1 == 1, Vote_President_2 == 0) %>% mutate(name = ELECTION) 
  pres1_4[,3:8] <- c(0)
pres1_5 <- filter(votecols, n == 3, Vote_President_1 == 1, Vote_President_2 == 1) %>% mutate(name = paste0(ELECTION, "_R1")) 
  pres1_5[,c(3:8, 10)] <- c(0)
pres2_2 <- filter(votecols, n == 3, Vote_President_1 == 1, Vote_President_2 == 1) %>% mutate(name = paste0(ELECTION, "_R2")) 
  pres2_2[,c(3:9)] <- c(0)
  
votevariablestable <- rbind(cbind(rbind(lh1_1, lh1_2, lh1_3, lh1_4, 
                            lh2_1, lh2_2, lh2_3), votes = "lower house"),
                            cbind(rbind(pres1_1, pres1_2, pres1_3, pres1_4, pres1_5,
                            pres2_1, pres2_2), votes = "presidential"))

# cutting out the CSES2 entries for Portugal 2002 (I use the CSES1 entry)

votevariablestable <- votevariablestable[-which(votevariablestable$ELECTION == "PRT_2002" & 
                                                 votevariablestable$MODULE == 2),]

write.csv(votevariablestable, "votevariablestable.csv")

vvt <- read.csv("votevariablestable.csv")


hetweights <- hetweights[-which(hetweights$code == "PRT_2002" & 
                                                 hetweights$module == 2),] 

CSES_filt <- CSES_filt[-which(CSES_filt$ELECTION == "PRT_2002" & CSES_filt$MODULE == 2),]

hetweights$group <- group_indices(hetweights, code, module)

ideorangefill <- function(g) {
  
  hetweights_filt <- hetweights %>% filter(group == g)
  E <- hetweights_filt %>% pull(code) %>% unique %>% as.character
  M <- hetweights_filt %>% pull(module) %>% unique
  votes <- votevariablestable %>% filter(MODULE == M, ELECTION == E) %>% pull(votes) %>% unique %>% as.character
   
  majlh <- dplyr::select(hetweights_filt, contains("LH"))[1,]
  majp <- dplyr::select(hetweights_filt, contains("_P"))[1,]
  
  if (votes == "lower house") {
   maj <- majlh
   if(all(is.na(majlh) == TRUE)) maj <- majp
  }
  
  if (votes == "presidential") {
    maj <- majp 
    if(all(is.na(majp) == TRUE)) maj <- majlh
  }

  parties <- LETTERS[1:9][which(maj == 1)]
  
  PPMATRIX %>% filter(Alpha == E, Module == M)
  
  ideos <- PPMATRIX %>% filter(Alpha == E, Module == M, Letter %in% parties) %>% pull(IdeoPos)
  ideorange <- max(ideos) - min(ideos)
  rep(ideorange, times = nrow(filter(hetweights_filt, group == g)))
}

ideorangefill_unprejudiced <- function(g) {
  
  hetweights_filt <- hetweights %>% filter(group == g)
  E <- hetweights_filt %>% pull(code) %>% unique %>% as.character
  M <- hetweights_filt %>% pull(module) %>% unique
  votes <- votevariablestable %>% filter(MODULE == M, ELECTION == E) %>% pull(votes) %>% unique %>% as.character
  
  majlh <- dplyr::select(hetweights_filt, contains("LH"))[1,]
  majp <- dplyr::select(hetweights_filt, contains("_P"))[1,]
  
  if (votes == "lower house") {
    maj <- majlh
    if(all(is.na(majlh) == TRUE)) maj <- majp
  }
  
  if (votes == "presidential") {
    maj <- majp 
    if(all(is.na(majp) == TRUE)) maj <- majlh
  }
  
  parties <- LETTERS[1:9][which(maj == 1)]
  
  PPMATRIX %>% filter(Alpha == E, Module == M)
  
  ideos <- PPMATRIX %>% filter(Alpha == E, Module == M, Letter %in% parties) %>% pull(IdeoPosUnprejudiced)
  ideorange <- max(ideos) - min(ideos)
  rep(ideorange, times = nrow(filter(hetweights_filt, group == g)))
}

hetweights %<>% mutate(ideorange_raw = group %>% unique %>% sapply(function(x) ideorangefill(x)) %>% unlist)
hetweights %<>% mutate(ideorange_unprejudiced_raw = group %>% unique %>% sapply(function(x) ideorangefill_unprejudiced(x)) %>% unlist)

# no vote share data for Romania 2014 so returning -Inf for Ideorange
# parties A and B were the only ones to field candidates gaining more than 10% of the vote share in the first round
# A - Ponta (40.44) and B - Iohannis (30.37)
# A is listed as 4, B as 6, so Ideorange is 2

hetweights$ideorange_raw[which(hetweights$code == "ROU_2014")] <- c(2)
hetweights$ideorange_unprejudiced_raw[which(hetweights$code == "ROU_2014")] <- c(2)

# now need to rescale ideorange to between 0.2 and 1
# the maximum possible is 10 and minimum 0

minhwir <- min(hetweights$ideorange_raw)
maxhwir <- max(hetweights$ideorange_raw)
div <- maxhwir - minhwir

hetweights %<>% mutate(ideorange = 0.2 + 0.8*(hetweights$ideorange_raw - minhwir)/div)

minhwirup <- min(hetweights$ideorange_unprejudiced_raw)
maxhwirup <- max(hetweights$ideorange_unprejudiced_raw)
divup <- maxhwirup - minhwirup

hetweights %<>% mutate(ideorange_unprejudiced = 0.2 + 0.8*(hetweights$ideorange_unprejudiced_raw - minhwirup)/divup)

weights <- dplyr::select(hetweights, module, code, GDPdiff, demsatis, ideorange, ideorange_unprejudiced) 

data <- cbind.data.frame(CSES_filt, weights)

recodevote <- function(x) {
  x %>% sapply(function(x) PPMf$Letter[match(x, PPMf$Num)] %>% as.character)
}

r2s <- unique(votevariablestable$name)[grep("R2", unique(votevariablestable$name))]

votevariablestable %>% filter(name %in% r2s)

for (e in 1:nrow(votevariablestable)) {

mod <- votevariablestable$MODULE[e]
elec <- votevariablestable$ELECTION[e]
df <- filter(data, ELECTION == elec, MODULE == mod)
N <- df %>% nrow

blank <- matrix(NA, nrow = N, ncol = 9)
colnames(blank) <- LETTERS[1:9]

vv <- colnames(dplyr::select(votevariablestable, contains("Vote_")))[
  which(dplyr::select(votevariablestable, contains("Vote_"))[e,] == 1)]

PPMf <- filter(PPMATRIX, Alpha == elec, Module == mod)

incumbents <- PPMf$Incumbent[match(LETTERS[1:9], PPMf$Letter)] %>% 
  rep(., times = N) %>% matrix(., ncol = 9, byrow = TRUE) %>% data.frame
colnames(incumbents) <- LETTERS[1:9] %>% paste0("inc_", .)
  
ideos <- PPMf$IdeoPos[match(LETTERS[1:9], PPMf$Letter)] %>% 
  rep(., times = N) %>% matrix(., ncol = 9, byrow = TRUE) %>% data.frame
colnames(ideos) <- LETTERS[1:9] %>% paste0("ideo_", .)

ideos_unprejudiced <- PPMf$IdeoPosUnprejudiced[match(LETTERS[1:9], PPMf$Letter)] %>% 
  rep(., times = N) %>% matrix(., ncol = 9, byrow = TRUE) %>% data.frame
colnames(ideos_unprejudiced) <- LETTERS[1:9] %>% paste0("ideo_unprejudiced_", .)

alliances <- PPMf$Alliances[match(LETTERS[1:9], PPMf$Letter)] %>% 
  rep(., times = N) %>% matrix(., ncol = 9, byrow = TRUE) %>% data.frame
colnames(alliances) <- LETTERS[1:9] %>% paste0("alliance_", .)

votes <- df %>% dplyr::select(vv) %>% recodevote(.) %>% as.character

closest <- df$CLOSEST_PARTY %>% recodevote(.) %>% as.character
closeness <- blank
closetable <- closest %>% dummy.code() %>% multiply_by(df$DEGREE_OF_CLOSENESS)
closeness[,colnames(closetable)] <- closetable
closeness %<>% data.frame
colnames(closeness) %<>% paste0("close_", .)

mask <- incumbents %>% transmute_all(function(x) ifelse(is.na(x) == FALSE, 1, x))
colnames(mask) <- LETTERS[1:9] %>% paste0("mask_", .)

if (sum(PPMf$Region, na.rm=T) > 0) {
  PPMf_reg <- filter(PPMf, is.na(Region) == FALSE)
  for (i in 1:nrow(PPMf_reg)) {
    mask[which(df$REGION_OF_RESIDENCE != PPMf_reg$Region[i]), sub("X", PPMf_reg$Letter[i], "mask_X")] <- NA
  }
}

if (elec == "BEL_2003") {
  mask[which(df$REGION_OF_RESIDENCE == 3),] <- 1
}

if (vv == "Vote_President_2") {
  mask[,which(LETTERS[1:9] %in% PPMf$Letter[which(PPMf$Round2 == 1)] == FALSE)] <- NA
}

output <- cbind.data.frame(df, incumbents, ideos, ideos_unprejudiced, alliances, closeness, mask, votes)
output$name <- votevariablestable$name[e] 

if (e == 1) {
  table <- output
} else {
  table %<>% rbind(output)
}

}




eval <- table %>% transmute_at(vars(contains("inc_")), 
                       function(x) table$EVALUATION_DIRECTIONAL %>% multiply_by(x) %>% add(1) %>% divide_by(2))
colnames(eval) %<>% sub("inc_", "eval_", .)
table %<>% cbind(eval)


write.csv(table, "cses_cv_data.csv")





=======

# DY Note

# This script should be run third

# This script tidies up a lot of loose ends, including identifying which of the 
# several variables coding participants' votes is the appropriate one for each election,
# including splitting this by MMD and SMD tier where necessary.
# This script also fills in missing ideology data in PPMatrix by calculating the average ideology
# attributed to parties by those with Bachelor degrees
# This script excludes some elections for missing data

# Requires: CSES_all, PPMATRIX_trim, hetweights

# Outputs: cses_cv_data.csv


library(readxl)
library(haven)
library(ggplot2)
library(varhandle)
library(magrittr)
library(reshape2)
library(dplyr)
library(psych)


rm(list = ls())

options(scipen = 999)

CSES_all <- read.csv("CSES_all.csv")
PPMATRIX <- read.csv("PPMATRIX_trim.csv")
hetweights <- read.csv("hetweights.csv", stringsAsFactors = FALSE)

CSES_all %>% filter(ELECTION == "FRA_2007") %>% 
  dplyr::select(contains("Vote_")) %>% unique

CSES_all %<>% mutate(
  MODULE = sapply(as.character(CSES_all$DATASET), function(x) strsplit(x, "CSES-MODULE-")[[1]][2]) %>% as.numeric,
  RESPONDENT_ID <- c(1:nrow(CSES_all)),
  group = group_indices(., ELECTION, DATASET)
)

# excluding Japan 2007 and 2013 because these are not head-of-state elections
PPMATRIX %<>% filter(Alpha %in% c("JPN_2007", "JPN_2013") == FALSE)
PPMATRIX %<>% mutate(group = group_indices(., Alpha, Module))

# filling in party ideologies
judged_means <- CSES_all %>% filter(EDUCATION >= 7) %>% group_by(ELECTION, MODULE) %>% 
  dplyr::select(contains("JI_")) %>% summarise_all(function(x) mean(x,na.rm=T))

expert_means <- CSES_all %>% group_by(ELECTION, MODULE) %>% 
  dplyr::select(contains("ExI_")) %>% summarise_all(function(x) x[1])

for (i in 1:nrow(PPMATRIX)) {

  key <- PPMATRIX$Letter[i] %>% as.character %>% paste0("_",. ) 
  
  if (key == "_NA") {
    
    ideo <- NA
    
  } else {
    
    filt <- filter(expert_means, MODULE == PPMATRIX$Module[i], ELECTION == as.character(PPMATRIX$Alpha[i])) %>% data.frame
    ideo <- filt %>% dplyr::select(contains(key)) %>% as.numeric
  
    if (is.na(ideo) == TRUE) {
     
      filt <- filter(judged_means, MODULE == PPMATRIX$Module[i], ELECTION == as.character(PPMATRIX$Alpha[i])) %>% data.frame
      ideo <- filt %>% dplyr::select(contains(key)) %>% as.numeric
      
    }
    
  }
  
  PPMATRIX$IdeoPos[i] <- ideo
}

# cutting parties with no ideology data
PPMATRIX <- PPMATRIX[-which(is.na(PPMATRIX$IdeoPos)), ]

# cutting other tables to match elections in PPMATRIX
CSES_filt <- filter(CSES_all, ELECTION %in% PPMATRIX$Alpha == TRUE)
hetweights %<>% filter(code %in% PPMATRIX$Alpha)

# In the Romanian and Mexican elections of modules 1 and 2, candidates standing for the Presidency
# were typically supported by alliances of parties. The collaborators assigned these alliances
# new numerical codes. For simplicity, and so that I can have a clean alignment between party
# letters and party codes, I recode the alliance code into the code for its first-listed 
# constituent party. 

CSES_filt$Vote_President_1 <- which(CSES_filt$ELECTION == "ROU_1996" & CSES_filt$Vote_President_1 == 33) %>% replace(CSES_filt$Vote_President_1, ., 4)
CSES_filt$Vote_President_1 <- which(CSES_filt$ELECTION == "ROU_1996" & CSES_filt$Vote_President_2 == 34) %>% replace(CSES_filt$Vote_President_1, ., 1)
CSES_filt$Vote_President_2 <- which(CSES_filt$ELECTION == "ROU_1996" & CSES_filt$Vote_President_2 == 33) %>% replace(CSES_filt$Vote_President_2, ., 4)
CSES_filt$Vote_President_2 <- which(CSES_filt$ELECTION == "ROU_1996" & CSES_filt$Vote_President_2 == 34) %>% replace(CSES_filt$Vote_President_2, ., 1)

CSES_filt$Vote_President_1 <- which(CSES_filt$ELECTION == "ROU_2004" & CSES_filt$Vote_President_1 == 1) %>% replace(CSES_filt$Vote_President_1, ., 9)
CSES_filt$Vote_President_1 <- which(CSES_filt$ELECTION == "ROU_2004" & CSES_filt$Vote_President_1 == 2) %>% replace(CSES_filt$Vote_President_1, ., 6)
CSES_filt$Vote_President_2 <- which(CSES_filt$ELECTION == "ROU_2004" & CSES_filt$Vote_President_2 == 1) %>% replace(CSES_filt$Vote_President_2, ., 9)
CSES_filt$Vote_President_2 <- which(CSES_filt$ELECTION == "ROU_2004" & CSES_filt$Vote_President_2 == 2) %>% replace(CSES_filt$Vote_President_2, ., 6)

CSES_filt$Vote_President_1 <- which(CSES_filt$ELECTION == "MEX_2000" & CSES_filt$Vote_President_1 == 7) %>% replace(CSES_filt$Vote_President_1, ., 1)
CSES_filt$Vote_President_1 <- which(CSES_filt$ELECTION == "MEX_2000" & CSES_filt$Vote_President_1 == 8) %>% replace(CSES_filt$Vote_President_1, ., 3)


CSES_filt$ELECTION %<>% as.character
PPMATRIX$Alpha %<>% as.character

# sorting out which variable contains the votes for each election, including where I look at multiple
# elections within one (i.e. MMD and SMD, round 1 and round 2)

votecols <- CSES_filt %>% group_by(MODULE, ELECTION) %>% dplyr::select(contains("VOTE_")) %>% summarise_all(
  function(x) length(which(is.na(x) == FALSE))/length(x)) %>% 
mutate_at(c("Vote_District_Candidate",     
               "Vote_District_List",          
               "Vote_Lower_House_1",  
               "Vote_Lower_House_2",
               "Vote_Lower_House_Candidate",  
               "Vote_Lower_House_List",       
               "Vote_President_1", "Vote_President_2"), function(x) ifelse(x > 0, 1, 0)) %>% 
  dplyr::select("MODULE", "ELECTION", "Vote_District_Candidate", "Vote_District_List", "Vote_Lower_House_1", "Vote_Lower_House_2", 
         "Vote_Lower_House_Candidate", "Vote_Lower_House_List", "Vote_President_1", "Vote_President_2") %>% ungroup
votecols %<>% mutate(n = votecols %>% dplyr::select(contains("Vote_")) %>% rowSums())

votecolsCSES5 <- CSES_filt %>% filter(MODULE == 5) %>% group_by(ELECTION, MODULE) %>% dplyr::select(
  contains("Vote_District"), contains("Vote_Lower"), contains("Vote_President"),
  ) %>% summarise_all(function(x) ifelse(length(unique(x)) == 1, 0, 1)) %>% ungroup
votecolsCSES5 %<>% mutate(n = votecolsCSES5 %>% dplyr::select(contains("Vote_")) %>% rowSums())

votecols %<>% rbind(votecolsCSES5)

lh1_1 <- filter(votecols, n == 1, Vote_President_1 == 0) %>% mutate(name = ELECTION)
pres1_1 <- filter(votecols, n == 1, Vote_President_1 == 1) %>% mutate(name = ELECTION)
pres1_2 <- filter(votecols, n == 2, Vote_President_1 == 1, Vote_President_2 == 0) %>% mutate(name = ELECTION)
  pres1_2[,3:8] <- c(0)
pres1_3 <- filter(votecols, n == 2, Vote_President_1 == 1, Vote_President_2 == 1) %>% mutate(name = ELECTION) %>% mutate(name = paste0(ELECTION, "_R1")) 
  pres1_3$Vote_President_2 <- c(0)
pres2_1 <- filter(votecols, n == 2, Vote_President_1 == 1, Vote_President_2 == 1) %>% mutate(name = ELECTION) %>% mutate(name = paste0(ELECTION, "_R2")) 
  pres2_1$Vote_President_1 <- c(0)
lh1_2 <- filter(votecols, n == 2, Vote_Lower_House_1 == 1, Vote_Lower_House_2 == 1) %>% mutate(name = paste0(ELECTION, "_SMD"))
  lh1_2$Vote_Lower_House_2 <- c(0)
lh2_1 <- filter(votecols, n == 2, Vote_Lower_House_1 == 1, Vote_Lower_House_2 == 1) %>% mutate(name = paste0(ELECTION, "_MMD"))
  lh2_1$Vote_Lower_House_1 <- c(0)
lh1_3 <- filter(votecols, n == 2, Vote_Lower_House_Candidate == 1, Vote_Lower_House_List == 1) %>% mutate(name = paste0(ELECTION, "_SMD"))
  lh1_3$Vote_Lower_House_List <- c(0)
lh2_2 <- filter(votecols, n == 2, Vote_Lower_House_Candidate == 1, Vote_Lower_House_List == 1) %>% mutate(name = paste0(ELECTION, "_MMD"))
  lh2_2$Vote_Lower_House_Candidate <- c(0)
lh1_4 <- filter(votecols, n == 2, Vote_District_Candidate == 1, Vote_District_List == 1) %>% mutate(name = paste0(ELECTION, "_SMD"))
  lh1_4$Vote_District_List <- c(0)
lh2_3 <- filter(votecols, n == 2, Vote_District_Candidate == 1, Vote_District_List == 1) %>% mutate(name = paste0(ELECTION, "_MMD"))
  lh2_3$Vote_District_Candidate <- c(0)
pres1_4 <- filter(votecols, n == 3, Vote_President_1 == 1, Vote_President_2 == 0) %>% mutate(name = ELECTION) 
  pres1_4[,3:8] <- c(0)
pres1_5 <- filter(votecols, n == 3, Vote_President_1 == 1, Vote_President_2 == 1) %>% mutate(name = paste0(ELECTION, "_R1")) 
  pres1_5[,c(3:8, 10)] <- c(0)
pres2_2 <- filter(votecols, n == 3, Vote_President_1 == 1, Vote_President_2 == 1) %>% mutate(name = paste0(ELECTION, "_R2")) 
  pres2_2[,c(3:9)] <- c(0)
  
votevariablestable <- rbind(cbind(rbind(lh1_1, lh1_2, lh1_3, lh1_4, 
                            lh2_1, lh2_2, lh2_3), votes = "lower house"),
                            cbind(rbind(pres1_1, pres1_2, pres1_3, pres1_4, pres1_5,
                            pres2_1, pres2_2), votes = "presidential"))

# cutting out the CSES2 entries for Portugal 2002 (I use the CSES1 entry)

votevariablestable <- votevariablestable[-which(votevariablestable$ELECTION == "PRT_2002" & 
                                                 votevariablestable$MODULE == 2),]

write.csv(votevariablestable, "votevariablestable.csv")

vvt <- read.csv("votevariablestable.csv")


hetweights <- hetweights[-which(hetweights$code == "PRT_2002" & 
                                                 hetweights$module == 2),] 

CSES_filt <- CSES_filt[-which(CSES_filt$ELECTION == "PRT_2002" & CSES_filt$MODULE == 2),]

hetweights$group <- group_indices(hetweights, code, module)

ideorangefill <- function(g) {
  
  hetweights_filt <- hetweights %>% filter(group == g)
  E <- hetweights_filt %>% pull(code) %>% unique %>% as.character
  M <- hetweights_filt %>% pull(module) %>% unique
  votes <- votevariablestable %>% filter(MODULE == M, ELECTION == E) %>% pull(votes) %>% unique %>% as.character
   
  majlh <- dplyr::select(hetweights_filt, contains("LH"))[1,]
  majp <- dplyr::select(hetweights_filt, contains("_P"))[1,]
  
  if (votes == "lower house") {
   maj <- majlh
   if(all(is.na(majlh) == TRUE)) maj <- majp
  }
  
  if (votes == "presidential") {
    maj <- majp 
    if(all(is.na(majp) == TRUE)) maj <- majlh
  }

  parties <- LETTERS[1:9][which(maj == 1)]
  
  PPMATRIX %>% filter(Alpha == E, Module == M)
  
  ideos <- PPMATRIX %>% filter(Alpha == E, Module == M, Letter %in% parties) %>% pull(IdeoPos)
  ideorange <- max(ideos) - min(ideos)
  rep(ideorange, times = nrow(filter(hetweights_filt, group == g)))
}

hetweights %<>% mutate(ideorange_raw = group %>% unique %>% sapply(function(x) ideorangefill(x)) %>% unlist)

# no vote share data for Romania 2014 so returning -Inf for Ideorange
# parties A and B were the only ones to field candidates gaining more than 10% of the vote share in the first round
# A - Ponta (40.44) and B - Iohannis (30.37)
# A is listed as 4, B as 6, so Ideorange is 2

hetweights$ideorange_raw[which(hetweights$code == "ROU_2014")] <- c(2)

# now need to rescale ideorange to between 0.2 and 1
# the maximum possible is 10 and minimum 0

minhwir <- min(hetweights$ideorange_raw)
maxhwir <- max(hetweights$ideorange_raw)
div <- maxhwir - minhwir

hetweights %<>% mutate(ideorange = 0.2 + 0.8*(hetweights$ideorange_raw - minhwir)/div)

weights <- dplyr::select(hetweights, module, code, GDPdiff, demsatis, ideorange) 

data <- cbind.data.frame(CSES_filt, weights)

recodevote <- function(x) {
  x %>% sapply(function(x) PPMf$Letter[match(x, PPMf$Num)] %>% as.character)
}

r2s <- unique(votevariablestable$name)[grep("R2", unique(votevariablestable$name))]

votevariablestable %>% filter(name %in% r2s)

for (e in 1:nrow(votevariablestable)) {

mod <- votevariablestable$MODULE[e]
elec <- votevariablestable$ELECTION[e]
df <- filter(data, ELECTION == elec, MODULE == mod)
N <- df %>% nrow

blank <- matrix(NA, nrow = N, ncol = 9)
colnames(blank) <- LETTERS[1:9]

vv <- colnames(dplyr::select(votevariablestable, contains("Vote_")))[
  which(dplyr::select(votevariablestable, contains("Vote_"))[e,] == 1)]

PPMf <- filter(PPMATRIX, Alpha == elec, Module == mod)

incumbents <- PPMf$Incumbent[match(LETTERS[1:9], PPMf$Letter)] %>% 
  rep(., times = N) %>% matrix(., ncol = 9, byrow = TRUE) %>% data.frame
colnames(incumbents) <- LETTERS[1:9] %>% paste0("inc_", .)
  
ideos <- PPMf$IdeoPos[match(LETTERS[1:9], PPMf$Letter)] %>% 
  rep(., times = N) %>% matrix(., ncol = 9, byrow = TRUE) %>% data.frame
colnames(ideos) <- LETTERS[1:9] %>% paste0("ideo_", .)

alliances <- PPMf$Alliances[match(LETTERS[1:9], PPMf$Letter)] %>% 
  rep(., times = N) %>% matrix(., ncol = 9, byrow = TRUE) %>% data.frame
colnames(alliances) <- LETTERS[1:9] %>% paste0("alliance_", .)

votes <- df %>% dplyr::select(vv) %>% recodevote(.) %>% as.character

closest <- df$CLOSEST_PARTY %>% recodevote(.) %>% as.character
closeness <- blank
closetable <- closest %>% dummy.code() %>% multiply_by(df$DEGREE_OF_CLOSENESS)
closeness[,colnames(closetable)] <- closetable
closeness %<>% data.frame
colnames(closeness) %<>% paste0("close_", .)

mask <- incumbents %>% transmute_all(function(x) ifelse(is.na(x) == FALSE, 1, x))
colnames(mask) <- LETTERS[1:9] %>% paste0("mask_", .)

if (sum(PPMf$Region, na.rm=T) > 0) {
  PPMf_reg <- filter(PPMf, is.na(Region) == FALSE)
  for (i in 1:nrow(PPMf_reg)) {
    mask[which(df$REGION_OF_RESIDENCE != PPMf_reg$Region[i]), sub("X", PPMf_reg$Letter[i], "mask_X")] <- NA
  }
}

if (elec == "BEL_2003") {
  mask[which(df$REGION_OF_RESIDENCE == 3),] <- 1
}

if (vv == "Vote_President_2") {
  mask[,which(LETTERS[1:9] %in% PPMf$Letter[which(PPMf$Round2 == 1)] == FALSE)] <- NA
}

output <- cbind.data.frame(df, incumbents, ideos, alliances, closeness, mask, votes)
output$name <- votevariablestable$name[e] 

if (e == 1) {
  table <- output
} else {
  table %<>% rbind(output)
}

}




eval <- table %>% transmute_at(vars(contains("inc_")), 
                       function(x) table$EVALUATION_DIRECTIONAL %>% multiply_by(x) %>% add(1) %>% divide_by(2))
colnames(eval) %<>% sub("inc_", "eval_", .)
table %<>% cbind(eval)


write.csv(table, "cses_cv_data.csv")







>>>>>>> 7dfd48aff9080e84e5bf37914150baa32b3f585c
