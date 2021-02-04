<<<<<<< HEAD

# DY Note:

# This script should be run second

# This script constructs, and scales, the variables used as weights
# for voters' preferences in the determination of the continuous consistency calculation.
# The exception is the range of ideologies, which is calculated in the 'penultimate processing' script

# Requires: cses1, cses2, cses3, cses4, cses5
#           Data_Extract_From_World_Development_Indicators-2.xlsx ("GDP")
#           Taiwan GDP.xlsx ("Taiwan_GDP")

# Outputs: hetweights.csv

rm(list = ls())


library(magrittr)
library(haven)
library(dplyr)

CSES1 <- read_sav("cses1.sav")
CSES2 <- read_sav("cses2.sav")
CSES3 <- read_sav("cses3.sav")
CSES4 <- read_sav("cses4.sav")
CSES5 <- read_sav("cses5.sav")


library(dplyr)
library(magrittr)

labels_to_names <- function(df) {
  new_df <- df
  colnames(new_df) %<>% sapply(function(x) pull(new_df, x) %>% attr(., "label"))
  new_df
}

guide <- function(df) {
  labels <- data.frame(
    variable = colnames(df), 
    label = colnames(df) %>% sapply(function(x) label = pull(CSES1, x) %>% attr("label")))
}

by_name <- function(x, df) {
  filter(guide(df), label == x) %>% pull(variable) %>% as.character
}



# the following functions are used to search through the variables so I can find
# what I am looking for

LABELS_TO_NAMES <- function(df) {
  n <- ncol(df)
  for (c in 1:n) {
    label <- attr(df[ , c, drop = TRUE], "label")
    name <- colnames(df)[c]
    colnames(df)[which(colnames(df) == name)] <- label
  }
  return(df)
}

CSES1n <- LABELS_TO_NAMES(CSES1)
CSES2n <- LABELS_TO_NAMES(CSES2)
CSES3n <- LABELS_TO_NAMES(CSES3)
CSES4n <- LABELS_TO_NAMES(CSES4)
CSES5n <- LABELS_TO_NAMES(CSES5)

# efficacy
CSES1n$`WHO IS IN POWER CAN MAKE DIFFERENCE`
CSES1n$`WHO PEOPLE VOTE FOR MAKES A DIFFERENCE`
CSES2n$`WHO IS IN POWER CAN MAKE DIFFERENCE`
CSES2n$`WHO PEOPLE VOTE FOR MAKES A DIFFERENCE`
CSES3n$`WHO IS IN POWER CAN MAKE DIFFERENCE`
CSES3n$`WHO PEOPLE VOTE FOR MAKES A DIFFERENCE`
CSES4n$`WHO IS IN POWER CAN MAKE DIFFERENCE`
CSES4n$`WHO PEOPLE VOTE FOR MAKES A DIFFERENCE`
CSES5n$`WHO IS IN POWER CAN MAKE DIFFERENCE`
CSES5n$`WHO PEOPLE VOTE FOR MAKES A DIFFERENCE`

# trying to find a variable to replace 'Political Parties are necessary' 
# and 'Political parties care what people think'

cbind(
  CSES1n$`WHO IS IN POWER CAN MAKE DIFFERENCE`, 
  CSES1n$`WHO PEOPLE VOTE FOR MAKES A DIFFERENCE`,
  CSES1n$`POLITICAL PARTIES ARE NECESSARY`,
  CSES1n$`POLITICAL PARTIES CARE WHAT PEOPLE THINK`,
  CSES1n$`SATISFACTION WITH DEMOCRATIC PROCESS`,
  CSES1n$`POLITICIANS KNOW WHAT PEOPLE THINK`) %>% 
data.frame %>% filter_all(function(x) x != 9) %>% cor

# satisfaction with democratic process correlates positively with both (r = 0.275 and r = 0.270)
# and is in all samples

# now scaling it to range between 0.2 and 1 for all Ss

cleandemsatis <- function(x) {
  x %>% unfactor %>% sapply(function(x) multiply_by(x, 0.8) %>% add(0.2) %>% ifelse(is.na(.) == TRUE, 0.2, .))
}


  satis1 = CSES1n$`SATISFACTION WITH DEMOCRATIC PROCESS` %>% factor(levels = c(1, 2, 4, 5), labels = c(3/3, 2/3, 1/3, 0/3)) %>% cleandemsatis
  satis2 = CSES2n$`SATISFACTION WITH DEMOCRATIC PROCESS` %>% factor(levels = c(1, 2, 3, 4), labels = c(3/3, 2/3, 1/3, 0/3)) %>% cleandemsatis
  satis3 = CSES3n$`SATISFACTION WITH DEMOCRACY` %>% factor(levels = c(1, 2, 4, 5), labels = c(3/3, 2/3, 1/3, 0/3)) %>% cleandemsatis
  satis4 = CSES4n$`SATISFACTION WITH DEMOCRACY` %>% factor(levels = c(1, 2, 4, 5), labels = c(3/3, 2/3, 1/3, 0/3)) %>% cleandemsatis
  satis5 = CSES5n$`SATISFACTION WITH DEMOCRACY` %>% factor(levels = c(1, 2, 4, 5), labels = c(3/3, 2/3, 1/3, 0/3)) %>% cleandemsatis


demsatis <- c(satis1, satis2, satis3, satis4, satis5)


# the next weight variables is the log of the absolute (modulus) difference between
# GDP growth in the election year, and the 10-year average of annual GDP growth

#https://databank.worldbank.org/reports.aspx?source=2&series=NY.GDP.MKTP.KD.ZG&country=#
  
GDP <- 
  read_excel("Data_Extract_From_World_Development_Indicators-2.xlsx")

GDP <- GDP[,c(3,5:ncol(GDP))]

# need to adjust some of the names to match what is in the CSES, and add in a row for Taiwan

GDP$`Country Name`[which(GDP$`Country Name` == "United Kingdom")] <- "Great Britain"
GDP$`Country Name`[which(GDP$`Country Name` == "United States")] <- "United States of America"
GDP$`Country Name`[which(GDP$`Country Name` == "Korea, Rep.")] <- "Republic of Korea"
GDP$`Country Name`[which(GDP$`Country Name` == "Slovak Republic")] <- "Slovakia"
GDP$`Country Name`[which(GDP$`Country Name` == "Hong Kong SAR, China")] <- "Hong Kong"
GDP$`Country Name`[which(GDP$`Country Name` == "Kyrgyz Republic")] <- "Kyrgyzstan"

# Taiwanese data taken from wikipedia citing Chinese gov figures
# http://statdb.dgbas.gov.tw/pxweb/Dialog/varval.asp?ma=NA8101A1A&ti=%B0%EA%A5%C1%A9%D2%B1o%B2%CE%ADp%B1`%A5%CE%B8%EA%AE%C6(2008SNA)-%A6~&path=../PXfile/NationalIncome/&lang=9&strList=L

Taiwan_GDP <- read_excel("Taiwan GDP.xlsx")

twnrow <- cbind("Taiwan", t(Taiwan_GDP$`GDP Change`)) %>% data.frame
colnames(twnrow) <- colnames(GDP)

GDP %<>% rbind(.,twnrow)

countryyr <- data.frame(
country =
    c(CSES1n$`ID VARIABLE - POLITY NAME`,
      CSES2n$`ID VARIABLE - POLITY NAME`,
      CSES3n$`ID VARIABLE - POLITY NAME`,
      CSES4n$`ID COMPONENT - POLITY NAME`,
      CSES5n$`ID COMPONENT - POLITY NAME`),
year =
    c(CSES1n$`ID COMPONENT - ELECTION YEAR`,
      CSES2n$`ID COMPONENT - ELECTION YEAR`,
      CSES3n$`ID COMPONENT - ELECTION YEAR`,
      CSES4n$`ID COMPONENT - ELECTION YEAR`,
      CSES5n$`ID COMPONENT - ELECTION YEAR`) )

countryyr %<>% mutate(
  group = group_indices(countryyr, country, year))

colnames(GDP) %<>% strsplit(" ") %>% sapply(function(x) ifelse(x[1] == "Country", x[1], as.numeric(x[1])))

GDPdiff <-  function(x) {
  filt <- filter(countryyr, group == x) 
  country <- pull(filt, country)[1]
  year <- pull(filt, year)[1]
  GDP_filt <- filter(GDP, `Country` == country)
  year10 <- year - 10
  GDP_cut <- select(GDP_filt, which(colnames(GDP_filt) == year10):which(colnames(GDP_filt) == year)) %>% data.frame
  average <- GDP_cut[1,] %>% apply(1, function(x) as.numeric(x) %>% mean(.,na.rm=T))
  thisyear <- select(GDP_filt, which(colnames(GDP_filt) == year)) %>% as.numeric
  log(Mod(thisyear - average)) %>% as.numeric(.) %>% rep(., times = nrow(filt))
}

countryyr %<>% mutate(
  GDPdiff_raw = countryyr$group %>% unique %>% sapply(function(x) GDPdiff(x)) %>% unlist,
  min = min(GDPdiff_raw),
  max = max(GDPdiff_raw),
  GDPdiff = (0.8*(GDPdiff_raw - min)/(max - min)) + 0.2
)

# now getting the vote shares for each party in each election

LH1 <- cbind(select(CSES1, A5005_A:A5005_F), c(NA), c(NA), c(NA))
LH2 <- select(CSES2, B5001_A:B5001_I)
LH3 <- select(CSES3, C5001_A:C5001_I)
LH4 <- select(CSES4, D5001_A:D5001_I)
LH5 <- select(CSES5, E5001_A:E5001_I)

colnames(LH1) = colnames(LH2) = colnames(LH3) = colnames(LH4) = colnames(LH5) = paste0(c("A", "B", "C", "D", "E", "F", "G", "H", "I"), "_LH")

LHpercs <- rbind(LH1, 
                 LH2 %>% transmute_all(as.numeric), 
                 LH3 %>% transmute_all(as.numeric), 
                 LH4 %>% transmute_all(as.numeric), 
                 LH5 %>% transmute_all(as.numeric))

P1 <- cbind(select(CSES1, A5009_A:A5009_F), c(NA), c(NA), c(NA))
P2 <- select(CSES2, B5005_A:B5005_I)
P3 <- select(CSES3, C5005_A:C5005_I)
P4 <- select(CSES4, D5005_A:D5005_I)
P5 <- select(CSES5, E5005_A:E5005_I)

colnames(P1) = colnames(P2) = colnames(P3) = colnames(P4) = colnames(P5) = paste0(c("A", "B", "C", "D", "E", "F", "G", "H", "I"), "_P")

Ppercs <- rbind(P1 %>% transmute_all(as.numeric),
                P2 %>% transmute_all(as.numeric),
                P3 %>% transmute_all(as.numeric),
                P4 %>% transmute_all(as.numeric),
                P5 %>% transmute_all(as.numeric))

majorcode <- cbind(LHpercs, Ppercs) %>% 
  transmute_all(function(x) ifelse(between(x, 9.99, 100), 1, NA))

module <- c(CSES1$A1001, CSES2$B1001, CSES3$C1001, CSES4$D1001, CSES5$E1001) %>% 
  sapply(function(x) strsplit(x, "CSES-MODULE-")[[1]][2]) %>% as.numeric

code <- c(CSES1$A1004, CSES2$B1004, CSES3$C1004, CSES4$D1004, CSES5$E1004)

filter(CSES4, D1004 == "ARG_2015") %>% View


hetweights <- cbind(countryyr, demsatis, majorcode, module, code)

# finding the elections for which no real satisfaction with democracy data was provided and
# replacing those elections' scores with the mean score for the remaining elections
demsatisnodata <- hetweights %>% group_by(year, code) %>% summarise(mean = mean(demsatis)) %>% filter(mean == 0.2) %>% pull(code) %>% as.character
meandemsatis <- filter(hetweights, code %in% demsatisnodata == FALSE) %>% group_by(year, code) %>% summarise(mean = mean(demsatis)) %>% pull(mean) %>% mean(.)
hetweights$demsatis[which(hetweights$code %in% demsatisnodata)] <- meandemsatis 

write.csv(hetweights, "hetweights.csv")


=======

# DY Note:

# This script should be run second

# This script constructs, and scales, the variables used as weights
# for voters' preferences in the determination of the continuous consistency calculation.
# The exception is the range of ideologies, which is calculated in the 'penultimate processing' script

# Requires: cses1, cses2, cses3, cses4, cses5
#           Data_Extract_From_World_Development_Indicators-2.xlsx ("GDP")
#           Taiwan GDP.xlsx ("Taiwan_GDP")

# Outputs: hetweights.csv

rm(list = ls())


library(magrittr)
library(haven)
library(dplyr)

CSES1 <- read_sav("cses1.sav")
CSES2 <- read_sav("cses2.sav")
CSES3 <- read_sav("cses3.sav")
CSES4 <- read_sav("cses4.sav")
CSES5 <- read_sav("cses5.sav")


library(dplyr)
library(magrittr)

labels_to_names <- function(df) {
  new_df <- df
  colnames(new_df) %<>% sapply(function(x) pull(new_df, x) %>% attr(., "label"))
  new_df
}

guide <- function(df) {
  labels <- data.frame(
    variable = colnames(df), 
    label = colnames(df) %>% sapply(function(x) label = pull(CSES1, x) %>% attr("label")))
}

by_name <- function(x, df) {
  filter(guide(df), label == x) %>% pull(variable) %>% as.character
}



# the following functions are used to search through the variables so I can find
# what I am looking for

LABELS_TO_NAMES <- function(df) {
  n <- ncol(df)
  for (c in 1:n) {
    label <- attr(df[ , c, drop = TRUE], "label")
    name <- colnames(df)[c]
    colnames(df)[which(colnames(df) == name)] <- label
  }
  return(df)
}

CSES1n <- LABELS_TO_NAMES(CSES1)
CSES2n <- LABELS_TO_NAMES(CSES2)
CSES3n <- LABELS_TO_NAMES(CSES3)
CSES4n <- LABELS_TO_NAMES(CSES4)
CSES5n <- LABELS_TO_NAMES(CSES5)

# efficacy
CSES1n$`WHO IS IN POWER CAN MAKE DIFFERENCE`
CSES1n$`WHO PEOPLE VOTE FOR MAKES A DIFFERENCE`
CSES2n$`WHO IS IN POWER CAN MAKE DIFFERENCE`
CSES2n$`WHO PEOPLE VOTE FOR MAKES A DIFFERENCE`
CSES3n$`WHO IS IN POWER CAN MAKE DIFFERENCE`
CSES3n$`WHO PEOPLE VOTE FOR MAKES A DIFFERENCE`
CSES4n$`WHO IS IN POWER CAN MAKE DIFFERENCE`
CSES4n$`WHO PEOPLE VOTE FOR MAKES A DIFFERENCE`
CSES5n$`WHO IS IN POWER CAN MAKE DIFFERENCE`
CSES5n$`WHO PEOPLE VOTE FOR MAKES A DIFFERENCE`

# trying to find a variable to replace 'Political Parties are necessary' 
# and 'Political parties care what people think'

cbind(
  CSES1n$`WHO IS IN POWER CAN MAKE DIFFERENCE`, 
  CSES1n$`WHO PEOPLE VOTE FOR MAKES A DIFFERENCE`,
  CSES1n$`POLITICAL PARTIES ARE NECESSARY`,
  CSES1n$`POLITICAL PARTIES CARE WHAT PEOPLE THINK`,
  CSES1n$`SATISFACTION WITH DEMOCRATIC PROCESS`,
  CSES1n$`POLITICIANS KNOW WHAT PEOPLE THINK`) %>% 
data.frame %>% filter_all(function(x) x != 9) %>% cor

# satisfaction with democratic process correlates positively with both (r = 0.275 and r = 0.270)
# and is in all samples

# now scaling it to range between 0.2 and 1 for all Ss

cleandemsatis <- function(x) {
  x %>% unfactor %>% sapply(function(x) multiply_by(x, 0.8) %>% add(0.2) %>% ifelse(is.na(.) == TRUE, 0.2, .))
}


  satis1 = CSES1n$`SATISFACTION WITH DEMOCRATIC PROCESS` %>% factor(levels = c(1, 2, 4, 5), labels = c(3/3, 2/3, 1/3, 0/3)) %>% cleandemsatis
  satis2 = CSES2n$`SATISFACTION WITH DEMOCRATIC PROCESS` %>% factor(levels = c(1, 2, 3, 4), labels = c(3/3, 2/3, 1/3, 0/3)) %>% cleandemsatis
  satis3 = CSES3n$`SATISFACTION WITH DEMOCRACY` %>% factor(levels = c(1, 2, 4, 5), labels = c(3/3, 2/3, 1/3, 0/3)) %>% cleandemsatis
  satis4 = CSES4n$`SATISFACTION WITH DEMOCRACY` %>% factor(levels = c(1, 2, 4, 5), labels = c(3/3, 2/3, 1/3, 0/3)) %>% cleandemsatis
  satis5 = CSES5n$`SATISFACTION WITH DEMOCRACY` %>% factor(levels = c(1, 2, 4, 5), labels = c(3/3, 2/3, 1/3, 0/3)) %>% cleandemsatis


demsatis <- c(satis1, satis2, satis3, satis4, satis5)


# the next weight variables is the log of the absolute (modulus) difference between
# GDP growth in the election year, and the 10-year average of annual GDP growth

#https://databank.worldbank.org/reports.aspx?source=2&series=NY.GDP.MKTP.KD.ZG&country=#
  
GDP <- 
  read_excel("Data_Extract_From_World_Development_Indicators-2.xlsx")

GDP <- GDP[,c(3,5:ncol(GDP))]

# need to adjust some of the names to match what is in the CSES, and add in a row for Taiwan

GDP$`Country Name`[which(GDP$`Country Name` == "United Kingdom")] <- "Great Britain"
GDP$`Country Name`[which(GDP$`Country Name` == "United States")] <- "United States of America"
GDP$`Country Name`[which(GDP$`Country Name` == "Korea, Rep.")] <- "Republic of Korea"
GDP$`Country Name`[which(GDP$`Country Name` == "Slovak Republic")] <- "Slovakia"
GDP$`Country Name`[which(GDP$`Country Name` == "Hong Kong SAR, China")] <- "Hong Kong"
GDP$`Country Name`[which(GDP$`Country Name` == "Kyrgyz Republic")] <- "Kyrgyzstan"

# Taiwanese data taken from wikipedia citing Chinese gov figures
# http://statdb.dgbas.gov.tw/pxweb/Dialog/varval.asp?ma=NA8101A1A&ti=%B0%EA%A5%C1%A9%D2%B1o%B2%CE%ADp%B1`%A5%CE%B8%EA%AE%C6(2008SNA)-%A6~&path=../PXfile/NationalIncome/&lang=9&strList=L

Taiwan_GDP <- read_excel("Taiwan GDP.xlsx")

twnrow <- cbind("Taiwan", t(Taiwan_GDP$`GDP Change`)) %>% data.frame
colnames(twnrow) <- colnames(GDP)

GDP %<>% rbind(.,twnrow)

countryyr <- data.frame(
country =
    c(CSES1n$`ID VARIABLE - POLITY NAME`,
      CSES2n$`ID VARIABLE - POLITY NAME`,
      CSES3n$`ID VARIABLE - POLITY NAME`,
      CSES4n$`ID COMPONENT - POLITY NAME`,
      CSES5n$`ID COMPONENT - POLITY NAME`),
year =
    c(CSES1n$`ID COMPONENT - ELECTION YEAR`,
      CSES2n$`ID COMPONENT - ELECTION YEAR`,
      CSES3n$`ID COMPONENT - ELECTION YEAR`,
      CSES4n$`ID COMPONENT - ELECTION YEAR`,
      CSES5n$`ID COMPONENT - ELECTION YEAR`) )

countryyr %<>% mutate(
  group = group_indices(countryyr, country, year))

colnames(GDP) %<>% strsplit(" ") %>% sapply(function(x) ifelse(x[1] == "Country", x[1], as.numeric(x[1])))

GDPdiff <-  function(x) {
  filt <- filter(countryyr, group == x) 
  country <- pull(filt, country)[1]
  year <- pull(filt, year)[1]
  GDP_filt <- filter(GDP, `Country` == country)
  year10 <- year - 10
  GDP_cut <- select(GDP_filt, which(colnames(GDP_filt) == year10):which(colnames(GDP_filt) == year)) %>% data.frame
  average <- GDP_cut[1,] %>% apply(1, function(x) as.numeric(x) %>% mean(.,na.rm=T))
  thisyear <- select(GDP_filt, which(colnames(GDP_filt) == year)) %>% as.numeric
  log(Mod(thisyear - average)) %>% as.numeric(.) %>% rep(., times = nrow(filt))
}

countryyr %<>% mutate(
  GDPdiff_raw = countryyr$group %>% unique %>% sapply(function(x) GDPdiff(x)) %>% unlist,
  min = min(GDPdiff_raw),
  max = max(GDPdiff_raw),
  GDPdiff = (0.8*(GDPdiff_raw - min)/(max - min)) + 0.2
)

# now getting the vote shares for each party in each election

LH1 <- cbind(select(CSES1, A5005_A:A5005_F), c(NA), c(NA), c(NA))
LH2 <- select(CSES2, B5001_A:B5001_I)
LH3 <- select(CSES3, C5001_A:C5001_I)
LH4 <- select(CSES4, D5001_A:D5001_I)
LH5 <- select(CSES5, E5001_A:E5001_I)

colnames(LH1) = colnames(LH2) = colnames(LH3) = colnames(LH4) = colnames(LH5) = paste0(c("A", "B", "C", "D", "E", "F", "G", "H", "I"), "_LH")

LHpercs <- rbind(LH1, 
                 LH2 %>% transmute_all(as.numeric), 
                 LH3 %>% transmute_all(as.numeric), 
                 LH4 %>% transmute_all(as.numeric), 
                 LH5 %>% transmute_all(as.numeric))

P1 <- cbind(select(CSES1, A5009_A:A5009_F), c(NA), c(NA), c(NA))
P2 <- select(CSES2, B5005_A:B5005_I)
P3 <- select(CSES3, C5005_A:C5005_I)
P4 <- select(CSES4, D5005_A:D5005_I)
P5 <- select(CSES5, E5005_A:E5005_I)

colnames(P1) = colnames(P2) = colnames(P3) = colnames(P4) = colnames(P5) = paste0(c("A", "B", "C", "D", "E", "F", "G", "H", "I"), "_P")

Ppercs <- rbind(P1 %>% transmute_all(as.numeric),
                P2 %>% transmute_all(as.numeric),
                P3 %>% transmute_all(as.numeric),
                P4 %>% transmute_all(as.numeric),
                P5 %>% transmute_all(as.numeric))

majorcode <- cbind(LHpercs, Ppercs) %>% 
  transmute_all(function(x) ifelse(between(x, 9.99, 100), 1, NA))

module <- c(CSES1$A1001, CSES2$B1001, CSES3$C1001, CSES4$D1001, CSES5$E1001) %>% 
  sapply(function(x) strsplit(x, "CSES-MODULE-")[[1]][2]) %>% as.numeric

code <- c(CSES1$A1004, CSES2$B1004, CSES3$C1004, CSES4$D1004, CSES5$E1004)

filter(CSES4, D1004 == "ARG_2015") %>% View


hetweights <- cbind(countryyr, demsatis, majorcode, module, code)

# finding the elections for which no real satisfaction with democracy data was provided and
# replacing those elections' scores with the mean score for the remaining elections
demsatisnodata <- hetweights %>% group_by(year, code) %>% summarise(mean = mean(demsatis)) %>% filter(mean == 0.2) %>% pull(code) %>% as.character
meandemsatis <- filter(hetweights, code %in% demsatisnodata == FALSE) %>% group_by(year, code) %>% summarise(mean = mean(demsatis)) %>% pull(mean) %>% mean(.)
hetweights$demsatis[which(hetweights$code %in% demsatisnodata)] <- meandemsatis 

write.csv(hetweights, "hetweights.csv")


>>>>>>> 7dfd48aff9080e84e5bf37914150baa32b3f585c
