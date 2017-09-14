# Load data into R

library(readr)
library(tidyr)
library(dplyr)

dcc <- read_csv("~/Documents/Eitan/R/Capstone/default of credit card clients.csv")

# Change column names from X1, X2 etc to the value in row 2

colnames(dcc) <- dcc[1,]

# Delete row 2 to remove duplicate titles

dcc = dcc[-1,]

# Remove ID column (column #1)

dcc <- dcc[-1]

# Clean up column names

names(dcc) <- c("LimitAmt", "Gender", "Education", "Marriage", "Age", "StatusSep05", "StatusAug05"
                , "StatusJul05", "StatusJun05", "StatusMay05", "StatusApr05", "BalSep05", "BalAug05",
                "BalJul05", "BalJun05", "BalMay05", "BalApr05", "PayAmtSep05", "PayAmtAug05",
                "PayAmtJul05", "PayAmtJun05", "PayAmtMay05", "PayAmtApr05", "DefaultOct05")

# For the education column, change values 4, 5, 6 to 0 as all represent "Other"

dcc$Education[dcc$Education == 4] <- 0
dcc$Education[dcc$Education == 5] <- 0
dcc$Education[dcc$Education == 6] <- 0

# Convert columns from character to numeric

dcc$LimitAmt <- as.numeric(dcc$LimitAmt)
dcc$Age <- as.numeric(dcc$Age)

dcc$BalSep05 <- as.numeric(dcc$BalSep05)
dcc$BalAug05 <- as.numeric(dcc$BalAug05)
dcc$BalJul05 <- as.numeric(dcc$BalJul05)
dcc$BalJun05 <- as.numeric(dcc$BalJun05)
dcc$BalMay05 <- as.numeric(dcc$BalMay05)
dcc$BalApr05 <- as.numeric(dcc$BalApr05)

dcc$PayAmtSep05 <- as.numeric(dcc$PayAmtSep05)
dcc$PayAmtAug05 <- as.numeric(dcc$PayAmtAug05)
dcc$PayAmtJul05 <- as.numeric(dcc$PayAmtJul05)
dcc$PayAmtJun05 <- as.numeric(dcc$PayAmtJun05)
dcc$PayAmtMay05 <- as.numeric(dcc$PayAmtMay05)
dcc$PayAmtApr05 <- as.numeric(dcc$PayAmtApr05)

# Change values from integer to categorical for Sex, Education, Marriage, Default columns

dcc$Gender[dcc$Gender == 1] <- "Male"
dcc$Gender[dcc$Gender == 2] <- "Female"

dcc$Education[dcc$Education == 0] <- "Other"
dcc$Education[dcc$Education == 1] <- "GradSch"
dcc$Education[dcc$Education == 2] <- "Bachelors"
dcc$Education[dcc$Education == 3] <- "HS"

dcc$Marriage[dcc$Marriage == 0] <- "Div"
dcc$Marriage[dcc$Marriage == 1] <- "Mar"
dcc$Marriage[dcc$Marriage == 2] <- "Single"
dcc$Marriage[dcc$Marriage == 3] <- "Div"

dcc$DefaultOct05[dcc$DefaultOct05 == 0] <- "NDef"
dcc$DefaultOct05[dcc$DefaultOct05 == 1] <- "Def"

dcc$Gender <- factor(dcc$Gender)
levels(dcc$Gender) <- c("Male", "Female")

dcc$Education <- factor(dcc$Education)
levels(dcc$Education) <- c("Other", "GradSch", "Bachelors", "HS")

dcc$Marriage <- factor(dcc$Marriage)
levels(dcc$Marriage) <- c("Div", "Mar", "Single", "Div")

dcc$DefaultOct05 <- factor(dcc$DefaultOct05)
levels(dcc$DefaultOct05) <- c("NDef", "Def")

# Change values from integer to categorical for payment status columns from Apr05 to May05

dcc$StatusSep05 <- factor(dcc$StatusSep05)
dcc$StatusAug05 <- factor(dcc$StatusAug05)
dcc$StatusJul05 <- factor(dcc$StatusJul05)
dcc$StatusJun05 <- factor(dcc$StatusJun05)
dcc$StatusMay05 <- factor(dcc$StatusMay05)
dcc$StatusApr05 <- factor(dcc$StatusApr05)

levels(dcc$StatusSep05)<- c("SepPaid", "SepNoCons", "SepRev", "Sep1MoD", "Sep2MoD", "Sep3MoD", "Sep4MoD", "Sep5MoD", "Sep6MoD", "Sep7MoD", "Sep8MoD")
levels(dcc$StatusAug05)<- c("AugPaid", "AugNoCons", "AugRev", "Aug1MoD", "Aug2MoD", "Aug3MoD", "Aug4MoD", "Aug5MoD", "Aug6MoD", "Aug7MoD", "Aug8MoD")
levels(dcc$StatusJul05)<- c("JulPaid", "JulNoCons", "JulRev", "Jul1MoD", "Jul2MoD", "Jul3MoD", "Jul4MoD", "Jul5MoD", "Jul6MoD", "Jul7MoD", "Jul8MoD")
levels(dcc$StatusJun05)<- c("JunPaid", "JunNoCons", "JunRev", "Jun1MoD", "Jun2MoD", "Jun3MoD", "Jun4MoD", "Jun5MoD", "Jun6MoD", "Jun7MoD", "Jun8MoD")
levels(dcc$StatusMay05)<- c("MayPaid", "MayNoCons", "MayRev", "May2MoD", "May3MoD", "May4MoD", "May5MoD", "May6MoD", "May7MoD", "May8MoD")
levels(dcc$StatusApr05)<- c("AprPaid", "AprNoCons", "AprRev", "Apr2MoD", "Apr3MoD", "Apr4MoD", "Apr5MoD", "Apr6MoD", "Apr7MoD", "Apr8MoD")

# Create new columns with Bins for Loan, Age, Balance and Pay columns

dcc$LimitAmtBin <- paste(dcc$LimitAmt)
dcc$LimitAmtBin <- as.numeric(as.character(dcc$LimitAmtBin))
sapply(dcc$LimitAmtBin, class)
LAmtCut <- cut(dcc$LimitAmtBin, breaks = c(0, 10000, 25000, 50000, 100000, 250000, 500000, 1000000),
               labels = c("10KLimit", "25KLimit", "50KLimit", "100KLimit", "250KLimit", "500KLimit", "1MilLimit"))
dcc$LimitAmtBin <- LAmtCut

dcc$AgeBin <- paste(dcc$Age)
dcc$AgeBin <- as.numeric(as.character(dcc$AgeBin))
sapply(dcc$AgeBin, class)
AgeCut <- cut(dcc$AgeBin, breaks = c(20, 25, 30, 35, 40, 50, 60, 80),
              labels = c("25Y", "30Y", "35Y", "40Y", "50Y", "60Y", "80Y"))
dcc$AgeBin <- AgeCut       

dcc$BalBinSep05 <- paste(dcc$BalSep05)
dcc$BalBinSep05 <- as.numeric(as.character(dcc$BalBinSep05))
sapply(dcc$BalBinSep05, class)
BalSepCut <- cut(dcc$BalBinSep05, breaks = c(-1000000, -1, 0, 3000, 10000, 25000, 50000, 100000, 250000, 500000, 1000000),
                 labels = c("NegSepBal", "ZeroSepBal", "3KSepBal", "10KSepBal", "25KSepBal", "50KSepBal", "100KSepBal", "250KSepBal", "500KSepBal", "1milSepBal"))
dcc$BalBinSep05 <- BalSepCut

dcc$BalBinAug05 <- paste(dcc$BalAug05)
dcc$BalBinAug05 <- as.numeric(as.character(dcc$BalBinAug05))
sapply(dcc$BalBinAug05, class)
BalAugCut <- cut(dcc$BalBinAug05, breaks = c(-1000000, -1, 0, 3000, 10000, 25000, 50000, 100000, 250000, 500000, 1000000),
                 labels = c("NegAugBal", "ZeroAugBal", "3KAugBal", "10KAugBal", "25KAugBal", "50KAugBal", "100KAugBal", "250KAugBal", "500KAugBal", "1milAugBal"))
dcc$BalBinAug05 <- BalAugCut

dcc$BalBinJul05 <- paste(dcc$BalJul05)
dcc$BalBinJul05 <- as.numeric(as.character(dcc$BalBinJul05))
sapply(dcc$BalBinJul05, class)
BalJulCut <- cut(dcc$BalBinJul05, breaks = c(-1000000, -1, 0, 3000, 10000, 25000, 50000, 100000, 250000, 500000, 1000000),
                 labels = c("NegJulBal", "ZeroJulBal", "3KJulBal", "10KJulBal", "25KJulBal", "50KJulBal", "100KJulBal", "250KJulBal", "500KJulBal", "1milJulBal"))
dcc$BalBinJul05 <- BalJulCut

dcc$BalBinJun05 <- paste(dcc$BalJun05)
dcc$BalBinJun05 <- as.numeric(as.character(dcc$BalBinJun05))
sapply(dcc$BalBinJun05, class)
BalJunCut <- cut(dcc$BalBinJun05, breaks = c(-1000000, -1, 0, 3000, 10000, 25000, 50000, 100000, 250000, 500000, 1000000),
                 labels = c("NegJunBal", "ZeroJunBal", "3KJunBal", "10KJunBal", "25KJunBal", "50KJunBal", "100KJunBal", "250KJunBal", "500KJunBal", "1milJunBal"))
dcc$BalBinJun05 <- BalJunCut

dcc$BalBinMay05 <- paste(dcc$BalMay05)
dcc$BalBinMay05 <- as.numeric(as.character(dcc$BalBinMay05))
sapply(dcc$BalBinMay05, class)
BalMayCut <- cut(dcc$BalBinMay05, breaks = c(-1000000, -1, 0, 3000, 10000, 25000, 50000, 100000, 250000, 500000, 1000000),
                 labels = c("NegMayBal", "ZeroMayBal", "3KMayBal", "10KMayBal", "25KMayBal", "50KMayBal", "100KMayBal", "250KMayBal", "500KMayBal", "1milMayBal"))
dcc$BalBinMay05 <- BalMayCut

dcc$BalBinApr05 <- paste(dcc$BalApr05)
dcc$BalBinApr05 <- as.numeric(as.character(dcc$BalBinApr05))
sapply(dcc$BalBinApr05, class)
BalAprCut <- cut(dcc$BalBinApr05, breaks = c(-1000000, -1, 0, 3000, 10000, 25000, 50000, 100000, 250000, 500000, 1000000),
                 labels = c("NegAprBal", "ZeroAprBal", "3KAprBal", "10KAprBal", "25KAprBal", "50KAprBal", "100KAprBal", "250KAprBal", "500KAprBal", "1milAprBal"))
dcc$BalBinApr05 <- BalAprCut

dcc$PayAmtBinSep05 <- paste(dcc$PayAmtSep05)
dcc$PayAmtBinSep05 <- as.numeric(as.character(dcc$PayAmtBinSep05))
sapply(dcc$PayAmtBinSep05, class)
PaySepCut <- cut(dcc$PayAmtBinSep05, breaks = c(-1000000, 0, 1000, 1500, 2000, 2500, 5000, 10000, 25000, 100000, 1000000),
                 labels = c("ZeroPayAmtSep", "1KPayAmtSep", "1.5KPayAmtSep", "2KPayAmtSep", "2.5KPayAmtSep", "5KPayAmtSep", "10KPayAmtSep", "25KPayAmtSep", "100KPayAmtSep", ">100KPayAmtSep"))
dcc$PayAmtBinSep05 <- PaySepCut

dcc$PayAmtBinAug05 <- paste(dcc$PayAmtAug05)
dcc$PayAmtBinAug05 <- as.numeric(as.character(dcc$PayAmtBinAug05))
sapply(dcc$PayAmtBinAug05, class)
PayAugCut <- cut(dcc$PayAmtBinAug05, breaks = c(-1000000, 0, 1000, 1500, 2000, 2500, 5000, 10000, 25000, 100000, 1000000),
                 labels = c("ZeroPayAmtAug", "1KPayAmtAug", "1.5KPayAmtAug", "2KPayAmtAug", "2.5KPayAmtAug", "5KPayAmtAug", "10KPayAmtAug", "25KPayAmtAug", "100KPayAmtAug", ">100KPayAmtAug"))
dcc$PayAmtBinAug05 <- PayAugCut

dcc$PayAmtBinJul05 <- paste(dcc$PayAmtJul05)
dcc$PayAmtBinJul05 <- as.numeric(as.character(dcc$PayAmtBinJul05))
sapply(dcc$PayAmtBinJul05, class)
PayJulCut <- cut(dcc$PayAmtBinJul05, breaks = c(-1000000, 0, 1000, 1500, 2000, 2500, 5000, 10000, 25000, 100000, 1000000),
                 labels = c("ZeroPayAmtJul", "1KPayAmtJul", "1.5KPayAmtJul", "2KPayAmtJul", "2.5KPayAmtJul", "5KPayAmtJul", "10KPayAmtJul", "25KPayAmtJul", "100KPayAmtJul", ">100KPayAmtJul"))
dcc$PayAmtBinJul05 <- PayJulCut

dcc$PayAmtBinJun05 <- paste(dcc$PayAmtJun05)
dcc$PayAmtBinJun05 <- as.numeric(as.character(dcc$PayAmtBinJun05))
sapply(dcc$PayAmtBinJun05, class)
PayJunCut <- cut(dcc$PayAmtBinJun05, breaks = c(-1000000, 0, 1000, 1500, 2000, 2500, 5000, 10000, 25000, 100000, 1000000),
                 labels = c("ZeroPayAmtJun", "1KPayAmtJun", "1.5KPayAmtJun", "2KPayAmtJun", "2.5KPayAmtJun", "5KPayAmtJun", "10KPayAmtJun", "25KPayAmtJun", "100KPayAmtJun", ">100KPayAmtJun"))
dcc$PayAmtBinJun05 <- PayJunCut

dcc$PayAmtBinMay05 <- paste(dcc$PayAmtMay05)
dcc$PayAmtBinMay05 <- as.numeric(as.character(dcc$PayAmtBinMay05))
sapply(dcc$PayAmtBinMay05, class)
PayMayCut <- cut(dcc$PayAmtBinMay05, breaks = c(-1000000, 0, 1000, 1500, 2000, 2500, 5000, 10000, 25000, 100000, 1000000),
                 labels = c("ZeroPayAmtMay", "1KPayAmtMay", "1.5KPayAmtMay", "2KPayAmtMay", "2.5KPayAmtMay", "5KPayAmtMay", "10KPayAmtMay", "25KPayAmtMay", "100KPayAmtMay", ">100KPayAmtMay"))
dcc$PayAmtBinMay05 <- PayMayCut

dcc$PayAmtBinApr05 <- paste(dcc$PayAmtApr05)
dcc$PayAmtBinApr05 <- as.numeric(as.character(dcc$PayAmtBinApr05))
sapply(dcc$PayAmtBinApr05, class)
PayAprCut <- cut(dcc$PayAmtBinApr05, breaks = c(-1000000, 0, 1000, 1500, 2000, 2500, 5000, 10000, 25000, 100000, 1000000),
                 labels = c("ZeroPayAmtApr", "1KPayAmtApr", "1.5KPayAmtApr", "2KPayAmtApr", "2.5KPayAmtApr", "5KPayAmtApr", "10KPayAmtApr", "25KPayAmtApr", "100KPayAmtApr", ">100KPayAmtApr"))
dcc$PayAmtBinApr05 <- PayAprCut

# Create combination columns

dcc$LA <- paste(dcc$LimitAmtBin, "-", dcc$AgeBin)
dcc$LG <- paste(dcc$LimitAmtBin, "-", dcc$Gender)
dcc$LE <- paste(dcc$LimitAmtBin, "-", dcc$Education)
dcc$LM <- paste(dcc$LimitAmtBin, "-", dcc$Marriage)

dcc$GE <- paste(dcc$Gender, "-", dcc$Education)
dcc$GM <- paste(dcc$Gender, "-", dcc$Marriage)
dcc$GA <- paste(dcc$Gender, "-", dcc$AgeBin)

dcc$EM <- paste(dcc$Education, "-", dcc$Marriage)
dcc$EA <- paste(dcc$Education, "-", dcc$AgeBin)

dcc$MA <- paste(dcc$Marriage, "-", dcc$AgeBin)

dcc$GEM <- paste(dcc$Gender, "-", dcc$Education, "-", dcc$Marriage)

dcc$LAG <- paste(dcc$LimitAmtBin, "-", dcc$AgeBin, "-", dcc$Gender)
dcc$LAE <- paste(dcc$LimitAmtBin, "-", dcc$AgeBin, "-", dcc$Education)
dcc$LAM <- paste(dcc$LimitAmtBin, "-", dcc$AgeBin, "-", dcc$Marriage)

dcc$LAGE <- paste(dcc$LimitAmtBin, "-", dcc$AgeBin, "-", dcc$Gender, "-", dcc$Education)
dcc$LAGM <- paste(dcc$LimitAmtBin, "-", dcc$AgeBin, "-", dcc$Gender, "-", dcc$Marriage)
dcc$LAEM <- paste(dcc$LimitAmtBin, "-", dcc$AgeBin, "-", dcc$Education, "-", dcc$Marriage)

dcc$LAGEM <- paste(dcc$LimitAmtBin, "-", dcc$AgeBin, "-", dcc$Gender, "-", dcc$Education, "-", dcc$Marriage)

write.csv(dcc, "clean.csv")
