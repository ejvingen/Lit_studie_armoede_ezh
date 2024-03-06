# Gegevens uitwisselen met Rayyan

library(revtools)
library(dplyr)
library(stringr)
library(summarytools)
library(stringdist)
library(ggplot2)
library(cowplot)
library(psych)
library(gmodels)

# set wd op thuis- of werklaptop
if(file.exists("C:/Users/ejvan")) {
    setwd("C:/Users/ejvan/OneDrive - Vrije Universiteit Amsterdam")
} else if(file.exists("C:/Users/ein900")) {
    setwd("C:/Users/ein900/OneDrive - Vrije Universiteit Amsterdam")
} else {
    warning("Geen van beide mappen gevonden")
}
setwd("./Onderzoeksprojecten/Armoede en eenzaamheid VWS")


# ************ DATA CHANTAL *****************************************

tmp1 <- read.csv("voorspellen inclusie/customizations_log 2024 02 27.csv")
#freq(tmp1$value)    # 1 = included; 2 = excluded?? (en 0 dan?)
tmp2 <- read.csv("voorspellen inclusie/articles 2024 02 27.csv")

# id universeel maken en mergen
tmp2$key <- substr(tmp2$key, 8, nchar(tmp2$key))
tmp2$article_id <- as.integer(tmp2$key)
tmp2 <- tmp2 %>% select(-key)
d <- left_join(tmp1, tmp2, by = "article_id")    
rm(tmp1, tmp2)

# eerste 10 entries; 5 niet beoordeeld en dan terug importeren
freq(d$user_id)
d <- d %>% slice(1:20)
d$value[15:20] <- NA

write.csv(d, file = "terug naar Ray.csv")     # werkt niet


# ************* Direct bibtex e.d. importeren *****************************

# vanuit Rayyan geëxporteerd als .bib
tmp <- read_bibliography("voorspellen inclusie/articles.bib")
names(tmp)
freq(tmp$database)

# screen abstract
str <- screen_abstracts(tmp)
str <- screen_abstracts(str)

# terug importeren na screening
terug <- read_bibliography("lukt dit.csv")
freq(terug$screened_abstracts)    # ja

# maak test bestand
write_bibliography(slice(terug, 1250:1300), 
                   filename = "voorspellen inclusie/test revtools.bib",
                   format = "bib")

#  exporteren Endnote (is niet helemaal perfect)
terug <- terug %>% filter(!is.na(screened_abstracts))
write_bibliography(terug, filename = "terug.bib", format = "bib")


# direct db van Linda - werkt helaas niet
tmp <- read_bibliography("armoede en eenzaamheid shared/RIS bestand 2654 refs_ontdubbeld 20240304.ris")










