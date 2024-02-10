# verkennen database armoede eenzaamheid
# en maken model dat inclusie voorspelt

# NOTES
# - Vaak onjuiste hits op poor vanwege "poor health", "poor outcomes", etc


# ------------------------------------------------------------------------------

library(bib2df)
library(dplyr)
library(stringr)
library(summarytools)
library(stringdist)
library(ggplot2)
library(cowplot)
library(psych)

setwd(paste0("C:/Users/ejvan/OneDrive - Vrije Universiteit Amsterdam/",
        "Onderzoeksprojecten/Armoede en eenzaamheid VWS/voorspellen inclusie"))
dir()

tmp1 <- read.csv("customizations_log.csv")
freq(tmp1$value)    # 1 = included; 2 = excluded?? (en 0 dan?)

tmp2 <- read.csv("articles.csv")

# id universeel maken en mergen
tmp2$key <- substr(tmp2$key, 8, nchar(tmp2$key))
tmp2$article_id <- as.integer(tmp2$key)
tmp2 <- tmp2 %>% select(-key)
d <- left_join(tmp1, tmp2, by = "article_id")    
rm(tmp1, tmp2)

# verkennen variabelen
names(d)
length(unique(d$title))         # 503 unieke titels op 668 cases
freq(d$value)
freq(d$user_id)                 # Bianca 141; Chantal 527

# dubbele titels bij Chantal? 
length(unique(d$title[d$user_id == 489417])) # 500 uniek vd 527

# in-/ exclusie - voor nu zonder 0, alleen Chantal, ontdubbeld
freq(d$value)
d <- d %>% filter(value !=0 & d$user_id == 489417)
which(duplicated(d$article_id))     # 16 dubbel
d <- d %>% filter(!duplicated(d$article_id))
d$inc <- ifelse(d$value==1, 1, 0)
freq(d$inc)                     # n = 490, waarvan 83 included


# ----------------------- VOORSPELLERS MAKEN -----------------------------------

# ----------- eenzaamheid in titel: ja of nee ---------------

d$ezh_tit <- ifelse(grepl("[lL]onely|[lL]oneliness", d$title), 1, 0)
freq(d$ezh_tit)     # 144 keer eenzaamheid in titel
d <- d %>% relocate(authors, year, title, ezh_tit, journal, abstract)
xtabs(~ inc + ezh_tit, data = d)        # enige samenhang
corr.test(d[c("inc", "ezh_tit")])
M <- glm(inc ~ ezh_tit, data = d, family = "binomial")
summary(M)       # just sig ( p ~ .04)


# ------------ eenzaamheid in abstract: count ----------------

d$ezh_abs <- str_count(d$abstract, "[lL]onely|[lL]oneliness")
freq(d$ezh_abs)     # heel vaak 1 keer
d <- d %>% relocate(authors, year, title, ezh_tit, journal, abstract, ezh_abs)
corr.test(d[c("inc", "ezh_abs")])
M <- glm(inc ~ ezh_abs, data = d,  family = "binomial")
summary(M)       #  sig ( p ~ .01)
head(M$fitted.values)


# ------------ armoede oid in titel --------------------------

str <- paste0("income|financial|poverty|poor|socioeconomic|socio-economic|",
                "wealth[a-z]*|economic depriv[a-z]*|economic hardship|",
                "material resources")
d$arm_tit <- ifelse(grepl(str, d$title, ignore.case = T), 1, 0)
freq(d$arm_tit)     # slechts 29 keer in titel; bijv. ook "poor health"
d <- d %>% relocate(authors, year, title, arm_tit, journal, abstract)
xtabs(~ inc + arm_tit, data = d)
M <- glm(inc ~ arm_tit, data = d,  family = "binomial")
summary(M)  # just sig .04 (but small n)


# --------------- armoede in abstract: count ------------------

d$arm_abs <- str_count(d$abstract, str)
freq(d$arm_abs)     # veel nullen en enen
# ook hoofdletters toestaan (str-count heeft geen ignore.case)
str2 <- paste0("[Ii]ncome|[Ff]inancial|[Pp]overty|[Pp]oor|[Ss]ocioeconomic|",
               "[Ss]ocio-economic|[Ww]ealth[a-z]*|[Ee]conomic depriv[a-z]*|", 
               "[Ee]conomic hardship|[Mm]aterial resources")
d$arm_abs <- str_count(d$abstract, str2)
freq(d$arm_abs)
M <- glm(inc ~ arm_abs, data = d,  family = "binomial")
summary(M)  #  sig < .01


# ---------------- afstand tussen woorden ---------------------------

# willeurige abstract met zowel lonely als armoede
# set.seed(937)
# abstr <- sample(d$abstract[d$ezh_abs>=1 & d$arm_abs >= 1], 1)
# abstr

# maak functie die kijkt naar kortste afstand tussen eenzaamheid en armoede
# input: some text
# output: kortste afstand en (absolute) afstandsmatrix
min_dist <- function(abs) {
    abs_l <- strsplit(abs, " ")[[1]]
    lon <- grep("lonely|loneliness", abs_l, ignore.case = T)
    if(length(lon) == 0) {return(NA)}
    arm <- grep(str, abs_l)
    if(length(arm) == 0) {return(NA)}
    m <- matrix(nrow = length(lon), ncol = length(arm), 
                dimnames = list(c(lon), c(arm)))
    for (i in 1:nrow(m)) {
        for (j in 1:ncol(m)) {
            m[i,j]<- abs(as.numeric(rownames(m)[i]) - as.numeric(colnames(m)[j]))
        }
    }
    return(min(m))
}

# nieuwe variabele met korte afstand; uiteraard wel wat NA's
d$afst <- as.integer(NA)
for (i in 1:length(d$abstract)) {
    d$afst[i] <- min_dist(d$abstract[[i]])
}
freq(d$afst)    # serious outliers
d$afst[d$afst>10] <- 10
M <- glm(inc ~ afst, data = d,  family = "binomial")
summary(M)  #  geen effect? nog uitzoeken


# -------------------- VOORSPELLEN INLCUSIE -----------------------------------

# zowel ezh als armoede in titel
M <- glm(inc ~ ezh_tit*arm_tit, data = d,  
         family = "binomial")
summary(M)  # interactie totaal NS??
xtabs(~ ezh_tit + arm_tit, data = d)        # only 5 with both in title

# alles
M <- glm(inc ~ ezh_tit*arm_tit + arm_abs + ezh_abs + afst, data = d,  
         family = "binomial")
summary(M)  # 




# --------------------------- OUDERE DB ---------------------------------------

d <- bib2df("Armoede eenzaamheid bibtex 2024 01 24.txt", separate_names = T)
d <- as_tibble(d)
dim(d)      # 865 references
names(d) <- tolower(names(d))
names(d)
d <- d %>% relocate(author, year, title, journal, abstract)







