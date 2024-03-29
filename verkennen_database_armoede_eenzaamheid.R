# verkennen database armoede eenzaamheid
# en maken model dat inclusie voorspelt

# NOTES
# - Vaak onjuiste hits op poor vanwege "poor health", "poor outcomes", etc

library(bib2df)

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
setwd("./Onderzoeksprojecten/Armoede en eenzaamheid VWS/voorspellen inclusie")


# ******** TEMP: direct importeren ************************

if(file.exists("C:/Users/ejvan")) {
    setwd("C:/Users/ejvan/OneDrive - Vrije Universiteit Amsterdam")
} else if(file.exists("C:/Users/ein900")) {
    setwd("C:/Users/ein900/OneDrive - Vrije Universiteit Amsterdam")
} else {
    warning("Geen van beide mappen gevonden")
}
setwd("./Onderzoeksprojecten/Armoede en eenzaamheid VWS")

# onderstaande geeft error maar importeert wel (some entries dropped)
# eerst RIS file in Endnote openen en dan exporteren naar Bibtex
tmp2 <- bib2df("C:/temp/test endnote.txt")
names(tmp)
freq(tmp$database)

freq(substr(tmp$url, 1, 30))
tmp$url2 <- substr(tmp$url, 1, 25)
freq(tmp$url2)

tmp$database2 <- substr(tmp$database, 1, 8)
freq(tmp$database2)

table(tmp$url2, tmp$database2)   # Web of Sc heeft scopus.com link?!



# **********************************************************


tmp1 <- read.csv("customizations_log 2024 02 27.csv")
#freq(tmp1$value)    # 1 = included; 2 = excluded?? (en 0 dan?)
tmp2 <- read.csv("articles 2024 02 27.csv")

# id universeel maken en mergen
tmp2$key <- substr(tmp2$key, 8, nchar(tmp2$key))
tmp2$article_id <- as.integer(tmp2$key)
tmp2 <- tmp2 %>% select(-key)
d <- left_join(tmp1, tmp2, by = "article_id")    
rm(tmp1, tmp2)


#' ## ******************** Ontdubbelen ed ********************

# names(d)
# length(unique(d$title))         # 1001 unieke titels op 1182 cases
# freq(d$value)
# freq(d$user_id)                 # Bianca 141; Chantal 527

# dubbele titels bij Chantal? 
length(unique(d$title[d$user_id == 489417])) # 998 uniek 

# in-/ exclusie - voor nu zonder 0, alleen Chantal, ontdubbeld
#freq(d$value)
d <- d %>% filter(value !=0 & d$user_id == 489417)
# which(duplicated(d$article_id))     # 16 dubbel
d <- d %>% filter(!duplicated(d$article_id))

d$inc <- ifelse(d$value==1, 1, 0)
#freq(d$inc)                     # n = 952, waarvan 136 included

# aanmaken search strings
str <- paste0("[Ii]ncome|[Ff]inancial|[Pp]overty|[Pp]oor|[Ss]ocioeconomic|",
              "[Ss]ocio-economic|[Ww]ealth[a-z]*|[Ee]conomic depriv[a-z]*|", 
              "[Ee]conomic hardship|[Mm]aterial resources")
# str2 werkt het beste bij het bestrijden van false negatives
str2 <- paste0("[Ii]ncome|[Ff]inancial|[Pp]overty|[Ss]ocioeconomic|",
              "[Ss]ocio-economic|[Ww]ealth[a-z]*|[Ee]conomic depriv[a-z]*|", 
              "[Ee]conomic hardship|[Mm]aterial resources")
str3 <- paste0("[Ii]ncome|[Ff]inancial|[Pp]overty|[Ss]ocioeconomic|",
               "[Ss]ocio-economic|[Ww]ealth[a-z]*|[Dd]epriv[a-z]*|", 
               "[Ee]conomic hardship|[Mm]aterial resources")


# ****************** Database uit URL halen? ***************************

freq(substr(d$url, 1, 30))
d$url2 <- substr(d$url, 1, 25)
freq(d$url2)

# Included per database
CrossTable(d$url2, d$inc, prop.c = F, prop.t = F, chisq = F, prop.chisq = F)





# ****************** Uitsluiten papers zonder abstract ****************

# artikelen zonder abstract zijn problematisch
# hoeveel zijn dat er?
which(train$abstract=="")      # 25 publicaties
d <- d %>% filter(abstract!="")   # artikelen eruit



#' ## ***** Hoe vaak komen termen voor en samenhang inclusie  **********

# functie die aantal keer woord (wrd) telt in vector met abstracts (abs)
freq_wrd <- function(abs_vec, wrd) {
    rtrn <- vector(mode = "integer", length = length(abs_vec))
    for (i in 1:length(abs_vec)) {
        #cat(abs_vec[[i]], "\n")
        abs_l <- strsplit(abs_vec[[i]], " ")[[1]]
        wrd_lst <- grep(wrd, abs_l, ignore.case = T)
        rtrn[i] <- length(wrd_lst)
    }
    return(rtrn)
}

# inkomen - voorspelt inclusie enigszins
d$frq_inc <- freq_wrd(d$abstract, "[Ii]ncome")
d$frq_inc <- ifelse(d$frq_inc > 2, 2, d$frq_inc)
CrossTable(d$frq_inc, d$inc, prop.c = F, prop.t = F, chisq = F, prop.chisq = F)

# financial - idem
d$frq_fin <- freq_wrd(d$abstract, "[Ff]inancial")
d$frq_fin <- ifelse(d$frq_fin > 2, 2, d$frq_fin)
CrossTable(d$frq_fin, d$inc, prop.c = F, prop.t = F, chisq = F, prop.chisq = F)

# Poverty  - idem
d$frq_pov <- freq_wrd(d$abstract, "[Pp]overty")
d$frq_pov <- ifelse(d$frq_pov > 2, 2, d$frq_pov)
CrossTable(d$frq_pov, d$inc, prop.c = F, prop.t = F, chisq = F, prop.chisq = F)

# Poor     doet het heel slecht
d$frq_por <- freq_wrd(d$abstract, "[Pp]oor")
d$frq_por <- ifelse(d$frq_por > 2, 2, d$frq_por)
CrossTable(d$frq_por, d$inc, prop.c = F, prop.t = F, chisq = F, prop.chisq = F)

# The poor??
d$frq_thp <- freq_wrd(d$abstract, "[Th]he poor")
freq(d$frq_thp)        # komt 0 keer voor
# Poor individuals??
d$frq_pin <- freq_wrd(d$abstract, "[Pp]oor indiv*")
freq(d$frq_pin)        # komt 0 keer voor
# Poor persons??
#d$frq_pps <- freq_w
# Poor people??
d$frq_ppl <- freq_wrd(d$abstract, "[Pp]oor people")
freq(d$frq_ppl)


# Socioeconomic
d$frq_soe <- freq_wrd(d$abstract, "[Ss]ocioeconomic")
d$frq_soe <- ifelse(d$frq_soe > 2, 2, d$frq_soe)
CrossTable(d$frq_soe, d$inc, prop.c = F, prop.t = F, chisq = F, prop.chisq = F)

# Socio-economic
d$frq_soe2 <- freq_wrd(d$abstract, "[Ss]ocio-economic")
d$frq_soe2 <- ifelse(d$frq_soe2 > 2, 2, d$frq_soe)
CrossTable(d$frq_soe2, d$inc, prop.c = F, prop.t = F, chisq = F, prop.chisq = F)

# [Ww]ealth[a-z]*  komt bijna niet voor
d$frq_wth <- freq_wrd(d$abstract, "[Ww]ealth[a-z]*")
CrossTable(d$frq_wth, d$inc, prop.c = F, prop.t = F, chisq = F, prop.chisq = F)

# [Ee]conomic depriv[a-z]* - komt helemaal niet voor
d$frq_dpr <- freq_wrd(d$abstract, "[Ee]conomic depriv[a-z]*")
CrossTable(d$frq_dpr, d$inc, prop.c = F, prop.t = F, chisq = F, prop.chisq = F)

# [Dd]epriv[a-z]* 
d$frq_dpr2 <- freq_wrd(d$abstract, "[Dd]epriv[a-z]*")
CrossTable(d$frq_dpr2, d$inc, prop.c = F, prop.t = F, chisq = F, prop.chisq = F)

# [Ee]conomic hardship - komt helemaal niet voor
d$frq_hsh <- freq_wrd(d$abstract, "[Ee]conomic hardship")
CrossTable(d$frq_hsh, d$inc, prop.c = F, prop.t = F, chisq = F, prop.chisq = F)

# hardship - 
d$frq_hsh2 <- freq_wrd(d$abstract, "[Hh]ardship")
CrossTable(d$frq_hsh2, d$inc, prop.c = F, prop.t = F, chisq = F, prop.chisq = F)

# [Mm]aterial resources - komt helemaal niet voor
d$frq_mrs <- freq_wrd(d$abstract, "[Mm]aterial resources")
CrossTable(d$frq_mrs, d$inc, prop.c = F, prop.t = F, chisq = F, prop.chisq = F)

# resources - komt helemaal niet voor
d$frq_res <- freq_wrd(d$abstract, "[Rr]esources")
CrossTable(d$frq_res, d$inc, prop.c = F, prop.t = F, chisq = F, prop.chisq = F)




#' ## ************** Voorspellers maken *********************************

#' ## Eenzaamheid in titel: ja of nee 

d$ezh_tit <- ifelse(grepl("[lL]onely|[lL]oneliness", d$title), 1, 0)
# freq(d$ezh_tit)     # 144 keer eenzaamheid in titel
CrossTable(d$ezh_tit, d$inc, prop.c = F, prop.t = F, chisq = F, prop.chisq = F)
# corr.test(d[c("inc", "ezh_tit")])
# M <- glm(inc ~ ezh_tit, data = d, family = "binomial")
# summary(M)       # just sig ( p ~ .04)


#' ## Eenzaamheid in abstract: count

d$ezh_abs <- str_count(d$abstract, "[lL]onely|[lL]oneliness")
# freq(d$ezh_abs)     # heel vaak 1 keer
CrossTable(d$ezh_abs, d$inc, prop.c = F, prop.t = F, chisq = F, prop.chisq = F)
# hercoderen ivm uitschieters?
d$ezh_abs <- ifelse(d$ezh_abs > 5, 5, d$ezh_abs)
# corr.test(d[c("inc", "ezh_abs")])
# M <- glm(inc ~ ezh_abs, data = d,  family = "binomial")
# summary(M)       #  sig ( p ~ .01)
# head(M$fitted.values)


#' ## Armoede oid in titel

d$arm_tit <- ifelse(grepl(str, d$title, ignore.case = T), 1, 0)
#freq(d$arm_tit)     # slechts 29 keer in titel; bijv. ook "poor health"
CrossTable(d$arm_tit, d$inc, prop.c = F, prop.t = F, chisq = F, prop.chisq = F)
# M <- glm(inc ~ arm_tit, data = d,  family = "binomial")
# summary(M)  # just sig .04 (but small n)
d$arm_tit2 <- ifelse(grepl(str2, d$title, ignore.case = T), 1, 0)
CrossTable(d$arm_tit2, d$inc, prop.c = F, prop.t = F, chisq = F, prop.chisq = F)
d$arm_tit3 <- ifelse(grepl(str3, d$title, ignore.case = T), 1, 0)
CrossTable(d$arm_tit3, d$inc, prop.c = F, prop.t = F, chisq = F, prop.chisq = F)

#' ## Zowel armoede als eenzaamheid in de titel

CrossTable(d$arm_tit2, d$ezh_tit, prop.c = F, prop.t = F, chisq = F, prop.chisq = F)
d$titel <- case_when(d$ezh_tit == 0 & d$arm_tit2 == 0 ~ 0,
                     d$ezh_tit == 1 & d$arm_tit2 == 0 ~ 1,
                     d$ezh_tit == 0 & d$arm_tit2 == 1 ~ 2,
                     d$ezh_tit == 1 & d$arm_tit2 == 1 ~ 3)
CrossTable(d$titel, d$inc, prop.c = F, prop.t = F, chisq = F, 
           prop.chisq = F)    # combi komt nauwelijks voor


#' ## Armoede in abstract: count

d$arm_abs <- str_count(d$abstract, str)
d$arm_abs <- ifelse(d$arm_abs > 5, 5, d$arm_abs)
CrossTable(d$arm_abs, d$inc, prop.c = F, prop.t = F, chisq = F, prop.chisq = F)
# M <- glm(inc ~ arm_abs, data = d,  family = "binomial")
# summary(M)  #  sig < .01
d$arm_abs2 <- str_count(d$abstract, str2)
d$arm_abs2 <- ifelse(d$arm_abs2 > 5, 5, d$arm_abs2)
CrossTable(d$arm_abs2, d$inc, prop.c = F, prop.t = F, chisq = F, prop.chisq = F)

# search string 1 of 2 scheelt 137 keer 0 hits in abstract. ZOu kwart schelen? 


d$arm_abs3 <- str_count(d$abstract, str3)
d$arm_abs3 <- ifelse(d$arm_abs3 > 5, 5, d$arm_abs3)
CrossTable(d$arm_abs3, d$inc, prop.c = F, prop.t = F, chisq = F, prop.chisq = F)


#' ## ************* Afstand tussen woorden ****************

# maak functie die kijkt naar kortste afstand tussen eenzaamheid en armoede
# input: some text
# output: kortste afstand en (absolute afstandsmatrix niet return)
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
# nieuwe variabele met korte afstand; gek genoeg wel wat NA's
d$afst <- as.integer(NA)
for (i in 1:length(d$abstract)) {
    d$afst[i] <- min_dist(d$abstract[[i]])
}
#freq(d$afst)    # serious outliers
d$afst[d$afst>20] <- 20
d$afst[d$afst==0] <- 1
plot(d$afst, d$inc)         # totaal geen relatie
CrossTable(d$afst, d$inc, prop.c = F, prop.t = F, chisq = F, prop.chisq = F)


# Zelfde verhaal maar nu obv str2
min_dist <- function(abs) {
    abs_l <- strsplit(abs, " ")[[1]]
    lon <- grep("lonely|loneliness", abs_l, ignore.case = T)
    if(length(lon) == 0) {return(NA)}
    arm <- grep(str2, abs_l)
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
# nieuwe variabele met korte afstand; gek genoeg wel wat NA's
d$afst2 <- as.integer(NA)
for (i in 1:length(d$abstract)) {
    d$afst2[i] <- min_dist(d$abstract[[i]])
}
#freq(d$afst)    # serious outliers
d$afst2[d$afst2>5] <- 5
d$afst2[d$afst2==0] <- 1
CrossTable(d$afst2, d$inc, prop.c = F, prop.t = F, chisq = F, prop.chisq = F)
d$afst2[is.na(d$afst2)] <- 10               # afstand NA wordt 10
CrossTable(d$afst2, d$inc, prop.c = F, prop.t = F, chisq = F, prop.chisq = F)


# Zelfde verhaal maar nu obv str3
min_dist <- function(abs) {
    abs_l <- strsplit(abs, " ")[[1]]
    lon <- grep("lonely|loneliness", abs_l, ignore.case = T)
    if(length(lon) == 0) {return(NA)}
    arm <- grep(str3, abs_l)
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
# nieuwe variabele met korte afstand; gek genoeg wel wat NA's
d$afst3 <- as.integer(NA)
for (i in 1:length(d$abstract)) {
    d$afst3[i] <- min_dist(d$abstract[[i]])
}
#freq(d$afst)    # serious outliers
d$afst3[d$afst3>5] <- 5
d$afst3[d$afst3==0] <- 1
CrossTable(d$afst3, d$inc, prop.c = F, prop.t = F, chisq = F, prop.chisq = F)
d$afst3[is.na(d$afst3)] <- 10               # afstand NA wordt 10
CrossTable(d$afst3, d$inc, prop.c = F, prop.t = F, chisq = F, prop.chisq = F)



#' ## ****************** Journal **************************

# freq(d$journal)
# unique(d$journal)

# social or sociological in journal? zegt wel iets
d$soc_jrnl <- grepl("soci", d$journal, ignore.case = T)
CrossTable(d$soc_jrnl, d$inc, prop.c = F, prop.t = F, chisq = F, prop.chisq = F)

# M <- glm(inc ~ soc_jrnl, data = d,  family = "binomial")
# summary(M) 




#' ## ************* Voorspellen inclusie *********************

# training data maken 
# 70-30? (je houdt wel weinig included artikelen over in de test data)
set.seed(0103)
tmp <- sample(1:nrow(d), 0.7*nrow(d), F)
train <- d[tmp,]
test <- d[setdiff(c(1:nrow(d)), tmp),]

# included?
freq(train$inc)     # 97
freq(test$inc)      # 39


#' ## ********* Model Training 1: met str2 **********************

MT1 <- glm(inc ~ as.factor(titel) + arm_abs2 + ezh_abs 
                + soc_jrnl 
                + as.factor(afst2), data = train, family = "binomial")
summary(MT1) 
# pseudo R^2
ll.null <- MT1$null.deviance / -2
ll.proposed <- MT1$deviance / -2
(ll.null - ll.proposed) / ll.null       # veel beter: .209

#' ## Plot data
pred_data <- data.frame(prob_of_inc = MT1$fitted.values, included = train$inc)
pred_data <- pred_data[order(pred_data$prob_of_inc, decreasing = F),]
pred_data$rank <- 1:nrow(pred_data)
ggplot(data = pred_data, aes(x = rank, y = prob_of_inc)) +
    scale_colour_manual(values = c("cyan", "black")) +
    geom_point(aes(color = as.factor(included)), alpha = 1, shape = 4, stroke = 2) +
    xlab("Index") +
    ylab("Predicted prob of inclusion")

#' ## Accuracy
# cut-off 0.3
pred_data$pred_incl_3 <- pred_data$prob_of_inc >= 0.3
CrossTable(pred_data$pred_incl_3, pred_data$included, prop.c = F, prop.t = F, 
           chisq = F, prop.chisq = F, prop.r = T)

# cut-off 0.10
pred_data$pred_incl_10 <- pred_data$prob_of_inc >= 0.10
CrossTable(pred_data$pred_incl_10, pred_data$included, prop.c = F, prop.t = F, 
           chisq = F, prop.chisq = F, prop.r = T)

# cut-off 0.05
pred_data$pred_incl_05 <- pred_data$prob_of_inc >= 0.05
CrossTable(pred_data$pred_incl_05, pred_data$included, prop.c = F, prop.t = F, 
           chisq = F, prop.chisq = F, prop.r = T)

# cut-off 0.02
pred_data$pred_incl_02 <- pred_data$prob_of_inc >= 0.02
CrossTable(pred_data$pred_incl_02, pred_data$included, prop.c = F, prop.t = F, 
           chisq = F, prop.chisq = F, prop.r = T)

# cut-off 0.015
pred_data$pred_incl_015 <- pred_data$prob_of_inc >= 0.015
CrossTable(pred_data$pred_incl_01, pred_data$included, prop.c = F, prop.t = F, 
           chisq = F, prop.chisq = F, prop.r = T)



# ******** OP TEST DATA ********

test_pred <- predict(MT1, test, type = "response")

#' ## Plot data
pred_data <- data.frame(prob_of_inc = test_pred, included = test$inc)
pred_data <- pred_data[order(pred_data$prob_of_inc, decreasing = F),]
pred_data$rank <- 1:nrow(pred_data)
ggplot(data = pred_data, aes(x = rank, y = prob_of_inc)) +
    scale_colour_manual(values = c("cyan", "black")) +
    geom_point(aes(color = as.factor(included)), alpha = 1, shape = 4, stroke = 2) +
    xlab("Index") +
    ylab("Predicted prob of inclusion")

# cut-off 0.20
pred_data$pred_incl_20 <- pred_data$prob_of_inc >= 0.2
CrossTable(pred_data$pred_incl_20, pred_data$included, prop.c = F, prop.t = F, 
           chisq = F, prop.chisq = F, prop.r = T)

# cut-off 0.10 - 5.2% false neg, 49% excluded
pred_data$pred_incl_10 <- pred_data$prob_of_inc >= 0.10
CrossTable(pred_data$pred_incl_10, pred_data$included, prop.c = F, prop.t = F, 
           chisq = F, prop.chisq = F, prop.r = T)

# cut-off 0.05 - 4.1% false neg, 35% excluded
pred_data$pred_incl_05 <- pred_data$prob_of_inc >= 0.05
CrossTable(pred_data$pred_incl_05, pred_data$included, prop.c = F, prop.t = F, 
           chisq = F, prop.chisq = F, prop.r = T)


# ***** Exporteer titels + abstracts van included onder .05 pred prob ****

temp <- data.frame(test$title[test_pred <= .05 & test$inc == 1], 
                   test$abstract[test_pred <= .05 & test$inc == 1])

for (i in 1:nrow(temp)) {
    write("Titel:", file = "text_out.txt", append = T)
    write(as.character(temp[i,1]), file = "text_out.txt", append = T)
    write("Abstract:", file = "text_out.txt", append = T)
    write(as.character(temp[i,2]), file = "text_out.txt", append = T)
    write("\n", file = "text_out.txt", append = T)
}


#' ## ********* Model Training 2: met str3 **********************

MT2 <- glm(inc ~ as.factor(titel) + arm_abs3 + ezh_abs 
           + soc_jrnl 
           + as.factor(afst3), data = train, family = "binomial")
summary(MT2) 
# pseudo R^2
ll.null <- MT2$null.deviance / -2
ll.proposed <- MT2$deviance / -2
(ll.null - ll.proposed) / ll.null       # veel beter: .208

#' ## Plot data
pred_data <- data.frame(prob_of_inc = MT2$fitted.values, included = train$inc)
pred_data <- pred_data[order(pred_data$prob_of_inc, decreasing = F),]
pred_data$rank <- 1:nrow(pred_data)
ggplot(data = pred_data, aes(x = rank, y = prob_of_inc)) +
    scale_colour_manual(values = c("cyan", "black")) +
    geom_point(aes(color = as.factor(included)), alpha = 1, shape = 4, stroke = 2) +
    xlab("Index") +
    ylab("Predicted prob of inclusion")

# ******** OP TEST DATA ********

test_pred <- predict(MT2, test, type = "response")

#' ## Plot data
pred_data <- data.frame(prob_of_inc = test_pred, included = test$inc)
pred_data <- pred_data[order(pred_data$prob_of_inc, decreasing = F),]
pred_data$rank <- 1:nrow(pred_data)
ggplot(data = pred_data, aes(x = rank, y = prob_of_inc)) +
    scale_colour_manual(values = c("cyan", "black")) +
    geom_point(aes(color = as.factor(included)), alpha = 1, shape = 4, stroke = 2) +
    xlab("Index") +
    ylab("Predicted prob of inclusion")

# cut-off 0.10 - 5.8% false neg, 56% excluded
pred_data$pred_incl_10 <- pred_data$prob_of_inc >= 0.10
CrossTable(pred_data$pred_incl_10, pred_data$included, prop.c = F, prop.t = F, 
           chisq = F, prop.chisq = F, prop.r = T)

# cut-off 0.05 - 5.7% false neg, 32% excluded
pred_data$pred_incl_05 <- pred_data$prob_of_inc >= 0.05
CrossTable(pred_data$pred_incl_05, pred_data$included, prop.c = F, prop.t = F, 
           chisq = F, prop.chisq = F, prop.r = T)







# ********************* RANDOM FORESTS ******************************

library(caret)

MRF1 <- train(y = train$inc, 
              x = train[c("ezh_tit", "arm_tit2", "arm_abs2", "ezh_abs",
                        "soc_jrnl", "afst2")],
              method = "rf")
test_pred <- predict(MRF1, 
                     newdata = test[c("ezh_tit", "arm_tit2", "arm_abs2", 
                                       "ezh_abs", "soc_jrnl", "afst2")])

#' ## Plot data
pred_data <- data.frame(prob_of_inc = test_pred, included = test$inc)
pred_data <- pred_data[order(pred_data$prob_of_inc, decreasing = F),]
pred_data$rank <- 1:nrow(pred_data)
ggplot(data = pred_data, aes(x = rank, y = prob_of_inc)) +
    scale_colour_manual(values = c("cyan", "black")) +
    geom_point(aes(color = as.factor(included)), alpha = 1, shape = 4, stroke = 2) +
    xlab("Index") +
    ylab("Predicted prob of inclusion")

# cut-off 0.2 - 9.0% false neg, 78% excluded
pred_data$pred_incl_2 <- pred_data$prob_of_inc >= 0.2
CrossTable(pred_data$pred_incl_2, pred_data$included, prop.c = F, prop.t = F, 
           chisq = F, prop.chisq = F, prop.r = T)

# cut-off 0.1 - 7.9% false neg, 62% excluded
pred_data$pred_incl_1 <- pred_data$prob_of_inc >= 0.1
CrossTable(pred_data$pred_incl_1, pred_data$included, prop.c = F, prop.t = F, 
           chisq = F, prop.chisq = F, prop.r = T)

# cut-off 0.05 - 6.9% false neg, 30% excluded
pred_data$pred_incl_05 <- pred_data$prob_of_inc >= 0.05
CrossTable(pred_data$pred_incl_05, pred_data$included, prop.c = F, prop.t = F, 
           chisq = F, prop.chisq = F, prop.r = T)

    

# ********************* SUPPORT VECTORS MACHINES ***************************

x_train <- train[c("ezh_tit", "arm_tit2", "arm_abs2", "ezh_abs",
            "soc_jrnl", "afst2")]
x_test <- test[c("ezh_tit", "arm_tit2", "arm_abs2", "ezh_abs",
                  "soc_jrnl", "afst2")]

MSVM1 <- train(x = x_train, y = train$inc, 
               preProcess = c("center", "scale"), 
               method = "svmLinear3")
test_pred <- predict(MSVM1, newdata = x_test)

#' ## Plot data
pred_data <- data.frame(prob_of_inc = test_pred, included = test$inc)
pred_data <- pred_data[order(pred_data$prob_of_inc, decreasing = F),]
pred_data$rank <- 1:nrow(pred_data)
ggplot(data = pred_data, aes(x = rank, y = prob_of_inc)) +
    scale_colour_manual(values = c("cyan", "black")) +
    geom_point(aes(color = as.factor(included)), alpha = 1, shape = 4, stroke = 2) +
    xlab("Index") +
    ylab("Predicted prob of inclusion")

# cut-off 0.2 - 7.0% false neg, 50% excluded
pred_data$pred_incl_2 <- pred_data$prob_of_inc >= 0.2
CrossTable(pred_data$pred_incl_2, pred_data$included, prop.c = F, prop.t = F, 
           chisq = F, prop.chisq = F, prop.r = T)

# cut-off 0.1 - 7.1% false neg, 30% excluded
pred_data$pred_incl_1 <- pred_data$prob_of_inc >= 0.1
CrossTable(pred_data$pred_incl_1, pred_data$included, prop.c = F, prop.t = F, 
           chisq = F, prop.chisq = F, prop.r = T)

# cut-off 0.05 - 4.7% false neg, 22% excluded
pred_data$pred_incl_05 <- pred_data$prob_of_inc >= 0.05
CrossTable(pred_data$pred_incl_05, pred_data$included, prop.c = F, prop.t = F, 
           chisq = F, prop.chisq = F, prop.r = T)





# ************************** OUDERE MODELLEN *************************


#' ## ********** Model 1: met str, zonder afstand **********************

M <- glm(inc ~ ezh_tit*arm_tit + arm_abs + ezh_abs + soc_jrnl, data = d,  
         family = "binomial")
summary(M)  # 
# pseudo R^2
ll.null <- M$null.deviance / -2
ll.proposed <- M$deviance / -2
(ll.null - ll.proposed) / ll.null     # .047

#' ## Plot data
pred_data <- data.frame(prob_of_inc = M$fitted.values, included = d$inc)
pred_data <- pred_data[order(pred_data$prob_of_inc, decreasing = F),]
pred_data$rank <- 1:nrow(pred_data)
ggplot(data = pred_data, aes(x = rank, y = prob_of_inc)) +
    scale_colour_manual(values = c("cyan", "black")) +
    geom_point(aes(color = as.factor(included)), alpha = 1, shape = 4, stroke = 2) +
    xlab("Index") +
    ylab("Predicted prob of inclusion")

#' ## Accuracy
# cut-off 0.5
pred_data$pred_incl_5 <- pred_data$prob_of_inc >= 0.5
# freq(pred_data$pred_incl_5)
CrossTable(pred_data$pred_incl_5, pred_data$included, prop.c = F, prop.t = F, 
           chisq = F, prop.chisq = F, prop.r = F)

# cut-off 0.3
pred_data$pred_incl_3 <- pred_data$prob_of_inc >= 0.3
# freq(pred_data$pred_incl_3)
CrossTable(pred_data$pred_incl_3, pred_data$included, prop.c = F, prop.t = F, 
           chisq = F, prop.chisq = F, prop.r = F)

# cut-off 0.15
pred_data$pred_incl_15 <- pred_data$prob_of_inc >= 0.15
# freq(pred_data$pred_incl_15)
CrossTable(pred_data$pred_incl_15, pred_data$included, prop.c = F, prop.t = F, 
           chisq = F, prop.chisq = F, prop.r = F)


#' ## ********** Model 2: met str2 **********************

M2 <- glm(inc ~ ezh_tit + arm_tit2 + arm_abs2 + ezh_abs + soc_jrnl + as.factor(afst2), 
          data = d, family = "binomial")
summary(M2) 
# pseudo R^2
ll.null <- M2$null.deviance / -2
ll.proposed <- M2$deviance / -2
(ll.null - ll.proposed) / ll.null       # veel beter: .142

#' ## Plot data
pred_data <- data.frame(prob_of_inc = M2$fitted.values, included = d$inc)
pred_data <- pred_data[order(pred_data$prob_of_inc, decreasing = F),]
pred_data$rank <- 1:nrow(pred_data)
ggplot(data = pred_data, aes(x = rank, y = prob_of_inc)) +
    scale_colour_manual(values = c("cyan", "black")) +
    geom_point(aes(color = as.factor(included)), alpha = 1, shape = 4, stroke = 2) +
    xlab("Index") +
    ylab("Predicted prob of inclusion")

#' ## Accuracy
# cut-off 0.5
pred_data$pred_incl_5 <- pred_data$prob_of_inc >= 0.5
# freq(pred_data$pred_incl_5)
CrossTable(pred_data$pred_incl_5, pred_data$included, prop.c = F, prop.t = F, 
           chisq = F, prop.chisq = F, prop.r = F)

# cut-off 0.3
pred_data$pred_incl_3 <- pred_data$prob_of_inc >= 0.3
# freq(pred_data$pred_incl_3)
CrossTable(pred_data$pred_incl_3, pred_data$included, prop.c = F, prop.t = F, 
           chisq = F, prop.chisq = F, prop.r = F)

# cut-off 0.10
pred_data$pred_incl_10 <- pred_data$prob_of_inc >= 0.10
CrossTable(pred_data$pred_incl_10, pred_data$included, prop.c = F, prop.t = F, 
           chisq = F, prop.chisq = F, prop.r = F)

# cut-off 0.05
pred_data$pred_incl_05 <- pred_data$prob_of_inc >= 0.05
CrossTable(pred_data$pred_incl_05, pred_data$included, prop.c = F, prop.t = F, 
           chisq = F, prop.chisq = F, prop.r = F)

# cut-off 0.07
pred_data$pred_incl_07 <- pred_data$prob_of_inc >= 0.07
CrossTable(pred_data$pred_incl_07, pred_data$included, prop.c = F, prop.t = F, 
           chisq = F, prop.chisq = F, prop.r = F)


#' ## ******* Model 3: met str2, en losse counts, zonder journal **************

M3 <- glm(inc ~ ezh_tit + arm_tit2 + arm_abs2 
                + ezh_abs 
                #+ soc_jrnl 
                + as.factor(afst2)
                + frq_res + frq_soe
                + frq_inc + frq_fin + frq_pov 
            , data = d, family = "binomial")
summary(M3) 
# pseudo R^2
ll.null <- M3$null.deviance / -2
ll.proposed <- M3$deviance / -2
(ll.null - ll.proposed) / ll.null       # .152

#' ## Plot data
pred_data <- data.frame(prob_of_inc = M3$fitted.values, included = d$inc)
pred_data <- pred_data[order(pred_data$prob_of_inc, decreasing = F),]
pred_data$rank <- 1:nrow(pred_data)
ggplot(data = pred_data, aes(x = rank, y = prob_of_inc)) +
    scale_colour_manual(values = c("cyan", "black")) +
    geom_point(aes(color = as.factor(included)), alpha = 1, shape = 4, stroke = 2) +
    xlab("Index") +
    ylab("Predicted prob of inclusion")

#' ## Accuracy
# cut-off 0.3
pred_data$pred_incl_3 <- pred_data$prob_of_inc >= 0.3
# freq(pred_data$pred_incl_3)
CrossTable(pred_data$pred_incl_3, pred_data$included, prop.c = F, prop.t = F, 
           chisq = F, prop.chisq = F, prop.r = F)

# cut-off 0.10
pred_data$pred_incl_10 <- pred_data$prob_of_inc >= 0.10
CrossTable(pred_data$pred_incl_10, pred_data$included, prop.c = F, prop.t = F, 
           chisq = F, prop.chisq = F, prop.r = F)

# cut-off 0.05
pred_data$pred_incl_05 <- pred_data$prob_of_inc >= 0.05
CrossTable(pred_data$pred_incl_05, pred_data$included, prop.c = F, prop.t = F, 
           chisq = F, prop.chisq = F, prop.r = F)



#' ## ********** Model 4: met str3, en losse counts **********************

M4 <- glm(inc ~ ezh_tit + arm_tit3 + arm_abs3 + ezh_abs + soc_jrnl + 
              as.factor(afst3)
              # + frq_res + frq_inc + frq_fin + frq_pov + frq_soe
          , data = d, family = "binomial")
summary(M4) 
# pseudo R^2
ll.null <- M4$null.deviance / -2
ll.proposed <- M4$deviance / -2
(ll.null - ll.proposed) / ll.null       # .135

#' ## Plot data
pred_data <- data.frame(prob_of_inc = M4$fitted.values, included = d$inc)
pred_data <- pred_data[order(pred_data$prob_of_inc, decreasing = F),]
pred_data$rank <- 1:nrow(pred_data)
ggplot(data = pred_data, aes(x = rank, y = prob_of_inc)) +
    scale_colour_manual(values = c("cyan", "black")) +
    geom_point(aes(color = as.factor(included)), alpha = 1, shape = 4, stroke = 2) +
    xlab("Index") +
    ylab("Predicted prob of inclusion")

#' ## Accuracy
# cut-off 0.3
pred_data$pred_incl_3 <- pred_data$prob_of_inc >= 0.3
# freq(pred_data$pred_incl_3)
CrossTable(pred_data$pred_incl_3, pred_data$included, prop.c = F, prop.t = F, 
           chisq = F, prop.chisq = F, prop.r = F)

# cut-off 0.10
pred_data$pred_incl_10 <- pred_data$prob_of_inc >= 0.10
CrossTable(pred_data$pred_incl_10, pred_data$included, prop.c = F, prop.t = F, 
           chisq = F, prop.chisq = F, prop.r = F)

# cut-off 0.05
pred_data$pred_incl_05 <- pred_data$prob_of_inc >= 0.05
CrossTable(pred_data$pred_incl_05, pred_data$included, prop.c = F, prop.t = F, 
           chisq = F, prop.chisq = F, prop.r = F)



# ****************** TO PDF **************************************
options(knitr.duplicate.label = "allow")
#rmarkdown::render("verkennen_database_armoede_eenzaamheid.R", "pdf_document")






