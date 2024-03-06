# scan sample web of science op "poor"

library(xlsx)
library(dplyr)
library(stringr)
library(summarytools)
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


dta <- read.xlsx("savedrecs.xls", sheetIndex = 1, header = T)
names(dta)


# ************* Hoe vaak komen zoektermen voor in abstract? ********************

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

# loneliness 
freq(str_count(dta$Abstract, "[lL]onely|[lL]oneliness"))   # 225 keer niet 

freq(str_count(dta$Abstract, "[Th]he poor"))   # 2 keer 
freq(str_count(dta$Abstract, "[Pp]overty"))    # 36 keer > 0
CrossTable(str_count(dta$Abstract, "[Th]he poor"), 
           str_count(dta$Abstract, "[Ii]ncome"), 
           prop.c = F, prop.t = F, chisq = F, prop.chisq = F)

CrossTable(str_count(dta$Abstract, "[lL]onely|[lL]oneliness"),
           str_count(dta$Abstract, "[Th]he poor"),
           prop.c = F, prop.t = F, chisq = F, prop.chisq = F)

dta$Abstract[grep("[Th]he poor", dta$Abstract)]
# 1: The poor prognosis
# 2: The poorer socioeconomic classes


# Poor individuals??
freq(str_count(dta$abstract, "[Pp]oor indiv*")) # komt 0 keer voor
freq(str_count(dta$abstract, "[Pp]oor people")) # komt 0 keer voor





