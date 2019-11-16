library("data.table")
library("stringdist")
library("stringi")
library("qlcMatrix")

setwd("~/repos/capelinker")

library("capelinker")
# devtools::load_all("~/repos/capelinker/", export_all = FALSE)

# tra <- read.csv('matched.csv')
# tra <- tra[1:608, ]

# only thing done in old roldata.r is
# ascii conversion, 
# \x... replacement
# 

# The 1826/8 data
opg = data.table::fread('~/dropbox/opgaafrol/fgvf15oct.csv', na.strings = '.')

# the links
tra = fread("/Users/auke/Dropbox/opgaafrol/matched.csv")
setnames(tra, 10:11, c("persid_1828", "persid_1826"))
tra = tra[!is.na(persid_1828) & !is.na(persid_1826), ]


# !validUTF8
opg[!validUTF8(lastnamemen), lastnamemen]
opg[, lastnamemen := gsub('\x86', 'U', opg$lastnamemen)]
opg[, lastnamemen := gsub('\x83', 'E', opg$lastnamemen)]
opg[, lastnamewomen := gsub('\x83', 'E', opg$lastnamewomen)]

if (length(tools::showNonASCII(opg$lastnamemen)) > 0 | 
    length(tools::showNonASCII(opg$firstnamemen)) > 0 | 
    length(tools::showNonASCII(opg$lastnamewomen)) > 0 | 
    length(tools::showNonASCII(opg$lastnamemen)) > 0){
        warning("NON ASCII in name field")
}

# analphabetics
opg[grep("[^A-Z .]", firstnamemen), .(firstnamemen, lastnamemen)]
opg[grep("[^A-Z .]", lastnamemen), .(firstnamemen, lastnamemen)]

opg[grep("[^A-Z .]", firstnamewomen), .(firstnamewomen, lastnamewomen)]
opg[grep("[^A-Z .]", lastnamewomen), .(firstnamewomen, lastnamewomen)]

# manual:
opg[lastnamemen == "ARREAR: SMIT", lastnamemen := "SMIT"]
opg[lastnamewomen == "NO NO. 1542", lastnamewomen := ""]
opg[lastnamewomen == "GEEN VROU MAAR IN SYFERKOLOM)", lastnamewomen := ""]
opg[lastnamewomen == "WOMAN INDICATED BUT NO NAMES)", lastnamewomen := ""]

opg[lastnamewomen == "VISAGIE(NO SURNAME) SARA MARGARETHA", firstnamewomen := "SARA MARGARETHA"]
opg[lastnamewomen == "SARA CATHARINA (GEEN VAN)", firstnamewomen := "SARA CATHARINA"]
opg[lastnamewomen == "SARA CATHARINA (GEEN VAN)", lastnamewomen := ""]
opg[lastnamewomen == "ANNA (NO SURNAME)", firstnamewomen := "ANNA"]
opg[lastnamewomen == "ANNA (NO SURNAME)", lastnamewomen := ""]
opg[lastnamewomen == "ANNA JACOBA (NO SURNAME)", firstnamewomen := "ANNA JACOBA"]
opg[lastnamewomen == "ANNA JACOBA (NO SURNAME)", lastnamewomen := ""]

# rest can be done with drop after analphabetics
opg[grep("[^A-Z .]", firstnamemen), firstnamemen := stringi::stri_replace_all_regex(firstnamemen, "[^A-Z .].*", "")]
opg[grep("[^A-Z .]", firstnamewomen), firstnamewomen := stringi::stri_replace_all_regex(firstnamewomen, "[^A-Z .].*", "")]
opg[grep("[^A-Z .]", lastnamemen), lastnamemen := stringi::stri_replace_all_regex(lastnamemen, "[^A-Z .].*", "")]
opg[grep("[^A-Z .]", lastnamewomen), lastnamewomen := stringi::stri_replace_all_regex(firstnamemen, "[^A-Z .].*", "")]
# different from original which still included the ampersand itself
# so then we do this
opg[grep("[^A-Z .]", firstnamemen), firstnamemen := stringi::stri_extract_first_regex(firstnamemen, ".*[^A-Z .]")]
opg[grep("[^A-Z .]", firstnamewomen), firstnamewomen := stringi::stri_extract_first_regex(firstnamewomen, ".*[^A-Z .]")]
opg[grep("[^A-Z .]", lastnamemen), lastnamemen := stringi::stri_extract_first_regex(lastnamemen, ".*[^A-Z .]")]
opg[grep("[^A-Z .]", lastnamewomen), lastnamewomen := stringi::stri_extract_first_regex(lastnamewomen, ".*[^A-Z .]")]
# apparently this does nothing

# this is not done in old opgaafrollen
# start/end white space
opg[, firstnamemen := stringi::stri_trim_both(firstnamemen)]
opg[, firstnamewomen := stringi::stri_trim_both(firstnamewomen)]
opg[, lastnamemen := stringi::stri_trim_both(lastnamemen)]
opg[, lastnamewomen := stringi::stri_trim_both(lastnamewomen)]

# drop or NA?
opg[(grepl("^ *$", firstnamemen) & grepl("^ *$", lastnamemen)), list(firstnamemen, lastnamemen, firstnamewomen, lastnamewomen)]
opg[(grepl("^ *$", firstnamemen) & grepl("^ *$", lastnamemen) & grepl("^ *$", firstnamewomen) & grepl("^ *$", lastnamewomen)), list(firstnamemen, lastnamemen, firstnamewomen, lastnamewomen)]

# drop of NA
opg[firstnamemen == "X", firstnamemen := ""]
opg[lastnamemen == "X", lastnamemen := ""]
opg[firstnamewomen == "X", firstnamewomen := ""]
opg[lastnamewomen == "X", lastnamewomen := ""]
opg[grepl("^ *$", firstnamemen), list(firstnamemen, lastnamemen)]

# in old opgaafrollen: mfirst == "" -> initals = ""
# right now NA == ""
opg[, minitials := initials(firstnamemen, return_NA_on_empty = FALSE)]
opg[, winitials := initials(firstnamewomen, return_NA_on_empty = FALSE)]

opg[, wifepresent := !(firstnamewomen == '' & lastnamewomen == '')] # because F & T = F
opg[, spousenamedist := stringdist::stringdist(lastnamemen, lastnamewomen, method='jw', p=0.1)]
opg[, wineproducer := as.numeric(vines) > 0 & !is.na(vines)]
opg[, districtall := ifelse(districtdum=='.', -1, as.numeric(districtdum))]

opg[, mfullname := paste(lastnamemen, firstnamemen)]

opg[firstnamemen != "", 
    mfirst_uniqueness := rowMeans(
        stringdist::stringdistmatrix(firstnamemen, firstnamemen, method = 'jw'),
        na.rm = TRUE), 
    by = year]
opg[firstnamemen != "", 
    mfirst_cos_uniqueness := 1 - rowMeans(qlcMatrix::sim.strings(firstnamemen), na.rm = TRUE), 
    by = year]

opg[, lastnamemen_unif := uniformise_string(lastnamemen, quiet = TRUE)]
opg[, namefreq := .N, by = lastnamemen_unif]

# alternative; maybe w/o by = year?
opg[lastnamemen != "", 
    mlast_uniqueness := 1 - rowMeans(qlcMatrix::sim.strings(lastnamemen)), 
    by = year]

# opg[, lastnamemen_uniqueness_alt := capelinker::uniformise_string_alt(lastnamemen)]

opg[, c("prefix_men", "lastnamemen_wo_prefix") := capelinker::split_prefixes(lastnamemen)]
opg[, c("prefix_women", "lastnamewomen_wo_prefix") := capelinker::split_prefixes(lastnamewomen)]

opg[!is.na(prefix_men), .(lastnamemen, prefix_men, lastnamemen_wo_prefix)]
opg[!is.na(prefix_women), .(lastnamewomen, prefix_women, lastnamewomen_wo_prefix)]

# young/old stuff clearly extracted from names
opg[grep("JUNIOR|JR|WD|WED|SON|SR", lastnamemen), unique(lastnamemen)]
opg[grep("JUNIOR|JR|WD|WED|SON", firstnamemen), unique(firstnamemen)]
# seemingly same in stellenbosch csv

opg[, persid := 1:.N]

out = opg[,
    list(persid = persid,
         nr = nr,
         districtall = districtall,
         mlast = lastnamemen,
         mfirst = firstnamemen,
         mlast_woprefix = lastnamemen_wo_prefix,
         mprefix = prefix_men,
         minitials = minitials,
         wlast = lastnamewomen,
         wfirst = firstnamewomen,
         winitials = winitials,
         wlast_woprefix = lastnamewomen_wo_prefix,
         wprefix = prefix_women,
         spousenamedist = spousenamedist,
         year = year,
         wineproducer = wineproducer,
         old = old,
         young = young,
         wifepresent = wifepresent,
         namefreq = namefreq,
         mlast_uniqueness,
         mfirst_uniqueness,
         mfirst_cos_uniqueness,
         settlerchildren = as.numeric(settlerchildren),
         settlermen = as.numeric(settlermen),
         settlerwomen = as.numeric(settlerwomen),
         slaves = as.numeric(slaves),
         horses = as.numeric(horses),
         cattle = as.numeric(cattle),
         sheep = as.numeric(sheep))]

tra[, linkid := .I]
tra = melt(tra[, .(persid_1826, persid_1828, linkid)], 
    id.var = "linkid", variable.name = "year", value.name = "persid")
tra[, year := as.numeric(gsub("persid_", "", year))]
out = tra[out, on = c("persid", "year")]
rein = copy(out)

saveRDS(rein, "data_raw/opgaafrollen.rds.gz")
save(rein, file = "data/rein.rda", version = 2)
