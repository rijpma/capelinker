rm(list = ls())

library("stringi")
library("readxl")
library("qlcMatrix")
library("data.table")

# library("remotes")
# remotes::install_github("rijpma/capelinker", dependencies = FALSE)
library("capelinker")



cape = readxl::read_xlsx(
    path = "~/repos/capelinker/data_raw/Clean_CapeOpgaaf.xlsx",
    sheet = "Merged")
setDT(cape)

cape[, mlast := lastnames_men]
cape[, mfirst := paste(
    ifelse(is.na(firstnames_men), "", firstnames_men), 
    ifelse(is.na(secondnames_men), "", secondnames_men))]
cape[, wlast := lastnames_women]
cape[, wfirst := paste(
    ifelse(is.na(firstnames_women), "", firstnames_women), 
    ifelse(is.na(secondnames_women), "", secondnames_women))]
cape[, settlerchildren := settlersons + settlerdaughters]

# issues
# 1800 has very few voorvoegsels, but they just actually dno't appear to be in it
# pasting first name from first and second names gives you "NA NA" or " "
# about 50% of women's names are missing
# harmonise ""/NA
# lot of mlast missing, but these do have wlast -- check whether this happend in the others (I don't think it did)
# some length 1 strings, mostly initials rather than names
# there's a decent number of firstnames in the surname field

# strip ampersand too because is done in previous versoins
cape[, mlast := stri_replace_all_regex(mlast, "[(/?&].*", "")]
cape[mlast == "T'GOUVERNEMENT", mlast := NA]
cape[mlast == "'T GOUVERNEMENT", mlast := NA]
cape[mlast == "'T KAAPSCH DISTRICT", mlast := NA]
cape[, mlast := stri_trans_general(mlast, "latin-ascii")]

cape[, mfirst := stri_replace_all_regex(mfirst, "[(].*", "")]

cape[, wlast := stri_replace_all_regex(wlast, "[(?].*", "")]
cape[, wlast := stri_trans_general(wlast, "latin-ascii")]

cape[, wfirst := stri_replace_all_regex(wfirst, "[(-].*", "")]

# maybe also strip those "'"?
# cape[, lapply(.SD, function(x) mean(stri_startswith_fixed(x, " "))), .SDcols = c("mlast", "mfirst", "wlast", "wfirst")]
# cape[, lapply(.SD, function(x) mean(stri_endswith_fixed(x, " "))), .SDcols = c("mlast", "mfirst", "wlast", "wfirst")]

cape[, mlast := stri_trim_both(mlast)]
cape[, wlast := stri_trim_both(wlast)]
cape[, mfirst := stri_trim_both(mfirst)]
cape[, wfirst := stri_trim_both(wfirst)]

cape[mfirst == "", mfirst := NA]
cape[wfirst == "", wfirst := NA]

cape[, minitials := capelinker::initials(mfirst)]
cape[, winitials := capelinker::initials(wfirst)]


cape[, c("mprefix", "mlast_woprefix") := capelinker::split_prefixes(mlast)]
cape[, c("wprefix", "wlast_woprefix") := capelinker::split_prefixes(wlast)]
# cape[grep(" ", mlast_woprefix), unique(mlast_woprefix)]
# cape[grep(" ", wlast_woprefix), unique(wlast_woprefix)]
# add MR MRS MISS etc?

cape[wlast == "F", wlast := NA] # one of one-letter surname
# the others are initials in the name field

cape[, persid := .I]
cape[, spousenamedist := stringdist::stringdist(mlast, wlast, method='jw', p=0.1)]
cape[wfirst == "", wfirst := NA]
cape[, wifepresent := !(is.na(wfirst) & is.na(wlast))] # because F & T = F

cape[, mlast_unif := uniformise_string(mlast, quiet = TRUE)]
cape[, namefreq := .N, by = mlast_unif]

cape[!is.na(mfirst), 
    mfirst_uniqueness := rowMeans(
        stringdist::stringdistmatrix(mfirst, mfirst, method = 'jw'),
        na.rm = TRUE), 
    by = year]
# not sure why I don't use the other measures here
cape[!is.na(mfirst), 
    mfirst_cos_uniqueness := 1 - rowMeans(qlcMatrix::sim.strings(mfirst), na.rm = TRUE), 
    by = year]
cape[!is.na(mlast), 
    mlast_uniqueness := 1 - rowMeans(qlcMatrix::sim.strings(mlast)), 
    by = year]


names(pretrained_models)
setdiff(gsub("dist|sdx|_from|_to|_osa", "", pretrained_models$m_boost_stel_rein$variables),
    names(cape))

cape[, settlerchildren := as.numeric(settlerchildren)]
cape[, settlermen := as.numeric(settlermen)]
cape[, settlerwomen := as.numeric(settlerwomen)]
cape[, districtall := NA]

preflight(cape, modstring = "m_boost_stel_rein")

save(cape, file = "~/repos/capelinker/data/cape.rda", version = 2)

# so now what?
# compare with GR and ST
# check where those were made because they're not in the main repo for sure
# make candidates?
# link
data(cape)
lbls = label(
    dat_from = cape[year == 1825 & !is.na(mlast)],
    dat_to = cape[year == 1824 & !is.na(mlast)],
    persid_from = "persid",
    persid_to = "persid",
    blockvariable = "mlast",
    N = 500,
    blocktype = "bigram distance"
)
