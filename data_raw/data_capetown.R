rm(list = ls())

library("stringi")
library("readxl")
library("qlcMatrix")
library("data.table")

# library("remotes")
# remotes::install_github("rijpma/capelinker", dependencies = FALSE)
library("capelinker")

cap = readxl::read_xlsx(
    path = "~/repos/capelinker/data_raw/Clean_CapeOpgaaf.xlsx",
    sheet = "Merged")
setDT(cap)

cap[, mlast := lastnames_men]
cap[, mfirst := paste(
    ifelse(is.na(firstnames_men), "", firstnames_men), 
    ifelse(is.na(secondnames_men), "", secondnames_men))]
cap[, wlast := lastnames_women]
cap[, wfirst := paste(
    ifelse(is.na(firstnames_women), "", firstnames_women), 
    ifelse(is.na(secondnames_women), "", secondnames_women))]
cap[, settlerchildren := settlersons + settlerdaughters]

# issues
# 1800 has very few voorvoegsels, but they just actually dno't appear to be in it
# pasting first name from first and second names gives you "NA NA" or " "
# about 50% of women's names are missing
# harmonise ""/NA
# lot of mlast missing, but these do have wlast -- check whether this happend in the others (I don't think it did)
# some length 1 strings, mostly initials rather than names
# there's a decent number of firstnames in the surname field

# strip ampersand too because is done in previous versoins
cap[, mlast := stri_replace_all_regex(mlast, "[(/?&].*", "")]
cap[mlast == "T'GOUVERNEMENT", mlast := NA]
cap[mlast == "'T GOUVERNEMENT", mlast := NA]
cap[mlast == "'T KAAPSCH DISTRICT", mlast := NA]
cap[, mlast := stri_trans_general(mlast, "latin-ascii")]

cap[, mfirst := stri_replace_all_regex(mfirst, "[(].*", "")]

cap[, wlast := stri_replace_all_regex(wlast, "[(?].*", "")]
cap[, wlast := stri_trans_general(wlast, "latin-ascii")]

cap[, wfirst := stri_replace_all_regex(wfirst, "[(-].*", "")]

# maybe also strip those "'"?
# cap[, lapply(.SD, function(x) mean(stri_startswith_fixed(x, " "))), .SDcols = c("mlast", "mfirst", "wlast", "wfirst")]
# cap[, lapply(.SD, function(x) mean(stri_endswith_fixed(x, " "))), .SDcols = c("mlast", "mfirst", "wlast", "wfirst")]

cap[, mlast := stri_trim_both(mlast)]
cap[, wlast := stri_trim_both(wlast)]
cap[, mfirst := stri_trim_both(mfirst)]
cap[, wfirst := stri_trim_both(wfirst)]

cap[, minitials := initials(mfirst)]
cap[, winitials := initials(wfirst)]

cap[mfirst == "", mfirst := NA]
cap[wfirst == "", wfirst := NA]

cap[, c("mprefix", "mlast_wo_prefix") := capelinker::split_prefixes(mlast)]
cap[, c("wprefix", "wlast_wo_prefix") := capelinker::split_prefixes(wlast)]
cap[grep(" ", mlast_wo_prefix), unique(mlast_wo_prefix)]
cap[grep(" ", wlast_wo_prefix), unique(wlast_wo_prefix)]
# add MR MRS MISS etc?

cap[wlast == "F", wlast := NA] # one of one-letter surname
# the others are initials in the name field

cap[, persid := .I]
cap[, spousenamedist := stringdist::stringdist(mlast, wlast, method='jw', p=0.1)]
cap[wfirst == "", wfirst := NA]
cap[, wifepresent := !(is.na(wfirst) & is.na(wlast))] # because F & T = F

cap[, mlast_unif := uniformise_string(mlast, quiet = TRUE)]
cap[, namefreq := .N, by = mlast_unif]

cap[!is.na(mfirst), 
    mfirst_uniqueness := rowMeans(
        stringdist::stringdistmatrix(mfirst, mfirst, method = 'jw'),
        na.rm = TRUE), 
    by = year]
# not sure why I don't use the other measures here
cap[!is.na(mfirst), 
    mfirst_cos_uniqueness := 1 - rowMeans(qlcMatrix::sim.strings(mfirst), na.rm = TRUE), 
    by = year]
cap[!is.na(mlast), 
    mlast_uniqueness := 1 - rowMeans(qlcMatrix::sim.strings(mlast)), 
    by = year]

preflight(cap, modstring = "opgaafrol_full")

names(pretrained_models)
setdiff(gsub("dist|sdx|_from|_to|_osa", "", pretrained_models$m_boost_stel_rein$variables),
    names(cap))

out = cap[,
    list(persid = persid,
         districtall = NA,
         mlast = mlast,
         mfirst = mfirst,
         mlast_woprefix = mlast_wo_prefix,
         mprefix = mprefix,
         minitials = minitials,
         wlast = wlast,
         wfirst = wfirst,
         winitials = winitials,
         wlast_wo_prefix = wlast_wo_prefix,
         wprefix = wprefix,
         spousenamedist = spousenamedist,
         year = year,
         # wineproducer = wineproducer,
         old = old,
         young = young,
         widow = widow,
         wifepresent = wifepresent,
         namefreq = namefreq,
         mlast_uniqueness,
         mfirst_uniqueness,
         mfirst_cos_uniqueness,
         settlerchildren = as.numeric(settlerchildren),
         settlermen = as.numeric(settlermen),
         settlerwomen = as.numeric(settlerwomen))]

save(out, file = "~/repos/capelinker/data/cape.rda", version = 2)

# so now what?
# compare with GR and ST
# check where those were made because they're not in the main repo for sure
# make candidates?
# link