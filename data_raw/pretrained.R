rm(list = ls())

library("randomForest")
library("xgboost")
library("data.table")
library("stringdist")

setwd("~/repos/capelinker")

# remotes::install_github("rijpma/capelinker", dependencies = FALSE)
library("capelinker")

## opgaafrollen
# -------------

# graaf reinet
# stellenbosch
# combined
# full and sparse

rein = readRDS("data_raw/candidates_miss.rds.gz")
stel = readRDS("data_raw/stel_candidates_miss.rds.gz")
both = rbindlist(list(rein, stel), fill = TRUE, idcol = "region")
regions = unique(both$region)
both[, (paste0("region", as.character(regions))) := lapply(unique(region), function(x) as.numeric(region == x))]

f = formula(correct ~ 
    mlastdist + mfirstdist + minitialsdist_osa + 
    mlastsdx + 
    mfirstsdx + 
    wlastdist + wfirstdist + winitialsdist_osa + 
    wlastsdx + 
    wfirstsdx + 
    namefreq_from + 
    spousenamedist_from + 
    namefreq_to + 
    spousenamedist_to + 
    wifepresent_from + 
    wifepresent_to + 
    wifeinboth + 
    settlerchildrengauss + 
    nextmfirst + 
    mfirst_uniqueness_to +
    mfirst_uniqueness_from +
    matches + 
    husb_wife_surnamedist + 
    region1)

f_sparse = update(f, . ~ . 
    - wfirstsdx - mfirstsdx - mlastsdx - wlastsdx 
    - wifeinboth - wifepresent_to - wifepresent_from
    - namefreq_to - namefreq_from
    - spousenamedist_to - spousenamedist_from)

set.seed(123871)
share_train = 0.7
both[, train := persid_from %in% sample(unique(persid_from), ceiling(length(unique(persid_from)) * share_train))]
trn_both = both[train == 1]
vld_both = both[train == 0]

m_boost_stel_rein = xgboost::xgb.train(
    data = capelinker::xgbm_ff(trn_both, f),
    nrounds = 1000,
    # watchlist = list(train = xgbm_ff(trn_both, f), eval = xgbm_ff(vld_both, f)),
    params = list(
        max_depth = 6,        # default 6
        min_child_weight = 1, # default 1
        gamma = 1,            # default 0
        eta = 0.3,            # default 0.3
        subsample = 0.8,        # default 1
        colsample_bytree = 0.5, # default 1
        objective = "binary:logistic"
))

m_boost_stel_rein_sparse = xgboost::xgb.train(
    data = capelinker::xgbm_ff(trn_both, f_sparse),
    nrounds = 1000,
    params = list(
        max_depth = 6,        # default 6
        min_child_weight = 1, # default 1
        gamma = 1,            # default 0
        eta = 0.3,            # default 0.3
        subsample = 0.8,        # default 1
        colsample_bytree = 0.5, # default 1
        objective = "binary:logistic"
))

predictions = data.table(
    wife = vld_both$wife * 2, # normalised originally
    correct = vld_both$correct,
    pred_bos = predict(m_boost_stel_rein, newdata = xgbm_ff(vld_both, f)))
# Metrics::precision(predictions$correct, predictions$pred_bos > 0.5)
# Metrics::recall(predictions$correct, predictions$pred_bos > 0.5)

# baptism and marriage records
# ----------------------
manual_baptisms = readRDS("data_raw/baptisms.rds.gz")
manual_marriages = readRDS("data_raw/marriages.rds.gz")

cnd = capelinker::candidates(
    dat_from = manual_baptisms,
    dat_to =  manual_marriages,
    idvariable_from = "baptid",
    idvariable_to = "marid",
    linktype = "many:one",
    blocktype = "string distance",
    blockvariable_from = "mlast",
    blockvariable_to = "mlast",
    maxdist = 0.15)

# cnd[is.na(marid)]
# NAs present because merge(..., all = TRUE), meaning NA when no plausible
# matches were found

# A problem that comes up here is that because we are doing a many-to-
# one match, we get more manual matches in the candidates dataset than
# we'd expect:

cnd[linkid_from == linkid_to][order(linkid_from), list(mlast_from, mlast_to, linkid_from, linkid_to, baptid, marid)]
# note the number of rows when subsetting to matches should be:
cnd[linkid_from == linkid_to, max(baptid)]

# Some sort of self-join is happening. Here we use the `baptid` made
# earlier to filter for duplicates.
# Apparently this isn't happening anymore. I suppose that's good?

cnd[, 
    duplicate := (linkid_from == linkid_to) & duplicated(baptid), 
    by = linkid_to]
cnd = cnd[duplicate == FALSE]
nrow(cnd[linkid_from == linkid_to])
# ? why is the self-join no longer happening?

# Next we create a variable indicating whether a candidate is a match in the
# manual data. We also get rid of the cases where either `linkid_from` or
# `linkid_to` was missing (need to check why that is.)

cnd[, correct := linkid_from == linkid_to]
cnd[, .N, by = correct]
# merge in make_candidates has all = TRUE, giving NAs in marid and baptid
# fix that
cnd = cnd[!is.na(correct)]


# train
cnd = distcalc(cnd, 
    character_variables = c("mlast", "mfirst", "wfirst", "minitials", "winitials", "mprof"),
    numeric_variables = "year")

# We are now finally ready to train a model on the manual links. We split the data in a training and a test data. Then we use the random forest classifier which proved efficient in the opgaafrollen. It might be worth testing a few more models.
# make this per-marid
set.seed(225) # for reproducability
cnd[, train := runif(.N) > 0.5]
train = cnd[train == TRUE]
test = cnd[train == FALSE]

x= train[, 
        grep("correct|dist|sdx|yeardist", names(train)), 
        with = FALSE]
x[!complete.cases(x)]
m_rf_baptisms_full = randomForest(
    as.factor(correct) ~ ., 
    data = train[, 
        grep("correct|dist|sdx|yeardist", names(train)), 
        with = FALSE],
    na.action = "na.exclude")

# no soundex, no professions
m_rf_baptisms_sparse = randomForest(
    as.factor(correct) ~ .,
    data = train[,
        grep("correct|mlastdist|mfirstdist|wfirstdist|yeardist", names(train)),
        with = FALSE],
    na.action = "na.exclude")

## couples in saf
# ---------------
jlabels = readxl::read_xlsx("~/data/cape/saf/Jeanne matches_full.xlsx")
setDT(jlabels)
alabels = readxl::read_xlsx("~/data/cape/saf/saf_spousallinks_labels_a_done.xlsx")
setDT(alabels)
alabels = alabels[match > 0, list(couple_id_from, couple_id_to)]

labelled = rbindlist(list(alabels, jlabels[, -"match"]))
labelled = unique(labelled)

saf_cnd = fread("~/data/cape/saf/saf_spousallinks_labels.csv", na.strings = "")
saf_cnd = saf_cnd[!is.na(couple_id_to)]

saf_cnd[paste(couple_id_from, couple_id_to) %in% paste(labelled$couple_id_from, labelled$couple_id_to), match := 1]

# remove duplicate
saf_cnd = saf_cnd[!(couple_id_from == "snymana1b7c1d2e7_mar_1" & couple_id_to == "snymana1b7c1d7e4_mar_1")]
# nb now more matches in labelled than in saf_cnd but is ok

setnames(saf_cnd, "match", "correct")

saf_cnd[, mfirstdist := stringdist::stringdist(firstnames_ego_husb, firstnames_spouse_husb, method = "jw")]
saf_cnd[, mlastdist := stringdist::stringdist(surname_ego_husb, surname_spouse_husb, method = "jw")]

saf_cnd[, wfirstdist := stringdist::stringdist(firstnames_ego_wife, firstnames_spouse_wife, method = "jw")]
saf_cnd[, wlastdist := stringdist::stringdist(surname_ego_wife, surname_spouse_wife, method = "jw")]

saf_cnd[, minitals_ego_husb := initials(firstnames_ego_husb)]
saf_cnd[, minitals_spouse_husb := initials(firstnames_spouse_husb)]
saf_cnd[, minitialsdist_osa := 1 - stringdist::stringsim(minitals_ego_husb, minitals_spouse_husb, method = "osa")]

saf_cnd[, winitals_ego_husb := initials(firstnames_ego_wife)]
saf_cnd[, winitals_spouse_husb := initials(firstnames_spouse_wife)]
saf_cnd[, winitialsdist_osa := 1 - stringdist::stringsim(winitals_ego_husb, winitals_spouse_husb, method = "osa")]

saf_cnd[, spousal_age_gap := startyear_ego_husb - startyear_ego_wife]
saf_cnd[, maryear_initialsdist_osa := 1 - stringdist::stringsim(as.character(married_year_ego_husb), as.character(married_year_ego_wife), method = "osa")]

f_saf = formula(correct ~ 
    # maryear_initialsdist_osa +  
    mlastdist + mfirstdist + minitialsdist_osa + 
    # mlastsdx + 
    # mfirstsdx + 
    wlastdist + wfirstdist + winitialsdist_osa
    # wlastsdx + 
    # wfirstsdx + 
    # namefreq_from + 
    # spousenamedist_from + 
    # namefreq_to + 
    # spousenamedist_to + 
    # wifepresent_from + 
    # wifepresent_to + 
    # wifeinboth + 
    # settlerchildrengauss + 
    # nextmfirst + 
    # mfirst_uniqueness_to +
    # mfirst_uniqueness_from +
    # matches + 
    # husb_wife_surnamedist + 
    # region1
    # implied_marriage_age_wife + 
    # implied_marriage_age_husb +
    # spousal_age_gap +
    # myeardiff
)

set.seed(987)
share_train = 0.7
saf_cnd[, train := couple_id_from %in% sample(unique(couple_id_from), ceiling(length(unique(couple_id_from)) * share_train))]
trn_saf = saf_cnd[train == 1]
vld_saf = saf_cnd[train == 0]

# some overfitting going on her
m_boost_saf = xgboost::xgb.train(
    data = capelinker::xgbm_ff(trn_saf, f_saf),
    nrounds = 1000,
    # watchlist = list(train = xgbm_ff(trn_saf, f), eval = xgbm_ff(vld_saf, f)),
    params = list(
        max_depth = 6,        # default 6
        min_child_weight = 1, # default 1 larger is more consevative
        gamma = 1,            # default 0, larger is more conservative
        eta = 0.3,            # default 0.3 lower for less overfitting
        max_delta_step = 0,   # deafult 0, useful for unbalanced, higher is more conservative
        subsample = 0.8,        # default 1 lower is less overfitting
        colsample_bytree = 0.5, # default 1 
        objective = "binary:logistic"
))

impmat = xgboost::xgb.importance(model = m_boost_saf)

predictions = data.table(
    correct = as.logical(vld_saf$correct),
    pred_bos = predict(m_boost_saf, newdata = xgbm_ff(vld_saf, f_saf)))
conf = table(actual = predictions$correct, predicted = predictions$pred_bos > 0.5)
print(xtable(conf), 
    add.to.row = list(pos=list(-1), command=c("& \\multicolumn{3}{c}{Predicted}\\\\")))

predictions[, .N, by = list(actual = correct, predicted = pred_bos > 0.5)]

Metrics::precision(predictions$correct, predictions$pred_bos > 0.5)
Metrics::recall(predictions$correct, predictions$pred_bos > 0.5)
Metrics::fbeta_score(predictions$correct, predictions$pred_bos > 0.5)

toplot = predictions[, .N, by = list(actual = correct, predicted = pred_bos > 0.5)]
steps = seq(0.01, 1, 0.01)
toplot = predictions[, list(
        prec = sapply(steps, function(x) Metrics::precision(correct, predicted = pred_bos > x)),
        rec = sapply(steps, function(x) Metrics::recall(correct, predicted = pred_bos > x)),
        fbeta = sapply(steps, function(x) Metrics::fbeta_score(correct, predicted = pred_bos > x)),
        tr = steps)]
pdf("~/repos/saf/precrec.pdf")
plot(rec ~ prec, data = toplot, type = 'b', pch = 19,
    xlab = "precision", ylab = "recal")
points(rec ~ prec, data = toplot[tr == 0.5], type = 'b', col = 2, pch = 19)
dev.off()

library(ggplot2)
ggplot(toplot, aes(prec, rec)) + 
    geom_line() + 
    geom_point(data = toplot[tr == 0.5], col = "black")


## saf to opgaafrol
# -----------------


# training data
alabels = readxl::read_xlsx("~/data/cape/saf/saf2opg_candidates_batch2_auke.xlsx")
jlabels = readxl::read_xlsx("~/data/cape/saf/saf2opg_candidates_batch1_jeanne.xlsx")

setDT(alabels)
setDT(jlabels)

# empty is no link
alabels[is.na(link), link := 0]
jlabels[is.na(link), link := 0]
jlabels[link == 2, link := 1] # typo

# check overlap
first50a = alabels[, unique(couple_id)[1:50]]
first50j = jlabels[, unique(couple_id)[1:50]]
all.equal(first50j, first50a)

# check intercoder reliability
intercoder = merge(
    alabels[couple_id %in% first50j], 
    jlabels[couple_id %in% first50j, list(couple_id, persid, link_j = link, comment_j = comment)], 
    by = c("couple_id", "persid"))
# w/o "based on opg" because j never linked those
writexl::write_xlsx(
    intercoder[link != link_j & (comment != "based on opg" | is.na(comment))],
    "~/data/cape/saf/saf2opg_intercoder.xlsx")

alabels[comment == "based on opg", link := 0] # for now don't use these (singles inferred from opg series)

saf2opg = rbindlist(
    list(alabels,
        jlabels[!couple_id %in% first50j]), # avoid doubles
    fill = TRUE)

# linking 248 in labelled, ~50%
saf2opg[, uniqueN(couple_id[link == 1])]
saf2opg[, uniqueN(couple_id[link == 1]) / uniqueN(couple_id)]

# link about 2000 opg observations
saf2opg[, uniqueN(persid[link == 1])]
saf2opg[, uniqueN(idx[link == 1])]
# can't say yet if that's a lot

# for compatability with other pretrained models
setnames(saf2opg, "link", "correct")

# labelling blocks
saf2opg[, uniqueN(couple_id)]

saf2opg[, mfirstdist := stringdist::stringdist(firstnames, mfirst, method = "jw")]
saf2opg[, mlastdist := stringdist::stringdist(surname, mlast, method = "jw")]

saf2opg[, wfirstdist := stringdist::stringdist(spouse_firstnames_clean, wfirst, method = "jw")]
saf2opg[, wlastdist := stringdist::stringdist(spouse_surname_clean, wlast, method = "jw")]

# maybe we had this at an earlier stage?
# later merge back into original cleaned files
saf2opg[, minitals_saf := initials(firstnames)]
saf2opg[, minitals_opg := initials(mfirst)] # def. here
saf2opg[, minitialsdist_osa := 1 - stringdist::stringsim(minitals_saf, minitals_opg, method = "osa")]

saf2opg[, winitals_saf := initials(spouse_firstnames_clean)]
saf2opg[, winitals_opg := initials(wfirst)]
saf2opg[, winitialsdist_osa := 1 - stringdist::stringsim(winitals_saf, winitals_opg, method = "osa")]

# add next year dist
# ...

# add surnamedist
saf2opg[, cross_surnamedist := stringdist::stringdist(firstnames, wlast, method = "jw")]

# add "name is initials"
len_longest_string = function(str){
    out = stringi::stri_extract_all_regex(str, "[a-z]+", simplify = FALSE)
    out = lapply(out, nchar)
    out = lapply(out, max)
    return(unlist(out))
} 

saf2opg[, wfirst_is_initials:= len_longest_string(wfirst) == 1]
saf2opg[, mfirst_is_initials:= len_longest_string(mfirst) == 1]

saf2opg[, years_married := year - married_year]
# saf_cnd[, maryear_initialsdist_osa := 1 - stringdist::stringsim(as.character(married_year_ego_husb), as.character(married_year_ego_wife), method = "osa")]
# x[, implied_age_gk := gk(year, sy)]

f_saf2opg = formula(correct ~ 
    # maryear_initialsdist_osa +  
    mlastdist + mfirstdist + minitialsdist_osa + 
    # mlastsdx + 
    # mfirstsdx + 
    wlastdist + wfirstdist + winitialsdist_osa + 
    cross_surnamedist + 
    wfirst_is_initials + 
    # mfirst_is_initials + 
    years_married + 
    implied_age
    # wlastsdx + 
    # wfirstsdx + 
    # namefreq_from + 
    # spousenamedist_from + 
    # namefreq_to + 
    # spousenamedist_to + 
    # wifepresent_from + 
    # wifepresent_to + 
    # wifeinboth + 
    # settlerchildrengauss + 
    # nextmfirst + 
    # mfirst_uniqueness_to +
    # mfirst_uniqueness_from +
    # matches + 
    # husb_wife_surnamedist + 
    # region1
    # implied_marriage_age_wife + 
    # implied_marriage_age_husb +
    # spousal_age_gap +
    # myeardiff
)

set.seed(123654)
share_train = 0.7
saf2opg[, train := couple_id %in% sample(unique(couple_id), ceiling(length(unique(couple_id)) * share_train))]
trn_saf2opg = saf2opg[train == 1]
vld_saf2opg = saf2opg[train == 0]

# slight overfitting going on here
m_saf2opg = xgboost::xgb.train(
    data = capelinker::xgbm_ff(trn_saf2opg, f_saf2opg),
    nrounds = 500,
    watchlist = list(train = xgbm_ff(trn_saf2opg, f_saf2opg), eval = xgbm_ff(vld_saf2opg, f_saf2opg)),
    params = list(
        max_depth = 6,        # default 6
        min_child_weight = 1, # default 1 larger is more consevative
        gamma = 1,            # default 0, larger is more conservative
        eta = 0.3,            # default 0.3 lower for less overfitting
        max_delta_step = 0,   # deafult 0, useful for unbalanced, higher is more conservative
        subsample = 0.8,        # default 1 lower is less overfitting
        colsample_bytree = 0.5, # default 1 
        objective = "binary:logistic"
))
predictions = data.table(
    correct = as.logical(vld_saf2opg$correct),
    predicted = predict(m_saf2opg, newdata = xgbm_ff(vld_saf2opg, f_saf2opg)))
table(actual = predictions$correct, predicted = predictions$predicted > 0.5)
predictions[, Metrics::precision(correct, predicted > 0.5)]
predictions[, Metrics::recall(correct, predicted > 0.5)]
predictions[, Metrics::fbeta_score(correct, predicted > 0.5)]

pretrained_models = list(
    m_boost_stel_rein = list(
        model = m_boost_stel_rein,
        variables = all.vars(f)[-1]),
    m_boost_stel_rein_sparse = list(
        model = m_boost_stel_rein_sparse,
        variables = all.vars(f_sparse)[-1]),
    m_rf_baptisms_sparse = list(
        model = m_rf_baptisms_sparse,
        variables = all.vars(formula(m_rf_baptisms_sparse))[-1]),
    m_rf_baptisms_full = list(
        model = m_rf_baptisms_full,
        variables = all.vars(formula(m_rf_baptisms_full))[-1]),
    m_boost_saf = list(
        model = m_boost_saf,
        variables = all.vars(f_saf)[-1]),
    m_boost_saf2opg = list(
        model = m_saf2opg,
        variables = all.vars(f_saf2opg)[-1])
)
lapply(pretrained_models, `[[`, "variables")

save(pretrained_models, 
    file = "data/pretrained_models.rda", 
    version = 2)
