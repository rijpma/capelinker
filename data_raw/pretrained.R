rm(list = ls())

library("randomForest")
library("stringdist")
library("data.table")
# library("devtools")
library("stringi")

setwd("~/repos/capelinker")
# devtools::document()
# devtools::load_all()

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
# table(predictions$correct, predictions$pred_bos > 0.5)
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
    blockvariable = "mlast",
    maxdist = 0.15)

cnd[is.na(marid)]

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


pretrained_models = list(
    m_boost_stel_rein = list(
        model = m_boost_stel_rein,
        variables = all.vars(f)[-1]),
    m_boost_stel_rein_sparse = list(
        model = m_boost_stel_rein_sparse,
        variables = all.vars(f_sparse)[-1]),
    m_rf_baptisms_sparse = list(
        model = m_rf_baptisms_sparse,
        variables = all.vars(formula(m_rf_baptisms_sparse))[-1]
    ),
    m_rf_baptisms_full = list(
        model = m_rf_baptisms_full,
        variables = all.vars(formula(m_rf_baptisms_full))[-1]
    )
)
lapply(pretrained_models, `[[`, "variables")

save(pretrained_models, 
    file = "data/pretrained_models.rda", 
    version = 2)
