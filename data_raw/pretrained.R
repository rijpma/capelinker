rm(list = ls())

library("randomForest")
library("randomForestSRC")
library("Rborist")
library("stringdist")
library("data.table")
library("devtools")
library("stringi")

setwd("~/repos/capelinker")
devtools::document()
devtools::load_all()

## opgaafrollen

opg = readRDS("~/repos/capelinker/data_raw/opgaafrollen.rds.gz")
opg[, mfirst_single := stri_extract_first_words(mfirst)]
opg[!is.na(mfirst_single) & !is.na(year) & !is.na(mlast), mname_uniqueness := stringdist_closest(stri_join(mfirst_single, mlast)), by = year]

tra = fread("/Users/auke/Dropbox/opgaafrol/matched.csv")
setnames(tra, 10:11, c("persid_1828", "persid_1826"))
tra = tra[!is.na(persid_1828) & !is.na(persid_1826), ]

cnd = capelinker::candidates(
    dat_from = opg[year == 1828 & grepl("^[A-L]", mlast), ], # 608
    dat_to = opg[year==1826 & grepl("^[A-L]", mlast), ],     # 674
    maxdist = 0.15,
    blockvariable = "mlast") # mlast regular

cnd[!is.na(persid_from) & !is.na(persid_to), 
    correct := paste0(persid_from, persid_to) %in% paste0(tra$persid_1828, tra$persid_1826)]
cnd[correct == TRUE, list(mlast_from, mlast_to)]
cnd = cnd[!is.na(correct), ]

cnd[, uniqueN(persid_from)] # 570
cnd[, sum(correct, na.rm = T)] # 441, so we lost 14, but ncnd is 7547
nrow(cnd) # 7547

cnd = capelinker::distcalc(cnd,
    character_variables = c("mlast", "mfirst", "wlast", "wfirst", 
        "mlast_woprefix", "wlast_woprefix", "minitials", "winitials"),
    numeric_variables = c("settlerchildren"))

cnd[, wifeinboth := (wifepresent_from == TRUE & wifepresent_to == TRUE)]
# should be upper
# cnd[, wifeinboth := (wifepresent_from == wifepresent_to)]

cnd[, matches := .N, by = persid_from]

cnd = cnd[, list(correct, wifepresent_from, spousenamedist_from,
    namefreq_from, wifepresent_to, spousenamedist_to,
    namefreq_to, mlastdist, mfirstdist,
    minitialsdist, wlastdist, wfirstdist,
    winitialsdist, mlastsdx, mfirstsdx,
    wlastsdx, wfirstsdx,wifeinboth, 
    mlast_woprefixdist, 
    wlast_woprefixdist, # including this apparently makes the 
                          # model rely on it too much and it
                          # breaks down on wives being absent
    mname_uniqueness_to, mname_uniqueness_from,
    settlerchildrendist, matches)]


set.seed(2718)
smpl = rbinom(nrow(cnd), 1, p=0.5)
# smpl = rbinom(nrow(cnd), 1, p=0.8)
trn = cnd[smpl == 1, ]
vld = cnd[smpl == 0, ]

trn = trn[complete.cases(trn), ]
trn = trn[, lapply(.SD, normalise)]
vld = vld[, lapply(.SD, normalise), .SDcols = names(trn)]

opgaafrol_full = randomForest(as.factor(correct) ~ 
    mlastdist + mfirstdist + minitialsdist + 
    mlastsdx + mfirstsdx + 
    wlastdist + wfirstdist + winitialsdist + 
    wlastsdx + wfirstsdx + 
    wifepresent_from + spousenamedist_from + namefreq_from + 
    wifepresent_to + spousenamedist_to + namefreq_to + 
    wifeinboth + settlerchildrendist + matches, data=trn, mty=5)
opgaafrol_full$confusion
tail(opgaafrol_full$err.rate[, "OOB"], 1)

sum(opgaafrol_full$confusion[1:2, 1:2]) # 3683

pred_rf_vld = predict(opgaafrol_full, newdata=vld)
conf_rf_vld = table(actual = vld$correct[as.numeric(names(pred_rf_vld))], predicted = pred_rf_vld)
conf_rf_vld
prop.table(conf_rf_vld, margin = 1) # row, share actually true correctly predicted
prop.table(conf_rf_vld, margin = 2) # col, share predicted actualy true

dat = data.frame(tr = seq(0.05, 0.95, by = 0.05), truneg = NA, falneg = NA, falpos = NA, trupos = NA)
for (i in dat$tr){
    pred_rf_vld = predict(opgaafrol_full, type = "vote", newdata=vld)[, 2] > i
    conf_rf_vld = table(actual = vld$correct[as.numeric(names(pred_rf_vld))], predicted = pred_rf_vld)
    dat[dat$tr == i, -1] <- c(conf_rf_vld)
}
dat$fpr = dat$falpos / (dat$trupos + dat$falneg)
dat$fnr = dat$falneg / (dat$trupos + dat$falneg)
plot(sqrt(dat$fpr^2 + dat$fnr^2))
matplot(dat$tr, dat[, c("fpr", "fnr")], type = 'l')
abline(v = 0.45)


conf_rf_vld / rowSums(conf_rf_vld) # share of correct correctly predicted
                                   # .887
conf_rf_vld / colSums(conf_rf_vld) # share predicted actually correct
                                   # .926

trn[wifeinboth == TRUE]
trn[wifeinboth == FALSE]
trn[wifepresent_to == FALSE]
trn[wifepresent_from == FALSE]

opgaafrol_full_wives = randomForest(as.factor(correct) ~ 
    mlastdist + mfirstdist + minitialsdist + 
    mlastsdx + mfirstsdx + 
    wlastdist + wfirstdist + winitialsdist + 
    wlastsdx + wfirstsdx + 
    wifepresent_from + spousenamedist_from + namefreq_from + 
    wifepresent_to + spousenamedist_to + namefreq_to + 
    wifeinboth + settlerchildrendist + matches, 
    data = trn[wifeinboth == TRUE], mty = 5)
opgaafrol_full_wives$confusion

opgaafrol_full_nowives = randomForest(as.factor(correct) ~ 
    mlastdist + mfirstdist + minitialsdist + 
    mlastsdx + mfirstsdx + 
    wlastdist + wfirstdist + winitialsdist + 
    wlastsdx + wfirstsdx + 
    wifepresent_from + spousenamedist_from + namefreq_from + 
    wifepresent_to + spousenamedist_to + namefreq_to + 
    wifeinboth + settlerchildrendist + matches, 
    data = trn[wifeinboth == FALSE], mty = 5)
opgaafrol_full_nowives$confusion

sum(opgaafrol_full_wives$confusion[1:2, 1:2]) # 3683

pred_rf_vld = predict(opgaafrol_full_wives, newdata=vld)
conf_rf_vld = table(actual = vld$correct[as.numeric(names(pred_rf_vld))], predicted = pred_rf_vld)
conf_rf_vld
conf_rf_vld / rowSums(conf_rf_vld) # share of correct correctly predicted
                                   # .887
conf_rf_vld / colSums(conf_rf_vld) # share predicted actually correct
                                   # .926

opgaafrol_full_src = rfsrc(as.factor(correct) ~ 
    mlastdist + mfirstdist + minitialsdist + 
    mlastsdx + mfirstsdx + 
    wlastdist + wfirstdist + winitialsdist + 
    wlastsdx + wfirstsdx + 
    wifepresent_from + spousenamedist_from + namefreq_from + 
    wifepresent_to + spousenamedist_to + namefreq_to + 
    wifeinboth + settlerchildrendist + matches, data=trn, mtry=5)

table(actual = opgaafrol_full_src$forest$yvar, predicted = predict(opgaafrol_full_src)$yvar)
vld[, complete := complete.cases(.SD), .SDcols = opgaafrol_full_src$forest$xvar.names]
vld[complete == TRUE, predicted_src := predict(opgaafrol_full_src, newdata = .SD)$yvar]
table(actual = vld$correct, predicted = vld$predicted_src)

vld[, complete := complete.cases(.SD), .SDcols = all.vars(formula(opgaafrol_full))[-1]]
vld[complete == TRUE, predicted := predict(opgaafrol_full, newdata = .SD)]
table(actual = vld$correct, predicted = vld$predicted)

m_rf_opgaafrol_genealogy = randomForest(
    as.factor(correct) ~ wifepresent_from + namefreq_from + wifepresent_to + 
    namefreq_to + mlastdist + mfirstdist + minitialsdist + wlastdist + 
    wfirstdist + winitialsdist + mlastsdx + mfirstsdx + wlastsdx + 
    wfirstsdx + 
    # settlerchildrendist + 
    # wlast_woprefixdist
     + matches,
    data = trn)

m_rf_opgaafrol_sparse = randomForest(
    as.factor(correct) ~ minitialsdist + mfirstdist + 
    # wlast_woprefixdist + 
    winitialsdist + wfirstdist + matches + wifepresent_to, 
    data = trn)

# experimental: noise in missing wives names and prefixes
wlastnames = opg[wifepresent == TRUE, wlast]
opg[wifepresent == FALSE, wlast := rand_strings_like(wlastnames, .N)]
wfirstnames = opg[wifepresent == TRUE, wfirst]
opg[wifepresent == FALSE, wfirst := rand_strings_like(wfirstnames, .N)]
opg[wifepresent == FALSE, winitials := initials(wfirst)]
cnd = capelinker::candidates(
    dat_from = opg[year == 1828 & grepl("^[A-L]", mlast), ], # 608
    dat_to = opg[year==1826 & grepl("^[A-L]", mlast), ],     # 674
    maxdist = 0.15,
    blockvariable = "mlast_woprefix") # mlast regular

cnd[!is.na(persid_from) & !is.na(persid_to), 
    correct := paste0(persid_from, persid_to) %in% paste0(tra$persid_1828, tra$persid_1826)]
cnd[correct == TRUE, list(mlast_from, mlast_to)]
cnd = cnd[!is.na(correct), ]

cnd[, uniqueN(persid_from)] # 567
cnd[, sum(correct, na.rm = T)] # 438
nrow(cnd) # 7476

cnd = capelinker::distcalc(cnd,
    character_variables = c("mlast", "mfirst", "wlast", "wfirst", 
        "mlast_woprefix", "wlast_woprefix", "minitials", "winitials"),
    numeric_variables = c("settlerchildren"))

cnd[, wifeinboth := (wifepresent_from == TRUE & wifepresent_to == TRUE)]
# should be upper
# cnd[, wifeinboth := (wifepresent_from == wifepresent_to)]

cnd[, matches := .N, by = persid_from]


cnd = cnd[, list(correct, wifepresent_from, spousenamedist_from,
    namefreq_from, wifepresent_to, spousenamedist_to,
    namefreq_to, mlastdist, mfirstdist,
    minitialsdist, wlastdist, wfirstdist,
    winitialsdist, mlastsdx, mfirstsdx,
    wlastsdx, wfirstsdx,wifeinboth, 
    mlast_woprefixdist, 
    wlast_woprefixdist, # including this apparently makes the 
                          # model rely on it too much and it
                          # breaks down on wives being absent
    mname_uniqueness_to, mname_uniqueness_from,
    settlerchildrendist, matches)]


set.seed(2718)
smpl = rbinom(nrow(cnd), 1, p=0.5)
trn = cnd[smpl == 1, ]
vld = cnd[smpl == 0, ]

trn = trn[complete.cases(trn), ]
trn = trn[, lapply(.SD, normalise)]
vld = vld[, lapply(.SD, normalise), .SDcols = names(trn)]


opgaafrol_exp = randomForest(as.factor(correct) ~ 
    mlastdist + mfirstdist + minitialsdist + 
    mlast_woprefixdist + 
    mlastsdx + mfirstsdx + 
    wlastdist + wfirstdist + winitialsdist + 
    wlast_woprefixdist + 
    wlastsdx + wfirstsdx + 
    wifepresent_from + spousenamedist_from + namefreq_from + 
    wifepresent_to + spousenamedist_to + namefreq_to + 
    wifeinboth + settlerchildrendist + matches, 
    data=trn, mty=5)
opgaafrol_exp$confusion
pred_rf_vld = predict(opgaafrol_exp, newdata=vld)
conf_rf_vld = table(actual = vld$correct[as.numeric(names(pred_rf_vld))], predicted = pred_rf_vld)
conf_rf_vld
prop.table(conf_rf_vld, margin = 1) # row, share actually true correctly predicted
prop.table(conf_rf_vld, margin = 2) # col, share predicted actualy true
conf_rf_vld / rowSums(conf_rf_vld) # share of correct correctly predicted
                                   # .887 # .868
conf_rf_vld / colSums(conf_rf_vld) # share predicted actually correct
                                   # .926 # .925

opgaafrol_exp_u = randomForest(as.factor(correct) ~ 
    mlastdist + mfirstdist + minitialsdist + 
    mlast_woprefixdist + 
    mlastsdx + mfirstsdx + 
    wlastdist + wfirstdist + winitialsdist + 
    wlast_woprefixdist + 
    wlastsdx + wfirstsdx + 
    mname_uniqueness_from + mname_uniqueness_to +
    wifepresent_from + spousenamedist_from + namefreq_from + 
    wifepresent_to + spousenamedist_to + namefreq_to + 
    wifeinboth + settlerchildrendist + matches, data=trn, mty=5)
opgaafrol_exp_u$confusion
pred_rf_vld = predict(opgaafrol_exp_u, newdata=vld)
conf_rf_vld = table(actual = vld$correct[as.numeric(names(pred_rf_vld))], predicted = pred_rf_vld)
conf_rf_vld
conf_rf_vld / rowSums(conf_rf_vld) # share of correct correctly predicted
                                   # .887 # .868
conf_rf_vld / colSums(conf_rf_vld) # share predicted actually correct
                                   # .926 # .925

par(mfrow = c(1, 3))
varImpPlot(opgaafrol_full)
varImpPlot(opgaafrol_exp)
varImpPlot(opgaafrol_exp_u)

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

# Training

# To train the model, the character (string) variables need to be
# turned into quantitative measures using string distances. This is
# done in a function `calc_distances()`, using a vector of variable
# names present in both datasets. Additionally we create a variable
# measuring the time between the marriage and baptism candidates,
# because this will influence the plausibility of a match.

cnd[, year_from := as.numeric(year_from)]

cnd
cnd = distcalc(cnd, 
    character_variables = c("mlast", "mfirst", "wfirst", "minitials", "winitials", "mprof"),
    numeric_variables = "year")


# cnd[, years_since_marriage := as.numeric(year_from) - as.numeric(year_to)]
cnd = cnd[yeardist > -100] # cases where year_from == 0

# We are now finally ready to train a model on the manual links. We split the data in a training and a test data. Then we use the random forest classifier which proved efficient in the opgaafrollen. It might be worth testing a few more models.

set.seed(225) # for reproducability
cnd[, train := runif(.N) > 0.5]
train = cnd[train == TRUE]
test = cnd[train == FALSE]

m_rf_baptisms_full = randomForest(
    as.factor(correct) ~ ., 
    data = train[, 
        grep("correct|dist|sdx|yeardist", names(train)), 
        with = FALSE])

# no soundex, no professions
m_rf_baptisms_sparse = randomForest(
    as.factor(correct) ~ .,
    data = train[,
        grep("correct|mlastdist|mfirstdist|wfirstdist|yeardist", names(train)),
        with = FALSE])


pretrained_models = list(
    m_rf_baptisms_sparse = list(
        model = m_rf_baptisms_sparse,
        variables = all.vars(formula(m_rf_baptisms_sparse))[-1]
    ),
    m_rf_baptisms_full = list(
        model = m_rf_baptisms_full,
        variables = all.vars(formula(m_rf_baptisms_full))[-1]
    ),
    opgaafrol_full = list(
        model = opgaafrol_full,
        variables = all.vars(formula(opgaafrol_full))[-1]
    ),
    opgaafrol_exp = list(
        model = opgaafrol_exp,
        variables = all.vars(formula(opgaafrol_exp))[-1]
    ),
    opgaafrol_exp_u = list(
        model = opgaafrol_exp_u,
        variables = all.vars(formula(opgaafrol_exp_u))[-1]
    ),
    # opgaafrol_full_src = list(
    #     model = opgaafrol_full_src,
    #     variables = opgaafrol_full_src$xvar.names
    # ),
    # opgaafrol_full_rng = list(
    #     model = opgaafrol_full_rng,
    #     variables = opgaafrol_full_rng$forest$independent.variable.names
    # ),
    m_rf_opgaafrol_genealogy = list(
        model = m_rf_opgaafrol_genealogy,
        variables = all.vars(formula(m_rf_opgaafrol_genealogy))[-1]
    )
    # m_rf_opgaafrol_sparse = list(
    #     model = m_rf_opgaafrol_sparse,
    #     variables = all.vars(formula(m_rf_opgaafrol_sparse))[-1]
    # )
)
lapply(pretrained_models, `[[`, "variables")

devtools::use_data(pretrained_models, overwrite = TRUE)
