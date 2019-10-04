#' Create candidate links from two datasets.
#' 
#' \code{candidates} merges two datasets based on a distance criterium. The resulting dataset can be used to predict links.
#' 
#' Blocking on multiple variables is currently not supported, but could be done by using \code{candidates()} repeatedly and merging the results might work.
#' 
#' Because historical records often provide limited information, it is possible to block on string distances. Note that this can become quite slow when there is a large number of records to in each dataset (say, tens of thousands). 
#' 
#' String distance blocking is done using Jaro-Winkler string distances, which de-emphasise differences at the end of the string. The distance ranging from 0 (perfect match) to 1 (completely mismatch). Set the maxdist (see Arguments) accordingly.
#' 
#' It is currently possible to return missing values when for a given record, no candidate is found. While these records can never be matched, they are left in to make comparisons of the dataset easier.
#' 
#' 
#' @param dat_from a data.table
#' @param dat_to a data.table
#' @param idvariable_from String giving the identifier variable in dat_from.
#' @param idvariable_to String giving the identifier variable in dat_from.
#' @param blockvariable String giving the name of the blocking varariable. Distance between this variable in both datasets determines whether a pair of records is a candidate. Should be present in both datasets. Defaults to "mlast", the male surname.
#' @param blocktype Type of blocking: string distance (default) or numeric.
#' @param linktype Should there be no more than one record in each dataset that can be linked (one:one), or is it possible for multiple records in \code{dat_from} to be linked to \code{dat_to}? Defaults to "one:one".
#' @param maxdist Maximum distance (0-1) to consider a record a candidate. Defaults to 0.15 for male surname string distance. If using numeric distance (for instance year of birth), very different values could be needed.
#' 
#' @return A dataset containing all candidate pairs, and all columns in dat_from and dat_to. Columns with the same name will get a suffix "_from" or "_to".
#' 
#' 
#' @examples
#' d1 = data.table::data.table(mlast = c("jong", "smid", "nauda"), persid = c(1:3))
#' d2 = data.table::data.table(mlast = c("jongh", "jong", "smit", "veld"), persid = c(1:4))
#' candidates(d1, d2)
#'  
#' @export
candidates = function(dat_from, dat_to, 
    blockvariable = "mlast", 
    idvariable_from = "persid", idvariable_to = "persid",
    blocktype = c("string distance", 
                 "numeric", 
                 "bigram distance", 
                 "idf bigram distance", 
                 "soundex"),
    linktype = c("one:one", "many:one"),
    maxdist = 0.15){

    linktype = match.arg(linktype)
    blocktype = match.arg(blocktype)

    stopifnot(data.table::is.data.table(dat_from))
    stopifnot(data.table::is.data.table(dat_to))

    stopifnot(nrow(dat_from) > 0)
    stopifnot(nrow(dat_to) > 0)

    if (linktype == "one:one" & (
            any(duplicated(dat_from[[idvariable_from]]))
          | any(duplicated(dat_to[[idvariable_to]])))){
        warning("One to one matching, but idvariables not unique")
    }

    if ((maxdist < 0 | maxdist > 1) & blocktype == "string distance"){
        warning("Maxdist should be between 0 and 1.")
    }
    maxsim = 1 - maxdist # for similarity measures

    if (blocktype == "string distance"){
        distmat = stringdist::stringdistmatrix(
            a = dat_from[, get(blockvariable)],
            b = dat_to[, get(blockvariable)],
            method = 'jw', p = 0.1)
        candidate_list = apply(distmat, 1, function(x) which(x < maxdist))
        score_list =distmat[rep(1:length(candidate_list), sapply(candidate_list, length)) + nrow(distmat) * (unlist(candidate_list) - 1)]
        # score_list = apply(distmat, 1, function(x) x[which(x < maxdist)])
    }
    if (blocktype == "numeric"){
        simmat = 1 - outer(
            X = dat_from[, get(blockvariable)],
            Y = dat_to[, get(blockvariable)],
            FUN = capelinker::gk)
        candidate_list = apply(simmat, 1, function(x) which(x > maxsim))
        score_list = apply(simmat, 1, function(x) x[which(x > maxsim)])
    }
    if (blocktype == "bigram distance"){
        simmat = qlcMatrix::sim.strings(
            strings1 = dat_from[, get(blockvariable)],
            strings2 = dat_to[, get(blockvariable)],
            boundary = TRUE,
            left.boundary = "#", right.boundary = "#") # maybe no right boundary?
        candidate_list = apply(simmat, 1, function(x) which(x > maxsim))
        score_list = apply(simmat, 1, function(x) x[which(x > maxsim)])
    }
    if (blocktype == "idf bigram distance"){
        s1 = qlcMatrix::splitStrings(
            strings = dat_from[, get(blockvariable)], 
            simplify = TRUE,
            boundary = TRUE,
            left.boundary = "#", right.boundary = "#")
        s2 = qlcMatrix::splitStrings(
            strings = dat_to[, get(blockvariable)],
            simplify = TRUE,
            boundary = TRUE,
            left.boundary = "#", right.boundary = "#")
        m = qlcMatrix::jMatrix(rownames(s1), rownames(s2))
        simmat = qlcMatrix::cosSparse((m$M1 * 1) %*% s1, (m$M2 * 1) %*% s2, weight = qlcMatrix::idf)
        candidate_list = apply(simmat, 1, function(x) which(x > maxsim))
        score_list = apply(simmat, 1, function(x) x[which(x > maxsim)])
    }
    if (blocktype == "soundex"){
        candidate_list = lapply(phonetic(dat_from[[blockvariable]]), 
            function(x) which(phonetic(dat_to[[blockvariable]]) %in% x))
    }

    tomerge = dat_to[unlist(candidate_list), ]
    tomerge[, score:= unlist(score_list)]
    tomerge[, linked_to  := rep(dat_from[, get(idvariable_from)], times=sapply(candidate_list, length))]
    dat_from[, linked_from := get(idvariable_from)]

    # all = TRUE means some of idvariable_from idvariable_to can come back as
    # NA. Undesirable, surely? Can never be linked, can only cause mistakes...
    combined = merge(dat_from, tomerge, 
        all = TRUE, 
        allow.cartesian = linktype == "one:one",
        by.x = 'linked_from', by.y = 'linked_to', 
        suffixes = c('_from', '_to'))

    return(combined) # maybe w/o linked_to, only confuses...
}

# make candidates
# requires named list of variables of interest for use down
# the road in calc scores, also to make sure there are no name
# clashses
# maybe this list of names is stupid, because we'll need to 
# enter the pairs in scoring function anyway

# add blocktype == exact?
# add multiple blocks?
# faster way than outer? can be used identically to stringdistmatrix though
# handle maxdist according to type

# more thoughts
# should accomodate one to many and many to one match
# so maybe function(dat_from, dat_to, ..., many = c("none", "to", "from", "both"))
# and then make persids and 
# warn for duplicates if none
# kill only to if from
# kill only from if to
# kill neither if both (would we ever want that)