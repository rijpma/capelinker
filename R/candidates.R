

# docs
# @param dat_from the dataset from which to match. To make 
#' Create a dataset of candidate links from two datasets.
#' 
#' @param dat_from a data.table
#' @param dat_to a data.table
#' @param idvariable_from String giving the identifier variable in dat_from.
#' @param idvariable_to String giving the identifier variable in dat_from.
#' @param blockvariable String giving the name of the blocking varariable. Should be present in both datasets. Defaults to "mlast", the male surname.
#' @param blocktype Type of blocking: string distance (default) or numeric.
#' @param linktype Should there be no more than one record in each dataset that can be linked (one:one), or is it possible for multiple records in \code{dat_from} to be linked to \code{dat_to}
#' @param maxdist Maximum distance (0-1) to consider a record a candidate. Defaults to 0.15 for male surname string distance. If using numeric distance (for instance year of birth), very different values could be needed.
#' 
#' @return A dataset containing all candidate pairs, and all columns in dat_from and dat_to. Columns with the same name will get a suffix "_from" or "_to".
#' 
#' blocking on multiple variables currently not supported, but could be done by using \code{candidates()} repeatedly and merging the results
#' 
#' string distance blocking is done using jaro-winkler string distances, ranging from 0 (perfect match) to 1 (completely dissimilar)
#' 
#' currently possible to return missing values when for a given record, no candidate is found. Should probably be fixed
#' 
#' @examples
#' d1 = data.table::data.table(mlast = c("jong", "smid"), persid = c(1:2))
#' d2 = data.table::data.table(mlast = c("jongh", "jong", "smit"), persid = c(1:3))
#' candidates(d1, d2)
#'  
#' @export
candidates = function(dat_from, dat_to, 
    idvariable_from = "persid", idvariable_to = "persid",
    blockvariable = "mlast", blocktype = c("string distance", "numeric"), 
    linktype = c("one:one", "many:one"),
    maxdist = 0.15){

    linktype = match.arg(linktype)
    blocktype = match.arg(blocktype)

    stopifnot(data.table::is.data.table(dat_from))
    stopifnot(data.table::is.data.table(dat_to))

    stopifnot(nrow(dat_from) > 0)
    stopifnot(nrow(dat_to) > 0)

    if (blocktype == "string distance"){
        distmat = stringdist::stringdistmatrix(
            a = dat_from[, get(blockvariable)],
            b = dat_to[, get(blockvariable)],
            method = 'jw', p = 0.1)
        candidate_list = apply(distmat, 1, function(x) which(x < maxdist))
    }
    if (blocktype == "numeric"){
        distmat = outer(
            X = dat_from[, get(blockvariable)],
            Y = dat_to[, get(blockvariable)],
            FUN = "-")
        candidate_list = apply(distmat, 1, function(x) which(abs(x) < maxdist))
    }

    # handle maxdist according to type

    tomerge = dat_to[unlist(candidate_list), ]
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

# more thoughts
# should accomodate one to many and many to one match
# so maybe function(dat_from, dat_to, ..., many = c("none", "to", "from", "both"))
# and then make persids and 
# warn for duplicates if none
# kill only to if from
# kill only from if to
# kill neither if both (would we ever want that)