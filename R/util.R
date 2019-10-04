#' Supplement an existing linking index with new links.
#' 
#' \code{exand_index}supplements an existing linking index using a 
#' new set of proposed links.
#' 
#' @param dat a dataset containing a variable index which is to be supplemented, and a variable candidate_index for the indexes which are to supplement it
#' 
#' @examples
#' dat = data.table(
#'      index = c(1, 1, 1, 2, 2, NA, 2, NA, NA, NA, 4, 4, NA),
#'      index_candidate = c(5, 5, NA, 6, 6, 6, 6, 7, 7, NA, 8, 8, 8))
#' expand_index(dat)
#' @export
expand_index = function(dat){ 

    # if all candidate not yet indexed, assign new index from persid
    dat[!is.na(index_candidate), 
        allna := all(is.na(index)), 
        by = index_candidate]
    dat[!is.na(index_candidate) & allna == T, 
        index := index_candidate]

    # if all candidate indexed, do nothing
    dat[!is.na(index_candidate), 
        allpresent := !anyNA(index), 
        by = index_candidate]
    dat[allpresent == T, 
        index := index] # just do nothing?

    # if candidate has more than one index and any NA, set old index
    dat[!is.na(index_candidate), 
        bridge := anyNA(index) & length(unique(na.omit(index))) == 1, 
        by = index_candidate]

    dat[!is.na(index_candidate) & bridge == T, 
        index:= ifelse(is.na(index), unique(na.omit(index)), index), 
        by = list(index_candidate)]
    # safer version: actual interpolation?
    # dat[!is.na(index_candidate) & bridge == T, 
    #     index := as.integer(zoo::na.approx(index, na.rm = FALSE)),
    #     by = list(index_candidate)]

    return(dat)
}

#' Split out prefixes from surname strings
#' 
#' \code{split_prefixes()} uses a list of common South African prefixes
#'  to split out prefixes from surnames
#' 
#' @param strings a vector of strings containing surnames
#' @param more_prefixes a vector of additional prefixes that are not
#' currently in the list already
#' 
#' @examples
#' split_prefixes(strings = c("VAN DER MOLEN", "DUMOULIN", "DU MOULIN"))
#' split_prefixes(strings = c("Van der Molen", "DUMOULIN", "DU MOULIN"))
#' 
#' @export
split_prefixes = function(strings, more_prefixes = c()){

    to_remove = c(
        "VNA DEN", "VEN", "OVER", "VAND DER", "AN", "DE LA", "VANN DER", 
        "VON", "TER", "VAN DE", "LA", "LE", "DE", "DU", "VAN DER", 
        "VAN", "VAN[.]", "VD", "PU", "VAN DEN", "VAN DER")
    to_remove = c(to_remove, more_prefixes)
    to_remove = unique(to_remove)
    to_remove = to_remove[order(-nchar(to_remove))] # longest first to extract those first
    pattern = paste0("", "^", to_remove, " ", collapse = "|")
    # pattern = paste0("\\b(", pattern, ")\\b")

    return(
        data.frame(
            prefix = stringi::stri_extract_first_regex(
                strings, pattern, case_insensitive = TRUE),
            string_wo_prefix = stringi::stri_replace_first_regex(
                strings, pattern, "", case_insensitive = TRUE),
            stringsAsFactors = FALSE
        )
    )
}


#' Create initials from a string containing names
#' 
#' \code{initials} takes a string containing names, seperated either by spaces of periods, and returns the initials.
#' 
#' @param strings string containing names, seperated by spaces or periods (or both). Vectorised.
#' 
#' @return Initials in the form JF, so no spaces and no periods.
#' 
#' @examples
#' strings = c("Auke B.", "Arnold Christoffer", "Arend X. Mark", "Albert X Johannes", "Albert X.Y. Johannes", "J. F.")
#' initials(strings)
#' @export
initials <- function(strings, return_NA_on_empty = FALSE){
    out = stringi::stri_extract_all_regex(
        str = strings, 
        pattern = "^[A-z]|\\s[A-z]|[.][A-z]",
        simplify = FALSE,
        omit_no_match = TRUE) # if TRUE, empty list -> "" below

    out = lapply(out, stringi::stri_replace_all_regex, "[ .]", "")

    if(return_NA_on_empty){
        out[sapply(out, length) == 0] = NA
    } else if (!return_NA_on_empty){
        out[sapply(out, length) == 0] = ""
    }

    sapply(out, stringi::stri_join, collapse = "")
}

#' Uniformise strings
#' 
#' This is not very effective because of the use of stringdistmat
#' but at least it does it on the unique names
#' 
#' @export
uniformise_string <- function(string, maxdist=0.2, quiet=FALSE){
    str_srtd <- names(sort(-table(string)))
    n_start <- length(str_srtd)
    strmat <- stringdistmatrix(str_srtd, str_srtd, method='jw', p=0.1)
    fill <- NULL

    # find similar strings, replace with one, drop those from strmat, and repeat
    while(nrow(strmat) > 0){
        ind <- strmat[1, ] < maxdist
        similar_strs <- str_srtd[ind]
        str_srtd <- str_srtd[!ind]
        strmat <- strmat[!ind, !ind, drop=FALSE]
        string[string %in% similar_strs] <- similar_strs[1]
        if (length(similar_strs) > 1 & !quiet){
            cat(similar_strs, sep=', ')
            cat('----->')
            cat(similar_strs[1], '\n')
        }
    }
    cat('From', n_start, ' to ', length(unique(string)), '\n')
    return(string)
}

#' @export
normalise <- function(x){
    if (is.vector(x)){
        (x - min(x, na.rm=T)) / (max(x, na.rm=T) - min(x, na.rm=T))
    } else {
        do.call(cbind, lapply(x, normalise))
    }
}

# 
# 

initials_old <- function(strings){
    intls <- gregexprr("^[A-z]|\\s[A-z]|[.][A-z]", strings)
    intls <- gsub('[ .]', '', intls)
    return(paste0(intls, collapse=''))
}

#' Remove diacretics from letters
#' 
#' \code{rm_diacretics} replaces letters with diacretics (like "é") with letters without diacretics (like "e"). \code{iconv(..., to = ASCII//TRANSLIT)} might also work, but fails for me sometimes.
#' 
#' Removing diacretics might be useful if they are being used inconsistently in the data being linked. Differences in diacretics count in the calculation of string distances.
#' 
#' The list of diacretics is currently far from complete, only what I encountered in the baptism and marriage records I looked at.
#' 
#' @param strings string containing names, seperated by spaces or periods (or both). Vectorised.
#' 
#' @return Initials in the form JF, so no spaces and no periods.
#' 
#' @examples
#' rm_diacretics(strings = "éå")
#' iconv("éå", "UTF-8", "ASCII//TRANSLIT") # bit unpredicatble for me
#' 
#' @export
rm_diacretics = function(strings){
    stringi::stri_replace_all_fixed(strings,
        c("é", "ë", "ê", "è", "â", "á", "à", "å", "ü", "û", "î", "ï", "ç"),
        c("e", "e", "e", "e", "a", "a", "a", "a", "u", "u", "i", "i", "c"),
        vectorize_all = FALSE)
}

#' Generate random strings resembling old strings
#' 
#' \code{rand_strings_like} creates new strings with the same distributions of letters and string lengths as a vector of supplied strings. Needs to be at least length two.
#' 
#' @param strings_like a vector of strings that the random strings should resemble
#' 
#' @export
rand_strings_like = function(strings_like, n_samples = 1){
    m = lm(ncharacters ~ 1, data = data.frame(ncharacters = nchar(strings_like)))
    charcount = table(unlist(strsplit(strings_like, "")))
    replicate(
        n = n_samples, 
        expr = paste0(
            sample(
                x = names(charcount), 
                prob = charcount, 
                size = abs(rnorm(n_samples, m$coef[1], summary(m)$sigma)),
                replace = TRUE), 
            collapse = ""))
}

#' Calculate closets string distance to another string in a vector
#' @export
stringdist_closest = function(string){
    # if only one string, unique so highest possible distance (there is no rival)
    if (length(string) == 1) return(1)

    distmat = stringdist::stringdistmatrix(string, string, method = "jw")
    diag(distmat) = NA # skip self-self
    return(apply(distmat, 2, function(column){
        ifelse(all(is.na(column)), NA, min(column, na.rm = TRUE))
        }
    ))
}

