#' Create initials from a string of names
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
initials <- function(strings){
    out = stringi::stri_extract_all_regex(
        str = strings, 
        pattern = "^[A-z]|\\s[A-z]|[.][A-z]",
        simplify = FALSE)
    out = lapply(out, stringi::stri_replace_all_regex, "[ .]", "")
    sapply(out, paste0, collapse = "")
}

# 
# 

initials_old <- function(strings){
    intls <- gregexprr("^[A-z]|\\s[A-z]|[.][A-z]", strings)
    intls <- gsub('[ .]', '', intls)
    return(paste0(intls, collapse=''))
}


# stringi::stri_extract_all_boundaries(strings, "")

# paste0(initials(strings), collapse = '')
# initials_old(strings)

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

