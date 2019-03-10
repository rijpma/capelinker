# utils

# initials_old <- function(strings){
#     # return first letter of each word in a string
#     # surely easier with stringi

#     intls <- gregexprr("^[A-z]|\\s[A-z]|[.][A-z]", strings)
#     intls <- gsub('[ .]', '', intls)
#     return(paste0(intls, collapse=''))
# }


initials <- function(strings){
    out = stringi::stri_extract_all_regex(
        str = strings, 
        pattern = "^[A-z]|\\s[A-z]|[.][A-z]",
        simplify = FALSE)
    out = lapply(out, stringi::stri_replace_all_regex, "[ .]", "")
    sapply(out, paste0, collapse = "")
}

# strings = c("Auke B.", "Arnold Christoffer", "Arend X. Mark", "Albert X Christ")
# initials(strings)

# stringi::stri_extract_all_boundaries(strings, "")

# paste0(initials(strings), collapse = '')
# initials_old(strings)

replace_accents = function(strings){
    stringi::stri_replace_all_fixed(strings,
        c("é", "ë", "ê", "è", "â", "á", "à", "ü", "û", "î", "ï", "ç"),
        c("e", "e", "e", "e", "a", "a", "a", "u", "u", "i", "i", "c"),
        vectorize_all = FALSE)
}

# replace_accents(strings = "éå")
# iconv("éå", "UTF-8", "ASCII//TRANSLIT") # bit unpredicatble
