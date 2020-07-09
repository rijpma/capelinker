# todo: make this take a modstring variable

#' Check whether dataset is ready for linkage
#' 
#' \code{preflight} checks whether a dataset is ready for linkage
#' 
#' Preflight does a number of checks on the dataset.
#' \itemize{
#'      \item Whether the required variables are present and correctly named.
#'      \item Whether the variables are of the correct type.
#'      \item The share of missing observations by variable (these cannot be matched)
#'      \item The share of length one observations by variable (these might break linkage)
#'      \item The share of case (lower, upper, title) by variable. \code{capelinker} 
#'          currently does not automatically convert to a single case, but leaves this 
#'          choice to the user. Different cases do count for the string distance.
#'      \item The share of accented letters and non-alphabetic symbols by variable. A
#'          gain, these are not automatically fixed, but left to the user's discretion. 
#'          Note that some analphabetics (for instance "." or "-") could be frequently 
#'          and consistently applied in the data. String distance calculation does not 
#'          fail on accented letters, but it does count towards the string distance.
#' }
#' 
#' Currently, capelinker expects the dataset to be about couples. That is, data about 
#' husband and spouse is used for linkage because together they contain far more 
#' information than a single individual. The pretrained data is done on marriage and the 
#' parents baptism registries.
#' 
#' The following variables are expected by the functions and models:
#' \itemize{
#'      \item \code{mlast} the male surname.
#'      \item \code{mfirst} the male first name.
#'      \item \code{wlast} the female last name.
#'      \item \code{wfist}the female first name.
#'      \item \code{minitils} male initials in the form JF (so no K, and no punctuation)
#'      \item \code{winitils} female initials in the form JF (so no K, and no punctuation)
#'      \item \code{year} the year of observation of the two records
#' }
#' 
#' For the baptisms
#' \itemize{
#'  \item \code{mprof} the male profession
#' }
#' 
#' For the opgaafrollen.
#' \itemize{
#'  \item \code{wifepresent}
#'  \item \code{wifeinboth}
#'  \item \code{spousenamedist}
#'  \item \code{namefreq}
#'  \item \code{settlerchildrendist}
#' }
#' 
#' 
#' The following models can be used 
#' \itemize{
#'  \item \code{m_rf_baptisms_sparse} a model linking parents in baptism records to marriage records, based on minimal information: male surname (mlast), male first name (mfirst), female first name (wfirst, female surname not used because it would typically not be reported in the baptism records), and year of marriage/baptism (year).
#'  \item \code{m_rf_baptisms_full} a model linking parents in baptism and marriage records, using additional information: initials, profession, and soundex distances of the names. Performance is not much better than the sparse model.
#'  \item \code{opgaafrol_full} a model linking households from one year to the next
#' }
#' 
#' @param dat the dataset to check, as a data.table.
#' 
#' @return Text to the console showing the results of the tests.
#' 
#' @examples
#' d2 = data.table::data.table(mlast = c("jongh", "Jong", "smit (Smid)"), persid = c(1:3))
#' preflight(d2)
#' 
#' @import data.table
#' @export
preflight = function(dat,
    modstring = c("m_rf_baptisms_sparse", "m_rf_baptisms_full", "opgaafrol_full")){

    modstring = match.arg(modstring)

    pattern = "dist$|sdx$"

    data("pretrained_models")

    vrbs = pretrained_models[[modstring]]$variables
    vrbs = gsub(pattern, "", vrbs[grepl(pattern, vrbs)])

    # vrblist = list(
    #     m_rf_baptisms_sparse = c("mlast", "mfirst", "wfirst", "year"),
    #     m_rf_baptisms_full = c("mlast", "mfirst", "wfirst", "winitials", "minitials", "mprof", "year"),
    #     opgaafrol_full = c("mlast", "mfirst", "wfirst", "wlast", "winitials", "minitials", "settlerchildren")
    # )
    # vrbs = vrblist[[modstring]]


    vrbs_missing = setdiff(vrbs, names(dat))

    cat("Missing for ", modstring, ": \n",vrbs_missing, 
        "\n--------------\n")

    vrbs_present = setdiff(vrbs, vrbs_missing)

    expected_classes = c(
        mlast = "character",
        mfirst = "character",
        wlast = "character",
        wfirst = "character",
        winitials = "character",
        minitials = "character",
        mprof = "character",
        settlerchildren = "numeric",
        year = "numeric"
    )

    actual_classes = sapply(dat, class)

    actual_classes = actual_classes[names(actual_classes) %in% names(expected_classes)]

    print(
        data.frame(
            expected_classes,
            actual_classes = actual_classes[names(expected_classes)]))

    cat("\nShare missing variables (cannot be matched):\n")
    print(dat[, 
        lapply(.SD, function(x) mean(is.na(x), na.rm = TRUE)), 
        .SDcols = vrbs_present])
    cat("\n\n")
    
    cat('Share empty variables ("") (matching may fail):\n')
    print(dat[, 
        lapply(.SD, function(x) mean(nchar(x) == 0, na.rm = TRUE)), 
        .SDcols = vrbs_present])
    cat("\n\n")
    
    cat('Share length one string variables ("*") (matching may fail):\n')
    print(dat[, 
        lapply(.SD, function(x) mean(nchar(x) == 1, na.rm = TRUE)), 
        .SDcols = vrbs_present])
    cat("\n\n")

    cat("Share multiword string variables (models expect joint middle names or joint prefixes)")
    print(dat[, 
        lapply(.SD, function(x) mean(stringi::stri_count_words(x) > 1)),
        .SDcols = vrbs_present])
    cat("\n\n")

    # these regex need a hard look
    # or just always convert to lower case
    cat('Share Sentence Case (matching requires consistency in case between datasets):\n')
    print(dat[, lapply(.SD, function(x) mean(grepl("[A-Z][a-z]+", x), na.rm = TRUE)), 
        .SDcols = vrbs_present])
    cat("\n\n")

    cat('Share UPPER CASE (matching requires consistency in case between datasets):\n')
    print(dat[, lapply(.SD, function(x) mean(stringi::stri_trans_toupper(x) == x, na.rm = TRUE)), 
        .SDcols = vrbs_present])
    cat("\n\n")

    cat('Share lower case (matching requires consistency in case between datasets):\n')
    print(dat[, lapply(.SD, function(x) mean(stringi::stri_trans_tolower(x) == x & x != "", na.rm = TRUE)), 
        .SDcols = vrbs_present])
    cat("\n\n")

    cat('Share accented letters and non-alphabetic symbols per variable (matching might require consistency):\n')
    print(dat[, 
        lapply(.SD, function(x) mean(grepl("[^A-z ]", x))), 
        .SDcols = vrbs_present])
    cat("\n\n")
    
    cat('Accented letters and non-alphabetic symbols\n')
    print(
        lapply(dat[, .SD, .SDcols = vrbs_present], function(column){
            stringi::stri_flatten(
                unique(
                    c(stringi::stri_extract_all_regex(column, "[^A-z ]", simplify = TRUE, omit_no_match = TRUE))
                ),
            na_empty = TRUE)
        })
    )
    cat("\n\n")

    cat('Range (important for numeric variables):\n')
    print(dat[, 
        lapply(.SD, range, na.rm = TRUE), 
        .SDcols = vrbs_present])
    cat("\n\n")



        # mfirst has middle names?
}
