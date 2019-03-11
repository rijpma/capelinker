#' Check whether dataset is ready for linkage
#' 
#' \code{preflight} checks whether a dataset is ready for 
#' linkage and prints the results.
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
#'          Note that some analphabetics (for instance "." or "-") could be frequently and consistently applied in the data. String distance calculation does not fail on accented letters, but it does count towards the string distance.
#' }
#' 
#' Currently, capelinker expects the dataset to be about couples. That is, data about 
#' husband and spouse is used for linkage because together they contain far more 
#' information than a single individual. The pretrained data is done on marriage and the 
#' parents baptism registries.
#' 
#' Later versions will include models based on the opgaafrolle, but these too usually 
#' provide husband and spouse names so linkage will work best on data with couples.
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
#'      \item \code{mprof} the male profession
#' }
#' 
#' For the opgaafrollen.
#' \itemize{
#'  \item \code{wifepresent}
#'  \item \code{wifeinboth}
#'  \item \code{spousenamedist}
#'  \item \code{namefreq}
#' }
#' 
#' The following models can be used 
#' \itemize{
#'  \item \code{m_rf_baptisms_sparse} a model linking parents in baptism records to marriage records, based on minimal information: male surname (mlast), male first name (mfirst), female first name (wfirst, female surname not used because it would typically not be reported in the baptism records), and year of marriage/baptism (year).
#'  \item \code{m_rf_baptisms_full} a model linking parents in baptism and marriage records, using additional information: initials, profession, and soundex distances of the names. Performance is not much better than the sparse model.
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
preflight = function(dat){
    vrbs_baptisms_sparse = c("mlast", "mfirst", "wfirst", "year")
    vrbs_baptisms_full = c("mlast", "mfirst", "wfirst", "winitials", "minitials", "mprof", "year")

    missing_v_sparse = setdiff(vrbs_baptisms_sparse, names(dat))
    missing_v_full = setdiff(vrbs_baptisms_full, names(dat))

    cat("Missing for baptisms sparse:", missing_v_sparse, sep = "\n")
    cat("Missing for baptisms full:", missing_v_full, sep = "\n")

    present_v_sparse = setdiff(vrbs_baptisms_sparse, missing_v_sparse)
    present_v_full = setdiff(vrbs_baptisms_full, missing_v_full)

    classes = c(
        mlast = "character",
        mfirst = "character",
        wfirst = "character",
        winitials = "character",
        minitials = "character",
        mprof = "character",
        year = "numeric"
    )

    dat_classes = sapply(dat, class)

    # classes_should_be = classes[names(classes) %in% names(dat_classes)] # [match(names(dat_classes), names(classes))]
    classes_are = dat_classes[names(dat_classes) %in% names(classes)] # [match(names(dat_classes), names(classes))]

    print(data.frame(classes_are = classes_are[names(classes)],
        classes_should_be = classes))

    cat("\nShare missing variables (cannot be matched):\n")
    print(dat[, lapply(.SD, function(x) mean(is.na(x), na.rm = TRUE)), 
        .SDcols = present_v_sparse])
    cat("\n\n")
    
    cat('Share empty variables ("") (matching may fail):\n')
    print(dat[, lapply(.SD, function(x) mean(nchar(x) == 0, na.rm = TRUE)), 
        .SDcols = present_v_sparse])
    cat("\n\n")
    
    cat('Share length one string variables ("*") (matching may fail):\n')
    print(dat[, lapply(.SD, function(x) mean(nchar(x) == 1, na.rm = TRUE)), 
        .SDcols = present_v_sparse])
    cat("\n\n")

    # these regex need a hard look
    # or just always convert to lower case
    cat('Share Sentence Case (matching requires consistency in case between datasets):\n')
    print(dat[, lapply(.SD, function(x) mean(grepl("[A-Z][a-z]+", x), na.rm = TRUE)), 
        .SDcols = present_v_sparse])
    cat("\n\n")

    cat('Share UPPER CASE (matching requires consistency in case between datasets):\n')
    print(dat[, lapply(.SD, function(x) mean(grepl("^[A-Z ]+$", x), na.rm = TRUE)), 
        .SDcols = present_v_sparse])
    cat("\n\n")

    cat('Share lower case (matching requires consistency in case between datasets):\n')
    print(dat[, lapply(.SD, function(x) mean(grepl("^[a-z ]+$", x), na.rm = TRUE)), 
        .SDcols = present_v_sparse])
    cat("\n\n")

    cat('Share accented letters and non-alphabetic or symbols per variable (matching might require consistency):\n')
    print(dat[, lapply(.SD, function(x) mean(grepl("[^A-z ]", x))), .SDcols = present_v_sparse])
    print(
        lapply(dat[, .SD, .SDcols = present_v_sparse], function(column){
            unique(c(stringi::stri_extract_all_regex(column, "[^A-z ]", simplify = TRUE, omit_no_match = TRUE)))
        })
    )
    cat("\n\n")

    cat('Range (important for numeric variables):\n')
    print(dat[, lapply(.SD, range, na.rm = TRUE), .SDcols = present_v_sparse])
    cat("\n\n")

    # mfirst has middle names?


    # dataset names, both numeric and character
    # from found names to 
        # enough names
        # printable ascii
            # accented characters removal?
        # missing values
            # number of observations as share of dataset
            # too many observations
        # upper/lower case
        # mfirst has middle names?
        # actually numeric
            # range of numeric

    # and check rest of data cleaning
}

# lapply(pretrained_models, function(x) x$variables)
# lapply(pretrained_models, `[`, "variables")