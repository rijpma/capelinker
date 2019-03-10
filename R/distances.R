# create distances etc in some sort of smart way 
# make/take pairs of variables
    # actually let's have candidates compare a sparse one, and
    # then you just say what the ID is?
# allow on/off for soundex

#' Calculate distances between character and numeric variables
#' 
#' \code{distcalc} calculates string distances and numeric distances
#' 
#' @param dat a dataset. \code{distcalc} expects this dataset 
#' to follow the naming conventions of the \code{candidates} 
#' function: the variables from the original datasets have the 
#' same name and are distinguished using \code{_from} and 
#' \code{_to} suffixes.
#' @param character_variables. The names of the character variables, without the suffixes. 
#' @param numeric_variables. The names of the numeric variables, without the suffixes. 
#' @return the dataset with the necessary distances to predict links. Note that reassignment of the data.table is not necessary. The original dataset is modified in place
#' 
#' uses datatable to handle potentially large numer of candidates
#' no need to assign to a new object
#' if you do this, the original dataset is also modified
#' maybe I should fix this?
#' @examples
#' @export
distcalc = function(dat, 
    character_variables = c("mlast", "mfirst", "wfirst", "minitials", "winitials", "mprof"),
    numeric_variables = c("year")){

    for (vrb in character_variables){
        dat[, 
            paste0(vrb, "dist") := stringdist::stringdist(
                get(paste0(vrb, "_from")), 
                get(paste0(vrb, "_to")),
                method = "jw", p = 0.1)]
        dat[, 
            paste0(vrb, "sdx") := stringdist::stringdist(
                get(paste0(vrb, "_from")), 
                get(paste0(vrb, "_to")),
                method = "soundex")]
    }
    for (vrb in numeric_variables){
        dat[, 
            paste0(vrb, "dist") := 
                get(paste0(vrb, "_from")) - get(paste0(vrb, "_to"))]
    }
    return(dat)
}
