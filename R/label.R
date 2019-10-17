
#' Manually label data
#' 
#' \code{label} wraps a sampling and the candidates function to make manual labelling of training data easier
#' 
#' label takes a random sample from dat_from, gathers candidates from dat_to
#' and presents them to the user to select the match or tell that there is no match
#' 
#' The labelling session is interactive, and the user is presented with a 
#' choice between
#' \itemize{
#'      \item{Persid}{One of the numbers of persid_to}
#'      \item{None}
#' }
#' At some point a "Back" option might be added
#' 
#' After selecting there is an annotation step, that can be done 
#'  \itemize{
#'      \item{Cancel}
#'      \item{Sure}
#'      \item{Maybe}
#'      \item{Doubtful}
#'      \item{Ambiguous}
#' }
#' 
#' @param dat_from a data.table
#' @param dat_to a data.table
#' @param persid_from string identifying the person id variable
#' @param persid_to string identifying the person id variable
#' @param blockvariable string identifying the blocking variable
#' @param N the number of observations to be labelled, defaults to 500
#' @param ... passed to candidates for customised blocking
#' 
#' @return A dataset the labelled pairs persid_from and persid_to
#' 
#' @examples
#' d1 = data.table::data.table(mlast = c("jong", "smid"), mfirst = c("Jan", "Jan"), wfirst = NA, wlast = NA, settlerchildren = NA, persid = c(1:2))
#' d2 = data.table::data.table(mlast = c("jongh", "jong", "smit"), mfirst = c("Jan", "Dirk", "Johan"), wlast = NA, wfirst = NA, settlerchildren = NA, persid = c(1:3))
#' label(d1, d2, "persid", "persid", "mlast", "bigram distance", 2)
#' 
#' @export
label = function(dat_from, dat_to,
    persid_from, persid_to,
    blockvariable,blocktype,
    N, ...){

    # dat_from = opg[year == 1828, ]
    # dat_to = opg[year == 1826]
    dat_from = dat_from[get(persid_from) %in% sample(get(persid_from), N), ]
    dat_from = dat_from[order(get(blockvariable)), ]

    cnd = candidates(
        dat_from = dat_from, 
        dat_to = dat_to,
        blockvariable = blockvariable,
        idvariable_from = persid_from,
        idvariable_to = persid_to,
        blocktype = blocktype,
        maxdist = 0.35,
        ...)

    cnd = distcalc(cnd,
        character_variables = c("mlast", "mfirst", "wlast", "wfirst"),
        numeric_variables = "settlerchildren")


    persid_from_incnd = paste0(persid_from, "_from")
    persid_to_incnd = paste0(persid_to, "_to")

    lbls = data.frame()[0:N, ]
    lbls[, persid_from_incnd] = unique(dat_from[[persid_from]])
    lbls[, persid_to_incnd] = NA

    i = 1
    while (i <= N){
        toshow = cnd[get(persid_from_incnd) == lbls[[persid_from_incnd]][i], ]
        toshow = toshow[
            order(mlastdist, mfirstdist, wfirstdist, wlastdist),
            .(persid_to = get(persid_to_incnd), mlast_from, mlast_to, mfirst_from, mfirst_to,
                                wlast_from, wlast_to, wfirst_from, wfirst_to,
                                settlerchildren_from, settlerchildren_to)]
        print(as.data.frame(toshow))

        
        answer = readline("Please enter the relevant persid, or tell if there is [Nn]one")
        if (answer == "N" | answer == "n" | answer == "none"){
            cat("Alas, moving on", "\n\n")
            lbls[i, persid_to_incnd] = NA
            i = i + 1
        } else if (answer %in% toshow[[persid_to_incnd]]){
            cat("Alright! Added", answer, "\n\n")
            lbls[i, persid_to_incnd] = answer
            i = i + 1
        } else {
            cat("Please answer N/n/none or a valid ", substitute(persid_to_incnd), "\n\n")
        }
        print(lbls)
    }
    return(lbls)
}
