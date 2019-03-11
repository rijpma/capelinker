#' Predict links
#' 
#' \code{predict links} takes a dataset of candidate links and distances, to predict links and select the best ones.
#' 
#' uses datatable to handle potentially large numer of candidates
#' no need to assign to a new object
#' if you do this, the original dataset is also modified
#' maybe I should fix this?
#' 
#' The following models can be used 
#' \itemize{
#'  \item \code{m_rf_baptisms_sparse} a model linking parents in baptism records to marriage records, based on minimal information: male surname (mlast), male first name (mfirst), female first name (wfirst, female surname not used because it would typically not be reported in the baptism records), and year of marriage/baptism (year).
#'  \item \code{m_rf_baptisms_full} a model linking parents in baptism and marriage records, using additional information: initials, profession, and soundex distances of the names. Performance is not much better than the sparse model.
#' }
#' 
#' @param dat_candidates. A dataset with link candidates, created using \code{make_candidates}, and with distances calculated using \code{distcalc}. \code{predict_links} expects this dataset to follow the naming conventions of the model you're using (detailed below).
#' 
#' @param id_from string giving the identifier variable in the candidates dataset for the observations from the \code{_from} dataset.
#' @param id_to string giving the identifier variable in the candidates dataset for the observations from the \code{_to} dataset.
#' @param minimum_confidence the minimum confidence level (vote share) to return a link. Defaults to 0.5.
#' @param modstring String giving the name of one of the pretrained models, either m_rf_baptisms_sparse or m_rf_baptisms_full. See details below.
#' 
#' @return The candidates dataset filtered down to only the best links for each record in the original \code{_from} dataset.
#' Also some details about one:one and many:one
#'
#' @export
predict_links = function(dat_candidates, id_from, id_to,
    minimum_confidence = 0.5, 
    modstring = c("m_rf_baptisms_full", "m_rf_baptisms_sparse"),
    linktype = c("one:one", "many:one"),
    some_measure_of_other_close_matches = "not_implemented"){

    linktype = match.arg(linktype)

    # check model name in dat_candidates
    # take model from list using that string?
    # nasty
    # better to ask for the actual object, but then users must handle lists
    # for now
    model = pretrained_models[[modstring]]$model


    dat_candidates$predicted = randomForest:::predict.randomForest(model, 
                                    newdata = dat_candidates, 
                                    type = "prob")[, 2]

    dat_candidates[, rank_from := rank(-predicted), by = get(id_from)]
    dat_candidates[, rank_to := rank(-predicted), by = get(id_to)]

    if (linktype == "many:one"){
        out = dat_candidates[rank_from == 1 & predicted > minimum_confidence]
    } else if (linktype == "one:one"){
        out = dat_candidates[rank_from == 1 & rank_to == 1 & predicted > minimum_confidence]
    }

    return(out)
}

# calculate link probs, allowing for chunking
# what did I mean by chunking? Doing year for year in the opgaafrollen? Should that be in here or in some sort of panellink function?