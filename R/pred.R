#' Predict links
#' 
#' @param dat_candidates. A dataset with link candidates, created using \code{make_candidates}, and with distances calculated using \code{distcalc}. \code{predict_links} expects this dataset to follow the naming conventions of the model you're using (detailed below).
#' 
#' @param id_from
#' @param id_to
#' @param minimum_confidence
#' @param modstring String giving the name of one of the pretrained models. See details below.

#' @return 
#' uses datatable to handle potentially large numer of candidates
#' no need to assign to a new object
#' if you do this, the original dataset is also modified
#' maybe I should fix this?

#' @examples
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