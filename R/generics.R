#' Create Injection Sequence
#'
#' @param plate PlateObj object
#' @param method choose method from database
#' @param repeat_std number of re-injections for calibration standards. Default is 1.
#' @param repeat_analyte number of re-injections for unknown samples. Default is 1
#' @param repeat_qc number of re-injections for QC wells. Default is 1
#' @param blank_after_top_conc If TRUE, adding blank after high concentrations of standards and QCS.
#' @param blank_at_end If True, adding blank at the end of queue.
#' @param rep_suitability Number of re-injections for suitability vial.
#' @param blank_every_n If no QCs, frequency of injecting blanks between analytes.
#' @param injec_vol volume of injection in micro liters.
#' @param descr Run description.
#' @param suffix string to be added to the end of the filename. Default is "1".
#' @param prefix string at the beginning of the filename. Default is today's date.
#' @param n_explore A number of exploratory samples to be injected at the top of the entire sequence. Default is 0
#' @param tray Location in sample manager.
#' @param conc_df data.frame matching compound name to a scaling factor. Maximum 20 compounds allowed.
#'
#' @details
#' n_explore controls if exploratory samples are to be injected. A random sample from each CS and QC group will be sampled along with 1 blank sample.
#' @returns InjecListObj object
#'@export
setGeneric(
    "build_injec_seq",
    function(
        plate,
        method,
        rep_DB = 2,
        rep_ISblank = 1,
        rep_suitability = 1,
        rep_blank = 2,
        repeat_std = 1,
        repeat_qc = 1,
        repeat_analyte = 1,
        repeat_dqc = 1,
        n_explore = 0,
        blank_after_top_conc = TRUE,
        blank_at_end = TRUE,
        blank_every_n = NULL,
        injec_vol,
        descr = "",
        prefix = Sys.Date(),
        suffix = "1",
        tray = 1,
        conc_df = NULL,
        grouped = TRUE
    ) {
        standardGeneric("build_injec_seq")
    }
)

setGeneric("get_plate_a_groups", function(plate) {
    standardGeneric("get_plate_a_groups")
})

#' This will save the plate to the database
#' @param plate PlateObj object or MultiPlate object
#' @returns PlateObj object or list of PlateObj objects
#' @export
setGeneric("register_plate", function(plate) standardGeneric("register_plate"))

#' Check if peak was integrated for a specific compound
#' @param chrom_res ChromRes or ChromResBase object
#' @param compound_id Compound ID
#' @param sample_id Sample ID. If NULL, all samples are checked
#' @return logical
#' @export
#' @examples
#' \dontrun{
#' lapply(1:10, \(x) is_integrated(chrom_res, sample_id = 1, compound_id = 1))
#' }
setGeneric("is_integrated", function(chrom_res, compound_id, sample_id = NULL) {
    standardGeneric("is_integrated")
})


#' @param x ChromRes or QuantRes object
#' @noRd
#' @author Omar I. Elashkar
setGeneric("get_vials", function(x) standardGeneric("get_vials"))

#' Run linearity for a specific compound
#' @param quantres QuantRes object
#' @param compound_id Compound ID
#' @param weight Weighting method. Default is "1/x^2"
#' @param model Model type. Default is "linear"
#' @param intercept Include intercept in the model. Default is TRUE
#' @param normalize Normalize response. Default is FALSE
#' @param response Response type. Default is "abs_response"
#' @param avg_rep Average replicates. Default is FALSE
#' @return Updated QuantRes object with linearity results
#' @author Omar I. Elashkar
#' @noRd
setGeneric(
    "run_linearity",
    function(
        quantres,
        compound_id,
        weight = "1/x^2",
        model = "linear",
        intercept = TRUE,
        normalize = FALSE,
        response = "abs_response",
        avg_rep = FALSE
    ) {
        standardGeneric("run_linearity")
    }
)

#' @author Omar Elashkar
#' @noRd
setGeneric(
    "tabulate_summary_linearity",
    function(object, compound_id = NULL) {
        standardGeneric("tabulate_summary_linearity")
    }
)


#' Tabulate linearity summary for a list (single compound)
#' @author Omar I. Elashkar
#' @noRd
setGeneric(
    "tabulate_summary_linearity_list",
    function(object) standardGeneric("tabulate_summary_linearity_list")
)


#' Filter data
#' @param x Dataframe or QuantRes Object
#' @param type QC, DQC, or Standard
#' @param acc_cutoff Accuracy cutoff. 20\% by default 
#' @param dev_cutoff Deviation cutoff. 20\% by default
#' @param compound_id Compound ID to filter. If NULL, all compounds are considered
#' @return Filtered data
#' @author Omar I. Elashkar
#' @export
setGeneric(
    "prefilter_precision_data",
    function(x, type, acc_cutoff = 0.2, dev_cutoff = 0.2, compound_id = NULL) {
        standardGeneric("prefilter_precision_data")
    }
)
