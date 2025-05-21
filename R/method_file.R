.parse_cmpds <- function(path){
    checkmate::assert_file_exists(path)
    res <- yaml::read_yaml(path)
    res$compounds <- do.call(rbind,
        lapply(res$compounds, function(x){
            data.frame(compound = x$cmpd, q1 = x$q1, q3 = x$q3, qualifier = x$qualifier)
        }))
    res
}

.save_cmpd_db <- function(cmpds_list){
    .check_sample_db()

    # check there is one method, description and compounds
    checkmate::assertNames(names(cmpds_list),
        must.include = c("method", "description", "gradient", "compounds"), type = "unique")
    checkmate::assertDataFrame(cmpds_list$compounds)
    # drop empty rows
    cmpds_list$compounds <- cmpds_list$compounds[!is.na(cmpds_list$compounds$compound), ]

    # compound names doen't have to be unique

    # assert combination of q1 and q3 is unique to be saved in transition table

    db <- .connect_to_db()
    # create new method ID
    q <- DBI::dbGetQuery(db, "SELECT MAX(method_id) FROM methodstab") |> as.numeric() |> max()
    method_id <- ifelse(is.na(q), 1, q+1)


    unique_methods_df <-  data.frame(method_id = method_id) |>
        dplyr::mutate(method = cmpds_list$method) |>
        dplyr::mutate(method_descr = cmpds_list$description) |>
        dplyr::mutate(method_gradient = cmpds_list$gradient) |>
        dplyr::distinct()

    stopifnot(nrow(unique_methods_df) == 1) # only single method passed from this call.

    # create method tab
    unique_trans_df <- cmpds_list$compounds |>
        dplyr::select("q1", "q3") |>
        dplyr::arrange("q1", "q3") |>
        dplyr::distinct() |>
        dplyr::mutate(method_id = method_id) |>
        dplyr::mutate(method_gradient = cmpds_list$gradient) |>
        dplyr::mutate(transition_label = paste0(.data$q1, " > ", .data$q3)) |>
        dplyr::mutate(transition_id = paste0("T", dplyr::row_number()))

    # avoid repeated transitions in the same method
    checkmate::assertVector(unique_trans_df$transition_label, unique = TRUE)

    checkmate::assertNames(names(unique_methods_df),
        must.include = c("method_id", "method_descr", "method_gradient"))

    # create sequence for autoincrement compound_id
    cmpd_id <- seq(1, nrow(cmpds_list$compounds))
    cmpd_id <- paste0("C", cmpd_id)

    # join transition_id to compoundstab
    transitions_df <- cmpds_list$compounds |>
        dplyr::left_join(unique_trans_df, by = c("q1", "q3"))


    # Begin a transaction
    DBI::dbBegin(db)

    tryCatch({
        # Add trans first to check if they were added before adding the entire method.
        DBI::dbAppendTable(db, "transtab", unique_trans_df)

        # Add to methodstab
        DBI::dbAppendTable(db, "methodstab", unique_methods_df)

        # Add to compoundstab. Add all compound names, but only one method_id
        DBI::dbAppendTable(db, "compoundstab",
            transitions_df |>
                dplyr::mutate(method_id = method_id) |>
                dplyr::mutate(compound_id = cmpd_id) |>
                dplyr::select(
                    "method_id",
                    "compound_id",
                    "compound",
                    "qualifier",
                    "transition_id")
        )

        # Commit the transaction if all operations succeed
        DBI::dbCommit(db)
    }, error = function(e) {
        # Roll back the transaction if any operation fails
        DBI::dbRollback(db)
        stop("Transaction failed: ", e$message)
    })


    duckdb::dbDisconnect(db, shutdown = TRUE)

}

#' Load methods database
#' @noRd
.get_methodsdb <- function(){
    .check_sample_db()
    db <- .connect_to_db()
    methods <- DBI::dbReadTable(db, "methodstab")
    duckdb::dbDisconnect(db, shutdown = TRUE)
    methods
}

.get_method_cmpds <- function(method_id){
    .check_sample_db()
    db <- .connect_to_db()
    cmpds <- DBI::dbGetQuery(db, paste0("SELECT * FROM compoundstab WHERE method_id = ", method_id)) |> 
        as.data.frame()
    
    if(nrow(cmpds) == 0){
        stop("No compounds found for method_id ", method_id)
    }

    transitionsdf <- DBI::dbGetQuery(db, paste0("SELECT * FROM transtab WHERE method_id = ", method_id)) |> 
        as.data.frame() |> 
        dplyr::select(-"method_id")
    duckdb::dbDisconnect(db, shutdown = TRUE)

    cmpds |>
        dplyr::left_join(transitionsdf, by = "transition_id")

}

.get_method_id <- function(method){
    .check_sample_db()
    db <- .connect_to_db()
    method_id <- DBI::dbGetQuery(db, paste0("SELECT method_id FROM methodstab WHERE method = '", method, "'")) |> as.numeric()
    duckdb::dbDisconnect(db, shutdown = TRUE)
    method_id
}
