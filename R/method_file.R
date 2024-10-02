.parse_cmpds <- function(path){
    checkmate::assert_file_exists(path)
    res <- yaml::read_yaml(path)
    res
}

.save_cmpd_db <- function(cmpds_list){
    .check_sample_db()

    # check there is one method, description and compounds
    checkmate::assertNames(names(cmpds_list),  
        must.include = c("method", "description", "compounds"), type = "unique")
    # assert unique names of list 
    checkmate::assertNames(cmpds_list$compounds, type = "unique")

    # drop empty strings
    cmpds_list$compounds <- cmpds_list$compounds[cmpds_list$compounds != ""]

    db <- .connect_to_db()

    # create new method ID 
    q <- DBI::dbGetQuery(db, "SELECT MAX(method_id) FROM methodstab") |> as.numeric() |> max()
    method_id <- ifelse(is.na(q), 1, q+1)

    # add to methodstab 
    DBI::dbAppendTable(
        db, "methodstab",
        data.frame(method_id = method_id,
                     method = cmpds_list$method,
                     method_descr = cmpds_list$description)
    )
    # create sequence for autoincrement compound_id
    cmpd_id <- seq(1, length(cmpds_list$compounds))
    cmpd_id <- paste0("C", cmpd_id)
    # add to compoundstab. add all compound names, but only one method_id
    DBI::dbAppendTable(db, "compoundstab", 
        data.frame(compound_id = cmpd_id, 
            method_id = method_id, 
            compound = cmpds_list$compounds)
    )

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
    cmpds <- DBI::dbGetQuery(db, paste0("SELECT * FROM compoundstab WHERE method_id = ", method_id))
    duckdb::dbDisconnect(db, shutdown = TRUE)
    as.data.frame(cmpds)
}

.get_method_id <- function(method){
    .check_sample_db()
    db <- .connect_to_db()
    method_id <- DBI::dbGetQuery(db, paste0("SELECT method_id FROM methodstab WHERE method = '", method, "'")) |> as.numeric()
    duckdb::dbDisconnect(db, shutdown = TRUE)
    method_id
}
