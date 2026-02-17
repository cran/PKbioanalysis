test_that("pkmerge_works", {

    has_pk_profiles(quantobj, "MITRAGYNINE") |> expect_false()
    has_pk_profiles(quantobj, "7OH-MITRA") |> expect_false()
    
    pkmerge(quantobj) |> expect_error()

    # write dummy sample log and injecseq
    write_test_pkset()

    # import quant
    lapply(names(quantobj@linearity),
        \(i) has_linearity(quantobj, i))

    # run linearity 
    suppressWarnings({
        x <- run_linearity(quantobj, compound_id = "MITRAGYNINE") |>
            expect_no_error()
        
        x <- run_linearity(x, compound_id = "7OH-MITRA") |>
            expect_no_error()
    }
    )   
    
    lapply(names(x@linearity),
        \(i) has_linearity(x, i)) |> unlist() |> sum() |> expect_equal(2)


    res <- pkmerge(x) |> expect_no_error()
    res@pkdata$MITRAGYNINE |> expect_class("data.frame")

    has_pk_profiles(res, "MITRAGYNINE") |> expect_true()
    has_pk_profiles(res, "7OH-MITRA") |> expect_true()


    plotconfig <- expand.grid(
                            shape = c(NA, "dil"),
                            stratify_by = c(NA, "group_label", "route", "extra_factors", "subject_id", "sex"), 
                            compound_id = c("MITRAGYNINE", "7OH-MITRA"),
                            stringsAsFactors = FALSE
                            )

    suppressWarnings({
        for(i in seq_len(nrow(plotconfig))){
            print(i)
            row <- plotconfig[i,]
            expect_no_error({
                plot_pk_profiles(res, compound_id = row$compound_id, 
                                stratify_by = row$stratify_by, 
                                shape = row$shape)
            })
        }

    plot_pk_profiles(res)  |> expect_no_error()
    
    })

    
    export_pk_profiles(res, "MITRAGYNINE", "NONMEM") |> expect_no_error()

    nca_table(res, "MITRAGYNINE") |> expect_class("data.frame")

}
)
