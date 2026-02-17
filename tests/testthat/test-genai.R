skip_on_cran()
skip_on_ci()

### chromatography 
test_that("integrate_ai returns expected structure", {
    skip_if_no_py()

    # Test integrate_ai directly
    result <- integrate_ai(
        main, 
        transition_id = 1, 
        sample_id = 4, 
        peak_start = 0.5, 
        peak_end = 0.7
    )

    # Check structure
    expect_type(result, "list")
    expect_true("peak_start" %in% names(result))
    expect_true("peak_end" %in% names(result))
    expect_true("observed_retention_time" %in% names(result))
    expect_true("flagged" %in% names(result))
    expect_true("comment" %in% names(result))

    # Check types
    expect_type(result$flagged, "logical")
    expect_type(result$comment, "character")

    # Check bounds are numeric and reasonable
    if (!is.na(result$peak_start)) {
        expect_true(is.numeric(result$peak_start))
        expect_true(result$peak_start >= 0)
    }
    if (!is.na(result$peak_end)) {
        expect_true(is.numeric(result$peak_end))
        expect_true(result$peak_end > result$peak_start)
    }
})

test_that("integrate_ai handles no peak scenario", {
    skip_if_no_py()

    # Test with range where no peak exists
    result <- integrate_ai(
        main, 
        transition_id = 1, 
        sample_id = 4, 
        peak_start = 5.0, 
        peak_end = 6.0
    )

    # When no peak is found, should be flagged
    expect_true(result$flagged)
    expect_type(result$comment, "character")
    expect_true(nchar(result$comment) > 0)
})

test_that(".integrate_individual_slack with AI mode works", {
    skip_if_no_py()

    # Set up expected bounds first
    chrom_with_bounds <- update_RT(
        main, 
        compound_id = 1,
        sample_id = NULL,
        peak_start = 0.5,
        peak_end = 0.7,
        target = "all",
        mode = "manual"
    )

    # Test AI integration at lower level
    result <- PKbioanalysis:::.integrate_individual_slack(
        chrom_with_bounds,
        compound_id = 1,
        sample_id = 4,
        peak_start = 0.5,
        peak_end = 0.7,
        mode = "ai"
    )

    expect_s4_class(result, "ChromRes")
    
    # Check that peak was integrated
    peak_data <- result@peaks %>%
        dplyr::filter(compound_id == 1, sample_id == 4)
    
    expect_equal(nrow(peak_data), 1)
    expect_false(is.na(peak_data$observed_peak_start))
    expect_false(is.na(peak_data$observed_peak_end))
    expect_true(is.character(peak_data$comment))
    expect_true(is.logical(peak_data$flag))
})

test_that("AI mode updates comment and flag correctly", {
    skip_if_no_py()

    result <- update_RT(
        main, 
        compound_id = 1,
        sample_id = 4, 
        peak_start = 0.5,
        peak_end = 0.7,
        target = "single",
        mode = "ai"
    )

    peak_data <- result@peaks %>%
        dplyr::filter(compound_id == 1, sample_id == 4)

    # AI should have added a comment
    expect_true(nchar(peak_data$comment) > 0)
    
    # Check flag is set (logical)
    expect_type(peak_data$flag, "logical")
    
    # Check bounds were refined by AI
    expect_false(is.na(peak_data$observed_peak_start))
    expect_false(is.na(peak_data$observed_peak_end))
    expect_true(peak_data$observed_peak_start >= 0.5)
    expect_true(peak_data$observed_peak_end <= 0.7)
})

test_that("AI mode works with all_next target", {
    skip_if_no_py()

    # Set expected bounds first
    chrom_with_bounds <- update_RT(
        main, 
        compound_id = 1,
        sample_id = NULL,
        peak_start = 0.5,
        peak_end = 0.7,
        target = "all",
        mode = "manual"
    )

    # Use AI for all_next
    result <- update_RT(
        chrom_with_bounds, 
        compound_id = 1,
        sample_id = 4, 
        peak_start = 0.5,
        peak_end = 0.7,
        target = "all_next",
        mode = "ai"
    )

    # Check multiple samples were processed
    peak_data <- result@peaks %>%
        dplyr::filter(compound_id == 1, sample_id >= 4)

    expect_true(nrow(peak_data) > 1)
    
    # All should have comments from AI
    expect_true(all(!is.na(peak_data$comment)))
    expect_true(all(nchar(peak_data$comment) > 0))
})

test_that("chrom_integration works end-to-end", {
    skip_if_no_py()

    plot_chrom(main, transitions_ids = 1, sample_id = 4, smoothed = TRUE)
    
    y <- update_RT(main, 
        compound_id = 1,
        sample_id = 4, 
        peak_start = 0.5,
        peak_end = 0.7,
        target = "single",
        mode = "ai")
    
    plot_chrom(y, transitions_ids = 1, sample_id = 4, smoothed = TRUE, integrated = TRUE, show_RT = TRUE)

    # Verify integration happened
    expect_true(is_integrated(y, compound_id = 1, sample_id = 4))
})
