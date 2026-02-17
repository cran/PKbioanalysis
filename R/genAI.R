chatfunc <- function() {
  chat <- ellmer::chat_openai(
    base_url = get_pkbioanalysis_option("api_base_url"),
    model = get_pkbioanalysis_option("ai_model"),
    api_key = get_pkbioanalysis_option("api_key"),
    params = ellmer::params(
      temperature = get_pkbioanalysis_option("temperature")
    ),
    system = "You are experienced bioanalytical researcher to interpret bioanalytical data according to best practices and regulatory guidelines. 
                Give concise less than 200 words reports. Use titles, bullets and highlight important parts in bold. Mark risky items in red. Answer in markdown format. 
                Your final bullet point is a single yes/no if the data is acceptable for conclusive decision making or not. For example, 'AI agent approves: No'. 
                You are allowed to say 'I don't know' if you are not sure about something and ask for more information.
                "
  )
  chat
}

suitability_ai <- function(chat, quantres) {
  stopifnot(has_suitability_results(quantres))

  x <- quantres@suitability$results

  chat$stream_async(
    paste(
      "The suitability is judged based on stabilization of instrument response over multiple runs",
      "Give hints if you suspect any experimental issues based on RSD, number of replicates, etc.",
      "After how many runs did the instrument equilibrate?",
      jsonlite::toJSON(quantres@suitability[["results"]])
    )
  )
}

linearity_ai <- function(chat, quantres, compound_id) {
  checkmate::assertClass(quantres, "QuantRes")

  if (!has_linearity(quantres, compound_id)) {
    stop("Linearity not found. Please run linearity first.")
  }
  x <- quantres@linearity[[compound_id[1]]]
  prompt <-
    paste(
      "Give hints if you suspect any experimental issues based on %dev, accuracy, precision, back-calculated concentration, etc.",
      "show table with parameter/ value/ comment",
      "Comment on intercept",
      jsonlite::toJSON(quantres@linearity[[compound_id]]$results[-1]),
      # jsonlite::toJSON(quantres@linearity[[compound_id]]$results$modelobj),
      jsonlite::toJSON(
        quantres@linearity[[compound_id]]$linearitytab |>
          dplyr::filter(.data$type %in% c("Standard", "QC"))
      )
    )

  chat$stream_async(prompt)
}

integrate_ai <- function(
  chrom_res,
  transition_id,
  sample_id,
  peak_start,
  peak_end
) {
  chat <- ellmer::chat_openai(
    base_url = get_pkbioanalysis_option("api_base_url"),
    model = get_pkbioanalysis_option("ai_model"),
    api_key = get_pkbioanalysis_option("api_key"),
    params = ellmer::params(temperature = 0),
    system = "You are bioanalyst looking into chromatographic data"
  )

  intensities <- .filter_peak(
    chrom_res,
    transition_id = transition_id,
    samples_ids = sample_id,
    peak_start = 0,
    peak_end = NULL,
    smoothed = FALSE
  ) |>
    select(1, 2)
  colnames(intensities) <- c("time", "Signal")
  prompt <- paste(
    "What is the observed retention time, peak start and peak end for this chromatogram? Look for the between peak signal roughly between ",
    peak_start,
    "and ",
    peak_end,
    " minutes respectively.",
    "Here is the observed chromatographic intensities (time, intensity) in JSON format:",
    jsonlite::toJSON(intensities),
    "Return only json string with the following fields: observed_retention_time (most intense point), peak_start, peak_end, flagged (TRUE if peak is not acceptable, FALSE if peak is acceptable), and a short",
    "comment in 50 words max commenting on the peak shape, peak start, peak end, and observed retention time (most intense point), and if the peak is acceptable or not (flagged).",
    "A peak is considered a peak if it has width between 0.05 and 0.5 minutes maximum",
    "Also, peak max should be at least 1M to be considered a peak.", 
    "S/N is defined as the ratio of the most intense point in the peak to the average intensity of the baseline (the region between 0.5 minutes before peak start and 0.5 minutes after peak end).", 
    "A peak is considered a peak if it has a clear apex, reasonable peak width, and more importantly, the S/N is above 10 from surrounding baseline. If the S/N is below 10, it should be flagged as not acceptable. If the peak is not acceptable, give a brief comment on why it is not acceptable.",
    "If no peak observed return flagged TRUE, write brief comment, and return NA for the rest of the json fields. Not a single word not in json format."
  )

  res <- jsonlite::fromJSON(chat$chat(prompt))

  # chat$chat_structured(
  #   "extract the information as a json object with the following fields",
  #   type = ellmer::type_object(
  #     observed_retention_time = ellmer::type_integer(required = FALSE),
  #     peak_start = ellmer::type_integer(required = TRUE),
  #     peak_end = ellmer::type_integer(required = FALSE),
  #     flagged = ellmer::type_boolean(required = TRUE),
  #     comment = ellmer::type_string()
  #   )
  # )

  
  res
}


studydesign_ai <- function(chat, study_id) {
  study <- retrieve_full_study_log(study_id)
  samples <- retrieve_full_study_log(study_id)

  prompt <- paste(
    "You are an expert in clinical trials design.",
    "Given the following study details and sample log, provide a concise summary of the study design, including key elements such as balancing, blocking, control groups, and sampling strategy.",
    "If the study is InVitro, ignore randomization.",
    "Give suggestions to improve the study design if needed.",
    "Give suggestion for interim analysis plan.",
    "Highlight any potential issues or areas for improvement in the study design.",
    "Note study subject type is ",
    get_study_subject_type(study_id),
    jsonlite::toJSON(study),
    jsonlite::toJSON(samples)
  )
  chat$stream_async(prompt)
}

plate_ai <- function(chat, plate) {
  df <- plate@df
  prompt <- paste(
    "You are an expert in bioanalytical assay design and execution.",
    "Given the following plate map, provide a concise summary of the assay design, including key elements such as controls, replicates, and sample distribution.",
    "Is there is suitability vial in type?",
    "Is there is QCs associated with each calibration curve that is covering the entire calibration range and follows regulatory guidelines?",
    "Highlight any potential issues or areas for improvement in the plate design.",
    jsonlite::toJSON(df)
  )
  chat$stream_async(prompt)
}

injeclist_ai <- function(chat, df) {
  prompt <- paste(
    "You are an expert in bioanalytical assay execution.",
    "Given the following injection list, provide a concise summary of the assay execution, including key elements such as run order, blanks, and sample distribution.",
    "Highlight any potential issues or areas for improvement in the injection list design.",
    jsonlite::toJSON(df)
  )
  chat$stream_async(prompt)
}


# ai_chat_module_ui.R
ai_chat_module_ui <- function(id, title = "AI Assistant") {
  ns <- NS(id)
  shinyWidgets::actionBttn(
    ns("invoke_btn"),
    paste("Open", title),
    icon = icon("robot"),
    style = "float",
    color = "primary",
    size = "sm"
  )
}


# ai_chat_module_server.R
ai_chat_module_server <- function(
  id,
  chatfunc,
  response_function,
  response_args,
  botname
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    currchat <- reactiveVal(NULL)

    observeEvent(input$invoke_btn, {
      req(response_args()[[1]]) # ensure args are available
      tryCatch(
        {
          progress <- shiny::Progress$new()
          on.exit(progress$close())
          progress$set(message = "Invoking AI...", value = 0)

          # Init chat session
          chat <- chatfunc()
          currchat(chat)

          # Show modal with shinychat UI
          removeModal()
          showModal(modalDialog(
            title = paste0("AI Assistant - ", botname),
            shinychat::chat_ui(id = ns("chat")),
            easyClose = TRUE,
            size = "l"
          ))

          # Get dynamic args from reactive
          args <- response_args()
          response <- do.call(response_function, c(list(chat), args))
          shinychat::chat_append(id = "chat", response)
        },
        error = function(e) {
          showNotification(
            paste("Error invoking AI:", e$message),
            type = "error"
          )
        }
      )
    })

    # Handle user input to chat
    observeEvent(input$chat_user_input, {
      req(currchat())
      chat <- currchat()
      stream <- chat$stream_async(input$chat_user_input)

      shinychat::chat_append(id = "chat", stream)
    })
  })
}
