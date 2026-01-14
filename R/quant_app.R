upload_quant_file_ui <- function(id) {
  ns <- NS(id)
  bslib::page_fillable(
    bslib::layout_columns(
      fileInput(
        "quant_file",
        "Upload Quant File",
        accept = c(".csv", ".txt", ".xml")
      ),

      selectInput(
        "upload_format",
        "Select Method",
        choices = c(
          "TargetLynx XML" = "targetlynx_xml",
          "TargetLynx CSV" = "targetlynx_csv",
          "General CSV/TXT" = "generic"
        ),
        selected = "targetlynx_csv"
      ),

      selectInput(
        ns("method_id"),
        "Select Method ID",
        choices = stats::setNames(.get_methodsdb()$method_id, .get_methodsdb()$method)
      )
    ),
    actionButton(ns("load_quant_btn"), "Load Quant File")
  )
}

res_ui <- function(id) {
  ns <- NS(id)
  bslib::page_fillable(
    bslib::navset_underline(
      bslib::nav_panel(
        title = "Intra-Precision",
        layout_sidebar(
          sidebar = sidebar(
            selectInput(ns("compound_id"), "Compound", choices = NA),
            selectInput(
              ns("filter_type"),
              "Type",
              choices = c("QC", "DQC", "Standard"),
              selected = "QC"
            ),
            numericInput(
              ns("accuracy_threshold"),
              "Accuracy Threshold",
              value = 0.2,
              min = 0,
              max = 1,
              step = 0.05
            ),
          ),
          bslib::card(plotOutput((ns("method_var_plot"))), full_screen = TRUE),
          bslib::layout_columns(
            col_widths = c(12, 12),
            bslib::card(
              reactable::reactableOutput((ns("method_var_naive_table"))),
              full_screen = TRUE
            ),
            bslib::card(
              reactable::reactableOutput((ns("method_var_estim"))),
              full_screen = TRUE
            )
          )
        )
      )
    )
  )
}

res_tab_server <- function(id, quantres, cmpds_vec) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$method_var_plot <- renderPlot({
      req(quantres())

      tryCatch(
        {
          x <- prefilter_precision_data(
            quantres(),
            input$filter_type,
            input$accuracy_threshold,
            input$compound_id
          )
          x <- calc_var_summary(x)
          plot_var_pattern(x, title = input$compound_id)
        },
        error = function(e) {
          showNotification(
            paste("Error in method_var_plot:", e$message),
            type = "error"
          )
        },
        warning = function(e) {
          showNotification(
            paste(e$warning),
            type = "warning"
          )
        }
      )
    })

    output$method_var_naive_table <- reactable::renderReactable({
      req(quantres())
      tryCatch(
        {
          x <- prefilter_precision_data(
            quantres(),
            input$filter_type,
            input$accuracy_threshold,
            input$compound_id
          )
          calc_var_summary(x) |>
            reactable::reactable()
        },
        error = function(e) {
          showNotification(
            paste("Error in method_var_naive_table:", e$message),
            type = "error"
          )
        },
        warning = function(e) {
          showNotification(
            paste(e$warning),
            type = "warning"
          )
        }
      )
    })

    output$method_var_estim <- reactable::renderReactable({
      req(quantres())

      tryCatch(
        {
          x <- prefilter_precision_data(
            quantres(),
            input$filter_type,
            input$accuracy_threshold,
            input$compound_id
          )
          fit_var(x) |>
            dplyr::select(
              "term",
              "est",
              "lwr",
              "upr",
              "method",
              "grad",
              "sd",
              "rse_pct"
            ) |>
            dplyr::mutate(dplyr::across(is.numeric, \(x) round(x, 2))) |>
            reactable::reactable(
              rownames = FALSE,
              columns = list(
                term = reactable::colDef(name = "Term"),
                est = reactable::colDef(name = "Estimate"),
                lwr = reactable::colDef(name = "Lower CI"),
                upr = reactable::colDef(name = "Upper CI"),
                method = reactable::colDef(name = "Method"),
                grad = reactable::colDef(name = "Gradient"),
                sd = reactable::colDef(name = "SE"),
                rse_pct = reactable::colDef(name = "RSE%")
              )
            )
        },
        error = function(e) {
          showNotification(
            paste("Error in method_var_estim:", e$message),
            type = "error"
          )
        },
        warning = function(e) {
          showNotification(
            paste(e$warning),
            type = "warning"
          )
        }
      )
    })
  })
}


pk_ui <- function(id) {
  ns <- NS(id)
  bslib::page_fillable(
    actionButton("update_pk_btn", "Merge"),
    bslib::navset_underline(
      bslib::nav_panel(
        title = "PK Profiles",
        bslib::card(ggiraph::girafeOutput(ns("pk_profs_plot"))),
        full_screen = TRUE
      ),
      bslib::nav_panel(
        title = "PK parameters",
        verbatimTextOutput(ns("pk_parameters_output"))
      ),
      bslib::nav_panel(title = "Exports", p("Export"))
    )
  )
}

pk_server <- function(id, quantres, cmpd_trans_df) {
  # stopifnot(is.reactive(chrom_res))
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$pk_profs_plot <- ggiraph::renderGirafe({
      tryCatch(
        {
          extract_pk_profiles(quantres()) |> quantres()
          plot_pk_profiles(quantres())
        },
        error = function(e) {
          print(e)
        },
        warning = function(e) {
          print(e)
        }
      )
    })

    output$pk_parameters_output <- renderPrint({
      tryCatch(
        {
          nca_table(quantres())
        },
        error = function(e) {
          print(e)
        },
        warning = function(e) {
          print(e)
        }
      )
    })
  })
}


linearity_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_columns(
    col_widths = c(12),
    row_heights = c(1, 1.5, 1.3),
    bslib::layout_columns(
      # top row
      col_widths = c(8, 4),
      row_heights = c(1),
      bslib::card(
        title = "Linearity Settings",
        bslib::layout_column_wrap(
          bslib::layout_columns(
            col_widths = c(5, 3, 3, 4, 4, 4, -3, 4),
            selectInput(ns("compound_id"), "Compound ID", choices = NA),
            selectInput(
              ns("weight"),
              "Weight",
              choices = c(
                "non",
                "1/x^0.5",
                "1/x",
                "1/x^2",
                "1/y^0.5",
                "1/y",
                "1/y^2"
              ),
              selected = "1/x^2"
            ),
            selectInput(
              ns("model"),
              "Model",
              choices = c("linear", "quadratic"),
              selected = "linear"
            ),
            checkboxInput(ns("intercept"), "Intercept", value = FALSE),
            checkboxInput(ns("normalize"), "Normalize", value = FALSE),
            checkboxInput(ns("avg_rep"), "Average Replicates", value = FALSE),
            actionButton(ns("run_linearity_btn"), "Run Linearity"),
            ai_chat_module_ui(ns("linearity_ai"))
          ),
          bslib::card(
            bslib::layout_columns(
              actionButton(ns("sync_linearity_btn"), "Sync"),
              actionButton(ns("exclude_cs_btn"), "Exclude CS"),
              actionButton(ns("include_cs_btn"), "Include CS")
            ),
            verbatimTextOutput(ns("last_points"))
          )
        )
      ),
      bslib::card(
        title = "Summary",
        htmlOutput(ns("linearity_summary"))
      )
    ),
    bslib::layout_columns(
      # middle row
      col_widths = c(4, 4, 4),
      bslib::card(
        title = "Linearity Plot",
        full_screen = TRUE,
        ggiraph::girafeOutput(
          ns("linearity_plot"),
          height = "100%",
          width = "100%"
        )
      ),
      bslib::card(
        title = "Residual Plot",
        full_screen = TRUE,
        ggiraph::girafeOutput(
          ns("residual_plot"),
          height = "100%",
          width = "100%"
        )
      ),
      bslib::card(
        title = "Deviations Plot",
        full_screen = TRUE,
        ggiraph::girafeOutput(
          ns("deviations_plot"),
          height = "100%",
          width = "100%"
        )
      )
    ),
    bslib::card(
      # bottom row
      title = "Linearity Table",
      full_screen = TRUE,
      DT::dataTableOutput(ns("linearity_table"))
    )
  )
}

linearity_data_server <- function(id, quantres, cmpd_df) {
  # stopifnot(is.reactive(chrom_res))
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    cmpd_id <- reactiveVal(NULL)

    output$linearity_table <- renderDT({
      req(quantres())
      input$compound_id |>
        cmpd_id()

      quantres()@linearity[[cmpd_id()]]$linearitytab |>
        select(
          "filename",
          "type",
          "include",
          "abs_response",
          "rel_response",
          "stdconc",
          "estimated_conc",
          "residual_conc",
          "dev_conc",
          "passed"
        ) |>
        DT::datatable(
          options = list(
            scrollX = TRUE,
            scrollY = TRUE,
            dom = "ft",
            pageLength = 2000,
            rownames = FALSE
          )
        )
    })

    # create proxy
    linearity_table_proxy <- DT::dataTableProxy("linearity_table")

    observeEvent(input$run_linearity_btn, {
      req(cmpd_id())
      tryCatch(
        {
          run_linearity(
            quantres(),
            cmpd_id(),
            weight = input$weight,
            model = input$model,
            normalize = input$normalize,
            intercept = input$intercept,
            avg_rep = input$avg_rep
          ) |>
            quantres()
        },
        error = function(e) {
          showNotification(paste("Error: ", e$message), type = "error")
        }
      )
    })

    output$linearity_plot <- ggiraph::renderGirafe({
      req(cmpd_id())
      req(quantres())
      req(has_linearity(quantres(), cmpd_id()))

      plot_linearity(quantres(), cmpd_id())
    })

    output$residual_plot <- ggiraph::renderGirafe({
      req(cmpd_id())
      req(quantres())
      req(has_linearity(quantres(), cmpd_id()))

      plot_residuals(quantres(), cmpd_id())
    })

    output$deviations_plot <- ggiraph::renderGirafe({
      req(cmpd_id())
      req(has_linearity(quantres(), cmpd_id()))
      req(quantres())

      plot_deviations(quantres(), cmpd_id())
    })

    output$linearity_summary <- renderUI({
      req(cmpd_id())
      req(has_linearity(quantres(), cmpd_id()))
      req(quantres())

      table <- tabulate_summary_linearity(quantres(), cmpd_id())

      bslib::card(
        tags$ul(
          tags$h3(paste("Linearity Summary for", cmpd_id())),
          tags$li(paste("Weight:", table$weight)),
          tags$li(paste("Average Replicates:", table$avg_rep)),
          tags$li(paste("Slope (Sensitivity):", table$slope)),
          tags$li(paste("Intercept:", table$intercept)),
          tags$li(paste("Adj. R Squared:", table$adj_r_squared)),
          tags$li(paste("MAPE CS:", table$mape_cs)),
          tags$li(paste("MAPE QC:", table$mape_qc)),
          tags$li(paste("AIC:", table$aic)),
          tags$li(paste("LLOQ (Assumed):", table$lloq_assumed)),
          tags$li(paste("ULOQ (Assumed):", table$uloq_assumed)),
          tags$li(paste("LLOQ (Passed):", table$lloq_passed)),
          tags$li(paste("ULOQ (Passed):", table$uloq_passed)),
          tags$li(paste("LOQ (Calibration SE):", table$loq)),
          tags$li(paste("Standards Passed:", table$standards_passed)),
          tags$li(paste("QCs Passed (Level):", table$QCs_passed_level)),
          tags$li(paste("QCs Passed (Total):", table$QCs_passed_total))
        ),
        full_screen = TRUE
      )
    })

    last_selected_points <- reactiveVal(NULL)

    observeEvent(
      input$linearity_plot_selected,
      {
        linearity_selection <- if (length(input$linearity_plot_selected) == 0) {
          ""
        } else {
          input$linearity_plot_selected
        }
        residual_selection <- if (length(input$residual_plot_selected) == 0) {
          ""
        } else {
          input$residual_plot_selected
        }
        deviations_selection <- if (
          length(input$deviations_plot_selected) == 0
        ) {
          ""
        } else {
          input$deviations_plot_selected
        }

        print("from linearity")
        print(linearity_selection)
        print(residual_selection)
        print(deviations_selection)

        # clear
        if (
          identical(sort(linearity_selection), sort(residual_selection)) ==
            FALSE
        ) {
          # session$sendCustomMessage(type = paste0(id, "-residual_plot_set"), message = character(0))
          session$sendCustomMessage(
            type = paste0(id, "-residual_plot_set"),
            message = input$linearity_plot_selected
          )
        }

        if (
          identical(sort(linearity_selection), sort(deviations_selection)) ==
            FALSE
        ) {
          # session$sendCustomMessage(type = paste0(id, "-deviations_plot_set"), message = character(0))
          session$sendCustomMessage(
            type = paste0(id, "-deviations_plot_set"),
            message = input$linearity_plot_selected
          )
        }

        req(input$linearity_plot_selected == input$residual_plot_selected)
        req(input$linearity_plot_selected == input$deviations_plot_selected)

        last_selected_points(input$linearity_plot_selected)
      },
      ignoreNULL = FALSE
    )

    observeEvent(
      input$residual_plot_selected,
      {
        linearity_selection <- if (length(input$linearity_plot_selected) == 0) {
          ""
        } else {
          input$linearity_plot_selected
        }
        residual_selection <- if (length(input$residual_plot_selected) == 0) {
          ""
        } else {
          input$residual_plot_selected
        }
        deviations_selection <- if (
          length(input$deviations_plot_selected) == 0
        ) {
          ""
        } else {
          input$deviations_plot_selected
        }

        print("from residual")
        print(linearity_selection)
        print(residual_selection)
        print(deviations_selection)

        if (
          identical(sort(residual_selection), sort(linearity_selection)) ==
            FALSE
        ) {
          # session$sendCustomMessage(type = paste0(id, "-linearity_plot_set"), message = character(0))
          session$sendCustomMessage(
            type = paste0(id, "-linearity_plot_set"),
            message = input$residual_plot_selected
          )
        }
        if (
          identical(sort(residual_selection), sort(deviations_selection)) ==
            FALSE
        ) {
          # session$sendCustomMessage(type = paste0(id, "-deviations_plot_set"), message = character(0))
          session$sendCustomMessage(
            type = paste0(id, "-deviations_plot_set"),
            message = input$residual_plot_selected
          )
        }

        req(input$residual_plot_selected == input$linearity_plot_selected)
        req(input$residual_plot_selected == input$deviations_plot_selected)

        last_selected_points(input$residual_plot_selected)
      },
      ignoreNULL = FALSE
    )

    observeEvent(
      input$deviations_plot_selected,
      {
        linearity_selection <- if (length(input$linearity_plot_selected) == 0) {
          ""
        } else {
          input$linearity_plot_selected
        }
        residual_selection <- if (length(input$residual_plot_selected) == 0) {
          ""
        } else {
          input$residual_plot_selected
        }
        deviations_selection <- if (
          length(input$deviations_plot_selected) == 0
        ) {
          ""
        } else {
          input$deviations_plot_selected
        }

        print("from deviations")
        print(linearity_selection)
        print(residual_selection)
        print(deviations_selection)

        if (
          identical(sort(deviations_selection), sort(linearity_selection)) ==
            FALSE
        ) {
          # session$sendCustomMessage(type = paste0(id, "-linearity_plot_set"), message = character(0))
          session$sendCustomMessage(
            type = paste0(id, "-linearity_plot_set"),
            message = input$deviations_plot_selected
          )
        }

        if (
          identical(sort(deviations_selection), sort(residual_selection)) ==
            FALSE
        ) {
          # session$sendCustomMessage(type = paste0(id, "-residual_plot_set"), message = character(0))
          session$sendCustomMessage(
            type = paste0(id, "-residual_plot_set"),
            message = input$deviations_plot_selected
          )
        }

        req(input$deviations_plot_selected == input$linearity_plot_selected)
        req(input$deviations_plot_selected == input$residual_plot_selected)

        last_selected_points(input$deviations_plot_selected)
      },
      ignoreNULL = FALSE
    )

    output$last_points <- renderPrint({
      last_selected_points()
    })

    observeEvent(input$sync_linearity_btn, {
      req(cmpd_id())
      tryCatch(
        {
          sync_linearity(quantres(), cmpd_id()) |> quantres()
        },
        error = function(e) {
          showNotification(paste("Error: ", e$message), type = "error")
        }
      )
    })

    observeEvent(input$exclude_cs_btn, {
      req(last_selected_points())
      req(cmpd_id())

      exclude_linearity(quantres(), cmpd_id(), last_selected_points()) |>
        quantres()
      tryCatch(
        {
          run_linearity(
            quantres(),
            cmpd_id(),
            input$weight,
            input$model,
            input$intercept,
            input$avg_rep
          ) |>
            quantres()
        },
        error = function(e) {
          showNotification(paste("Error: ", e$message), type = "error")
        }
      )
    })

    observeEvent(input$include_cs_btn, {
      req(last_selected_points())
      req(cmpd_id())

      include_linearity(quantres(), cmpd_id(), last_selected_points()) |>
        quantres()

      tryCatch(
        {
          run_linearity(
            quantres(),
            cmpd_id(),
            input$weight,
            input$model,
            input$intercept,
            input$avg_rep
          ) |>
            quantres()
        },
        error = function(e) {
          showNotification(paste("Error: ", e$message), type = "error")
        }
      )
    })

    ai_chat_module_server(
      "linearity_ai",
      chatfunc = chatfunc,
      response_function = linearity_ai,
      response_args = reactive({
        list(
          quantres(),
          cmpd_id()
        )
      }),
      botname = "Linearity Reviewer"
    )
  })
}


quantapp_ui <- function() {
  bslib::page_navbar(
    title = "Quantification App",
    header = shinyjs::useShinyjs(),
    bslib::nav_panel(
      "Upload",
      id = "upload_page",
      upload_quant_file_ui("uploadmod")
    ),
    bslib::nav_panel(
      title = "Suitability",
      id = "suitability_page",
      tabsetPanel(
        type = "tabs",
        id = "suitability_tabs",
        tabPanel(
          "samples",
          bslib::layout_columns(
            selectInput(
              "select_vial_suitability",
              "Select Vial",
              choices = NULL
            ),
            actionButton("suitability_run_btn", "Update")
          ),
          reactable::reactableOutput("suitability_table")
        ),
        tabPanel(
          "suitability",
          bslib::layout_columns(
            width = NULL,
            style = bslib::css(grid_template_columns = "2fr 1fr"),
            # height = "800px",
            bslib::card(
              title = "Suitability Plot",
              full_screen = TRUE,
              bslib::navset_tab(
                id = "suitability_plot_tabs",
                bslib::nav_panel(
                  "Trend",
                  plotOutput("suitability_trend_plot")
                ),
                bslib::nav_panel(
                  "Status",
                  plotOutput("suitability_plot")
                )
              )
            ),
            bslib::card(
              title = "Suitability Text",
              full_screen = TRUE,
              DTOutput("suitability_text"),
              ai_chat_module_ui("suitability_ai")
            )
          )
        ),
      )
    ),
    bslib::nav_panel(
      "Linearity",
      id = "linearity_page",
      # uiOutput("linearity_ui")
      linearity_ui("linearitymod")
    ),
    bslib::nav_panel(
      "Residuals Pattern",
      id = "res_page",
      res_ui("resmod")
    ),
    bslib::nav_panel("Merge", id = "pk_page", pk_ui("pkmod")),
    bslib::nav_panel(
      "Reports",
      id = "exports_settings",
      h2("Exports tab content"),
      DTOutput("exports_table"),
      downloadButton("downloadData", "Download")
    ),
    bslib::nav_menu(
      title = "more",
      align = "right",
      bslib::nav_item(config_module_ui("config")),
      bslib::nav_item(actionButton("exit", "Exit"))
    )
  )
}


quantapp_server <- function(input, output, session) {
  js <- "
    function(el, x, inputName){
      var id = el.getAttribute('id');
      var gd = document.getElementById(id);
      var d3 = Plotly.d3;
      Plotly.update(id).then(attach);
        function attach() {
          var coordinates = [null, null]

          gd.addEventListener('click', function(evt) {
            var xaxis = gd._fullLayout.xaxis;
            var yaxis = gd._fullLayout.yaxis;
            var bb = evt.target.getBoundingClientRect();
            var x = xaxis.p2d(evt.clientX - bb.left);
            var y = yaxis.p2d(evt.clientY - bb.top);
            var coordinates = [x, y];
            Shiny.setInputValue(inputName, coordinates);
          });
        };
  }
  "

  quantobj <- reactiveVal(NULL)
  current_cmpds_names <- reactiveVal(NULL)

  observeEvent(input[["uploadmod-load_quant_btn"]], {
    req(input$quant_file)
    tryCatch(
      {
        df <- read_experiment_results(
          input$quant_file$datapath,
          vendor = input$upload_format
        )
        df <- create_quant_object(
          df,
          method_id = as.numeric(input$`uploadmod-method_id`)
        )

        quantobj(df)
        showNotification("Quant file loaded successfully", type = "message")
      },
      error = function(e) {
        showNotification(e$message, type = "error")
      }
    )
  })

  observeEvent(quantobj(), {
    current_cmpds(quantobj()) |> current_cmpds_names()
  })

  ################################################################################
  ## suitability tab ####
  observeEvent(quantobj(), {
    updateSelectInput(
      session,
      "select_vial_suitability",
      choices = get_vials(quantobj()),
      selected = quantobj()@suitability$config$vial
    )
  })

  observeEvent(input$suitability_run_btn, {
    req(input$select_vial_suitability)
    tryCatch(
      {
        config_suitability(
          quantobj(),
          vial_pos = input$select_vial_suitability
        ) |>
          run_suitability() |>
          quantobj()
      },
      error = function(e) {
        showNotification(e$message, type = "error")
      }
    )
  })

  output$suitability_table <- reactable::renderReactable({
    req(quantobj())
    req(nrow(quantobj()@samples_metadata) > 0)
    req(quantobj()@suitability$suitabilitytab)
    quantobj()@suitability$suitabilitytab |>
      tidyr::pivot_wider(
        names_from = "compound",
        values_from = "abs_response"
      ) |>
      reactable::reactable(
        selection = "single",
        onClick = "select",
        columns = list(
          filename = reactable::colDef(name = "Filename", minWidth = 200),
          include = reactable::colDef(name = "Include", minWidth = 70)
        ),
        defaultColDef = reactable::colDef(
          minWidth = 100,
          format = reactable::colFormat(digits = 2)
        ),
        filterable = FALSE,
        searchable = FALSE,
        sortable = FALSE,
        paginationType = "simple",
        defaultPageSize = 20,
        highlight = TRUE,
        bordered = TRUE,
        striped = TRUE,
        compact = TRUE
      )
  })

  observeEvent(reactable::getReactableState("suitability_table", "selected"), {
    selected_row <- reactable::getReactableState(
      "suitability_table",
      "selected"
    )
    config_suitability(
      quantobj(),
      vial_pos = input$select_vial_suitability,
      start = selected_row
    ) |>
      quantobj()
  })

  ## suitability plot and table ####
  output$suitability_text <- renderDT({
    req(quantobj())
    req(nrow(quantobj()@samples_metadata) > 0)
    quantobj()@suitability[["results"]] |>
      DT::datatable(
        options = list(
          pageLength = 100,
          searching = FALSE,
          paging = FALSE,
          info = FALSE
        )
      )
  })

  output$suitability_plot <- renderPlot({
    req(nrow(quantobj()@samples_metadata) > 0)
    plot_suitability(quantobj())
  })

  output$suitability_trend_plot <- renderPlot({
    req(nrow(quantobj()@samples_metadata) > 0)
    plot_suitability_trend(quantobj())
  })

  ## AI suitability report ####
  ai_chat_module_server(
    "suitability_ai",
    chatfunc = chatfunc(),
    response_function = suitability_ai,
    response_args = reactive({
      list(quantobj())
    }),
    botname = "suitability reviewer"
  )

  ########################################################################################
  #### Linearity tab

  # have navset_tab with each nav_panel the linearity_module. The name is compound_id
  # output$linearity_ui <- renderUI({
  #   req(nrow(isolate(peaksobj()@peaks)) > 0)
  #   bslib::navset_card_tab(
  #     nav_panel(linearity_ui("linearitymod", current_cmpds_df()$compound_trans, selected_cmpd = isolate(input$compound_trans_input)))
  #   )
  # })

  observeEvent(current_cmpds_names(), {
    updateSelectInput(
      session,
      "linearitymod-compound_id",
      choices = current_cmpds_names()
    )
  })

  linearity_data_server("linearitymod", quantobj, current_cmpds_names)
  #############################################################
  observeEvent(current_cmpds_names(), {
    updateSelectInput(
      session,
      "resmod-compound_id",
      choices = current_cmpds_names()
    )
  })

  res_tab_server("resmod", quantobj, current_cmpds_names)

  ###############################################
  pk_server("pkmod", quantobj, current_cmpds_names)

  config_module_server("config")
  # exit button ####
  observeEvent(input$exit, {
    shinyalert::shinyalert(
      "Are you sure you want to exit?",
      type = "warning",
      showConfirmButton = TRUE,
      showCancelButton = TRUE,
      callbackR = function() {
        stopApp(quantobj())
      }
    )
  })
}


#' @title Quantification App
#' @description This function creates a shiny app for quantification after peak integration
#' @export
quant_app <- function() {
  # user input
  app <- shinyApp(
    ui = quantapp_ui(),
    server = function(input, output, session) {
      quantapp_server(input, output, session)
    }
  )
  x <- runApp(app, launch.browser = TRUE, port = 12345)
  return(x)
}
