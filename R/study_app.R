clean_rht_to_df <- function(mylist) {
  cleaned <- lapply(mylist, function(row) {
    lapply(row, function(x) if (is.null(x)) NA else x)
  })
  cleaned <- lapply(cleaned, \(x) {
    x <- as.data.frame(x)
    colnames(x) <- paste("col", seq_along(x), sep = "_")
    x
  })
  cleaned <- do.call(rbind, cleaned)
  cleaned
}


orderdf <- function(df, cols, dirs = "asc") {
  # cols <- names(sortlist)
  # dirs <- sapply(sortlist, \(x) x)
  dirs <- rep(dirs, length(cols)) # for now only

  ordering_list <- mapply(
    function(col, dir) {
      vec <- df[[col]]
      if (dir == "desc") {
        return(-xtfrm(vec)) # use -xtfrm for general (e.g., strings, factors)
      } else {
        return(xtfrm(vec))
      }
    },
    cols,
    dirs,
    SIMPLIFY = FALSE
  )

  df <- df[do.call(order, ordering_list), ]
  rownames(df) <- NULL
  df
}


remove_old_ui <- function() {
  removeUI(selector = "#dynamic_ui", immediate = TRUE)
}


plate_plot_module_ui <- function(id, str) {
  popover(
    bsicons::bs_icon("gear"),
    selectInput(
      paste0(str, "_color_toggle"),
      "Color By",
      choices = c(
        "Spiked Concentration" = "conc",
        "Analytical Group" = "group",
        "Dilution" = "dil",
        "Study" = "study",
        "Nominal Time" = "time",
        "Factor" = "factor",
        "Samples" = "samples",
        "Arm" = "arm",
        "Sex" = "sex",
        "Dose" = "dose",
        "Route" = "route",
        "Matrix" = "matrix"
      )
    ),
    selectInput(
      paste0(str, "_transform_dilution"),
      "Transform Dilution",
      choices = c(TRUE, FALSE),
      selected = FALSE
    ),
    numericInput(
      paste0(str, "_font_size"),
      "Font Size",
      value = 1,
      step = 0.2
    ),
    bslib::input_switch(
      paste0(str, "_study_name_switch"),
      "Show Study Name",
      value = FALSE
    ),
    bslib::input_switch(
      paste0(str, "_arm_switch"),
      "Show Arm",
      value = TRUE
    ),
    bslib::input_switch(
      paste0(str, "_time_switch"),
      "Show time",
      value = TRUE
    ),
    bslib::input_switch(
      paste0(str, "_factor_switch"),
      "Show Factor",
      value = TRUE
    ),
    bslib::input_switch(
      paste0(str, "_sex_switch"),
      "Show Sex",
      value = FALSE
    ),
    bslib::input_switch(
      paste0(str, "_dose_switch"),
      "Show Dose",
      value = FALSE
    ),
    bslib::input_switch(
      paste0(str, "_use_subject_id_switch"),
      "Use Subject ID",
      value = FALSE
    ),
    bslib::input_switch(
      paste0(str, "_dil_label_switch"),
      "Use Dilution Label",
      value = TRUE
    ),
    title = "Plate Display Config"
  )
}


injec_seq_block_protocol_ui <- function(id, number) {
  ns <- NS(id)

  equi <- fluidRow(
    column(
      width = 3,
      selectizeInput(
        ns(paste0("equi_vial_prot", number)),
        "Equi Vial",
        choices = c("A1")
      )
    ),
    column(
      width = 3,
      numericInput(ns(paste0("equi_n_prot", number)), "Equi N", value = 5)
    ),
    column(
      width = 3,
      numericInput(
        ns(paste0("equi_vol_prot", number)),
        "Equi Vol",
        value = 0.5
      )
    )
  )

  bslib::accordion_panel(
    title = paste0("Protocol ", number),
    value = paste0("protocol_", number),
    if (number == 1) NULL else equi,
    selectInput(
      ns(paste0("a_group", number)),
      "Analytical Group",
      choices = c("A", "B", "C", "D"),
      multiple = TRUE
    ) |>
      bslib::tooltip("Not supported. All groups will be included"),
    selectInput(
      ns(paste0("inlet_method_select_prot", number)),
      "Inlet Method",
      choices = NA
    ),
    numericInput(
      ns(paste0("exploratory_samples_alg_prot", number)),
      "Exploratory Samples",
      value = 0
    ) |>
      bslib::tooltip(
        "Exploratory samples are samples that are not part of the sample list. They are used to check the system"
      ),
    p("Repeats"),
    fluidRow(
      column(
        width = 3,
        numericInput(
          ns(paste0("repeat_std_prot", number)),
          "Standard",
          value = "1"
        )
      ),
      column(
        width = 3,
        numericInput(
          ns(paste0("repeat_sample_prot", number)),
          "Sample",
          value = "1"
        ) |>
          bslib::tooltip(
            "Number of sample injections. Currently working only if there are no QCs"
          )
      ),
      column(
        width = 3,
        numericInput(ns(paste0("repeat_qc_prot", number)), "QC", value = "1") |>
          bslib::tooltip("Not working :(")
      ),
      column(
        width = 3,
        numericInput(
          ns(paste0("rep_suitability_number_prot", number)),
          "Suitability",
          value = "3"
        ) |>
          bslib::tooltip(
            "Number of suitability injections. Must set to 0 or remove it if not in the plate"
          )
      ),
      column(
        width = 6,
        bslib::input_switch(
          ns(paste0("blank_after_top_conc_prot", number)),
          "Blank after top conc",
          value = TRUE
        )
      ),
      column(
        width = 6,
        bslib::input_switch(
          ns(paste0("blank_at_end_prot", number)),
          "Blank at end",
          value = TRUE
        )
      ),
      column(
        width = 6,
        numericInput(
          ns(paste0("blank_every_n_prot", number)),
          "Blank every n analytes",
          value = "20"
        )
      ),
      column(
        width = 6,
        numericInput(
          ns(paste0("injec_vol_prot", number)),
          "Injection Volume",
          value = "10"
        )
      ),
      column(
        width = 12,
        textInput(
          ns(paste0("descr_prot", number)),
          "Description",
          value = ""
        ) |>
          bslib::tooltip(
            "Description of each injection. You can modify individually from the table"
          )
      ),
      column(
        width = 6,
        textInput(ns(paste0("suffix_prot", number)), "Suffix", value = "1")
      )
    )
  )
}

injec_seq_block_server <- function(
  id,
  number,
  methodsdb,
  currplate,
  current_cmpd_df,
  lock_export
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    observeEvent(methodsdb(), {
      updateSelectInput(
        session = shiny::getDefaultReactiveDomain(),
        inputId = paste0("inlet_method_select_prot", number),
        choices = methodsdb()$method
      )
    })

    observeEvent(currplate(), {
      updateSelectInput(
        session = shiny::getDefaultReactiveDomain(),
        inputId = paste0("a_group", number),
        choices = get_plate_a_groups(currplate()),
        selected = get_plate_a_groups(currplate())
      )
    })

    # for each protcol add, extra protocol accordion panel
    #### methods
    observeEvent(methodsdb(), {
      updateSelectInput(
        session = shiny::getDefaultReactiveDomain(),
        inputId = paste0("inlet_method_select_prot", number),
        choices = methodsdb()$method
      )
    })

    observeEvent(input$inlet_method_select_prot1, {
      req(input$inlet_method_select_prot1)
      method_id <- .get_method_id(input$inlet_method_select_prot1)
      req(method_id)
      .get_method_cmpds(method_id) |>
        dplyr::mutate(method = input$inlet_method_select_prot1, ratio = 1) |>
        dplyr::select("method", "compound", "ratio") |>
        distinct() |>
        current_cmpd_df()
    })

    # nav_hide("sample_list_nav", "Export")
    # disable and clear export sample list on any change till click regenerate again.
    observeEvent(
      c(
        input[[paste0("repeat_std_prot", number)]],
        input[[paste0("repeat_qc_prot", number)]],
        input[[paste0("repeat_sample_prot", number)]],
        input[[paste0("rep_suitability_number_prot", number)]],
        input[[paste0("blank_after_top_conc_prot", number)]],
        input[[paste0("blank_at_end_prot", number)]],
        input[[paste0("blank_every_n_prot", number)]],
        input[[paste0("injec_vol_prot", number)]],
        input[[paste0("descr_prot", number)]],
        input[[paste0("suffix_prot", number)]],
        input[[paste0("tray_prot", number)]],
        input[[paste0("exploratory_samples_alg_prot", number)]],
        current_cmpd_df()
      ),
      {
        req(number >= 1)
        # nav_hide("sample_list_nav", "Export")
        # nav_hide("sample_list_nav", "Summary")
        # nav_hide("sample_list_nav", "Sample List")
        lock_export(TRUE) # FIXME introduce a loop bug, but without it the tables will not clear
      }
    )
  })
}


ui <- bslib::page_navbar(
  title = "Study Management",
  header = shinyjs::useShinyjs(),
  bslib::nav_panel(title = "Dashboard", uiOutput("plate_creation_ui")),
  bslib::nav_panel(
    title = "Study Design",
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 600,
        actionButton("create_new_study_btn", "Create New Study"),
        ai_chat_module_ui(id = "study_ai", title = "Study Design Assistant"),
        reactable::reactableOutput("studies_db_RT"),
        # divider
        tags$hr(),
        h4("Predefined Designs"),
        actionButton("metabolic_study_gen_btn", "Metabolic Study"),
        actionButton(
          "cells_metabolic_stability_btn",
          "Cells Metabolic Stability"
        ),
      ),
      bslib::navset_card_pill(
        nav_panel(
          "Study Overview",
          reactable::reactableOutput("study_overview_RT")
        ),
        nav_panel(
          "Arms",
          bslib::layout_columns(
            col_widths = c(4, 4),
            uiOutput("update_arms_db_btn_ui"),
            actionButton("download_arms_btn", "Download")
          ),
          rhandsontable::rHandsontableOutput("studyarms_RT")
        ),
        nav_panel(
          "Subjects",
          bslib::layout_columns(
            col_widths = c(4, 4, 4),
            actionButton(
              "update_subjects_btn",
              "Save",
              width = "50%",
              icon = icon("save")
            ),
            actionButton("download_subjects_btn", "Download"),
            actionButton("autofill_subjects_btn", "Autofill")
          ),
          rhandsontable::rHandsontableOutput("subjects_RT")
        ),
        nav_panel(
          "Sample Log",
          bslib::layout_columns(
            col_widths = c(4, 4, 4),
            actionButton(
              "update_sample_log_btn",
              "Save",
              width = "50%",
              icon = icon("save")
            ),
            actionButton("download_sample_log_btn", "Download"),
            actionButton("autofill_sample_log_btn", "Autofill")
          ),
          rhandsontable::rHandsontableOutput("sample_log_RT")
        ),
        nav_panel(
          "Study Chart",
          card(
            full_screen = TRUE,
            card_header("Study Chart"),
            div(
              DiagrammeR::grVizOutput(
                "study_chart_plot",
                width = "100%",
                height = "100%"
              ),
              tags$script(shiny::HTML('panzoom($(".grViz").get(0))')),
              shinyWidgets::actionGroupButtons(
                inputIds = c("zoomout", "zoomin", "reset"),
                labels = list(icon("minus"), icon("plus"), "Reset"),
                status = "primary"
              )
            )
          )
        ),
        nav_panel(
          "Analysed samples",
          reactable::reactableOutput("analysed_samples_RT")
        )
      )
    )
  ),
  bslib::nav_panel(
    title = "Methods",
    # create 70 30 layout
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 600,
        actionButton("add_method", "Add New Method"),
        actionButton("update_method", "Update Selected Method"),
        DT::DTOutput("methods_dt")
      ),
      DT::DTOutput("cmpd_methods_dt"),
    )
  ),
  bslib::nav_panel(
    title = "Runs Database",
    bslib::layout_sidebar(
      sidebar = sidebar(
        width = 500,
        actionButton(
          "change_samplelist_metadata_descr_btn",
          "Change Description"
        ),
        DT::DTOutput("sample_list_metatable_DT")
      ),
      actionButton(
        "redownload_current_db_list_btn",
        "Download Current List",
        icon = icon("download")
      ),
      actionButton(
        "select_plates_current_list_btn",
        "Select Plates",
        icon = icon("check")
      ),
      DT::DTOutput("sample_list_filtered_DT")
    )
  ), ## sample lists panel
  bslib::nav_panel(
    title = "Plate Design",
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 450,
        bslib::layout_columns(
          col_widths = c(4, 4, 4),
          actionButton("undo_plate_design_btn", "Undo"),
          actionButton("save_plate_design_btn", "Save"),
          actionButton("new_plate_design_btn", "New")
        ),
        ai_chat_module_ui(id = "plate_ai", title = "Plate Design Assistant"),
        h6("Layout Options"),
        shinyWidgets::switchInput(
          "layout_horizontal",
          "Layout",
          value = TRUE,
          onLabel = "H",
          offLabel = "V"
        ),
        bslib::layout_column_wrap(
          width = 1 / 2,
          selectInput(
            "top_left_layout_input",
            "Top Left",
            choices = gen_plate_positions(),
            selected = "A1"
          ),
          selectInput(
            "bottom_right_layout_input",
            "Bottom Right",
            choices = gen_plate_positions(),
            selected = "H12"
          ),
        ),
        tags$hr(),
        h6("Add Elements"),
        bslib::layout_column_wrap(
          width = 1 / 3,
          actionButton("add_blank_btn", "Blank"),
          actionButton("add_double_blank_btn", "Double Blank"),
          actionButton("add_standards_btn", "Standards"),
          actionButton("add_qc_btn", "QC"),
          actionButton("add_dqc_btn", "DQC"),
          # actionButton("add_samples_btn", "Samples"),
          actionButton("add_suitability_btn", "Suitability")
        ),
        tags$div(id = "gen_plate_ui")
      ),
      div(
        style = "display: flex; flex-direction: column; height: 100%;",
        div(
          style = "flex: 1 1 auto;",
          bslib::navset_pill(
            id = "plate_design_nav",
            bslib::nav_panel(
              "Plate",
              bslib::card(
                full_screen = TRUE,
                min_height = 800,
                card_header(
                  "Plate Map",
                  plate_plot_module_ui("plate_design", "plate_design")
                ),
                plotOutput(
                  "plate_design_plotOutput",
                  brush = "plate_design_brush",
                  # click = "plate_design_click",
                  width = "100%",
                  height = "100%"
                ) #,
                # verbatimTextOutput("clicked_plate_design")
              )
            ),
            bslib::nav_panel(
              "Tree",
              bslib::card(
                DiagrammeR::grVizOutput(
                  "plate_design_treeOutput",
                  height = "400px"
                ),
                full_screen = TRUE
              )
            ),
            bslib::nav_panel(
              "Add Samples",
              tags$script(HTML(
                " $(document).on('blur', '.text-extra', function() { Shiny.setInputValue('text_blur', 'focusOut', {priority: 'event'}); }); "
              )),
              rhandsontable::rHandsontableOutput(
                "plate_design_samples_selector_RT"
              ),
            )
          )
        )
      )
    )
  ),
  bslib::nav_panel(
    title = "Plates Database", ## plates panel
    bslib::layout_columns(
      # style = htmltools::css(grid_template_columns = "2fr 1fr"),
      col_widths = c(8, 4),
      bslib::layout_columns(
        col_widths = c(12),
        row_heights = c(4, 1),
        bslib::navset_card_pill(
          id = "plate_nav",
          bslib::nav_panel(
            "Plate View",
            bslib::card(
              full_screen = TRUE,
              card_header(
                "Plate Map",
                plate_plot_module_ui("plate_map", "plate_map"),
              ),
              plotOutput("plate_map_plot1", width = "100%", height = "100%")
            )
          ),
          bslib::nav_panel(
            "Plate Tree",
            bslib::card(
              full_screen = TRUE,
              card_header("Plate Tree"),
              DiagrammeR::grVizOutput(
                "plate_tree_grviz_out",
                width = "100%",
                height = "100%"
              )
            )
          )
        )
      ),
      bslib::card(
        textOutput("plate_id_plateview_output"),
        actionButton(
          "change_plate_meta_btn",
          "Change Plate Description",
          icon = icon("edit")
        ),
        downloadButton(
          "export_plate_image",
          "Export Plate Image",
          icon = icon("download")
        ),
        actionBttn(
          "reuse_plate_button",
          "Reuse Plate",
          icon = icon("redo"),
          color = "primary"
        ),
        reactable::reactableOutput("plate_db_RT")
      )
    )
  ),
  bslib::nav_panel(
    title = "Generators", ## generators panel
    fluidPage(
      # tabset with plate, sample list, dilution
      bslib::navset_card_pill(
        id = "generator_nav",
        bslib::nav_panel(
          "Sample List",
          bslib::layout_sidebar(
            sidebar = sidebar(
              width = 500,
              textOutput("plate_ids_for_sample_list"),
              selectInput(
                "tray_prot1",
                "Tray",
                choices = as.character(1:12),
                multiple = TRUE
              ),
              actionButton("add_protocols", "Add Protocol"),
              bslib::accordion(
                id = "protocols_accordion",
                injec_seq_block_protocol_ui("prot1", 1),
                div(id = "prot_holder") #,
                # accordion_panel(
                #   title = "Compounds dilution",
                #   value = "compounds_accordion",
                #   div(id = "cmpd_holder"),
                #   fluidRow(
                #     column(width = 2, actionButton("add_cmpd", "Add")),
                #     column(width = 2, actionButton("remove_cmpd", "Remove"))),
                #   )
              ),
              actionButton("create_sample_list", "Create Sample List")
            ),
            bslib::navset_bar(
              id = "sample_list_nav",
              bslib::nav_panel(
                "Compound Ratio",
                DT::DTOutput("cmpd_ratio_seq_dt")
              ),
              bslib::nav_panel(
                "Sample List",
                DT::DTOutput("sample_list_table")
              ),
              bslib::nav_panel(
                "Summary",
                p(
                  "Check if total volume is OK. Volume will depend on injection and filtration modes"
                ),
                fluidRow(
                  column(
                    width = 4,
                    textOutput("total_injections"),
                    textOutput("max_vol"),
                    textOutput("min_vol")
                  ),
                  column(width = 8, DT::DTOutput("sample_list_summary")),
                  ai_chat_module_ui(
                    id = "injeclist_ai",
                    title = "Injection List Assistant"
                  )
                )
              ),
              bslib::nav_panel(
                "Export",
                selectInput(
                  "sample_list_vendor",
                  "Select Vendor",
                  choices = c("masslynx", "masshunter", "analyst")
                ),
                actionButton("write_sample_list", "Write Sample List"),
                downloadButton(
                  "export_sample_list",
                  "Export",
                  icon = icon("download")
                )
              )
            )
          )
        ),
        bslib::nav_panel(
          "Dilution",
          h2("Dilution"),
          layout_column_wrap(
            width = 1 / 2, #height = 100,
            numericInput(
              "dil_factor",
              "Parallel Dilution Factor",
              value = "10"
            ),
            textInput("dil_unit", "Dilution Unit", value = "ng"),
            selectInput(
              "dil_type",
              "Vial Type",
              choices = c("Standard", "QC", "DQC")
            ),
            selectInput("dil_rep", "Replicate", choices = 1:10)
          ),
          actionButton("dilute", "Dilute", icon = icon("flask")),

          # layout_column_wrap(width = 1/2,
          #   textInput("add_dil_cmpd_textinput", "Dilution concentration") |>
          #     bslib::tooltip("See help for format"),
          #   actionButton("add_dil_cmpd_btn", "Add Dilution Step")
          # ),
          # shinyMatrix::matrixInput(
          #   inputId = "lower_tri_matrix",
          #   value = matrix(0, nrow = 4, ncol = 4),
          #   rows = list(names = TRUE),
          #   cols = list(names = TRUE),
          #   class = "numeric"
          # ),
          rhandsontable::rHandsontableOutput("dilution_dt"),
          actionButton(
            "gen_dil_graph",
            "Generate Dilution Graph",
            icon = icon("chart-line")
          ),
          layout_columns(
            col_widths = c(10, 2),
            bslib::card(
              id = "dil_graph_grviz_card",
              full_screen = TRUE,
              height = 700,
              card_header("Schema"),
              DiagrammeR::grVizOutput("dil_graph_grviz_out", width = "100%")
            ),
            fluidRow(
              bslib::card(
                id = "dil_dilution_calculator",
                card_header("Dilution Calculator"),
                textOutput("selected_dilution_node_text"),
                textOutput("dilution_factor_text"),
                numericInput(
                  "final_vol_input",
                  "Final Volume",
                  value = 1,
                  min = 1
                ),
                textOutput("final_vol_output"),
                height = 500
              ),
              downloadButton(
                "export_dil_graph",
                "Export",
                icon = icon("download")
              )
            )
          )
        ),
      )
    )
  ),
  nav_spacer(),
  bslib::nav_menu(
    title = "Links",
    align = "right",
    nav_item(shiny::actionButton("exit", "Exit", icon = icon("power-off"))),
    nav_item(config_module_ui("config")),
    nav_item(shiny::actionButton(
      "help",
      "Help",
      icon = icon("question-circle")
    ))
  )
)


study_app_server <- function(input, output, session) {
  # plate_positions <- gen_plate_positions()

  current_cmpd_df <- reactiveVal(NULL)

  ########################## sample list

  current_sample_list_metatable <- reactiveVal(.get_samplesdb_metadata())
  output$sample_list_metatable_DT <- DT::renderDT({
    shiny::validate(shiny::need(
      nrow(current_sample_list_metatable()) > 0,
      "No Sample Lists in Database"
    ))
    current_sample_list_metatable() |>
      DT::datatable(
        selection = list(mode = "single", target = "row"),
        options = list(scrollX = TRUE, scrollY = TRUE, scrollCollapse = TRUE)
      )
  })

  current_visible_sample_db <- reactiveVal(NULL)
  observeEvent(input$sample_list_metatable_DT_rows_selected, {
    # get row id and recover sample list from db
    index <- input$sample_list_metatable_DT_rows_selected
    id <- current_sample_list_metatable() |>
      filter(row_number() == index) |>
      pull("list_id")
    .get_samplelist(id) |>
      select(-"row", -"col", -"list_id") |>
      current_visible_sample_db()
  })

  output$sample_list_filtered_DT <- DT::renderDT({
    shiny::validate(shiny::need(
      nrow(current_visible_sample_db()) > 0,
      "No Samples in Selected List"
    ))

    current_visible_sample_db() |>
      DT::datatable(
        selection = list(mode = "single", target = "row"),
        options = list(scrollX = TRUE, scrollY = TRUE, scrollCollapse = TRUE)
      )
  })
  observeEvent(input$redownload_current_db_list_btn, {
    showModal(modalDialog(
      title = "Redownload Current List",
      selectInput(
        "vendor_redownload",
        "Vendor",
        choices = c("masslynx", "masshunter")
      ),
      downloadButton("redownload_btn_final", "Redownload")
    ))
  })

  output$redownload_btn_final <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), "_sample_list.csv")
    },
    content = function(file) {
      download_sample_list(
        current_visible_sample_db(),
        input$vendor_redownload
      ) |>
        write.csv(file, row.names = FALSE)
    }
  )

  #################################################################################################################
  ###### make study ######
  all_studies_db <- reactiveVal(list_all_studies())
  observeEvent(input$create_new_study_btn, {
    showModal(modalDialog(
      title = "Create New Study",
      textInput("new_study_title", "Study Title", value = ""),
      textInput("new_study_descr", "Description", value = ""),
      selectInput("new_study_type", "Type", choices = c("SD", "MD", "FA")),
      selectInput(
        "new_study_subject_type",
        "Subject Type",
        choices = c("Human", "Animal", "InVitro", "Other")
      ),
      bslib::input_switch("new_study_pkstudy", "PK Study", value = FALSE),
      actionButton("create_study_btn", "Create")
    ))
  })

  observeEvent(input$create_study_btn, {
    tryCatch(
      {
        df <- data.frame(
          title = input$new_study_title,
          type = input$new_study_type,
          pkstudy = input$new_study_pkstudy,
          description = input$new_study_descr,
          subject_type = input$new_study_subject_type
        )
        create_new_study(df)
        showNotification("Study created successfully!", type = "message")
        all_studies_db(list_all_studies())
        removeModal()
      },
      error = function(e) {
        showNotification(
          paste("Error creating study:", e$message),
          type = "error"
        )
      }
    )
  })

  output$studies_db_RT <- reactable::renderReactable({
    shiny::validate(shiny::need(nrow(all_studies_db()) > 0, "No Studies in Database"))
    reactable::reactable(
      all_studies_db(),
      resizable = TRUE,
      selection = "single",
      onClick = "select",
      highlight = TRUE,
      columns = list(
        id = reactable::colDef(name = "Study ID"),
        type = reactable::colDef(name = "Type"),
        description = reactable::colDef(name = "Description"),
        pkstudy = reactable::colDef(name = "PK Study")
      )
    )
  })

  ai_chat_module_server(
    id = "study_ai",
    chatfunc = chatfunc,
    response_function = studydesign_ai,
    response_args = reactive({
      list(currStudyid())
    }), # extra args for reponder
    botname = "Study Design Reviewer"
  )

  currStudyid <- reactiveVal(NULL)
  observeEvent(reactable::getReactableState("studies_db_RT", "selected"), {
    i <- reactable::getReactableState("studies_db_RT", "selected")
    currStudyid(all_studies_db()[i, "id"])
  })

  ### Dosing Arms Table ###
  curr_dosing_db <- reactiveVal(NULL)
  observeEvent(currStudyid(), {
    req(currStudyid())
    df <- retrieve_dosing_db(currStudyid()) |> auto_add_row()
    curr_dosing_db(df)
  })

  output$studyarms_RT <- rhandsontable::renderRHandsontable({
    req(currStudyid())
    shiny::validate(
      shiny::need(currStudyid() != "", "No Study Selected"),
      shiny::need(nrow(curr_dosing_db()) > 0, "No Dosing Arms in Database")
    )

    curr_dosing_db() |>
      rhandsontable::rhandsontable(
        useTypes = TRUE,
        overflow = "visible",
        stretchH = "all",
        colHeaders = c(
          "Arm ID",
          "Study ID",
          "Group Label",
          "Period Number",
          "Dose Freq (hr)",
          "Dose Addl",
          "Dose",
          "Unit",
          "Route",
          "Formulation"
        ),
        fillHandle = list(direction = "vertical", autoInsertRow = TRUE)
      ) |>
      rhandsontable::hot_col(col = c(1, 2), readOnly = TRUE) |>
      rhandsontable::hot_col(col = 3, type = "text") |>
      rhandsontable::hot_col(col = 4, type = "numeric") |>
      rhandsontable::hot_col(col = 5, type = "numeric") |>
      rhandsontable::hot_col(col = 6, type = "numeric") |>
      rhandsontable::hot_col(col = 7, type = "numeric") |>
      rhandsontable::hot_col(
        col = 8,
        type = "dropdown",
        source = c("mg", "g", "ug")
      ) |>
      rhandsontable::hot_col(
        col = 9,
        type = "dropdown",
        source = c("IV", "PO", "SC", "IP", "IM", "SL")
      ) |>
      rhandsontable::hot_col(col = 10, type = "text")
  })

  output$update_arms_db_btn_ui <- renderUI({
    req(currStudyid())
    input$studyarms_RT # detect change in table
    actionButton("update_arms_db_btn", "Update Arms")
  })

  observeEvent(input$update_arms_db_btn, {
    df <- clean_rht_to_df(input$studyarms_RT$data)
    df <- remove_all_empty_row(df) |> empty_string_to_na()

    colnames(df) <- c(
      "arm_id",
      "study_id",
      "group_label",
      "period_number",
      "dose_freq",
      "dose_addl",
      "dose_amount",
      "dose_unit",
      "route",
      "formulation"
    )

    tryCatch(
      {
        update_dosing_db(currStudyid(), df)
        curr_dosing_db(retrieve_dosing_db(currStudyid()))
        showNotification(
          "Dosing database updated successfully!",
          type = "message"
        )
      },
      error = function(e) {
        showNotification(
          paste("Error updating dosing database:", e$message),
          type = "error"
        )
      }
    )
  })

  ### Subjects Table ###
  currSubjectTable <- reactiveVal(NULL)

  observeEvent(currStudyid(), {
    currSubjectTable(retrieve_subjects_db(currStudyid()) |> auto_add_row())
  })

  output$subjects_RT <- rhandsontable::renderRHandsontable({
    shiny::validate(
      shiny::need(currStudyid(), "No Study Selected")
    )

    currSubjectTable() |>
      rhandsontable::rhandsontable(
        useTypes = TRUE,
        overflow = "visible",
        stretchH = "all",
        width = "100%",
        fillHandle = list(direction = "vertical", autoInsertRow = TRUE),
        columnSorting = TRUE
      ) |>
      rhandsontable::hot_col(col = 1, readOnly = TRUE) |> # uuid_subject
      rhandsontable::hot_col(col = 2, type = "text") |>
      rhandsontable::hot_col(col = 3, readOnly = TRUE) |> # study id
      rhandsontable::hot_col(
        col = 4,
        type = "dropdown",
        source = curr_dosing_db()$group_label
      ) |> # group label
      rhandsontable::hot_col(col = 5, type = "numeric", readOnly = TRUE) |> # gp rep
      rhandsontable::hot_col(
        col = 6,
        type = "dropdown",
        source = c("M", "F")
      ) |>
      rhandsontable::hot_col(col = 7, type = "numeric") |> # age
      rhandsontable::hot_col(col = 8, type = "text") |> # rage
      rhandsontable::hot_col(col = 9, type = "text") # extra factors
  })

  observeEvent(input$update_subjects_btn, {
    req(currStudyid())
    df <- clean_rht_to_df(input$subjects_RT$data)
    df <- remove_all_empty_row(df) |> empty_string_to_na()

    colnames(df) <- c(
      "uuid_subject",
      "subject_id",
      "study_id",
      "group_label",
      "group_replicate",
      "sex",
      "age",
      "race",
      "extra_factors"
    )

    tryCatch(
      {
        update_subjects_db(currStudyid(), df)
        currSubjectTable(retrieve_subjects_db(currStudyid()))
        showNotification(
          "Subjects database updated successfully!",
          type = "message"
        )
      },
      error = function(e) {
        showNotification(
          paste("Error updating subjects database:", e$message),
          type = "error"
        )
      }
    )
  })

  observeEvent(input$autofill_subjects_btn, {
    req(currStudyid())
    showModal(
      modalDialog(
        title = "Autofill Subjects",
        easyClose = TRUE,
        selectInput(
          "autofill_subjects_arm",
          "Select Arm",
          choices = curr_dosing_db()$group_label,
          multiple = TRUE
        ),
        selectInput(
          "autofill_subjects_sex",
          "Select Sex",
          choices = c("M", "F"),
          multiple = TRUE
        ),
        numericInput(
          "n_subjects_autofill",
          "Number of Subjects",
          value = 10,
          min = 1,
          step = 1
        ),
        actionButton("autofill_subjects_final_btn", "Autofill")
      )
    )
  })

  ### Sample Log Table ###
  currSampleLogTable <- reactiveVal(NULL)

  observeEvent(currStudyid(), {
    currSampleLogTable(retrieve_sample_log(currStudyid()) |> auto_add_row())
  })

  output$sample_log_RT <- rhandsontable::renderRHandsontable({
    shiny::validate(
      shiny::need(currStudyid(), "No Study Selected")
    )
    currSampleLogTable() |>
      rhandsontable::rhandsontable(
        useTypes = TRUE,
        overflow = "visible",
        # stretchH = "all",
        width = "90%",
        fillHandle = list(direction = "vertical", autoInsertRow = TRUE)
      ) |>
      rhandsontable::hot_col(col = 1, readOnly = TRUE) |>
      rhandsontable::hot_col(col = 2, type = "text") |>
      rhandsontable::hot_col(col = 3, readOnly = TRUE) |>
      rhandsontable::hot_col(col = 4, type = "date", dateFormat = "HH:mm") |> # nominal time
      rhandsontable::hot_col(col = 5, type = "date", dateFormat = "HH:mm") |> # actual time
      rhandsontable::hot_col(col = 6, type = "text") |> # time unit
      rhandsontable::hot_col(
        col = 7,
        type = "dropdown",
        source = c("Collected", "Processed")
      ) |>
      rhandsontable::hot_col(
        col = 8,
        type = "dropdown",
        source = c(
          "Plasma",
          'Serum',
          'Whole Blood',
          'Urine',
          'Depot',
          'CSF',
          'Tissue',
          "Saliva",
          'Other'
        )
      ) |>
      rhandsontable::hot_col(col = 9, type = "text") # notes
  })

  observeEvent(input$update_sample_log_btn, {
    req(currStudyid())

    df <- clean_rht_to_df(input$sample_log_RT$data)
    df <- remove_all_empty_row(df) |> empty_string_to_na()

    colnames(df) <- c(
      "log_id",
      "subject_id",
      "study_id",
      "nominal_time",
      "actual_time",
      "time_unit",
      "status",
      "sample_type",
      "notes"
    )
    tryCatch(
      {
        update_sample_log(currStudyid(), df)
        currSampleLogTable(retrieve_sample_log(currStudyid()))
        showNotification("Sample log updated successfully!", type = "message")
      },
      error = function(e) {
        showNotification(
          paste("Error updating sample log:", e$message),
          type = "error"
        )
      }
    )
  })

  output$study_chart_plot <- DiagrammeR::renderGrViz({
    shiny::validate(
      shiny::need(currStudyid(), "No Study Selected"),
      shiny::need(nrow(currSampleLogTable()) > 0, "No samples in sample log")
    )
    currSampleLogTable()
    currSubjectTable()
    curr_dosing_db()
    plot_study_design(currStudyid())
  })

  output$analysed_samples_RT <- reactable::renderReactable({
    shiny::validate(
      shiny::need(currStudyid(), "No Study Selected")
    )

    exported_list() # listen to changes saving
    reactable::reactable(
      get_injecseq_relation(currStudyid())
    )
  })

  #######################################################################################################
  ##### Predefined Study Design#####
  observeEvent(input$metabolic_study_gen_btn, {
    showModal(modalDialog(
      title = "Generate Metabolic Study",
      textInput(
        "metabolic_study_title",
        "Study Title",
        value = "Metabolic Study"
      ),
      textInput(
        "metabolic_study_cmpds_input",
        "Compounds (comma separated)",
        value = "Cmpd1, Cmpd2"
      ),
      textInput(
        "metabolic_study_timepoints_input",
        "Time Points (comma separated)",
        value = "0,0.5,1,2,4,8,12,24"
      ),
      textInput("metabolic_study_dose", "Dose (e.g. 10)", value = "10"),
      numericInput(
        "n_NAD_input",
        "Number of NAD subjects",
        value = 3,
        min = 0,
        step = 1
      ),
      numericInput(
        "n_nonNAD_input",
        "Number of non-NAD subjects",
        value = 3,
        min = 0,
        step = 1
      ),
      actionButton("generate_metabolic_study_final_btn", "Generate Study")
    ))
  })

  observeEvent(input$generate_metabolic_study_final_btn, {
    tryCatch(
      {
        progress <- shiny::Progress$new(
          session,
          min = 0,
          max = 1
        )
        progress$set(message = "Generating metabolic study...", value = 0)
        on.exit(progress$close())
        make_metabolic_study(
          study = input$metabolic_study_title,
          cmpds = trimws(unlist(strsplit(
            input$metabolic_study_cmpds_input,
            ","
          ))),
          time_points = trimws(unlist(strsplit(
            input$metabolic_study_timepoints_input,
            ","
          ))),
          dose = input$metabolic_study_dose,
          n_NAD = input$n_NAD_input,
          n_noNAD = input$n_nonNAD_input
        )
        showNotification(
          "Metabolic study generated successfully!",
          type = "message"
        )
        progress$set(value = 1)
        all_studies_db(list_all_studies())
        removeModal()
      },
      error = function(e) {
        showNotification(
          paste("Error generating metabolic study:", e$message),
          type = "error"
        )
      }
    )
  })

  observeEvent(input$cells_metabolic_stability_btn, {
    showModal(modalDialog(
      easyClose = TRUE,
      title = "Generate Cell Metabolic Stability Study",
      textInput(
        "cells_metabolic_stability_title",
        "Study Title",
        value = "Cell Metabolic Stability Study"
      ),
      textInput(
        "cells_metabolic_stability_cmpds_input",
        "Compounds (comma separated)",
        value = "Cmpd1, Cmpd2"
      ),
      shinyWidgets::switchInput(
        "cells_metabolic_stability_include_control_switch",
        "Include Standard Compound",
        value = TRUE, 
        size = "small"
      ),
      textInput(
        "cells_metabolic_stability_timepoints_input",
        "Time Points (comma separated)",
        value = "0,1,6"
      ),
      textInput("cell_metabolic_stability_time_unit", "Time Unit", value = "hr"),
      textInput(
        "cells_metabolic_stability_arms_input",
        "Arms (comma separated)",
        value = "1.DMSO, 2.Saline"
      ),
      textInput(
        "cells_metabolic_stability_conditions_input",
        "Conditions (comma separated)",
        value = "4.neg80C, 3.neg20C, 2.neg4C, 1.RT",
      ),
      numericInput(
        "cells_metabolic_stability_n_replicates",
        "Number of Replicates",
        value = 3,
        min = 1,
        step = 1
      ),
      actionButton(
        "generate_cells_metabolic_stability_final_btn",
        "Generate Study"
      )
    ))
  })

  observeEvent(input$generate_cells_metabolic_stability_final_btn, {
    tryCatch(
      {
        progress <- shiny::Progress$new(
          session,
          min = 0,
          max = 1
        )
        progress$set(message = "Generating cell stability study...", value = 0)
        on.exit(progress$close())
        make_cell_stability_study(
          study_title = input$cells_metabolic_stability_title,
          cmpds = str_to_vec(input$cells_metabolic_stability_cmpds_input),
          time_points = str_to_vec(input$cells_metabolic_stability_timepoints_input),
          time_unit = input$cell_metabolic_stability_time_unit,
          arms = str_to_vec(input$cells_metabolic_stability_arms_input),
          conditions = str_to_vec(input$cells_metabolic_stability_conditions_input),
          n_replicates = input$cells_metabolic_stability_n_replicates
        )
        showNotification(
          "Cell metabolic stability study generated successfully!",
          type = "message"
        )
        progress$set(value = 1)
        all_studies_db(list_all_studies())
        removeModal()
      },
      error = function(e) {
        showNotification(
          paste("Error generating cell metabolic stability study:", e$message),
          type = "error"
        )
      }
    )
  })

  #################################################################################################################
  ###### plate Generator ######
  curr_gen_plate_starter <- reactiveVal(NULL)
  curr_gen_plate_expr <- reactiveVal(NULL)
  layoutfrombrush <- reactiveVal(FALSE)
  observeEvent(input$new_plate_design_btn, {
    showModal(modalDialog(
      title = "Create New Plate Design",
      textInput("new_plate_design_descr", "Description", value = ""),
      actionButton("create_plate_design_btn", "Create")
    ))
  })

  observeEvent(input$create_plate_design_btn, {
    removeModal()
    curr_gen_plate_expr(bquote(generate_96(
      descr = .(input$new_plate_design_descr)
    )))
    curr_gen_plate_starter(eval(curr_gen_plate_expr()))
  })

  observeEvent(input$save_plate_design_btn, {
    req(curr_gen_plate_starter())
    register_plate(curr_gen_plate_starter())
    plate_db(.get_plates_db())
    curr_gen_plate_starter(NULL)
    curr_gen_plate_expr(NULL)
    # showNotification("Plate design saved successfully!", type = "message")
  })

  output$plate_design_plotOutput <- renderPlot({
    shiny::validate(
      shiny::need(curr_gen_plate_starter(), "Create new plate")
    )
    curr_gen_plate_starter() |>
      samples_naming_style(
        study_name = input$plate_design_study_name_switch,
        arm = input$plate_design_arm_switch,
        time = input$plate_design_time_switch,
        factor = input$plate_design_factor_switch,
        sex = input$plate_design_sex_switch,
        dose = input$plate_design_dose_switch,
        use_subject_id = input$plate_design_use_subject_id_switch,
        dilution = input$plate_design_dil_label_switch
      ) |>
      plot(
        color = input$plate_design_color_toggle,
        transform_dil = input$plate_design_transform_dilution,
        label_size = input$plate_design_font_size,
        layoutOverlay = TRUE
      )
  })

  observeEvent(input$plate_design_brush, {
    req(curr_gen_plate_starter())
    brush_data <- input$plate_design_brush

    lbound <- round(brush_data$xmin)
    lbound <- max(lbound, 1)
    rbound <- round(brush_data$xmax)
    rbound <- min(rbound, 12)
    tbound <- LETTERS[max(round(brush_data$ymin), 1)]
    bbound <- LETTERS[min(round(brush_data$ymax), 8)]

    curr_gen_plate_expr(
      bquote(
        .(curr_gen_plate_expr()) |>
          fill_scheme(
            fill = .(ifelse(input$layout_horizontal, "h", "v")),
            lbound = .(lbound),
            rbound = .(rbound),
            tbound = .(tbound),
            bbound = .(bbound)
          )
      )
    )

    curr_gen_plate_starter(eval(curr_gen_plate_expr()))
    layoutfrombrush(TRUE)
    updateSelectInput(
      session,
      "top_left_layout_input",
      selected = paste0(tbound, lbound)
    )
    updateSelectInput(
      session,
      "bottom_right_layout_input",
      selected = paste0(bbound, rbound)
    )

    session$resetBrush("plate_design_brush")
  })

  output$clicked_plate_design <- renderPrint({
    req(curr_gen_plate_starter())
    clicked_data <- input$plate_design_click
    req(clicked_data)
    x <- round(as.numeric(clicked_data$x))
    y <- round(as.numeric(clicked_data$y))
    print(paste("Clicked on plate design at:", "Row:", y, "Column:", x))
    clicked_data <- curr_gen_plate_starter()@df |>
      dplyr::filter(row == y, col == x)
    clicked_data
  })

  output$plate_design_treeOutput <- DiagrammeR::renderGrViz({
    req(curr_gen_plate_starter())
    plate_tree(curr_gen_plate_starter())
  })

  observeEvent(input$undo_plate_design_btn, {
    req(curr_gen_plate_starter())
    undo_last_call(curr_gen_plate_expr()) |> curr_gen_plate_expr()
    curr_gen_plate_starter(eval(curr_gen_plate_expr()))
  })

  observeEvent(
    c(
      input$layout_horizontal,
      input$top_left_layout_input,
      input$bottom_right_layout_input
    ),
    {
      req(curr_gen_plate_starter())
      if (layoutfrombrush()) {
        layoutfrombrush(FALSE)
      } else {
        tbound <- gsub("(\\D+)(\\d+)", "\\1", input$top_left_layout_input)
        lbound <- gsub("(\\D+)(\\d+)", "\\2", input$top_left_layout_input) |>
          as.numeric()

        bbound <- gsub("(\\D+)(\\d+)", "\\1", input$bottom_right_layout_input)
        rbound <- gsub(
          "(\\D+)(\\d+)",
          "\\2",
          input$bottom_right_layout_input
        ) |>
          as.numeric()

        # undo first
        while (!is.null(check_last_fill(curr_gen_plate_expr()))) {
          curr_gen_plate_expr(undo_last_call(curr_gen_plate_expr()))
        }

        curr_gen_plate_expr(
          bquote(
            .(curr_gen_plate_expr()) |>
              fill_scheme(
                fill = .(ifelse(input$layout_horizontal, "h", "v")),
                lbound = .(lbound),
                rbound = .(rbound),
                tbound = .(tbound),
                bbound = .(bbound)
              )
          )
        )

        curr_gen_plate_starter(eval(curr_gen_plate_expr()))
      }
    }
  )

  observeEvent(input$add_blank_btn, {
    req(curr_gen_plate_starter())
    remove_old_ui()
    insertUI(
      selector = "#gen_plate_ui",
      ui = div(
        id = "dynamic_ui",
        wellPanel(
          selectizeInput(
            "blank_group",
            "Group",
            options = list(create = TRUE),
            choices = plate_groups(curr_gen_plate_starter())
          ),
          bslib::input_switch("has_analyte_blank", "Analyte+", value = FALSE),
          bslib::input_switch("is_IS_blank", "IS+", value = TRUE),
          bslib::input_switch(
            "blank_matrix",
            "Matrix+ (Off for analytical blank)",
            value = TRUE
          ),
          actionButton("add_blank_btn_final", "Add Blank")
        )
      )
    )
  })

  observeEvent(input$add_blank_btn_final, {
    tryCatch(
      {
        req(curr_gen_plate_expr())
        curr_gen_plate_expr(
          bquote(
            .(curr_gen_plate_expr()) |>
              add_blank(
                group = .(input$blank_group),
                IS = .(input$is_IS_blank),
                analyte = .(input$has_analyte_blank),
                analytical = .(!input$blank_matrix)
              )
          )
        )
        curr_gen_plate_starter(eval(curr_gen_plate_expr()))
        updateSelectizeInput(
          session,
          "blank_group",
          choices = plate_groups(curr_gen_plate_starter()),
          selected = input$blank_group
        )
        removeModal()
      },
      error = function(e) {
        undo_last_call(curr_gen_plate_expr()) |> curr_gen_plate_expr()
        showNotification(paste("Error:", e$message), type = "error")
      }
    )
  })

  observeEvent(input$add_double_blank_btn, {
    req(curr_gen_plate_starter())
    remove_old_ui()
    insertUI(
      selector = "#gen_plate_ui",
      ui = div(
        id = "dynamic_ui",
        wellPanel(
          selectizeInput(
            "db_group",
            "Group",
            options = list(create = TRUE),
            choices = plate_groups(curr_gen_plate_starter())
          ),
          bslib::input_switch("db_matrix", "Analytical", value = FALSE),
          actionButton("add_double_blank_btn_final", "Add")
        )
      )
    )
  })

  observeEvent(input$add_double_blank_btn_final, {
    tryCatch(
      {
        req(curr_gen_plate_expr())
        curr_gen_plate_expr(
          bquote(
            .(curr_gen_plate_expr()) |>
              add_DB(group = .(input$db_group), analytical = .(input$db_matrix))
          )
        )
        curr_gen_plate_starter(eval(curr_gen_plate_expr()))
        updateSelectizeInput(
          session,
          "db_group",
          choices = plate_groups(curr_gen_plate_starter()),
          selected = input$db_group
        )
        removeModal()
      },
      error = function(e) {
        undo_last_call(curr_gen_plate_expr()) |> curr_gen_plate_expr()
        showNotification(paste("Error:", e$message), type = "error")
      }
    )
  })

  # CS

  observeEvent(input$add_standards_btn, {
    req(curr_gen_plate_starter())
    remove_old_ui()
    insertUI(
      selector = "#gen_plate_ui",
      ui = div(
        id = "dynamic_ui",
        wellPanel(
          textInput(
            "plate_std",
            "Standard Sepearated by commas",
            "1, 3, 10, 50, 80, 100, 200"
          ),
          numericInput("std_rep", "Replicate", value = 1, min = 1, max = 10),
          selectizeInput(
            "standard_group",
            "Group",
            options = list(create = TRUE),
            choices = plate_groups(curr_gen_plate_starter())
          ),
          actionButton("add_standards_btn_final", "Add")
        )
      )
    )
  })

  observeEvent(input$add_standards_btn_final, {
    tryCatch(
      {
        req(curr_gen_plate_expr())
        curr_gen_plate_expr(
          bquote(
            .(curr_gen_plate_expr()) |>
              add_cs_curve(
                plate_std = .(as.numeric(trimws(unlist(strsplit(
                  input$plate_std,
                  ","
                ))))),
                rep = .(input$std_rep),
                group = .(input$standard_group)
              )
          )
        )
        updateSelectizeInput(
          session,
          "standard_group",
          choices = plate_groups(curr_gen_plate_starter()),
          selected = input$standard_group
        )
        curr_gen_plate_starter(eval(curr_gen_plate_expr()))
      },
      error = function(e) {
        undo_last_call(curr_gen_plate_expr()) |> curr_gen_plate_expr()
        showNotification(paste("Error:", e$message), type = "error")
      }
    )
  })

  observeEvent(input$add_qc_btn, {
    req(curr_gen_plate_starter())
    remove_old_ui()
    insertUI(
      selector = "#gen_plate_ui",
      ui = div(
        id = "dynamic_ui",
        wellPanel(
          numericInput(
            "qc_lqc_conc_input",
            "LQC Concentration",
            value = 1,
            min = 0.001
          ),
          numericInput(
            "qc_mqc_conc_input",
            "MQC Concentration",
            value = 1,
            min = 0.001
          ),
          numericInput(
            "qc_hqc_conc_input",
            "HQC Concentration",
            value = 1,
            min = 0.001
          ),
          numericInput("qc_rep", "Replicate", value = 1, min = 1, max = 10),
          bslib::input_switch(
            "qc_serial_input",
            "Serial Adding (Turn off for multichannel pipetting)",
            value = TRUE
          ),
          bslib::input_switch(
            "qc_reg_input",
            "Enforce Regulatory Limits",
            value = TRUE
          ),
          selectizeInput(
            "qc_group",
            "Group",
            options = list(create = TRUE),
            choices = plate_groups(curr_gen_plate_starter())
          ),
          actionButton("add_qc_btn_final", "Add QC")
        )
      )
    )
  })

  observeEvent(input$add_qc_btn_final, {
    tryCatch(
      {
        req(curr_gen_plate_expr())
        curr_gen_plate_expr(
          bquote(
            .(curr_gen_plate_expr()) |>
              add_QC(
                lqc = .(input$qc_lqc_conc_input),
                mqc_conc = .(input$qc_mqc_conc_input),
                hqc_conc = .(input$qc_hqc_conc_input),
                n_qc = .(input$qc_rep),
                qc_serial = .(input$qc_serial_input),
                reg = .(input$qc_reg_input),
                group = .(input$qc_group)
              )
          )
        )
        updateSelectizeInput(
          session,
          "qc_group",
          choices = plate_groups(curr_gen_plate_starter()),
          selected = input$qc_group
        )
        curr_gen_plate_starter(eval(curr_gen_plate_expr()))
      },
      error = function(e) {
        undo_last_call(curr_gen_plate_expr()) |> curr_gen_plate_expr()
        showNotification(paste("Error:", e$message), type = "error")
      }
    )
  })

  observeEvent(input$add_dqc_btn, {
    req(curr_gen_plate_starter())
    remove_old_ui()
    insertUI(
      selector = "#gen_plate_ui",
      ui = div(
        id = "dynamic_ui",
        wellPanel(
          numericInput(
            "dqc_conc_input",
            "Undiluted Concentration",
            value = 1,
            min = 0.001
          ),
          shinyWidgets::autonumericInput(
            "dqc_dilfac_input",
            "Dilution Factor",
            value = 10,
            minimumvalue = 1.2,
            currencySymbol = "X",
            currencySymbolPlacement = "p"
          ),
          numericInput("dqc_rep", "Replicate", value = 1, min = 1, max = 10),
          selectizeInput(
            "dqc_group",
            "Group",
            options = list(create = TRUE),
            choices = plate_groups(curr_gen_plate_starter())
          ),
          actionButton("add_dqc_btn_final", "Add DQC")
        )
      )
    )
  })

  observeEvent(input$add_dqc_btn_final, {
    tryCatch(
      {
        req(curr_gen_plate_expr())
        curr_gen_plate_expr(
          bquote(
            .(curr_gen_plate_expr()) |>
              add_DQC(
                conc = .(input$dqc_conc_input),
                fac = .(input$dqc_dilfac_input),
                rep = .(input$dqc_rep),
                group = .(input$dqc_group)
              )
          )
        )
        updateSelectizeInput(
          session,
          "dqc_group",
          choices = plate_groups(curr_gen_plate_starter()),
          selected = input$dqc_group
        )
        curr_gen_plate_starter(eval(curr_gen_plate_expr()))
      },
      error = function(e) {
        undo_last_call(curr_gen_plate_expr()) |> curr_gen_plate_expr()
        showNotification(paste("Error:", e$message), type = "error")
      }
    )
  })

  observeEvent(input$add_suitability_btn, {
    req(curr_gen_plate_starter())
    remove_old_ui()
    insertUI(
      selector = "#gen_plate_ui",
      ui = div(
        id = "dynamic_ui",
        wellPanel(
          numericInput(
            "suitability_conc_input",
            "Concentration",
            value = 1,
            min = 0.001
          ),
          textInput("suitability_descr_input", "Label", value = "Suit"),
          selectizeInput(
            "suitability_group",
            "Group",
            options = list(create = TRUE),
            choices = plate_groups(curr_gen_plate_starter())
          ),
          actionButton("add_suitability_btn_final", "Add Suitability")
        )
      )
    )
  })

  observeEvent(input$add_suitability_btn_final, {
    tryCatch(
      {
        req(curr_gen_plate_expr())
        curr_gen_plate_expr(
          bquote(
            .(curr_gen_plate_expr()) |>
              add_suitability(
                conc = .(input$suitability_conc_input),
                group = .(input$suitability_group),
                label = .(input$suitability_descr_input)
              )
          )
        )
        updateSelectizeInput(
          session,
          "suitability_group",
          choices = plate_groups(curr_gen_plate_starter()),
          selected = input$suitability_group
        )
        curr_gen_plate_starter(eval(curr_gen_plate_expr()))
      },
      error = function(e) {
        undo_last_call(curr_gen_plate_expr()) |> curr_gen_plate_expr()
        showNotification(paste("Error:", e$message), type = "error")
      }
    )
  })

  curr_plate_sample_log_dil <- reactiveVal(NULL)
  observeEvent(currSampleLogTable(), {
    req(currStudyid())
    req(currSampleLogTable())

    logIds <- currSampleLogTable()$log_id
    req(!all(is.na(logIds)))

    # captured_dil(NULL)

    retrieve_full_log_by_id(logIds) |>
      dplyr::mutate(dil = 1) |>
      dplyr::mutate(select = FALSE) |>
      dplyr::relocate("dil") |>
      dplyr::relocate("select", .before = "dil") |>
      curr_plate_sample_log_dil()
  })

  output$plate_design_samples_selector_RT <- rhandsontable::renderRHandsontable(
    {
      req(curr_gen_plate_starter())
      shiny::validate(
        shiny::need(
          currStudyid(),
          "Select a study to be able to add samples to current plate."
        ),
        shiny::need(
          nrow(curr_plate_sample_log_dil()) > 0,
          "No samples in sample log to add to plate."
        )
      )
      req(input$rank_list_logtable_asc)
      curr_plate_sample_log_dil() |>
        orderdf(input$rank_list_logtable_asc) |>
        rhandsontable::rhandsontable(search = TRUE, multiColumnSort = TRUE) |>
        rhandsontable::hot_col(col = 1, type = "checkbox") |>
        rhandsontable::hot_col(col = 2, type = "numeric") |>
        rhandsontable::hot_col(
          col = seq(3, ncol(curr_plate_sample_log_dil())),
          readOnly = TRUE
        )
    }
  )

  observeEvent(input$plate_design_nav, {
    shiny::validate(
      shiny::need(
        curr_gen_plate_starter(),
        "Create new plate"
      ),
      shiny::need(
        currStudyid(),
        "Select a study to be able to add samples to current plate."
      ),
      shiny::need(
        nrow(curr_plate_sample_log_dil()) > 0,
        "No samples in sample log to add to plate."
      )
    )

    selectedtab <- input$plate_design_nav

    if (selectedtab == "Add Samples") {
      remove_old_ui()
      insertUI(
        selector = "#gen_plate_ui",
        ui = div(
          id = "dynamic_ui",
          wellPanel(
            textOutput("num_samples_selected_plate_design_txt"),
            sliderInput(
              "plate_design_add_samples_slider",
              "Samples",
              min = 1,
              max = nrow(curr_plate_sample_log_dil()),
              value = c(1, 12),
              step = 1
            ),
            sortable::rank_list(
              text = "Sorting",
              labels = list(
                "nominal_time",
                "group_label",
                "extra_factors",
                "group_replicate",
                "dose_amount"
              ),
              input_id = "rank_list_logtable_asc"
            ),
            selectizeInput(
              "samplesdb_group_input",
              "Group",
              options = list(create = TRUE),
              choices = "No Group"
            ),
            textInput("samplesdb_dilution_input", "Dilution", value = "1,1") |> 
              bslib::tooltip(
                "Dilution factor(s), single value or comma separated for repeated addition with different dilutions. E.g. 1,10,100",
                placement = "right"
              ),
            actionButton("add_samples_db_btn_final", "Add Samples")
          )
        )
      )
    }
  })

  observeEvent(input$rank_list_logtable_asc, {
    req(curr_plate_sample_log_dil())
    # reorder curr_plate_sample_log_dil()
    curr_plate_sample_log_dil() |>
      orderdf(input$rank_list_logtable_asc) |> 

      curr_plate_sample_log_dil()
  })

  observeEvent(input$add_samples_db_btn_final, {
    req(curr_gen_plate_starter())
    req(curr_gen_plate_expr())
    req(currStudyid())

    selected_rows <- input$plate_design_samples_selector_RT$data |>
      clean_rht_to_df()
    colnames(selected_rows) <- colnames(curr_plate_sample_log_dil())
    selected_rows <- selected_rows |>
      dplyr::filter(select == TRUE)

    req(nrow(selected_rows) > 0)

    if (is.null(selected_rows)) {
      showNotification("No samples selected", type = "error")
      req(FALSE)
    }
    tryCatch(
      {
        sortedsampleid <- selected_rows |>
          dplyr::pull("log_id")

        sorted_dil <- selected_rows |>
          dplyr::pull("dil")
      },
      error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
        req(FALSE)
      }
    )

    tryCatch(
      {
        curr_gen_plate_expr(
          bquote(
            .(curr_gen_plate_expr()) |>
              add_samples_db2(
                logIds = .(sortedsampleid),
                # dil = .(sorted_dil),
                dil = .(str_to_vec(input$samplesdb_dilution_input, TRUE)),
                group = .(input$samplesdb_group_input)
              )
          )
        )

        updateSelectizeInput(
          session,
          "samplesdb_group_input",
          choices = plate_groups(curr_gen_plate_starter()),
          selected = input$samplesdb_group_input
        )

        curr_gen_plate_starter(eval(curr_gen_plate_expr()))
        showNotification("Samples added to plate", type = "message")
      },
      error = function(e) {
        undo_last_call(curr_gen_plate_expr()) |> curr_gen_plate_expr()
        showNotification(paste("Error:", e$message), type = "error")
      }
    )
  })

  output$num_samples_selected_plate_design_txt <- renderText({
    req(curr_plate_sample_log_dil())
    req(input$plate_design_samples_selector_RT)
    selected_rows <- input$plate_design_samples_selector_RT$data |>
      clean_rht_to_df()
    colnames(selected_rows) <- colnames(curr_plate_sample_log_dil())

    selected_rows <- selected_rows |>
      dplyr::filter(select == TRUE) |>
      rownames()
    if (is.null(selected_rows)) {
      "No samples selected"
    } else {
      paste(length(selected_rows), "samples selected")
    }
  })

  observeEvent(input$plate_design_add_samples_slider, {
    req(curr_plate_sample_log_dil())
    slider_range <- input$plate_design_add_samples_slider
    # change curr_plate_sample_log_dil()
    curr_plate_sample_log_dil(
      curr_plate_sample_log_dil() |>
        dplyr::mutate(select = FALSE) |>
        dplyr::mutate(
          select = dplyr::row_number() >= slider_range[1] &
            dplyr::row_number() <= slider_range[2]
        )
    )
  })

  

  ai_chat_module_server(
    id = "plate_ai",
    chatfunc = chatfunc,
    response_function = plate_ai,
    response_args = reactive({
      list(curr_gen_plate_starter())
    }), # extra args for reponder
    botname = "Plate Design Reviewer"
  )

  #################################################################################################################
  ###### plate Database ######

  # used to create checkboxes
  shinyInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), label = NULL, ...))
    }
    inputs
  }

  ############################# Gen

  plate_db <- reactiveVal(.get_plates_db())
  current_plate <- reactiveVal(NULL)
  current_plate_row <- reactiveVal(1)
  current_injec_seq <- reactiveVal(NULL)
  current_injec_protcols <- reactiveVal(1)
  selected_ids <- reactiveVal(NULL) # actuall plates ids
  # https://stackoverflow.com/questions/34157684/dynamic-number-of-actionbuttons-tied-to-unique-observeevent

  # default to last
  observeEvent(plate_db(), {
    current_plate(plate_db()[1, "id"] |> .retrieve_plate())
  })

  output$plate_ids_for_sample_list <- renderText({
    req(class(current_plate()) == "RegisteredPlate")
    paste0("Selected Plates ID: ", paste(selected_ids(), collapse = "& "))
  })

  # insertUI(
  #   selector = "#cmpd_holder",
  #   where    = "beforeEnd",
  #   ui       = tagList(module_compounds("var1", 1))
  # )

  # observeEvent(input$add_cmpd, {
  #   cmpd_last <- sum(input$add_cmpd, 1)
  #   insertUI(
  #     selector = "#cmpd_holder",
  #     where    = "beforeEnd",
  #     ui       = tagList(module_compounds(paste0("var", cmpd_last), cmpd_last))
  #   )

  # })

  # already_removed <- reactiveVal(1)
  # observeEvent(input$remove_cmpd, {
  #   cmpd_last <- names(input) |> grep(pattern = "^var\\d+\\-compound_name", value = TRUE) |>
  #     gsub(pattern = "^(var)(\\d+).*", replacement = "\\2") |> as.numeric()

  #   cmpd_last <- cmpd_last[!cmpd_last %in% already_removed()] |> max()

  #   req(cmpd_last > 1)

  #   removeUI(
  #     selector = paste0("#var", cmpd_last, "-cmpd_holder")
  #   )

  #   already_removed(c(already_removed(), cmpd_last))
  # })

  ##
  injec_seq_block_server(
    "prot1",
    1,
    methodsdb,
    current_plate,
    current_cmpd_df,
    lock_export
  )

  observeEvent(
    input$add_protocols,
    {
      protocol_last <- sum(input$add_protocols, 1)

      if (protocol_last > 12) {
        showNotification(
          "Maximum number of protocols reached",
          type = "warning"
        )
        req(FALSE)
      }

      insertUI(
        selector = "#prot_holder",
        where = "beforeEnd",
        ui = tagList(injec_seq_block_protocol_ui(
          paste0("prot", protocol_last),
          protocol_last
        ))
      )

      # insertUI(
      #   selector = paste0("#cmpd_holder_prot", protocol_last),
      #   where    = "beforeEnd",
      #   ui       = tagList(module_compounds("var1", 1))
      # )

      current_injec_protcols(current_injec_protcols() + 1)
      injec_seq_block_server(
        paste0("prot", protocol_last),
        protocol_last,
        methodsdb,
        current_plate,
        current_cmpd_df,
        lock_export
      )
    },
    priority = 1
  )

  output$plate_db_RT <- reactable::renderReactable({
    shiny::validate(
      shiny::need(
        nrow(plate_db()) > 0,
        "No plates in database. Create and save new plates to get started."
      )
    )

    plate_db() |>
      reactable::reactable(selection = "multiple", onClick = "select")
  })


  current_plate_row <- reactive({
    # get selected plates ids
    selected <- reactable::getReactableState("plate_db_RT", "selected")
    if (length(selected) == 0) {
      selected_ids(plate_db()[1, ]$id)
    } else {
      selected_ids(plate_db()[selected, ]$id)
    }

    selected # row/plates indices
  })

  output$plate_map_plot1 <- renderPlot({
    shiny::validate(
      shiny::need(current_plate_row(), "Select a plate from the table")
    )
    plate_db()[current_plate_row(), ]$id |> selected_ids()

    # select last id for current plate list
    .retrieve_plate(rev(selected_ids())[[1]]) |> current_plate()
    current_plate() |>
      samples_naming_style(
        study_name = input$plate_design_study_name_switch,
        arm = input$plate_design_arm_switch,
        time = input$plate_design_time_switch,
        factor = input$plate_design_factor_switch,
        sex = input$plate_design_sex_switch,
        dose = input$plate_design_dose_switch,
        use_subject_id = input$plate_design_use_subject_id_switch,
        dilution = input$plate_map_dil_label_switch
      ) |>
      plot(
        color = input$plate_map_color_toggle,
        label_size = input$plate_map_font_size,
        transform_dil = input$plate_map_transform_dilution
      )
  })

  output$plate_tree_grviz_out <- DiagrammeR::renderGrViz({
    req(current_plate())
    plate_tree(current_plate())
  })

  ########################
  observeEvent(selected_ids(), {
    req(selected_ids())
    req(current_injec_protcols() > 0)
    index <- ifelse(current_injec_protcols() < 13, current_injec_protcols(), 12)
    for (i in 1:index) {
      updateSelectizeInput(
        inputId = paste0("plate_id_prot", i),
        choices = selected_ids()
      )
    }
  })

  observeEvent(current_injec_protcols(), {
    req(current_injec_protcols() > 0)
    updateSelectizeInput(
      inputId = paste0("plate_id_prot", current_injec_protcols()),
      choices = selected_ids()
    )
  })

  # remove dilutions tab if no std
  observeEvent(current_plate(), {
    if (.last_entity(current_plate(), "Standard") == 0) {
      nav_hide("generator_nav", "Dilution")
    } else {
      nav_show("generator_nav", "Dilution")
    }
  })

  #########################
  output$cmpd_ratio_seq_dt <- DT::renderDT({
    req(current_cmpd_df())
    current_cmpd_df() |>
      DT::datatable(
        escape = FALSE,
        rownames = FALSE,
        options = list(
          scrollX = TRUE,
          scrollY = TRUE,
          scrollCollapse = TRUE,
          pageLength = 100
        ),
        editable = list(target = "all", disable = list(columns = c(0, 1)))
      )
  })

  proxy_ratio_cmpds = dataTableProxy('cmpd_ratio_seq_dt')

  observeEvent(input$cmpd_ratio_seq_dt_cell_edit, {
    DT::editData(
      current_cmpd_df(),
      input$cmpd_ratio_seq_dt_cell_edit,
      'cmpd_ratio_seq_dt',
      proxy = proxy_ratio_cmpds,
      rownames = FALSE
    ) |>
      current_cmpd_df()
  })

  lock_export <- reactiveVal(TRUE)

  observeEvent(lock_export(), {
    if (lock_export()) {
      hide("write_sample_list")
      hide("export_sample_list")
    }
  })

  observeEvent(input$create_sample_list, {
    req(class(current_plate()) == "RegisteredPlate")

    tryCatch(
      {
        plates_list <- list()
        injseq_list <- list()

        index_plates <- if (length(selected_ids()) == 1) {
          1
        } else {
          seq(1, length(selected_ids()), 1)
        }

        for (i in index_plates) {
          plates_list[[i]] <- .retrieve_plate(selected_ids()[[i]]) |>
            samples_naming_style(
              study_name = input$plate_map_study_name_switch,
              arm = input$plate_map_arm_switch,
              time = input$plate_map_time_switch,
              factor = input$plate_map_factor_switch,
              sex = input$plate_map_sex_switch,
              dose = input$plate_map_dose_switch,
              use_subject_id = input$plate_map_use_subject_id_switch,
              dilution = input$plate_map_dil_label_switch
            )
        }

        plates_list <- combine_plates(plates_list) # one big plate

        # create custom protocol for the big plate
        index_prot <- ifelse(
          current_injec_protcols() < 13,
          current_injec_protcols(),
          12
        )
        index_prot <- if (index_prot == 1) 1 else seq(1, index_prot, 1)

        for (i in index_prot) {
          if (!is.null(current_cmpd_df())) {
            # filter only correct method
            cmpd_df <- current_cmpd_df() |>
              filter(
                .data$method ==
                  input[[paste0("prot", i, "-inlet_method_select_prot", i)]]
              ) |> # filter only correct method
              dplyr::select("compound", "ratio")
          } else {
            cmpd_df <- NULL
          }
          injseq_list[[i]] <- plates_list |>
            build_injec_seq(
              descr = input[[paste0("prot", i, "-descr_prot", i)]],
              method = input[[paste0(
                "prot",
                i,
                "-inlet_method_select_prot",
                i
              )]],
              suffix = input[[paste0("prot", i, "-suffix_prot", i)]],
              tray = input[[paste0("tray_prot", i)]],
              blank_after_top_conc = input[[paste0(
                "prot",
                i,
                "-blank_after_top_conc_prot",
                i
              )]],
              blank_at_end = input[[paste0(
                "prot",
                i,
                "-blank_at_end_prot",
                i
              )]],
              blank_every_n = input[[paste0(
                "prot",
                i,
                "-blank_every_n_prot",
                i
              )]],
              rep_suitability = input[[paste0(
                "prot",
                i,
                "-rep_suitability_number_prot",
                i
              )]],
              repeat_std = input[[paste0("prot", i, "-repeat_std_prot", i)]],
              repeat_analyte = input[[paste0(
                "prot",
                i,
                "-repeat_sample_prot",
                i
              )]],
              repeat_qc = input[[paste0("prot", i, "-repeat_qc_prot", i)]],
              n_explore = input[[paste0(
                "prot",
                i,
                "-exploratory_samples_alg_prot",
                i
              )]],
              conc_df = cmpd_df,
              injec_vol = input[[paste0("prot", i, "-injec_vol_prot", i)]]
            )
        } # filter only correct method

        # enable export button
        nav_show("sample_list_nav", "Export")
        nav_show("sample_list_nav", "Summary")
        nav_show("sample_list_nav", "Sample List")

        shinyjs::show("write_sample_list")
        shinyjs::enable("write_sample_list")
        shinyjs::hide("export_sample_list")

        nav_select("sample_list_nav", "Sample List")

        if (length(injseq_list) == 1) {
          current_injec_seq(injseq_list[[1]])
        } else {
          combine_injec_lists(injseq_list, equi_pos = "A,3") |>
            current_injec_seq()
        }

        lock_export(FALSE)
      },
      error = function(e) {
        showNotification(e$message, type = "error")
      }
    )
  })

  # change plate metadata of descr and instrument
  observeEvent(input$change_plate_meta_btn, {
    showModal(modalDialog(
      title = "Change Plate Description",
      textInput(
        "new_plate_descr",
        "New Description",
        value = current_plate()@descr
      ),
      pickerInput(
        "compounds_metadata",
        "Compounds",
        choices = "",
        multiple = TRUE,
        options = list(`live-search` = TRUE)
      ),
      pickerInput(
        "instruments_metadata",
        "Instruments",
        choices = "",
        multiple = TRUE,
        options = list(`live-search` = TRUE)
      ),
      pickerInput(
        "IS_metadata",
        "Internal Standards",
        choices = "",
        multiple = TRUE,
        options = list(`live-search` = TRUE)
      ),
      pickerInput(
        "solvents_metadata",
        "Solvents",
        choices = "",
        multiple = TRUE,
        options = list(`live-search` = TRUE)
      ),
      actionButton("change_plate_descr_btn_final", "Change")
    ))
  })
  observeEvent(input$change_plate_descr_btn_final, {
    req(class(current_plate()) == "RegisteredPlate")
    tryCatch(
      {
        current_plate() |> plate_metadata(input$new_plate_descr)
        plate_db(.get_plates_db())
      },
      error = function(e) {
        showNotification(e$message, type = "error")
      }
    )
  })

  output$sample_list_table <- DT::renderDT({
    unique_vol <- unique(current_injec_seq()$injec_list$INJ_VOL)
    unique_conc <- unique(current_injec_seq()$injec_list$conc)

    if (!lock_export()) {
      # red pallete
      redpal <- colorRampPalette(c("red", "white"))(length(unique_vol)) |>
        paste0(50)

      # blue color plalle
      bluepal <- colorRampPalette(c("blue", "white"))(length(unique_conc)) |>
        paste0(50)

      req(class(current_plate()) == "RegisteredPlate")
      req(current_injec_seq())

      showNotification(
        "Check the summary tab for total volume",
        type = "message"
      )

      current_injec_seq()$injec_list |>
        dplyr::select(
          "Index",
          "FILE_NAME",
          "FILE_TEXT",
          "SAMPLE_LOCATION",
          "INJ_VOL",
          "conc",
          "TYPE",
          starts_with("COMPOUND"),
          starts_with("CONC")
        ) |>
        dplyr::rename(
          "Sample Location" = "SAMPLE_LOCATION",
          Description = "FILE_TEXT"
        ) |>
        DT::datatable(
          selection = list(mode = "single", target = "cell"),
          options = list(
            scrollX = TRUE,
            scrollY = "550px",
            scrollCollapse = TRUE,
            dom = "ft",
            pageLength = 10000000
          ),
          rownames = FALSE
        ) |>
        DT::formatStyle(
          columns = "INJ_VOL",
          valueColumns = "INJ_VOL",
          backgroundColor = DT::styleEqual(unique_vol, redpal)
        ) |>
        DT::formatStyle(
          columns = "conc",
          valueColumns = "conc",
          backgroundColor = DT::styleEqual(unique_conc, bluepal)
        )
    } else {
      NULL
    }
  })

  current_injec_seq_summary <- reactiveVal(NULL)

  output$sample_list_summary <- DT::renderDT({
    req(class(current_plate()) == "RegisteredPlate")
    req(current_injec_seq())

    if (!lock_export()) {
      current_injec_seq() |> summary() |> current_injec_seq_summary()

      DT::datatable(
        current_injec_seq_summary(),
        options = list(
          scrollX = TRUE,
          scrollCollapse = TRUE,
          dom = "ft",
          scrollY = "550px"
        )
      ) |>
        DT::formatStyle(
          columns = "total_volume",
          valueColumns = "total_volume",
          backgroundColor = DT::styleEqual(
            unique(current_injec_seq_summary()$total_volume),
            colorRampPalette(c("red", "white"))(length(unique(
              current_injec_seq_summary()$total_volume
            )))
          )
        )
    } else {
      NULL
    }
  })

  output$total_injections <- renderText({
    req(class(current_plate()) == "RegisteredPlate")
    req(current_injec_seq())

    if (!lock_export()) {
      x <- nrow(current_injec_seq()$injec_list)
      paste0("Total Injections: ", x)
    } else {
      NULL
    }
  })

  output$max_vol <- renderText({
    req(class(current_plate()) == "RegisteredPlate")
    req(current_injec_seq())

    if (!lock_export()) {
      max_vol <- current_injec_seq_summary() |>
        dplyr::pull(.data$total_volume) |>
        max()
      paste0("Max Volume: ", max_vol)
    } else {
      NULL
    }
  })

  output$min_vol <- renderText({
    req(class(current_plate()) == "RegisteredPlate")
    req(current_injec_seq())

    if (!lock_export()) {
      min_vol <- current_injec_seq_summary() |>
        dplyr::pull(.data$total_volume) |>
        min()
      paste0("Min Volume: ", min_vol)
    } else {
      NULL
    }
  })

  ai_chat_module_server(
    id = "injeclist_ai",
    chatfunc = chatfunc,
    response_function = injeclist_ai,
    response_args = reactive({
      list(current_injec_seq()$injec_list)
    }), # extra args for reponder
    botname = "Injection List Reviewer"
  )
  ###############################################################################################
  ### Dilutions
  current_dil_df <- reactiveVal(NULL)
  parallel_dil_df <- reactiveVal(NULL)

  observeEvent(input$dil_type, {
    if (input$dil_type == "QC") {
      updateSelectInput(
        session,
        "dil_rep",
        choices = 1:.last_entity(current_plate(), "QC"),
        selected = 1
      )
    }
  })

  observeEvent(input$dilute, {
    # click dilute button to only generate parallel table
    req(class(current_plate()) == "RegisteredPlate")

    d <- tryCatch(
      .parallel_dilution(
        current_plate(),
        fold = input$dil_factor,
        unit = input$dil_unit,
        type = input$dil_type,
        rep = as.numeric(input$dil_rep)
      ),
      error = function(e) {
        showNotification(e$message, type = "error")
      }
    )

    empty_rows <- data.frame(v4 = NA, v3 = NA, v2 = NA)
    d <- cbind(empty_rows, d)

    if (input$dil_type == "QC") {
      # delete the vial position for now and aggregate
      d$v0 <- gsub("(.*)_(.*)", "\\1", d$v0)
      d <- d |> distinct()
    }

    current_dil_df(d)

    shinyjs::hide("dil_graph_grviz_card")
    shinyjs::hide("export_dil_graph")
  })

  output$dilution_dt <- rhandsontable::renderRHandsontable({
    req(class(current_plate()) == "RegisteredPlate")
    req(current_dil_df())

    #  columns = data.frame(#title=c('From/To', 'From/to', 'From/to', 'From/to', 'Plate', "TYPE"),
    #                 type=c('text', 'text', 'text', 'text', 'text', 'text'))
    current_dil_df() |>
      dplyr::mutate(across(everything(), as.character)) |>
      rhandsontable::rhandsontable(useTypes = TRUE) |>
      rhandsontable::hot_col(c("v1"), readOnly = TRUE) |>
      rhandsontable::hot_col(c("v0"), readOnly = TRUE) |>
      rhandsontable::hot_col(c("TYPE"), readOnly = TRUE) |>
      rhandsontable::hot_col(c("dil"), readOnly = TRUE) |>
      rhandsontable::hot_context_menu(
        allowRowEdit = FALSE,
        allowColEdit = FALSE
      )
  })

  observeEvent(input$dilution_dt, {
    rhandsontable::hot_to_r(input$dilution_dt) |> current_dil_df()
  })

  dil_graphs_observer <- reactiveVal(NULL)
  observeEvent(input$gen_dil_graph, {
    req(class(current_plate()) == "RegisteredPlate")
    req(current_dil_df())

    tryCatch(
      {
        d <- current_dil_df()
        d[d == ""] <- NA
        x <- d |>
          select(where(function(x) !all(is.na(x)))) |> # FIXME
          # group_by(TYPE) |> # to make sure not mixing both things
          tidyr::fill(everything(), .direction = "downup") |>
          select(-"TYPE", -"dil") |>
          # ungroup() |>
          # .multi_graph()
          .gen_graph()

        dil_graphs_observer(x)

        shinyjs::show("dil_graph_grviz_card")
        shinyjs::show("export_dil_graph")
      },
      error = function(e) {
        showNotification(e$message, type = "error")
      }
    )
  })

  output$dil_graph_grviz_out <- DiagrammeR::renderGrViz({
    req(dil_graphs_observer())
    dil_graphs_observer() |>
      render_graph()
  })

  dilution_factor_label <- reactiveVal(NULL)
  observeEvent(input$dil_graph_grviz_out_click, {
    dil_graphs_observer()

    node_id <- input$dil_graph_grviz_out_click
    node_label <- ifelse(
      length(node_id$nodeValues) == 3,
      node_id$nodeValues[[3]],
      node_id$nodeValues[[2]]
    )
    DiagrammeR::get_edge_df(dil_graphs_observer()) |>
      dplyr::filter(.data$to == node_label) |>
      dplyr::pull("label") |>
      dilution_factor_label()

    if (length(dilution_factor_label()) == 0) {
      showNotification("Vial has no precedents", type = "warning")
    } else {
      output$selected_dilution_node_text <- renderText({
        paste0(
          "Selected Node: ",
          node_id$nodeValues[[1]],
          " (",
          node_id$nodeValues[[2]],
          ")"
        )
      })
    }
  })

  output$dilution_factor_text <- renderText({
    req(dilution_factor_label())
    paste0("Dilution Factor: ", dilution_factor_label())
  })

  output$final_vol_output <- renderText({
    req(input$final_vol_input)
    req(dilution_factor_label())
    paste0(
      "C1:(C2-C1):  ",
      .final_vol(dilution_factor_label(), input$final_vol_input)
    )
  })

  output$export_dil_graph <- downloadHandler(
    filename = function() {
      paste(Sys.Date(), input$dil_type, "_schema.png")
    },
    content = function(file) {
      DiagrammeR::export_graph(dil_graphs_observer(), file_name = file)
    }
  )

  ###############################################################################################
  # export
  exported_list <- reactiveVal(NULL)
  observeEvent(input$write_sample_list, {
    req(current_injec_seq())
    tryCatch(
      {
        write_injec_seq(current_injec_seq()) |> exported_list()

        show("export_sample_list")
        hide("write_sample_list")
      },
      error = function(e) {
        showNotification(e$message, type = "error")
      }
    )
    current_sample_list_metatable(.get_samplesdb_metadata())
  })
  output$plate_id_plateview_output <- renderText({
    paste0("Plate ID:", current_plate()@plate_id)
  })
  output$export_sample_list <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), "_sample_list.csv")
    },
    content = function(file) {
      download_sample_list(exported_list(), input$sample_list_vendor) |>
        write.csv(file, row.names = FALSE, na = "")
    }
  )

  output$export_plate_image <- downloadHandler(
    filename = function() {
      paste0(current_plate()@plate_id, ".jpeg")
    },
    content = function(file) {
      ggsave(
        file,
        current_plate() |>
          samples_naming_style(
            study_name = input$plate_map_study_name_switch,
            arm = input$plate_map_arm_switch,
            time = input$plate_map_time_switch,
            factor = input$plate_map_factor_switch,
            sex = input$plate_map_sex_switch,
            dose = input$plate_map_dose_switch,
            use_subject_id = input$plate_map_use_subject_id_switch,
            dilution = input$plate_map_dil_label_switch
          ) |>
          plot(
            color = input$plate_map_color_toggle,
            label_size = input$plate_map_font_size,
            transform_dil = input$plate_map_transform_dilution
          ),
        width = 20,
        height = 10
      )
    }
  )

  # reuse plate
  observeEvent(input$reuse_plate_button, {
    current_plate_id <- current_plate()@plate_id
    showModal(modalDialog(
      title = "Reuse Plate",
      h3("Plate ID: ", current_plate_id),
      numericInput("refill_gaps", "Displacements", value = 0),
      actionButton("reuse_plate_final_btn", "Reuse Plate")
    ))
  })

  observeEvent(input$reuse_plate_final_btn, {
    req(class(current_plate()) == "RegisteredPlate")
    tryCatch(
      {
        id <- as.numeric(strsplit(current_plate()@plate_id, "_")[[1]][1])

        curr_gen_plate_expr(bquote(reuse_plate(.(id), .(input$refill_gaps))))
        curr_gen_plate_starter(eval(curr_gen_plate_expr()))

        updateTabsetPanel(session, "main_tabs", "gen_tab") # switch

        shinyWidgets::show_alert(
          title = "Plate Ready To Reuse",
          text = tags$div(
            h3("A new variable captured in R. Please close this window now")
          )
        )

        # shiny::stopApp(x) # return the new plate
      },
      error = function(e) {
        showNotification(e$message, type = "error")
      }
    )

    removeModal()
  })

  ## methods
  methodsdb <- reactiveVal(.get_methodsdb()) # get methods from db
  current_method_capture_df <- reactiveVal(NULL)
  observeEvent(input$add_method, {
    i <- rep(NA, 5)

    current_method_capture_df(data.frame(
      compound = as.character(i),
      q1 = as.numeric(i),
      q3 = as.numeric(i),
      qualifier = as.logical(i),
      IS_id = as.character(i),
      expected_peak_start = as.numeric(i),
      expected_peak_end = as.numeric(i)
    ))

    showModal(modalDialog(
      title = "Add New Method",
      # either import a YAML file or manually add
      fluidPage(
        textInput("method_name", "Method Name"),
        textInput("method_description", "Description"),
        textInput("method_gradient", "Gradient"),
        textInput("method_column", "Column"),
        bslib::tooltip(
          bsicons::bs_icon("question-circle"),
          "For more compounds: Right-click > Insert row or use the autofill. \\n",
          "Compound and Q1 columns must be filled",
          placement = "right"
        ),
        rhandsontable::rHandsontableOutput("cmpd_methods_entry_dt"),
        actionButton("add_method_final_btn", "Add")
      )
    ))
  })

  output$cmpd_methods_entry_dt <- rhandsontable::renderRHandsontable({
    req(current_method_capture_df())
    current_method_capture_df() |>
      rhandsontable::rhandsontable(
        useTypes = TRUE,
        colHeaders = c(
          "Compound",
          "Q1",
          "Q3",
          "Qualifier?",
          "IS",
          "RT Start",
          "RT End"
        ),
        fillHandle = list(direction = "vertical", autoInsertRow = TRUE)
      )
  })
  observeEvent(input$cmpd_methods_entry_dt, {
    rhandsontable::hot_to_r(input$cmpd_methods_entry_dt) |>
      current_method_capture_df()
  })

  observeEvent(input$add_method_final_btn, {
    req(input$method_name)
    # remove complete NA rows
    # switch any "" to NA
    capture_method_cmpd_df <- current_method_capture_df() |>
      dplyr::mutate(q1 = as.numeric(.data$q1), q3 = as.numeric(.data$q3)) |> # convert to numeric|>
      dplyr::mutate(across(everything(), ~ ifelse(. == "", NA, .))) |>
      remove_all_empty_row()

    req(nrow(capture_method_cmpd_df) > 0)

    tryCatch(
      {
        # check if any cmpd is NA.
        if (any(is.na(capture_method_cmpd_df$compounds))) {
          stop("Compounds cannot be empty")
        }

        # if there is duplicate cmpd
        if (any(duplicated(capture_method_cmpd_df$compounds))) {
          stop("Duplicate compounds detected")
        }
      },
      error = function(e) {
        showNotification(e$message, type = "error")
      }
    )

    res <- list(
      method = input$method_name,
      description = input$method_description,
      gradient = input$method_gradient,
      column = input$method_column,
      compounds = capture_method_cmpd_df
    )

    tryCatch(
      {
        .save_cmpd_db(res)
      },
      error = function(e) {
        showNotification(e$message, type = "error")
      }
    )

    methodsdb(.get_methodsdb())
  })

  output$methods_dt <- DT::renderDT({
    shiny::validate(
      shiny::need(
        nrow(methodsdb()) > 0,
        "No methods in the database. Please add a method"
      )
    )
    methodsdb() |>
      DT::datatable(
        selection = list(mode = "single", target = "row"),
        options = list(scrollX = TRUE, scrollY = TRUE, scrollCollapse = TRUE)
      )
  })

  output$cmpd_methods_dt <- DT::renderDT({
    # get the method_id from the methodsdb
    shiny::validate(
      shiny::need(
        nrow(methodsdb()) > 0,
        "No methods in the database. Please add a method"
      ),
      shiny::need(
        length(input$methods_dt_rows_selected) == 1,
        "Please select a method"
      )
    )
    method_id <- methodsdb()[input$methods_dt_rows_selected, "method_id"]

    req(method_id)

    .get_method_cmpds(method_id) |>
      DT::datatable(
        selection = list(mode = "single", target = "row"),
        options = list(scrollX = TRUE, scrollY = TRUE, scrollCollapse = TRUE)
      )
  })

  # config
  config_module_server("config")

  # exit button ####
  observeEvent(input$exit, {
    shinyalert::shinyalert(
      "Are you sure you want to exit?",
      type = "warning",
      showConfirmButton = TRUE,
      showCancelButton = TRUE
    )
    if (input$exit) {
      stopApp()
    }
  })
}

#' @title bioanalytic_app
#' @description This function creates a shiny app for plate management
#' @import bsicons
#' @returns A shiny app. No default return value. Can return a PlateObj if reuse_plate_button is clicked
#' @export
study_app <- function() {
  #   js_checkboxdt <- c(
  #   "$('[id^=checkb]').on('click', function(){",
  #   "  var id = this.getAttribute('id');",
  #   "  var i = parseInt(/checkb(\\d+)/.exec(id)[1]);",
  #   "  var value = $(this).prop('checked');",
  #   "  var info = [{row: i, col: 1, value: value}];",
  #   "  Shiny.setInputValue('dtable_cell_edit:DT.cellInfo', info);",
  #   "})"
  # )

  # js_checkboxdt <-
  #    c(
  #   "$('body').on('click', '[id^=checkb]', function(){",
  #   "  var id = this.getAttribute('id');",
  #   "  var i = parseInt(/checkb(\\d+)/.exec(id)[1]);",
  #   "  var value = $(this).prop('checked');",
  #   "  var info = [{row: i, col: 1, value: value}];",
  #   "  Shiny.setInputValue('dtable_cell_edit:DT.cellInfo', info);",
  #   "})"
  # )

  grep_input <- function(pattern, x) {
    x |> names() |> grep(pattern = pattern, value = TRUE) |> sapply(\(y) x[[y]])
  }

  # module_compounds <- function(id, number){
  #   ns <- NS(id)

  #   tagList(fluidRow(
  #     id = ns("cmpd_holder"),

  #     column(
  #       width = 7,
  #       textInput( inputId = ns("compound_name"), label = paste0("Compound ", number))
  #     ),
  #     column(
  #       width = 4,
  #       numericInput( inputId = ns("compound_conc"),
  #       label = tooltip( trigger = list("conc/unit", bsicons::bs_icon("info-circle")),
  #         "Factor for how much concentation of compound per unit"), value = 1, min = 0.001, max = 1000)
  #     )
  #   ))
  # }

  methodsdb_init <- .get_methodsdb()

  runApp(
    list(ui = ui, server = study_app_server),
    port = 12344,
    host = '127.0.0.1',
    launch.browser = TRUE
  )
}
