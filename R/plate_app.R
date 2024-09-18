#' @title bioanalytic_app
#' @description This function creates a shiny app for plate management
#' @import shiny
#' @import bslib
#' @import bsicons
#' @import shinyWidgets
#' @import DiagrammeR
#' @importFrom shinyjs hide show enable disable useShinyjs
#' @returns A shiny app. No default return value. Can return a PlateObj if reuse_plate_button is clicked
#' @export
plate_app <- function() {


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



  grep_input <- function(pattern, x){
    x |> names() |> grep(pattern = pattern, value = TRUE) |>
      sapply(\(y) x[[y]])
  }

  module_compounds <- function(id, number){
    ns <- NS(id)

    tagList(fluidRow(
      id = ns("cmpd_holder"),

      column(
        width = 7,
        textInput( inputId = ns("compound_name"), label = paste0("Compound ", number))
      ),
      column(
        width = 4,
        numericInput( inputId = ns("compound_conc"),
        label = tooltip( trigger = list("conc/unit", bsicons::bs_icon("info-circle")),
          "Factor for how much concentation of compound per unit"), value = 1, min = 0.001, max = 1000)
      )
    ))
  }

  module_protocols <- function(id, number){
    ns <- NS(id)

    accordion_panel(
      # ns("prot_holder")
      title = paste0("Protocol ", number),
      value = paste0("protocol_", number),
      fluidRow(
        column(
          width = 3,
          selectizeInput(paste0("equi_vial_prot", number), "Equi Vial", choices = c("A1") ),
        ),
        column(
          width = 3,
          numericInput(paste0("equi_n_prot", number), "Equi N", value = 5),
        ),
        column(
          width = 3,
          numericInput(paste0("equi_vol_prot", number), "Equi Vol", value = 0.5),
        ),
      ),
      fileInput(paste0("inlet_method_fileinput_prot", number), "Inlet File", accept = c(".txt")),
      bslib::input_switch(paste0("exploratory_samples_alg_prot", number), "Exploratory Samples", value = FALSE) |>
        bslib::tooltip("Exploratory samples are samples that are not part of the sample list. They are used to check the system"),
      p("Repeats"),
      fluidRow(
        column(
          width = 3,
          numericInput(paste0("repeat_std_prot", number), "Standard", value = "1"),
        ),
        column(
          width = 3,
          numericInput(paste0("repeat_sample_prot", number), "Sample", value = "1") |>
            bslib::tooltip("Number of sample injections. Currently working only if there are no QCs"),
        ),
        column(
          width = 3,
          numericInput(paste0("repeat_qc_prot", number), "QC", value = "1") |>
            bslib::tooltip("Not working :(")
        ),
        column(
          width = 3,
          numericInput(paste0("system_suitability_number_prot", number), "Suitability", value = "3") |>
            bslib::tooltip("Number of suitability injections. Must set to 0 or remove it if not in the plate")
          ),
        column(
            width = 6,
            bslib::input_switch(paste0("blank_after_top_conc_prot", number), "Blank after top conc", value = TRUE)
          ),
          column(
            width = 6,
            bslib::input_switch(paste0("blank_at_end_prot", number), "Blank at end", value = TRUE)
          ),
          column(
            width = 6,
            numericInput(paste0("blank_every_n_prot", number), "Blank every n analytes", value = "20")
          ),
          column(
            width = 6,
            numericInput(paste0("injec_vol_prot", number), "Injection Volume", value = "10")
          ),
          column(
            width = 12,
            textInput(paste0("descr_prot", number), "Description", value = "") |>
              bslib::tooltip("Description of each injection. You can modify individually from the table")
          ),
          column(
            width = 6,
            textInput(paste0("suffix_prot", number), "Suffix", value = "1")
          )#,
          #column(
          #  width = 6,
          #  textInput(paste0("tray_prot", number), "Tray", value = "1")
          #)
      ))

  }



  ui <- bslib::page_navbar(
    title = "Plate Management",
    shinyjs::useShinyjs(),
    bslib::nav_panel(title = "Dashboard", p("Welome to my app!")), ## dashboard panel
    bslib::nav_panel(title = "Sample Lists",
      bslib::layout_sidebar(
        sidebar = sidebar(
          width = 500,
          actionButton("change_samplelist_metadata_descr_btn", "Change Description"),
          DT::DTOutput("sample_list_metatable_DT")
        ),
        actionButton("redownload_current_db_list_btn", "Download Current List", icon = icon("download")),
        actionButton("select_plates_current_list_btn", "Select Plates", icon = icon("check")),
        DT::DTOutput("sample_list_filtered_DT")

    )), ## sample lists panel
    bslib::nav_panel(title = "Plates",           ## plates panel
      bslib::layout_column_wrap(
        width = NULL, height = 1200,
        style = htmltools::css(grid_template_columns = "2fr 1fr"),
        bslib::card(
          full_screen = TRUE,
          card_header("Plate Map", popover(
            bs_icon("gear"),
            selectInput("plate_map_color_toggle", "Color By", choices = c("conc", "factor", "time", "TYPE", "samples")),
            numericInput("plate_map_font_size", "Font Size", value = 12),
            title = "Color By"
          )),
          plotOutput("plate_map_plot1", width = "100%", height = "500px" )
        ),
        bslib::card(
          textOutput("plate_id_plateview_output"),
          actionButton("change_plate_meta_btn", "Change Plate Description", icon = icon("edit")),
          downloadButton( "export_plate_image", "Export Plate Image", icon = icon("download")),
          actionBttn("reuse_plate_button", "Reuse Plate", icon = icon("redo"), color = "primary"),
          # tabset with plate, sample list, dilution
          actionButton("clear_selected_plates_btn", "Clear All"),
          DT::DTOutput("plate_db_table")
            ))),

    bslib::nav_panel(title = "Generators",       ## generators panel
      fluidPage(
          # tabset with plate, sample list, dilution
          bslib::navset_card_pill(
            id = "generator_nav",
            bslib::nav_panel("Sample List",
              bslib::layout_sidebar(
                sidebar = sidebar(
                  width = 500,
                  textOutput("plate_ids_for_sample_list"),
                  actionButton("add_protocols", "Add Protocol"),
                  bslib::accordion(
                    id = "protocols_accordion",
                    bslib::accordion_panel(
                      title = "Protocol 1",
                      value = "protocol_1",
                      fileInput("inlet_method_fileinput_prot1", "Inlet File", accept = c(".txt")),
                      bslib::input_switch("exploratory_samples_alg_prot1", "Exploratory Samples", value = FALSE) |>
                          bslib::tooltip("Exploratory samples are samples that are not part of the sample list. They are used to check the system"),
                      p("Repeats"),
                      fluidRow(
                        column(
                          width = 3,
                          numericInput("repeat_std_prot1", "Standard", value = "1")
                        ),
                        column(
                          width = 3,
                          numericInput("repeat_sample_prot1", "Sample", value = "1") |>
                            bslib::tooltip("Number of sample injections. Currently working only if there are no QCs"),
                        ),
                        column(
                          width = 3,
                          numericInput("repeat_qc_prot1", "QC", value = "1") |>
                            bslib::tooltip("Not working :(")
                        ),
                        column(
                          width = 3,
                          numericInput("system_suitability_number_prot1", "Suitability", value = "3") |>
                            bslib::tooltip("Number of suitability injections. Must set to 0 or remove it if not in the plate")
                          ),
                        column(
                            width = 6,
                            bslib::input_switch("blank_after_top_conc_prot1", "Blank after top conc", value = TRUE)
                          ),
                          column(
                            width = 6,
                            bslib::input_switch("blank_at_end_prot1", "Blank at end", value = TRUE)
                          ),
                          column(
                            width = 6,
                            numericInput("blank_every_n_prot1", "Blank every n analytes", value = "20")
                          ),
                          column(
                            width = 6,
                            numericInput("injec_vol_prot1", "Injection Volume", value = "10")
                          ),
                          column(
                            width = 12,
                            textInput("descr_prot1", "Description", value = "") |>
                              bslib::tooltip("Description of each injection. You can modify individually from the table")
                          ),
                          column(
                            width = 6,
                            textInput("suffix_prot1", "Suffix", value = "1")
                          ),
                          column(
                            width = 6,
                            selectInput("tray_prot1", "Tray", choices = as.character(1:12), multiple = TRUE)
                          ))),
                    div(id = "prot_holder"),
                    accordion_panel(
                      title = "Compounds dilution",
                      value = "compounds_accordion",
                      div(id = "cmpd_holder"),
                      fluidRow(
                        column(width = 2, actionButton("add_cmpd", "Add")),
                        column(width = 2, actionButton("remove_cmpd", "Remove"))),
                      )
                    ),
                  actionButton("create_sample_list", "Create Sample List")),
                  bslib::navset_bar(
                    id = "sample_list_nav",
                    bslib::nav_panel("Sample List",  DT::DTOutput("sample_list_table")),
                    bslib::nav_panel("Summary",
                      p("Check if total volume is OK. Volume will depend on injection and filtration modes"),
                      fluidRow(
                        column(width = 4,
                          textOutput("total_injections"),
                          textOutput("max_vol"), textOutput("min_vol")),
                        column(width = 8, DT::DTOutput("sample_list_summary")),
                      )),
                    bslib::nav_panel("Export",
                        selectInput("sample_list_vendor", "Select Vendor", choices = c("masslynx", "masshunter")),
                        actionButton("write_sample_list", "Write Sample List"),
                        downloadButton("export_sample_list", "Export", icon = icon("download"))
                )))),
            bslib::nav_panel("Dilution",
                      h2("Dilution"),
                      layout_column_wrap(
                        width = 1/2, #height = 100,
                        numericInput("dil_factor", "Parallel Dilution Factor", value = "10"),
                        textInput("dil_unit", "Dilution Unit", value = "ng"),
                        selectInput("dil_type", "Vial Type", choices = c("Standard", "QC")),
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
                      DT::DTOutput("dilution_dt"),
                      actionButton("gen_dil_graph", "Generate Dilution Graph", icon = icon("chart-line")),
                      bslib::card(
                          id = "dil_graph_grviz_card",
                          full_screen = TRUE,
                          height = 500,
                          card_header("Schema"),
                          DiagrammeR::grVizOutput("dil_graph_grviz_out", width = "100%")),
                      downloadButton("export_dil_graph", "Export", icon = icon("download"))
            )
          ))),
    nav_spacer(),
    bslib::nav_menu(
      title = "Links",
      align = "right",
      nav_item(shiny::actionButton("exit", "Exit", icon = icon("power-off"))),
      nav_item(shiny::actionButton("help", "Help", icon = icon("question-circle")))
    )
    )



  server <- function(input, output, session) {
    ########################## sample list


    current_sample_list_metatable <- reactiveVal(.get_samplesdb_metadata())
    output$sample_list_metatable_DT <- DT::renderDT({
      current_sample_list_metatable() |>
        DT::datatable(
          selection = list(mode = "single", target = "row"),
          options = list(scrollX=TRUE, scrollY=TRUE, scrollCollapse=TRUE)
        )
    })

    current_visible_sample_db <- reactiveVal(NULL)
    observeEvent(input$sample_list_metatable_DT_rows_selected, {
      # get row id and recover sample list from db
      index <- input$sample_list_metatable_DT_rows_selected
      id <- current_sample_list_metatable() |>
        filter(row_number() == index) |> pull(id)
      .get_samplelist(id)  |> select(-"row", -"col", -"list_id") |>
        current_visible_sample_db()
    })

    output$sample_list_filtered_DT <- DT::renderDT({
      req(current_visible_sample_db())
      current_visible_sample_db() |>
        DT::datatable(
          selection = list(mode = "single", target = "row"),
          options = list(scrollX=TRUE, scrollY=TRUE, scrollCollapse=TRUE)
        )
    })
    observeEvent(input$redownload_current_db_list_btn, {
      showModal(modalDialog(
        title = "Redownload Current List",
        selectInput("vendor_redownload", "Vendor", choices = c("masslynx", "masshunter")),
        downloadButton("redownload_btn_final", "Redownload")
      ))
    })

    output$redownload_btn_final <- downloadHandler(
      filename = function(){
        paste0(Sys.Date(), "_sample_list.csv")
      },
      content = function(file){
        download_sample_list(current_visible_sample_db(), input$vendor_redownload) |>
          write.csv(file, row.names = FALSE)
      }
    )


    ############################### plate

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
    # https://stackoverflow.com/questions/34157684/dynamic-number-of-actionbuttons-tied-to-unique-observeevent



    # default to last
    observeEvent(plate_db(), {
      current_plate(plate_db()[1, "id"] |> .retrieve_plate())
    })


    output$plate_ids_for_sample_list <- renderText({
      req(class(current_plate()) == "PlateObj")
      paste0("Selected Plates ID: ", paste(selected_ids(), collapse = "& "))
    })



    insertUI(
      selector = "#cmpd_holder",
      where    = "beforeEnd",
      ui       = tagList(module_compounds("var1", 1))
    )



    # This chuck look if the number of protocols is increased and add the new protocol
    # observeEvent(current_injec_protcols(), {
    #   print("added obs")
    #   observeEvent(input[[paste0("add_cmpd_prot", current_injec_protcols())]], {
    #     cmpd_last <- sum(input[[paste0("add_cmpd_prot", 1)]], 1)
    #     insertUI(
    #       selector = paste0("#cmpd_holder_prot", current_injec_protcols()),
    #       where    = "beforeEnd",
    #       ui       = tagList(module_compounds(paste0("var", cmpd_last), cmpd_last))
    #     )

    #   })


    #   already_removed <- reactiveVal(1)
    #   observeEvent(input[[paste0("remove_cmpd_prot", current_injec_protcols())]], {
    #     cmpd_last <- names(input) |> grep(pattern = "^var\\d+\\-compound_name", value = TRUE) |>
    #       gsub(pattern = "^(var)(\\d+).*", replacement = "\\2") |> as.numeric()

    #     cmpd_last <- cmpd_last[!cmpd_last %in% already_removed()] |> max()

    #     req(cmpd_last > 1)

    #     removeUI(
    #       selector = paste0("#var", cmpd_last, paste0("-cmpd_holder_prot", current_injec_protcols()))
    #     )

    #     already_removed(c(already_removed(), cmpd_last))
    #   })
    # })


      observeEvent(input$add_cmpd, {
        cmpd_last <- sum(input$add_cmpd, 1)
        insertUI(
          selector = "#cmpd_holder",
          where    = "beforeEnd",
          ui       = tagList(module_compounds(paste0("var", cmpd_last), cmpd_last))
        )

      })


      already_removed <- reactiveVal(1)
      observeEvent(input$remove_cmpd, {
        cmpd_last <- names(input) |> grep(pattern = "^var\\d+\\-compound_name", value = TRUE) |>
          gsub(pattern = "^(var)(\\d+).*", replacement = "\\2") |> as.numeric()

        cmpd_last <- cmpd_last[!cmpd_last %in% already_removed()] |> max()

        req(cmpd_last > 1)

        removeUI(
          selector = paste0("#var", cmpd_last, "-cmpd_holder")
        )

        already_removed(c(already_removed(), cmpd_last))
      })


  # for each protcol add, extra protocol accordion panel

  observeEvent(input$add_protocols, {
    protocol_last <- sum(input$add_protocols, 1)

    if(protocol_last > 12){
      showNotification("Maximum number of protocols reached", type = "warning")
      req(FALSE)
    }

    insertUI(
      selector = "#prot_holder",
      where    = "beforeEnd",
      ui       = tagList(module_protocols(paste0("var", protocol_last), protocol_last))
    )

    insertUI(
      selector = paste0("#cmpd_holder_prot", protocol_last),
      where    = "beforeEnd",
      ui       = tagList(module_compounds("var1", 1))
    )

    current_injec_protcols(current_injec_protcols() + 1)

  }, priority = 1)

   output$plate_db_table <- DT::renderDT({
      # cbind(
      #   check = shinyInput(checkboxInput,nrow(plate_db()), "checkdb"),
      #   plate_db()) |>
        plate_db() |>
        DT::datatable(
          # rownames = TRUE,
          escape = FALSE,
          # editable = list(target = "cell", disable = list(columns = 1)),
          selection = list(target = "row", mode = "multiple"),
          # callback = JS(js_checkboxdt)#,
          options = list( scrollX=TRUE, scrollY=TRUE, scrollCollapse=TRUE)
          )
    }, server = FALSE)

    observeEvent(input$plate_db_table_rows_selected, {
      current_plate_row(input$plate_db_table_rows_selected)
    })


    selected_ids <- reactiveVal(NULL)

    output$plate_map_plot1 <- renderPlot({
       plate_db()[current_plate_row(),  ]$id |> selected_ids()

      # select last id for current plate list
      .retrieve_plate(rev(selected_ids())[[1]]) |> current_plate()
      plot(current_plate(), color = input$plate_map_color_toggle) +
          theme(text = element_text(size = input$plate_map_font_size))
    })


########################
    observeEvent(selected_ids(), {

      req(selected_ids())
      req(current_injec_protcols() > 0)
      index <- ifelse(current_injec_protcols() < 13, current_injec_protcols(), 12)
      for(i in 1:index){
        updateSelectizeInput( inputId = paste0("plate_id_prot", i), choices = selected_ids())
      }
      }
    )

    observeEvent(current_injec_protcols(), {
      req(current_injec_protcols() > 0)
        updateSelectizeInput( inputId = paste0("plate_id_prot", current_injec_protcols()),
          choices = selected_ids())
    }
    )

    # remove dilutions tab if no std
    observeEvent(current_plate(), {
      if(.last_std(current_plate()) == 0){
        nav_hide("generator_nav", "Dilution")
      } else{
        nav_show("generator_nav", "Dilution")
      }
    })
#########################



    # nav_hide("sample_list_nav", "Export")
    # disable and clear export sample list on any change till click regenerate again.
    lock_export <- reactiveVal(TRUE) # FIXME
    observeEvent(current_injec_protcols(), {
      req(current_injec_protcols() >=1)
      if(current_injec_protcols()  <= 10){
        observeEvent(
          c(input[[paste0("repeat_std_prot", current_injec_protcols())]], input[[paste0("repeat_qc_prot", current_injec_protcols())]],
            input[[paste0("repeat_sample_prot", current_injec_protcols())]], input[[paste0("system_suitability_number_prot", current_injec_protcols())]],
            input[[paste0("blank_after_top_conc_prot", current_injec_protcols())]], input[[paste0("blank_at_end_prot", current_injec_protcols())]],
            input[[paste0("blank_every_n_prot", current_injec_protcols())]], input[[paste0("injec_vol_prot", current_injec_protcols())]],
            input[[paste0("descr_prot", current_injec_protcols())]], input[[paste0("suffix_prot", current_injec_protcols())]],
            input[[paste0("tray_prot", current_injec_protcols())]],
            input[[paste0("exploratory_samples_alg_prot", current_injec_protcols())]],
            current_cmpd_df(), input$add_cmpd, input$remove_cmpd
            ), {

            # nav_hide("sample_list_nav", "Export")
            # nav_hide("sample_list_nav", "Summary")
            # nav_hide("sample_list_nav", "Sample List")

            hide("write_sample_list")
            hide("export_sample_list")
            lock_export(TRUE) # FIXME introduce a loop bug, but without it the tables will not clear
        })
      }
    })



    current_cmpd_df <- reactiveVal(NULL)
    observeEvent(input$create_sample_list, {
      req(class(current_plate()) == "PlateObj")

      cmpds_df <- data.frame(
        compound_name =  grep_input("^var\\d+\\-compound_name", input),
        compound_conc = grep_input("^var\\d+\\-compound_conc", input)
      )
      current_cmpd_df(cmpds_df)


      tryCatch(
        {
          plates_list <- list()
          injseq_list <- list()


          index_plates <- if(length(selected_ids()) == 1) 1 else seq(1, length(selected_ids()), 1)

          for(i in index_plates){
            plates_list[[i]] <- .retrieve_plate(selected_ids()[[i]])
          }

          plates_list <- combine_plates(plates_list) # one big plate

          # create custom protocol for the big plate
          index_prot <- ifelse(current_injec_protcols() < 13, current_injec_protcols(), 12)
          index_prot <- if(index_prot == 1) 1 else seq(1, index_prot, 1)

          for(i in index_prot){
             injseq_list[[i]] <- plates_list |>
              build_injec_seq(descr = input[[paste0("descr_prot", i)]],
                inlet_method =   input$inlet_method_fileinput_prot1$name,
                suffix = input[[paste0("suffix_prot", i)]],
                tray = input$tray_prot1, # always the same
                blank_after_top_conc = input[[paste0("blank_after_top_conc_prot", i)]],
                blank_at_end = input[[paste0("blank_at_end_prot", i)]],
                blank_every_n = input[[paste0("blank_every_n_prot", i)]],
                system_suitability = input[[paste0("system_suitability_number_prot", i)]],
                repeat_std = input[[paste0("repeat_std_prot", i)]],
                repeat_analyte = input[[paste0("repeat_sample_prot", i)]],
                repeat_qc = input[[paste0("repeat_qc_prot", i)]],
                explore_mode = input[[paste0("exploratory_samples_alg_prot", i)]],
                conc_df = current_cmpd_df(),
                inject_vol = input[[paste0("injec_vol_prot", i)]])
          }


          # enable export button
          nav_show("sample_list_nav", "Export")
          nav_show("sample_list_nav", "Summary")
          nav_show("sample_list_nav", "Sample List")

          shinyjs::show("write_sample_list")
          shinyjs::enable("write_sample_list")
          shinyjs::hide("export_sample_list")

          nav_select("sample_list_nav", "Sample List")

          if(length(injseq_list) == 1){
            current_injec_seq(injseq_list[[1]])
          } else{
            combine_injec_lists(injseq_list , equi_pos = "A,3") |> current_injec_seq()
          }


          lock_export(FALSE)

        },
        error = function(e) {showNotification(e$message, type = "error")}
      )

    })

    # change plate metadata of descr and instrument
    observeEvent(input$change_plate_meta_btn, {
      showModal(modalDialog(
        title = "Change Plate Description",
        textInput("new_plate_descr", "New Description", value = current_plate()$desc),
        pickerInput("compounds_metadata", "Compounds", choices = "", multiple = TRUE, options = list(`live-search` = TRUE)),
        pickerInput("instruments_metadata", "Instruments", choices = "", multiple = TRUE, options = list(`live-search` = TRUE)),
        pickerInput("IS_metadata", "Internal Standards", choices = "", multiple = TRUE, options = list(`live-search` = TRUE)),
        pickerInput("solvents_metadata", "Solvents", choices = "", multiple = TRUE, options = list(`live-search` = TRUE)),
        actionButton("change_plate_descr_btn_final", "Change")
      ))
    })
    observeEvent(input$change_plate_descr_btn_final, {
      req(class(current_plate()) == "PlateObj")
      tryCatch(
        {
          current_plate() |> plate_metadata(input$new_plate_descr)
          plate_db(.get_plates_db())
        },
        error = function(e) {showNotification(e$message, type = "error")}
      )
    })

    output$sample_list_table <- DT::renderDT({
      unique_vol <- unique(current_injec_seq()$injec_list$INJ_VOL)
      unique_conc <- unique(current_injec_seq()$injec_list$conc)

      if(!lock_export()){
        # red pallete
        redpal <- colorRampPalette(c("red", "white"))(length(unique_vol)) |>
          paste0(50)

        # blue color plalle
        bluepal <- colorRampPalette(c("blue", "white"))(length(unique_conc)) |>
          paste0(50)

        req(class(current_plate()) == "PlateObj")
        req(current_injec_seq())

        showNotification("Check the summary tab for total volume", type = "message")

        current_injec_seq()$injec_list  |>
        dplyr::select("Index", "FILE_NAME", "FILE_TEXT", "SAMPLE_LOCATION",
          "INJ_VOL",  "conc", "TYPE", starts_with("COMPOUND"), starts_with("CONC")) |>
        dplyr::rename("Sample Location" = .data$SAMPLE_LOCATION, Description = .data$FILE_TEXT) |>
        mutate(FILE_NAME = paste0(.data$FILE_NAME, "_R", row_number())) |> # only visual reflection for actual result
        DT::datatable(
          selection = list(mode = "single", target = "cell"),
          options = list(scrollX=TRUE, scrollY = "550px",
          scrollCollapse=TRUE, dom = "ft", pageLength = 10000000), rownames = FALSE) |>
        DT::formatStyle(columns = "INJ_VOL", valueColumns = "INJ_VOL",
          backgroundColor = DT::styleEqual(unique_vol, redpal)) |>
        DT::formatStyle(columns = "conc", valueColumns = "conc",
          backgroundColor = DT::styleEqual(unique_conc, bluepal))
      } else{
        NULL
      }
    })


    current_injec_seq_summary <- reactiveVal(NULL)

    # outputOptions(output, "sample_list_summary", suspendWhenHidden = FALSE)
    # outputOptions(output, "total_injections", suspendWhenHidden = FALSE)
    # outputOptions(output, "max_vol", suspendWhenHidden = FALSE)
    # outputOptions(output, "min_vol", suspendWhenHidden = FALSE)

    output$sample_list_summary <- DT::renderDT({
      req(class(current_plate()) == "PlateObj")
      req(current_injec_seq())

      if(!lock_export()){
        d <- current_injec_seq()$injec_list  |>
          dplyr::select("INJ_VOL", "SAMPLE_LOCATION", "value") |>
          dplyr::summarise(total_vol = sum(.data$INJ_VOL), .by = c("SAMPLE_LOCATION", "value"))

        current_injec_seq_summary(d)

        DT::datatable(d, options = list(scrollX=TRUE,
          scrollCollapse=TRUE , dom = "ft", scrollY = "550px"))
      } else{
        NULL
      }
    })

    output$total_injections <- renderText({
      req(class(current_plate()) == "PlateObj")
      req(current_injec_seq())

      if(!lock_export()){
        x <- nrow(current_injec_seq()$injec_list)
        paste0("Total Injections: ", x)
      } else{
        NULL
      }

    })

    output$max_vol <- renderText({
      req(class(current_plate()) == "PlateObj")
      req(current_injec_seq())

      if(!lock_export()){
        max_vol <- current_injec_seq_summary() |> dplyr::pull(.data$total_vol) |> max()
        paste0("Max Volume: ", max_vol)
      } else{
        NULL
      }
    })

    output$min_vol <- renderText({
      req(class(current_plate()) == "PlateObj")
      req(current_injec_seq())

      if(!lock_export()){
        min_vol <- current_injec_seq_summary() |> dplyr::pull(.data$total_vol) |> min()
        paste0("Min Volume: ", min_vol)
      } else{
        NULL
      }
    })
###############################################################################################
    ### Dilutions
    current_dil_df <- reactiveVal(NULL)
    parallel_dil_df <- reactiveVal(NULL)
    observeEvent(input$dilute, { # click dilute button to only generate parallel table
      req(class(current_plate()) == "PlateObj")

      d <- tryCatch(
        .parallel_dilution(current_plate(),
          fold = input$dil_factor, unit = input$dil_unit,
          type = input$dil_type, rep = as.numeric(input$dil_rep)
        ),
          error = function(e) {showNotification(e$message, type = "error")}
          )

      empty_rows <- data.frame(v4 = NA, v3 = NA, v2 = NA)
      d <- cbind(empty_rows, d)

      if(input$dil_type == "QC"){ # delete the vial position for now and aggregate
        d$v0 <- gsub("(.*)_(.*)", "\\1", d$v0)
        d <- d |> distinct()
      }

      current_dil_df(d)
      # parallel_dil_df(d[,c(4,5)] |> .gen_graph())

      # nodes <- c(d$v1, d$v0)
      # adj_matrix <- matrix(0, nrow = length(nodes), ncol = length(nodes))
      # rownames(adj_matrix) <- colnames(adj_matrix) <- nodes
      # for (i in 1:(nrow(d))) {
      #   from <- d$v1[i]
      #   to <- d$v0[i]
      #   adj_matrix[from, to] <- 1
      # }

      # shinyMatrix::updateMatrixInput(session, "lower_tri_matrix", value = adj_matrix)

      shinyjs::hide("dil_graph_grviz_card")
      shinyjs::hide("export_dil_graph")
      })

    output$dilution_dt <- DT::renderDT({
      req(class(current_plate()) == "PlateObj")
      req(current_dil_df())

      current_dil_df() |>
        DT::datatable(
        # colnames = rep("", ncol(current_dil_df())),
        rownames = FALSE,
        colnames = c(rep("From/To", 4), "To",  "Type"),
        options = list(ordering = FALSE,
                  dom = "ft", scrollY = "300px", scrollX = TRUE, pageLength = 10000),
         editable = list(target = "all", disable = list(columns = c(4,5,6))))
    })

    proxy = dataTableProxy('dilution_dt')
    observeEvent(input$dilution_dt_cell_edit, {
      print(input$dilution_dt_cell_edit)
      DT::editData(current_dil_df(), input$dilution_dt_cell_edit, 'dilution_dt', rownames = FALSE) |>
        current_dil_df()
    })

    dil_graphs_observer <- reactiveVal(NULL)
    observeEvent(input$gen_dil_graph, {
      req(class(current_plate()) == "PlateObj")
      req(current_dil_df())

      d <- current_dil_df()
      d[d == ""] <- NA
      x <- d |>
        select( where(function(x) !all(is.na(x)))) |> # FIXME
        # group_by(TYPE) |> # to make sure not mixing both things
        tidyr::fill(everything(), .direction = "downup") |>
        select(-"TYPE") |>
        # ungroup() |>
        # .multi_graph()
        .gen_graph()

      dil_graphs_observer(x)

      shinyjs::show("dil_graph_grviz_card")
      shinyjs::show("export_dil_graph")
    })



    # trim the matrix and give error if not numeric of off diagonal
    # observe({
    #   mat <- input$lower_tri_matrix
    #   mat[lower.tri(mat)] <- NA  # Set upper triangular part to NA
    #   diag(mat) <- NA


    #    # Restrict values to 0, NA, or 1
    #   mat <- apply(mat, c(1, 2), function(x) {
    #   if (is.na(x) || x == 0 || x == 1) {
    #     return(x)
    #   } else {
    #     return(NA)
    #   }
    #   })

    #   shinyMatrix::updateMatrixInput(session, "lower_tri_matrix", value = mat)
    # })

    # observeEvent(input$add_dil_cmpd_btn , {
    #   req(input$add_dil_cmpd_textinput)

    #   expanded_matrix <- matrix(0, nrow = nrow(input$lower_tri_matrix) + 1, ncol = ncol(input$lower_tri_matrix) + 1)
    #   rownames(expanded_matrix) <- colnames(expanded_matrix) <- c(rownames(input$lower_tri_matrix), input$add_dil_cmpd_textinput)

    #   # Find the indices of the non-zero elements
    #   edges <- which(adj_matrix != 0, arr.ind = TRUE)

    #   # Create a dataframe with "to" and "from" columns
    #   df <- data.frame(
    #     from = rownames(adj_matrix)[edges[, 1]],
    #     to = colnames(adj_matrix)[edges[, 2]]
    #   )

    #   shinyMatrix::updateMatrixInput(session, "lower_tri_matrix", value = expanded_matrix)

    # })

    output$dil_graph_grviz_out <- DiagrammeR::renderGrViz({
      req(dil_graphs_observer())
      dil_graphs_observer() |>
        render_graph()
    })


    dilution_factor_label <- reactiveVal(NULL)
    observeEvent(  input$dil_graph_grviz_out_click, {
      dil_graphs_observer()

      node_id <- input$dil_graph_grviz_out_click
      edge_id <- select_edges_by_node_id(dil_graphs_observer(),
        grep(x = node_id$id,  pattern = "\\d+")) |>
        get_selection()

       get_edge_df(dil_graphs_observer()) |>
        filter(id == edge_id) |> pull(.data$label) |> dilution_factor_label()

      # get_edge_df(dil_graphs_observer()) |>
      #   mutate(label = ifelse(.data$id == edge_id, label, label))

      showModal(modalDialog(
        node_id$nodeValues[[1]],
        paste0("Dilution factor: ", dilution_factor_label()),
        numericInput("final_dil_vol", "Final Volume", value = 1, min = 0.1, max = 10000),
        textOutput("final_vol_output")
      ))
    })

    output$final_vol_output <- renderText({
      paste0("Final Volume: ", .final_vol(dilution_factor_label(), input$final_dil_vol))
    })

    output$export_dil_graph <- downloadHandler(
      filename =  function(){
        paste(Sys.Date(), input$dil_type ,"_schema.png")
      },
      content = function(file) {
        DiagrammeR::export_graph(dil_graphs_observer(), file_name = file)
      }
    )

    # export
    exported_list <- reactiveVal(NULL)
    observeEvent(input$write_sample_list, {
      req(current_injec_seq())
      tryCatch({
        write_injec_seq(current_injec_seq()) |> exported_list()

        show("export_sample_list")
        hide("write_sample_list")

      },
        error = function(e) {showNotification(e$message, type = "error")}
      )

      current_sample_list_metatable(.get_samplesdb_metadata())
    })
    output$plate_id_plateview_output <- renderText({
      paste0("Plate ID:", current_plate()$plate_id)
    })
    output$export_sample_list <- downloadHandler(
      filename =  function(){
        paste0(Sys.Date(), "_sample_list.csv")
      },
      content = function(file) {
          download_sample_list(exported_list(), input$sample_list_vendor) |>
            write.csv(file, row.names = FALSE, na = "")
      }
    )

    output$export_plate_image <- downloadHandler(
      filename = function(){
        paste0(current_plate()$plate_id, ".png")
      },
      content = function(file){
        ggsave(file,  current_plate() |>
            plot(color = input$plate_map_color_toggle))
      }
    )

    # reuse plate
    observeEvent(input$reuse_plate_button, {
      current_plate_id <- current_plate()$plate_id
      showModal(modalDialog(
        title = "Reuse Plate",
        h3("Plate ID: ", current_plate_id),
        numericInput("refill_gaps", "Displacements", value = 0),
        actionButton("reuse_plate_final_btn", "Reuse Plate")
      ))
    })

    observeEvent(input$reuse_plate_final_btn, {
      req(class(current_plate()) == "PlateObj")
      tryCatch(
        {
        id <- as.numeric(strsplit(current_plate()$plate_id, "_")[[1]][1])
        #assign("plate", reuse_plate(id, input$refill_gaps), envir = .GlobalEnv)
        reuse_plate(id, input$refill_gaps)
        } ,
        error = function(e) {showNotification(e$message, type = "error")}
      )

    removeModal()
    show_alert(
      title = "Plate Successfully Exported",
      text = tags$div(
        h3("A new variable has been assigned in the environment. \n you can exit the app.")
      )
      )

    })

    # exit button ####
    observeEvent(input$exit, {
      shinyalert::shinyalert("Are you sure you want to exit?",
        type = "warning",
        showConfirmButton = TRUE,
        showCancelButton = TRUE
      )
    })

  }
  runApp(list(ui = ui, server = server))
}
