config_module_ui <- function(id) {
  ns <- NS(id)
  actionButton(ns("config_btn"), "Configure Settings", icon = icon("cog"))
}


config_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(input$config_btn, {
      removeModal()
      showModal(modalDialog(
        title = "PKbioanalysis Configuration",
        textInput(
          ns("logkey"), 
          "Log Key (Column name in quant data for sample log ID)",
          value = "#"
        ),
        shiny::tags$hr(),
        textInput(
          ns("base_url"),
          "API Base URL",
          value = get_pkbioanalysis_option("api_base_url")
        ),
        textInput(
          ns("api_key"),
          "API Key",
          value = get_pkbioanalysis_option("api_key")
        ),
        selectInput(
          ns("model"),
          "AI Model",
          selected = get_pkbioanalysis_option("ai_model"),
          choices = c(
            'llama-3.1-70b-instruct',
            'llama-3.1-8b-instruct',
            'llama-3.1-nemotron-nano-8B-v1',
            'llama-3.3-70b-instruct',
            'mistral-7b-instruct',
            'mistral-small-3.1',
            'codestral-22b',
            'gemma-3-27b-it',
            'gpt-oss-20b',
            'gpt-oss-120b',
            'granite-3.3-8b-instruct',
            'sfr-embedding-mistral',
            'nomic-embed-text-v1.5',
            'flux.1-dev',
            'flux.1-schnell',
            'whisper-large-v3',
            'kokoro'
          )
        ),
        numericInput(
          ns("temperature"),
          "AI Response Temperature",
          value = get_pkbioanalysis_option("temperature"),
          min = 0,
          max = 1,
          step = 0.1
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("save_btn"),
            "Save",
            class = "btn btn-primary"
          )
        ),
        easyClose = TRUE
      ))
    })

    observeEvent(input$save_btn, {
      tryCatch(
        {
          update_config(
            logkey = input$logkey,
            base_url = input$base_url,
            api_key = input$api_key,
            model = input$model,
            temperature = input$temperature
          )
          refresh_config()
          showNotification(
            "Configuration saved successfully.",
            type = "message"
          )
        },
        error = function(e) {
          showNotification(
            paste("Error saving configuration:", e$message),
            type = "error"
          )
        }
      )
    })
  })
}
