
#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import shinydashboardPlus
#' @import sensemakerdatar
#' @import com.icatalyst.singularity
#' @import shinyWidgets
#' @import shinyjs
#' @import shinyvalidate
#' @import httr
#' @importFrom jsonlite fromJSON
#' @importFrom lubridate as_datetime interval now as_datetime as.difftime
#' @importFrom shinyStore updateStore
#' @importFrom purrr map
#' @importFrom dplyr filter
#' @importFrom utils URLencode
#' @importFrom jose base64url_decode
#' @importFrom stringr str_remove
#' @importFrom utils URLencode
#' @importFrom jose base64url_decode
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  # Basic helpers file


  source("R/helpers.R")
  security_token <- R6::R6Class("security_token",
                                public = list(
                                  #' @field tokenr
                                  tokenr = NULL,
                                  token_expires = NULL,
                                  refresh_token = NULL,
                                  #' @description
                                  #' Create a new instance of the security token object
                                  #' @param token The token to store
                                  initialize = function(token = NULL) {
                                    if (!is.null(token)) {
                                      self$tokenr <- token
                                      # extract expiry and refresh token and populate
                                      self$token_expires <- private$extract_expiry(token)
                                      self$refresh_token <- private$extract_refresh_token(token)
                                    }
                                  },
                                  #' @description
                                  #' Add a new token to the token object
                                  #' @param token The token to add
                                  add_token = function(token) {
                                    self$tokenr <- token
                                  },
                                  #' @description
                                  #' Get the current token
                                  #' @returns the token
                                  get_token = function() {
                                    return(self$tokenr)
                                  },
                                  #' @description
                                  #' Get the token expiry
                                  #' @returns the token expiry date/time
                                  get_token_expiry = function() {
                                    return(self$token_expires)
                                  },
                                  #' @description
                                  #' Get the token refresh token
                                  #' @returns the refresh token
                                  get_refresh_token = function() {
                                    return(self$refresh_token)
                                  }

                                ),
                                private = list(

                                  # extract out the expiry date from the token
                                  extract_expiry = function(token) {
                                    return("")
                                  },
                                  # extract out the expiry refresh token
                                  extract_refresh_token = function(token) {
                                    return("")
                                  }

                                )
  )

  token_object <- security_token$new()
  security_return <- reactiveValues(sec_values = NULL, return = FALSE, rtoken = NULL, refresh_token = NULL)
  control_values <- reactiveValues(workbenchID = NULL, allowed_workbench_ids = NULL, allowed_dashboard_ids = NULL, selected_workbench_id = "nothing selected", selected_dashboard_id = "nothing selected", authorised_click = FALSE)
  update_type <-  reactiveValues(type = NULL)
  build_headers <- reactiveVal(TRUE)
  fw_allowed <- reactive(get_authorised_frameworks(security_return$sec_values[["securitySettingsToken"]]))
  db_allowed <- reactive(get_authorised_dashboards(security_return$sec_values[["securitySettingsToken"]]))
  fwd <- reactive({
    if (is.null(update_type$type)) {
      tmpframework_id <- NULL
      tmpdashboard_id <- NULL
    } else {
      if (update_type$type == "dashboard") {tmpframework_id <- NULL} else {tmpframework_id <- control_values$selected_workbench_id}
      if (update_type$type == "workbench") {tmpdashboard_id <- NULL} else {tmpdashboard_id <- control_values$selected_dashboard_id}
    }

    sensemakerdatar::Data$new(framework_id = tmpframework_id, dashboard_id = tmpdashboard_id, token = security_return$sec_values[["securitySettingsToken"]])
  })


  # start security
  securityVal <- NULL
  securityNR <- FALSE


  # end point for the api calls.
  openAPIEndPoint <- "openapi"

  security_return$return <-FALSE
  observe ({

    while(!securityNR) {
    securityVal <- handleSecurity(session, input, openAPIEndPoint)
    security_return$sec_values <- securityVal
    security_return$rtoken <- securityVal[["securitySettingsToken"]]
    security_return$refresh_token <- securityVal[["refresh_token"]]
    if (securityVal[["doReturn"]]) {
      security_return$return <-TRUE
      break}
    securityNR <- securityVal$security
    # if ( security_return$return) {return()}
    if (securityNR) {break}
  }

    print("the token is")
    print(securityVal[["securitySettingsToken"]])

    if (!is.null(isolate(security_return$rtoken))) {

      control_values$allowed_workbench_ids <- unlist(unname(fw_allowed()))
      control_values$allowed_dashboard_ids <- unlist(unname(db_allowed()))

      if (control_values$authorised_click == FALSE) {
        if (!is.null((security_return$sec_values[["workbenchID"]])) &&   !((stringr::str_remove(security_return$sec_values[["workbenchID"]], "/") %in% control_values$allowed_workbench_ids) |
                                                                           (stringr::str_remove(security_return$sec_values[["workbenchID"]], "/") %in% control_values$allowed_dashboard_ids) )) {
          output$select_framework <- renderText("workbenchID in URL is invalid or not authorised")
        } else {
          if (!is.null(security_return$sec_values[["workbenchID"]])) {
            output$select_framework_framework <- renderText("")
            wb_id <- stringr::str_remove(security_return$sec_values[["workbenchID"]], "/")
            if (wb_id %in% control_values$allowed_workbench_ids) {
              control_values$selected_workbench_id <- wb_id

            } else {
              if (wb_id %in% control_values$allowed_dashboard_ids) {

                control_values$selected_dashboard_id <- wb_id
              }
            }

            updateQueryString("?noParameters", mode = "replace")
            # security_return$sec_values[["workbenchID"]] <- NULL
          }
        }
      }

      # we get the stuff
      if (!fwd()$is_invalid) {
        output$full_count <- renderText(ifelse(fwd()$has_zero_records, 0, nrow(fwd()$df1)))
        output$data_count <- renderText(ifelse(fwd()$has_zero_records, 0, nrow(filter_data(fwd()))))
        output$select_framework <- renderText(fwd()$sm_framework$get_parent_framework_name())
        # Assign the actual R6 class object to a non reactive
        nrfwd <- fwd()
      } else {
        output$full_count <- renderText(0)
        output$data_count <- renderText(0)
      }



      if(build_headers()) {

        output$authorisedFrameworks <- renderUI({

          tagList(
            dropdown(
              tags$h3("Collection options"),
              pickerInput(inputId = "authorisedFramework",
                          label = "Your Authorised Frameworks",
                          choices = fw_allowed(),
                          selected =  ifelse(!control_values$authorised_click, control_values$selected_workbench_id, "nothing selected"),
                          multiple = FALSE,
                          options = list(
                            `actions-box` = TRUE)
              ),
              pickerInput(inputId = "authorisedDashboard",
                          label = "Your Authorised Dashboards",
                          choices = db_allowed(),
                          selected = ifelse(!control_values$authorised_click, control_values$selected_dashboard_id, "nothing selected"),
                          multiple = FALSE,
                          options = list(
                            `actions-box` = TRUE)
              ),
              # style = "unite", icon = icon("gear"),
              tooltip = tooltipOptions(title = "Click to select workbench and language options"),
              status = "danger", width = "300px",
              label = "Select Workbench"
            )
          )
        })
        build_headers(FALSE)
      }


      observeEvent(input$authorisedFramework, ignoreInit = TRUE, handlerExpr = {
        if (input$authorisedFramework != "nothing selected") {

          securitySettingsToken <- input$wbtokStore$token[[2]]
          strings <- strsplit(securitySettingsToken, ".", fixed = TRUE)
          tokenInside <- rawToChar(jose::base64url_decode(strings[[1]][2]))
          jsonTokenInside <- jsonlite::fromJSON(tokenInside)
          tokenExpiry <- jsonTokenInside[["exp"]]

           if (lubridate::as.difftime(interval(now(), lubridate::as_datetime(tokenExpiry, tz = "Europe/London"))) < 0) {
        #  if (lubridate::as.difftime(now() %--% lubridate::as_datetime(tokenExpiry, tz = "UTC")) < 0) {
            tok1Tok <- vector("list", length = 2)
            tok1Tok[[1]] <- NULL
            tok1 <- get2.4RefreshedTokan(openAPIEndPoint, security_return$refresh_token)
            tok1 <- strsplit(tok1, "\"")
            tok1Tok[[2]] <- tok1[[1]][[which(tok1[[1]] %in% "access_token") + 2]]
            shinyStore::updateStore(session, name = "token", value = isolate(tok1Tok))
            security_return$rtoken <-  isolate(tok1Tok)
            security_return$sec_values[["securitySettingsToken"]] <- security_return$rtoken
          }


          if (input$authorisedDashboard != "nothing selected") {
            updatePickerInput(session = session, "authorisedDashboard", selected = "nothing selected")
          }
          if (input$authorisedFramework != "nothing selected") {
            #  output$select_framework <- renderText("")
          }
          update_type$type <- "workbench"
          control_values$authorised_click <- TRUE
          control_values$selected_workbench_id <- input$authorisedFramework
        }

      })

      observeEvent(input$authorisedDashboard, ignoreInit = TRUE, handlerExpr = {
        if (input$authorisedDashboard != "nothing selected") {
          if (input$authorisedFramework != "nothing selected") {
            updatePickerInput(session = session, "authorisedFramework", selected = "nothing selected")
          }
          if (input$authorisedDashboard != "nothing selected") {
            #   output$select_framework <- renderText("")
          }
          update_type$type <- "dashboard"
          control_values$authorised_click <- TRUE
          control_values$selected_dashboard_id <- input$authorisedDashboard

        }
      })



    }
  })
}
