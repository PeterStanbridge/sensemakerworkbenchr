# Security interface
handleSecurity <- function(session, input, openAPIEndPoint) {

  server_root <- 'https://openapi.sensemaker-suite.com/singularity/token'

  returnList <- vector("list", length = 6)
  names(returnList) <- c("security", "workbenchID", "language", "dataLanguages", "doReturn", "securitySettingsToken")
  returnList[["security"]] <- FALSE
  # we need to see if there is a workbenchID because we are going to test if it is authorised
  queryParms <- parseQueryString(session$clientData$url_search)
  workbenchID <- queryParms[['workbenchID']]
  returnList[["workbenchID"]] <- workbenchID

  language <- queryParms[['language']]
  returnList[["language"]] <- language
  # data languages is optional in case filtering wanted in the export
  dataLanguages <-  queryParms[['dataLanguages']]
  returnList[["dataLanguages"]] <- dataLanguages
  #  server_params specifies the redirect URL for returning.


  server_params <- paste0('redirect_uri=', utils::URLencode(paste0(session$clientData$url_protocol,
                                                                   "//", session$clientData$url_hostname, ":", session$clientData$url_port, session$clientData$url_pathname,
                                                                   "?workbenchID=", workbenchID), reserved = TRUE))

  # Retrieve the hash from the URL - this will be the token if it is there
  hash <- session$clientData$url_hash

  hashParams <- lapply(
    strsplit(strsplit(hash, '#', TRUE)[[1]], '&'),
    function(x){
      kvp <- strsplit(x, '=')
      if (length(kvp) == 0 || kvp[[1]][1] != 'token') NULL else kvp[[1]][2]
    }
  )



  if(is.null(isolate(input$wbtokStore$token)) && length(hashParams) == 0) {

    singularity$shiny_redirect(session, paste(server_root, server_params, sep="?"))
    returnList[["doReturn"]] <- TRUE
    return(returnList)
  }

  if (length(hashParams) > 0) {


   # updateQueryString(ifelse(base::trimws(workbenchID) != "/", paste0("?workbenchID=", workbenchID), ""))
    updateQueryString("?noParameters")
    # update the browser store with the token
    shinyStore::updateStore(session, name = "token", value = isolate(hashParams))


  } else {

    hashParams <- input$wbtokStore$token

  }
  securitySettingsToken <- hashParams[[2]]


  strings <- strsplit(securitySettingsToken, ".", fixed = TRUE)
  tokenInside <- rawToChar(jose::base64url_decode(strings[[1]][2]))
  jsonTokenInside <- jsonlite::fromJSON(tokenInside)

  tokenExpiry <- jsonTokenInside[["exp"]]


  # if the token is expired then get a refresh - updating the storage will restart the whole server

  #    if (ifelse(as.difftime(lubridate::ymd_hms(lubridate::now(tzone = "Europe/London"))
  # %--%  lubridate::as_datetime(tokenExpiry/1000, tz = "GB")) < 0, TRUE, FALSE)) {
 # if (as.difftime(now() %--% lubridate::as_datetime(tokenExpiry, tz = "UTC")) < 0) {
  if (as.difftime(interval(now(), as_datetime(tokenExpiry, tz = "UTC"))) < 0) {

    tok1Tok <- vector("list", length = 2)
    tok1Tok[[1]] <- NULL

    tok1 <- get2.4RefreshedTokan("openapi", jsonTokenInside[["refresh_token"]])

    if (length(unlist(tok1[[1]])) == 0) {
      singularity$shiny_redirect(session, paste(server_root, server_params, sep="?"))
      returnList[["doReturn"]] <- FALSE
      returnList[["security"]] <- TRUE
      return(returnList)
    }
    if (grepl("Refresh token was invalid or expired", unlist(tok1[[1]]), fixed = TRUE)) {
      singularity$shiny_redirect(session, paste(server_root, server_params, sep="?"))
      returnList[["doReturn"]] <- FALSE
      returnList[["security"]] <- TRUE
      return(returnList)
    }
    tok1 <- strsplit(tok1, "\"")

    tok1Tok[[2]] <- tok1[[1]][[which(tok1[[1]] %in% "access_token") + 2]]

    shinyStore::updateStore(session, name = "token", value = isolate(tok1Tok))

    returnList[["securitySettingsToken"]] <- securitySettingsToken
    returnList[["doReturn"]] <- FALSE
    returnList[["security"]] <- TRUE
    return(returnList)

  }

  returnList[["securitySettingsToken"]] <- securitySettingsToken
  returnList[["doReturn"]] <- FALSE
  returnList[["security"]] <- TRUE
 # print(paste("return list workbenchid", returnList[["workbenchID"]]))
  return(returnList)

}


filter_data <- function(fwd) {
  if (!is.null(fwd$df1)) {
  # for now the first MCQ with items count greater than 1 and use the first option
    ids <- unlist(purrr::map(fwd$sm_framework$get_single_select_list_ids(sig_class = "signifier"), ~ {fwd$sm_framework$get_list_num_items(.x)}))
    id <- fwd$sm_framework$get_single_select_list_ids(sig_class = "signifier")[which(ids > 1)[[1]]]
    id_value <- fwd$sm_framework$get_list_items_ids(id)[[1]]
  return(fwd$df1 %>% dplyr::filter(.data[[id]] == id_value))
  }
}

get_authorised_frameworks <- function(rtoken) {

  projectJSON <- get2.4FrameworkDefinition("openapi", "", rtoken)
  project_list <- as.list(projectJSON[["id"]])
  project_names <- projectJSON[["name"]]
  duplicatedFrameworks <- which(duplicated(project_names))
  for (i in seq_along(duplicatedFrameworks)) {
    project_names[duplicatedFrameworks[[i]]] <- paste(project_names[duplicatedFrameworks[[i]]], "Duplicate -", i)
  }
  names(project_list) <- project_names
  project_list <- project_list[order(names(project_list))]
  authorisedFrameworks <-  append(list(`nothing selected` = "nothing selected"), project_list)

  return(authorisedFrameworks)

}
get_authorised_dashboards <- function(rtoken) {

  dashboardJSON <- get2.4DashboardDefinition("openapi", "", rtoken)
  authorisedDashboards <-  dashboardJSON[["id"]]
  dashboardNames <- dashboardJSON[["name"]]
  duplicateddashboards<- which(duplicated(dashboardNames))
  for (i in seq_along(duplicateddashboards)) {
    dashboardNames[duplicateddashboards[[i]]] <- paste(dashboardNames[duplicateddashboards[[i]]], "Duplicate -", i)
  }
  names(authorisedDashboards) <- dashboardNames

  lAllowPublic <- FALSE
  if (length(authorisedDashboards) > 0) {
    numPublicDashboards <- length(which(grepl("PUBLIC", names(authorisedDashboards), fixed = TRUE)))
    if (!lAllowPublic) {
      if (numPublicDashboards > 0) {
        authorisedDashboards <- authorisedDashboards[-which(grepl("PUBLIC", names(authorisedDashboards), fixed = TRUE))]
      }
    }
    authorisedDashboards <- authorisedDashboards[order(names(authorisedDashboards))]
  }


    authorisedDashboards <-  append(list(`nothing selected` = "nothing selected"), authorisedDashboards)

  return(authorisedDashboards)

}

get2.4FrameworkDefinition <- function(topenAPIEndPoint, tworkbenchID, trToken) {

  # running for debugging
  if (1 == 3) {
    topenAPIEndPoint <- openAPIEndPoint
    tworkbenchID <- workbenchID
    trToken <- rToken
  }


  out <- try( {
    # get the json from the returned project definition

    return(fromJSON(content(GET(
      paste0("https://", topenAPIEndPoint, ".sensemaker-suite.com/apis/projectdefinition/",  tworkbenchID),
      add_headers(.headers = c('Authorization' = paste("Bearer", trToken, sep = " ")
                               , 'Content-Type' = 'application/json'))
    ), as = 'text', encoding = 'utf-8'), simplifyVector = TRUE, simplifyDataFrame = TRUE ,flatten = FALSE))

  }
  )
  if(inherits(out, "try-error"))
  {
    return(NULL)
  }
  if(inherits(out, "try-warning"))
  {
    return(NULL)
  }


  return(out)
}

get2.4DashboardDefinition <- function(topenAPIEndPoint, tcDashboardID, trToken) {
  return(fromJSON(content(GET(
    paste0("https://", topenAPIEndPoint, ".sensemaker-suite.com/apis/dashboards/",  tcDashboardID),
    add_headers(.headers = c('Authorization' = paste("Bearer", trToken, sep = " ")
                             , 'Content-Type' = 'application/json'))
  ), as = 'text', encoding = 'utf-8'), simplifyVector = TRUE, simplifyDataFrame = TRUE ,flatten = FALSE))
}


get2.4RefreshedTokan <- function(topenAPIEndPoint, trRefrshToken) {

  return(content(GET(
    paste0("https://openapi.sensemaker-suite.com/singularity/token/refresh?refresh_token=", trRefrshToken),
  ), as = 'text', encoding = 'utf-8', verbose()))
}

