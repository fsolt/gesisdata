#' Download datasets from the GESIS data archive
#'
#' \code{gesis_download} provides a programmatic and reproducible means to download datasets 
#'   from the GESIS data archive
#'
#' @param file_id The unique identifier (or optionally a vector of these identifiers).
#'  for the dataset(s) to be downloaded (see details).
#' @param email,password Your GESIS email and password (see details).
#' @param use The number of a 'use of data' (see details).
#' @param reset If TRUE, you will be asked to re-enter your username, password, and use.
#' @param download_dir The directory (relative to your working directory) to
#'   which files from the GESIS data archive will be downloaded.
#' @param msg If TRUE, outputs a message showing which data set is being downloaded.
#' @param convert If TRUE, converts downloaded file(s) to .RData format.
#' @param delay If the speed of your connection to the GESIS data archive is particularly slow, 
#'   \code{gesis_download} may encounter problems.  Increasing the \code{delay} parameter
#'   may help.
#'
#' @details 
#'  To avoid requiring others to edit your scripts to insert their own email, 
#'  password, and use or to force them to do so interactively, the default is set to fetch 
#'  this information from the user's .Rprofile.  Before running \code{gesis_download}, 
#'  then, you should be sure to add these options to your .Rprofile substituting your 
#'  info for the example below:
#'
#'  \code{
#'   options("gesis_email" = "juanita-herrara@uppermidwest.edu",
#'           "gesis_password" = "password123!",
#'           "gesis_use" = 5)
#'  }
#'   In addition to accepting the terms of use, you need to input a purpose for
#'   downloading a data set. The options are as follows:
#'
#' 1. for final thesis of the study programme (e.g. Bachelor/Master thesis)
#' 2. for reserach with a commercial mission
#' 3. for non-scientific purposes
#' 4. for further education and qualification
#' 5. for scientific research (incl. doctorate)
#' 6. in the course of my studies
#' 7. in a course as lecturer
#'
#' @return The function returns downloaded files.
#'
#' @examples
#' \dontrun{
#'  gesis_download(file_id = c("ZA6644", "ZA6900"),
#'                 download_dir = tempdir()) # remember to specify a directory for your download
#' }
#' 
#' @import RSelenium
#' @importFrom dplyr case_when
#' @importFrom foreign read.dta
#' @importFrom magrittr '%>%'
#' @importFrom netstat free_port
#' @importFrom rio import export
#' @importFrom stringr str_detect str_subset
#' @importFrom tools file_path_sans_ext
#' @importFrom utils unzip
#' 
#' @export
gesis_download <- function(file_id, 
                          email = getOption("gesis_email"),
                          password = getOption("gesis_password"),
                          use = getOption("gesis_use"),
                          reset = FALSE,
                          download_dir = "gesis_data",
                          msg = TRUE,
                          convert = TRUE,
                          delay = 5) {
    
    # detect login info
    if (reset) {
        use <- email <- password <- NULL
    }
    
    if (is.null(email)) {
        gesis_user <- readline(prompt = "GESIS requires your user account information.  Please enter your email address: \n")
        options("gesis_user" = gesis_user)
        user <- getOption("gesis_user")
    }
    
    if (is.null(password)) {
        gesis_password <- readline(prompt = "Please enter your GESIS password: \n")
        options("gesis_password" = gesis_password)
        password <- getOption("gesis_password")
    }
    
    if (is.null(use)) {
        gesis_use <- readline(prompt = "Please enter your GESIS use: \n")
        options("gesis_use" = gesis_use)
        use <- getOption("gesis_use")
    }
    
    use <- dplyr::case_when(
        use == 1 ~ "for final thesis of the study programme (e.g. Bachelor/Master thesis)",
        use == 2 ~ "for reserach with a commercial mission",
        use == 3 ~ "for non-scientific purposes",
        use == 4 ~ "for further education and qualification",
        use == 6 ~ "in the course of my studies",
        use == 7 ~ "in a course as lecturer",
        TRUE ~ "for scientific research (incl. doctorate)"
    )
    
    # build path to firefox's default download directory
    if (Sys.info()[["sysname"]]=="Linux") {
        default_dir <- file.path("home", Sys.info()[["user"]], "Downloads")
    } else {
        default_dir <- file.path("", "Users", Sys.info()[["user"]], "Downloads")
    }
    
    # create specified download directory if necessary
    if (!dir.exists(download_dir)) dir.create(download_dir, recursive = TRUE)
    
    # initialize driver
    if(msg) message("Initializing RSelenium driver")
    rD <- rsDriver(browser="firefox", 
                   port = free_port(),
                   verbose = FALSE,
                   chromever = NULL)
    remDr <- rD[["client"]]
    
    # sign in
    signin <- "https://login.gesis.org"
    remDr$navigate(signin)
    Sys.sleep(delay)
    remDr$findElement(using = "id", "username")$sendKeysToElement(list(email))
    remDr$findElement(using = "id", "password")$sendKeysToElement(list(password))
    remDr$findElement(using = "id", "kc-login")$clickElement()
    Sys.sleep(delay)

    # loop through files
    for (i in seq(file_id)) { 
        item <- file_id[[i]]
        if(msg) message("Downloading GESIS Data Archive file: ", item, sprintf(" (%s)", Sys.time()))
        
        # get list of current default download directory contents
        dd_old <- list.files(default_dir)
        
        # navigate to download page
        url <- paste0("https://search.gesis.org/research_data/", item)
        remDr$navigate(url)
        Sys.sleep(delay)
        
        # switch to English
        if (try(unlist(remDr$findElement(using = "partial link text", "Englis")$getElementAttribute('id')), silent = TRUE) =="") {
            remDr$findElement(using = "partial link text", "Englis")$clickElement()
        }
        Sys.sleep(delay)
        
        # download codebook, if available
        if (try(unlist(remDr$findElement(using = "link text", "Codebook")$getElementAttribute('id')), silent = TRUE) == "") {
            remDr$findElement(using = "link text", "Codebook")$clickElement()
        }
        Sys.sleep(delay)
        
        # get list of current default download directory contents (after codebook download)
        dd_old_plus_cdbk <- list.files(default_dir)
        
        # download data
        remDr$findElement(using = "partial link text", "Datasets")$clickElement() # initiate data download
        remDr$findElement(using = "class", "data_purpose")$sendKeysToElement(list(use)) # select use
        
        remDr$findElement(using = "partial link text", "dta")$clickElement()
        Sys.sleep(delay)

        # check that download has completed
        dd_new <- setdiff(list.files(default_dir), dd_old_plus_cdbk)
        wait <- TRUE
        tryCatch(
            while(all.equal(stringr::str_detect(dd_new, "\\.part$"), logical(0))) {
                Sys.sleep(1)
                dd_new <- setdiff(list.files(default_dir), dd_old_plus_cdbk)
            }, error = function(e) 1 )
        while(any(stringr::str_detect(dd_new, "\\.crdownload$"))) {
            Sys.sleep(1)
            dd_new <- setdiff(list.files(default_dir), dd_old_plus_cdbk)
        }
        dd_new <- setdiff(list.files(default_dir), dd_old)
        dd_cdbk <- setdiff(dd_old_plus_cdbk, dd_old)
        dd_data <- setdiff(dd_new, dd_cdbk)
        
        # move to specified directory
        if (stringr::str_detect(dd_data, "\\.zip$")) { # unzip data if needed
            dld_old <- list.files(download_dir)
            if (!dir.exists(file.path(download_dir, item))) dir.create(file.path(download_dir, item), recursive = TRUE)
            unzip(file.path(default_dir, dd_data), exdir = file.path(download_dir, item))
            unlink(file.path(default_dir, dd_data))
            file.rename(from = file.path(default_dir, dd_cdbk), to = file.path(download_dir, item, dd_cdbk))
        } else {
            dir.create(file.path(download_dir, item), showWarnings = FALSE)
            file.rename(from = file.path(default_dir, dd_new), to = file.path(download_dir, item, dd_new))
            unlink(file.path(default_dir, dd_new))
        }
        
        # convert to .RData
        data_files <- list.files(path = file.path(download_dir, item), recursive = TRUE) %>%
            stringr::str_subset("\\.dta$")
        if (convert == TRUE) {
            for (i in seq_along(data_files)) {
                data_file <- data_files[i]
                tryCatch(rio::import(file.path(download_dir, item, data_file),
                                           convert.factors = FALSE) %>%
                             rio::export(paste0(tools::file_path_sans_ext(file.path(download_dir,
                                                                             item,
                                                                             basename(data_file))), ".RData")),
                         error = function(c) suppressWarnings(
                             foreign::read.dta(file.path(download_dir, item, data_file),
                                               convert.factors = FALSE) %>%
                                 rio::export(paste0(tools::file_path_sans_ext(file.path(download_dir,
                                                                                 item,
                                                                                 basename(data_file))), ".RData")))
                )
            }
        }
    }
    
    # Close driver
    remDr$close()
    rD[["server"]]$stop()
}  

