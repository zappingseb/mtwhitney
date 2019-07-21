###################################################################################################
#                    RSelenium - An addition to the Selenium software                             #
###################################################################################################
#                                                                                                 #
#                                                                                                 #
# Author:          Sebastian Wolf <sebastian.wolf.sw1@roche.com>                                  #
# Date:            27 - April - 2017                                                              #
#                                                                                                 #
###################################################################################################
selKeys <- structure(
    list(null = "\uE000", cancel = "\uE001", help = "\uE002",
        backspace = "\uE003", tab = "\uE004", clear = "\uE005",
        return = "\uE006", enter = "\uE007", shift = "\uE008",
        control = "\uE009", alt = "\uE00A", pause = "\uE00B",
        escape = "\uE00C", space = "\uE00D", page_up = "\uE00E",
        page_down = "\uE00F", end = "\uE010", home = "\uE011",
        left_arrow = "\uE012", up_arrow = "\uE013", right_arrow = "\uE014",
        down_arrow = "\uE015", insert = "\uE016", delete = "\uE017",
        semicolon = "\uE018", equals = "\uE019", numpad_0 = "\uE01A",
        numpad_1 = "\uE01B", numpad_2 = "\uE01C", numpad_3 = "\uE01D",
        numpad_4 = "\uE01E", numpad_5 = "\uE01F", numpad_6 = "\uE020",
        numpad_7 = "\uE021", numpad_8 = "\uE022", numpad_9 = "\uE023",
        multiply = "\uE024", add = "\uE025", separator = "\uE026",
        subtract = "\uE027", decimal = "\uE028", divide = "\uE029",
        f1 = "\uE031", f2 = "\uE032", f3 = "\uE033", f4 = "\uE034",
        f5 = "\uE035", f6 = "\uE036", f7 = "\uE037", f8 = "\uE038",
        f9 = "\uE039", f10 = "\uE03A", f11 = "\uE03B", f12 = "\uE03C",
        command_meta = "\uE03D", backslash="\u005C"),
    .Names = c("null", "cancel", "help", "backspace", "tab", "clear",
        "return", "enter", "shift",  "control", "alt", "pause",
        "escape", "space", "page_up", "page_down", "end", "home",
        "left_arrow", "up_arrow", "right_arrow", "down_arrow",
        "insert", "delete", "semicolon", "equals", "numpad_0",
        "numpad_1", "numpad_2", "numpad_3", "numpad_4", "numpad_5",
        "numpad_6", "numpad_7", "numpad_8", "numpad_9", "multiply",
        "add", "separator", "subtract", "decimal", "divide", "f1",
        "f2", "f3", "f4", "f5", "f6", "f7", "f8", "f9", "f10", "f11",
        "f12", "command_meta","backslash")
)
#' Safe Setting of Generic
#'
#' ...
#'
#' This method is an adapted version from \url{https://r-forge.r-project.org/scm/viewvc.php/*checkout*/branch/Poisson/lossDev/R/zzz.R?revision=3&root=lossdev&pathrev=121}.
#'
#' @param    name               (\code{character}) The name of the method to set.
#' @param    ...             Additional parameters passed to \code{\link[methods]{setGeneric}}
#'
#' @return   -
#'
#' @seealso  \code{\link[methods]{setGeneric}}
#'
#' @author   Matthias Pfeifer \email{matthias.pfeifer@@roche.com}
bioWARP_setSafeGeneric <- function(name, ...) {
  if(!isGeneric(name, where = parent.frame())) {
    setGeneric(name, where = parent.frame(), ...)

  } else {
    return(NULL)
  }
}

# bioWARP_load_value_string ###############################################################################

#' Makes String UTF8 Compatible
#'
#' Mask some special characters by utf-8 characters to be handled as utf8 inside the application
#'
#' @param    s     (\code{character}) Passed to paste for collapsing the individual strings.
#' @param    forward    (\code{logical}) Wheter to exchange letters by UTF-8 (true) or UTF-8 by
#'               standard R letters
#' @return   (\code{character})
#' @export
#' @author   Sebastian Wolf \email{sebastian.wolf.sw1@@roche.com}
bioWARP_load_value_string <- function(s, forward=TRUE){

  match_frame <- data.frame(
      utf = c(
          rawToChar(as.raw(c(0xc2,0xb5))),# mu
          rawToChar(as.raw(c(0xc3,0xa4))),#ae
          rawToChar(as.raw(c(0xc3,0xb6))),#oe
          rawToChar(as.raw(c(0xc3,0xbc))),#ue
          rawToChar(as.raw(c(0xce,0x94))),#large delta
          rawToChar(as.raw(c(0xce,0xb1))),#alpha
          rawToChar(as.raw(c(0xe2,0x89,0xa5))),#≥
          rawToChar(as.raw(c(0xe2,0x89,0xa4))),#≤
          rawToChar(as.raw(c(0xce,0xbc)))#mu
      ),
      utf_match = c(
          "\u00B5",
          "\u00E4",
          "\u00F6",
          "\u00FC",
          "\u0394",
          "\u03B1",
          "\u2265",
          "\u2264",
          "\u03bc"
      ),
      xml_match = c(
          "&#181;",
          "&#228;",
          "&#246;",
          "&#252;",
          "&#916;",
          "&#945;",
          "&ge;",
          "&le;",
          "&#956;"
      ),

      stringsAsFactors = FALSE
  )
  if(forward){

    for(i in 1:dim(match_frame)[1]){

      s <- gsub(as.character(match_frame$utf[i]),match_frame$utf_match[i],s)
      s <- gsub(rawToChar(as.raw(c(0xc3,0x82))),"",s)
    }

  }else{
    for(i in 1:dim(match_frame)[1]){

      s <- str_replace_all(s,match_frame$utf[i],       match_frame$xml_match[i])
      s <- str_replace_all(s,match_frame$utf_match[i], match_frame$xml_match[i])
    }
  }
  return(s)
}

#' Initialize an RSelenium Environment
#'
#' @name    init_RSelenium
#' @details This function opens an Internet Explorer window to perform UI tests there. Therefore it
#'   needs a running Selenium Server. If there is no Selenium Server, one will be started by calling
#'   the selenium-server-standalone-x.x.x.jar. This jar shall  be located at the users computer.
#'   Furthermore Java8 is needed for versions after 3.0.0.
#'
#'   The Server communicates with an InternetExplorerDriver. This IEDriver.exe file shall be located
#'   in the user's path.
#'
#'   Finally it allows navigating in the Web-site with command line parameters instead of clicking
#'   within the browser window.
#'
#' @param url (\code{character}) The url that shall be called in the browser
#' @param driver_location (\code{character}) The location of the selenium-server-standalone-x.x.x.jar
#'   that shall be used to run the selenium server.
#' @param java_home (\code{character}) The JAVA_HOME directory where Java8 is located on the
#'   computer. Please do not end this location with "\" or "/".
#' @param ie_driver_folder (\code{character}) Define the folder where the IEDriver.exe of Selenium
#'   is located. If this folder is not defined the IEDriver.exe will be search in the system path.
#'  IEDriver.exe can be downloaded from \url{http://www.seleniumhq.org/download/}
#' @param implicit_timeout (\code{numeric}) An initial timeout that will be set after calling the
#'   first web site in milliseconds.
#' @param page_load_time_out (\code{numeric}) A timeout that will be waited after the first page
#'   is loaded milliseconds.
#'
#' @param browser (\code{character}) Browser to be used
#'
#' @return TRUE/FALSE due to the success. A global variable remDr will be constructed which is of
#'    class \link[RSelenium]{remoteDriver-class}. This can be used by all other functionalities
#'    to perform clicks, movements or element reads.
#'
#'
#' @export
#' @seealso Sys.setenv(JAVA_HOME=""), \link[RSelenium]{remoteDriver-class}
#' @author  Sebastian Wolf \email{sebastian.wolf.sw1@roche.com}
init_RSelenium <- function(
    url = NULL,
    driver_location = NULL,
    java_home = NULL,
    ie_driver_folder=NULL,
    implicit_timeout=10000,
    page_load_time_out=5000,
    browserName = "internet explorer",
    ...
){
  # Locate the default selenium-server-standalone
  if(is.null(driver_location)){
    driver_location <- "C:\\Programme_2\\ROCHE-R\\Selenium\\selenium-server-standalone-3.4.0.jar"
  }

  # Check if a url was provided
  if(is.null(url)){
    stop("RSeleniumTest needs a an url to be intialized")
  }
  # Check if JAVA_HOME has to be changed
  if(!is.null(java_home)){
    Sys.setenv(JAVA_HOME=java_home)
  }

  if(is.null(ie_driver_folder)){
    Sys.setenv(PATH=paste("C:\\Programme_2\\ROCHE-R\\Selenium\\",
                  Sys.getenv("PATH"),
                sep=";"))
  }else{
    Sys.setenv(PATH=paste(ie_driver_folder,
            Sys.getenv("PATH"),
            sep=";"))
  }

  if(browserName == "internet explorer"){
    extraCapabilities=list(
        ie.forceCreateProcessApi=FALSE,
        InternetExplorerDriver.INTRODUCE_FLAKINESS_BY_IGNORING_SECURITY_DOMAIN=TRUE,
        InternetExplorerDriver.IGNORE_ZOOM_SETTING=TRUE,
        requireWindowFocus=TRUE,
        enablePersistentHover=FALSE)
  }else{
    extraCapabilities = list()
  }
  # set the output to be negative by default
  SUCCESS <- FALSE

  # Try to initiate a Driver Session with InternetExplorer. If this Session failes it is
  # due to the reason no Selenium Server was initiated. So in case of an error a Selenium
  # Server is started
  result = tryCatch({

        # Set the remoteDriver globally
        remDr <<- remoteDriver(
            browserName = browserName,
            extraCapabilities = extraCapabilities
        )
        remDr$open()

        remDr$setImplicitWaitTimeout(as.numeric(implicit_timeout))
        remDr$navigate(url)
        remDr$setTimeout(type = "page load", milliseconds = as.numeric(page_load_time_out))
        # Navigate to the provided url
        remDr$maxWindowSize()
        remDr$setWindowSize(1936, 1056)
        remDr$setWindowPosition(0,0)
        remDr$sessionInfo$in_shiny<<-F
        remDr$switchToFrame(NULL)
        Sys.sleep(10)
        SUCCESS <- TRUE

      }, error = function(e) {

        # Start a selenium Server from command line
        sys_log <- paste("\"",
            Sys.getenv("JAVA_HOME"),
            "\\bin\\java.exe\" -jar ",
            "\"",driver_location,"\"",
            sep="")

        message(paste("starting selenium server from",sys_log))

        system(sys_log, wait=FALSE)
        Sys.sleep(5)
        print("started")

      }
  )
  if(!SUCCESS){
    # Set the remoteDriver globally
    remDr <<- remoteDriver(
        browserName = browserName,
        extraCapabilities = extraCapabilities
    )
    remDr$open()
    remDr$setImplicitWaitTimeout(as.numeric(implicit_timeout))
    remDr$navigate(url)
    remDr$setTimeout(type = "page load", milliseconds = as.numeric(page_load_time_out))
    remDr$maxWindowSize()
    remDr$setWindowSize(1936, 1056)
    remDr$setWindowPosition(0,0)
    # Navigate to the provided url
    remDr$sessionInfo$in_shiny<<-FALSE
    remDr$switchToFrame(NULL)
    #Sys.sleep(10)
    SUCCESS<-TRUE
  }
  return(SUCCESS)
}

bioWARP_setSafeGeneric(
    "navigate_to",
    function (object, url, ...) { standardGeneric("navigate_to") } )

bioWARP_setSafeGeneric(
    "click_element",
    function (object, type_of_search, value, ...) { standardGeneric("click_element") } )

#' Click an element by defining its id, class or element_text
#'
#' @name     click_element
#' @param    object   (\code{object}) The \link[RSelenium]{remoteDriver-class} object
#' @param    type_of_search The type of html attribute it should be searched
#'   searched for. "xpath", "css selector", "id", "name",
#'   "tag name", "class name", "link text", "partial link text" are possible entries.
#'   Best working are "id" and "class name". This function allows multiple entries as a character
#'   vector to limit multiple classes or use tag and class as restrictions. These will be restricted
#'   in the inserted order. The \code{value} parameter has to have the same length as this parameter.
#' @param    value (\code{character}) The value that shall match the type_of_search argument. E.g.
#'    type_of_search = "class name" and value="btn" means the html element shall look like
#'    \code{<div class="btn btn-blue"></div>} to be found.
#' @param    text (\code{character}) To search for text within a specific html element this
#'    parameter can be set. It can be inserted to search for example for a button with a specific
#'    text on it. Therefore one would search for an element of the class button and the text "close"
#' @param    find (\code{logical}) Whether to search for the \code{text} parameter within the html
#'    element (TRUE) or to have an exact match (FALSE).
#' @param    iframe (\code{logical}) If this parameter is set to TRUE, the function will proof if it
#'    is inside the right iframe for looking at a Shiny App. It should be set to FALSE if the
#'    web-site to look it is not a Shiny App.
#' @seealso \code{\link[RSelenium]{findElements-method}}, \code{\link{RSeleniumTest-class}}
#' @export
#' @author  Sebastian Wolf \email{sebastian.wolf.sw1@roche.com}
setMethod(
    "click_element",
    signature = "remoteDriver",
    definition = function(object,
        type_of_search=c("xpath", "css selector", "id", "name", "tag name", "class name", "link text", "partial link text"),
        value,
        text=NULL,
        find=FALSE,
        nr_of_clicks=1,
        x=5,
        iframe=T,
        which=NULL,
        move_mouse=FALSE){
      if(iframe){
        switch_to_right_frame(object)
      }

      Dataset_selector <- get_element(object,type_of_search=type_of_search,value=value,text=text,find=find,iframe=iframe,which=which)
      # Try to move the mouse to the specific element
      # If the mouse cannot be moved there an error is brought to the user
      tryCatch({

            if(move_mouse){
              object$mouseMoveToLocation(x=round(Dataset_selector$getElementSize()$width/3),
                    y=round(Dataset_selector$getElementSize()$height/3),webElement=Dataset_selector)
              Dataset_selector$clickElement()
            }else{
              remDr$executeScript("arguments[0].click();", list(Dataset_selector));
            }
          },
          error=function(e){
            stop(paste("Element with",type_of_search,":",
                    value,
                    ifelse(is.null(text),"",paste("with text \"",text,"\"",sep="")),
                    "not found"))
          })

    }
)

bioWARP_setSafeGeneric(
    "click_hidden_element",
    function (object, type_of_search, value, ...) { standardGeneric("click_hidden_element") } )

#' Click an element which is not displayed on the page but active in the Source Code
#'
#' @details  This function is different to \link{click_element-method} as it does not care whether
#'     the item to click is visible or whether there is a full text mat
#'
#' @name     click_hidden_element
#' @param    object   (\code{object}) The \link[RSelenium]{remoteDriver-class} object
#' @param    type_of_search The type of html attribute it should be searched
#'   searched for. "xpath", "css selector", "id", "name",
#'   "tag name", "class name", "link text", "partial link text" are possible entries.
#'   Best working are "id" and "class name". This function allows multiple entries as a character
#'   vector to limit multiple classes or use tag and class as restrictions. These will be restricted
#'   in the inserted order. The \code{value} parameter has to have the same length as this parameter.
#' @param    value (\code{character}) The value that shall match the type_of_search argument. E.g.
#'    type_of_search = "class name" and value="btn" means the html element shall look like
#'    \code{<div class="btn btn-blue"></div>} to be found.
#' @param    text (\code{character}) To search for text within a specific html element this
#'    parameter can be set. It can be inserted to search for example for a button with a specific
#'    text on it. Therefore one would search for an element of the class button and the text "close"
#' @param    find (\code{logical}) Whether to search for the \code{text} parameter within the html
#'    element (TRUE) or to have an exact match (FALSE).
#' @param    iframe (\code{logical}) If this parameter is set to TRUE, the function will proof if it
#'    is inside the right iframe for looking at a Shiny App. It should be set to FALSE if the
#'    web-site to look it is not a Shiny App.
#' @seealso \code{\link[RSelenium]{findElements-method}}, \code{\link{RSeleniumTest-class}}, \code{\link{click_element-method}}
#'
#' @author  Sebastian Wolf \email{sebastian.wolf.sw1@roche.com}
setMethod(
    "click_hidden_element",
    signature = "remoteDriver",
    definition = function(object,
        type_of_search=c("xpath", "css selector", "id", "name", "tag name", "class name", "link text", "partial link text"),
        value,
        text=NULL,
        find=TRUE,
        x=5,
        iframe=T){
      if(iframe){
        switch_to_right_frame(object)
      }

      # Find elements which are not visible
      Dataset_selector <- get_element(object,type_of_search,value,text,find=find,iframe,visibility = FALSE)

      tryCatch({

            remDr$executeScript("arguments[0].click();", list(Dataset_selector));
          },
          error=function(e){
            stop(paste("Element with",type_of_search,":",
                    value,
                    ifelse(is.null(text),"",paste("with text \"",text,"\"",sep="")),
                    "not found"))
          })

    }
)

bioWARP_setSafeGeneric(
    "switch_to_right_frame",
    function (object, type_of_search, value, ...) { standardGeneric("switch_to_right_frame") } )

#' On Shiny Apps switch into the main "iframe"
#'
#' @name     switch_to_right_frame
#' @param    object   (\code{object}) The \code{\link[RSelenium]{remoteDriver-class}} object
#' @author   Sebastian Wolf \email{sebastian.wolf.sw1@roche.com}
#' @details  Shiny Apps are all rendered within iframes. Selenium can have problems finding the
#'   right iframe. To overcome this problem within Selenium this function navigates to the frame
#'  that's far outside. Afterwards it checks if there is any iframe inside, and if so it navigates
#'  to the first one it finds.
setMethod("switch_to_right_frame",
    signature = "remoteDriver",
    definition = function(object){

      if(!remDr$sessionInfo$in_shiny){
        # Go to main frame
        object$switchToFrame(NULL)

        # Set waiting to 1000 milliseconds
        object$setImplicitWaitTimeout(1000)

        # Search for any iframe within the main frame and if there is one navigate to the first
        # one
        iframe_found<-TRUE
        if(length(remDr$findElements("tag","iframe"))==0 || remDr$sessionInfo$in_shiny){
          iframe_found = FALSE
        }else{
          tryCatch({
                remDr$sessionInfo$in_shiny<-TRUE

              },error=function(e){print("")})
          tryCatch({
                remDr$switchToFrame(remDr$findElements("tag","iframe")[[1]])
              },error=function(e){print("")})
        }
      }

    })

bioWARP_setSafeGeneric(
    "move_single_slider",
    function (object, id, ...) { standardGeneric("move_single_slider") } )

#' Move a slider with just one value in a specific direction
#'
#' @name     move_slider
#' @param    object   (\code{object}) The \code{\link[RSelenium]{remoteDriver-class}} object
#' @param    type_of_search The type of html attribute it should be searched
#'   searched for. "xpath", "css selector", "id", "name",
#'   "tag name", "class name", "link text", "partial link text" are possible entries.
#'   Best working are "id" and "class name". This function allows multiple entries as a character
#'   vector to limit multiple classes or use tag and class as restrictions. These will be restricted
#'   in the inserted order. The \code{value} parameter has to have the same length as this parameter.
#' @param    value (\code{character}) The value that shall match the type_of_search argument. E.g.
#'    type_of_search = "class name" and value="btn" means the html element shall look like
#'    \code{<div class="btn btn-blue"></div>} to be found.
#' @param    text (\code{character}) To search for text within a specific html element this
#'    parameter can be set. It can be inserted to search for example for a button with a specific
#'    text on it. Therefore one would search for an element of the class button and the text "close"
#' @param    find (\code{logical}) Whether to search for the \code{text} parameter within the html
#'    element (TRUE) or to have an exact match (FALSE).
#' @param    units_to_move (\code{integer}) The number of units to move the slider in upwards or
#'  downwards direction. For downwards negative values have to be used.
#' @param    unit_type (\code{character}) Defines if the slider is moved by a certain percentage
#'   ("percent") or by integer "steps". To define steps the minimum value and maximum value of the
#'   slider have to be defined.
#' @param   max_value (\code{integer}) Maximum value of the slider. Just integer values allowed.
#' @param    min_value (\code{integer}) Minimum value of the slider. Just integer values allowed.
#' @param    reset (\code{logical}) Defines if the slider should just be moved and be reset
#'  immediately afterwards. To reset the slider set this parameter to TRUE.
#' @param    iframe (\code{logical}) If this parameter is set to TRUE, the function will proof if it
#'    is inside the right iframe for looking at a Shiny App. It should be set to FALSE if the
#'    web-site to look it is not a Shiny App.
#' @export
#' @seealso  \code{\link{RSeleniumTest-class}}
#'
#' @author  Sebastian Wolf \email{sebastian.wolf.sw1@roche.com}
setMethod(
    "move_single_slider",
    signature = "remoteDriver",
    definition = function(object,
        type_of_search,
        value,
        text=NULL,
        find=FALSE,
        units_to_move=0,
        unit_type=c("percent","steps"),
        max_value=NULL,
        min_value=NULL,
        from_to = "single",
        reset=FALSE,
        iframe=TRUE){

      if(iframe){
        switch_to_right_frame(object)
      }


      if(!from_to%in%c("from","to","single")){
        stop("from_to can just be 'from', 'to' or 'single'")
      }

      # Get the slider element and its width
      slider_element <- get_element(object,type_of_search,value,text,find)

      width_of_slider <- tryCatch({
            slider_element$findChildElements("class name","irs")[[1]]$getElementSize()$width},
          error=function(e){
            stop(paste0("The selected element '",value,"' is no slider element",collapse=","))
          })


      moving_button<-NULL
      # Get the id of the round button that can be moved
      moving_button <- tryCatch(

          {slider_element$findChildElements("class name",from_to)[[1]]},

          error=function(e){stop(
            paste0("No slider element with \"",
                type_of_search,
                "\" with value \"",
                value,"\" found that can be slided "))
      })
      # Define a function to return the button to its old position
      return_to_pos <- function(current_mouse_loc,position){

        object$mouseMoveToLocation(
            x=-(current_mouse_loc$x - position$x),
            y=-(current_mouse_loc$y - position$y))

      }
      if(!is.null(moving_button) && moving_button$isElementDisplayed()[[1]]){
        # Move the mouse to the slider. If there is no slider return an error
        tryCatch({
              moving_button_start <- moving_button$getElementLocation()
              object$mouseMoveToLocation(x=5,y=5,webElement=moving_button)

            },
            error=function(e){
              stop(
                  paste0("No slider element with \"",
                      type_of_search,
                      "\" with value \"",
                      value,"\" found that can be slided "))
            })
        # Move the mouse to the slider button
        object$mouseMoveToLocation(x=5,y=5,webElement=moving_button)

        # Hold the mouse
        object$buttondown(buttonId = 0)

        # Move the slider by the number of steps or the percentage to the left or right
        if(unit_type=="steps"){
          width_of_slider <- (slider_element$getElementSize()$width + width_of_slider)/1.8
          if(!is.null(max_value)&&!is.null(min_value)){
            nr_of_units = max_value - min_value + 1
          }
          object$mouseMoveToLocation(
              x=width_of_slider/nr_of_units*units_to_move,
              y=0)


        }else{
          width_of_slider <- (slider_element$getElementSize()$width + width_of_slider)/2
          object$mouseMoveToLocation(x=(units_to_move/100)*width_of_slider,y=0)
        }
      }else{
        stop(
            paste0("Slider element with \"",
                type_of_search,
                "\" with value \"",
                value,"\" is not displayed."))
      }

      tryCatch({
            # Print out the value of the slider after moving
            if(from_to=="single"){
              value_of_slider <- slider_element$findChildElements("class name","irs-single")[[1]]$getElementText()[[1]]
            }else if(from_to=="from"){
              value_of_slider <- slider_element$findChildElements("class name","irs-from")[[1]]$getElementText()[[1]]
            }else{
              value_of_slider <- slider_element$findChildElements("class name","irs-to")[[1]]$getElementText()[[1]]
            }
          },error=function(e){stop(
                paste0("No slider element with \"",
                    type_of_search,
                    "\" with value \"",
                    value,"\" found that can be slided "))
          })
      # Move the slider back to its old position if this information is needed
      if(reset){
        current_mouse_loc <- list(x=moving_button_start$x-width_of_slider*percent,
            y=moving_button_start$y)

        return_to_pos(current_mouse_loc,moving_button_start)
      }

      object$buttonup()

      return(value_of_slider)
    }
)

bioWARP_setSafeGeneric(
    "select_option",
    function (object, id, ...) { standardGeneric("select_option") } )

#' Click the option inside a drop-down field
#'
#' @name     select_option
#' @details  This function uses a Web User interface to click on a specific drop-down menu. The
#'   displayed items are checked for their order or for their entries. The user can specify
#'   if the nth item shall be clicked or an item with a specific text. If the div container
#'   with the specific id or no option could be found an error is returned
#'
#' @param    object   (\code{object}) The \code{\link[RSelenium]{remoteDriver-class}} object
#' @param    type_of_search The type of html attribute it should be searched
#'   searched for. "xpath", "css selector", "id", "name",
#'   "tag name", "class name", "link text", "partial link text" are possible entries.
#'   Best working are "id" and "class name". This function allows multiple entries as a character
#'   vector to limit multiple classes or use tag and class as restrictions. These will be restricted
#'   in the inserted order. The \code{value} parameter has to have the same length as this parameter.
#' @param    value (\code{character}) The value that shall match the type_of_search argument. E.g.
#'    type_of_search = "class name" and value="btn" means the html element shall look like
#'    \code{<div class="btn btn-blue"></div>} to be found.
#' @param    text (\code{character}) To search for text within a specific html element this
#'    parameter can be set. It can be inserted to search for example for a button with a specific
#'    text on it. Therefore one would search for an element of the class button and the text "close"
#' @param    find (\code{logical}) Whether to search for the \code{text} parameter within the html
#'    element (TRUE) or to have an exact match (FALSE).
#' @param    iframe (\code{logical}) If this parameter is set to TRUE, the function will proof if it
#'    is inside the right iframe for looking at a Shiny App. It should be set to FALSE if the
#'    web-site to look it is not a Shiny App.
#' @param   nr_of_option (\code{integer}) The nth element of the drop-down field will be chosen.
#'   This number defines n.
#' @param    text_of_option (\code{character}) Here the user can type a character string that shall
#'   be displayed in the options field. This options field will be selected
#' @export
#' @author  Sebastian Wolf \email{sebastian.wolf.sw1@roche.com}
setMethod(
    "select_option",
    signature = "remoteDriver",
    definition = function(object,
        id,
        type_of_search,
        value,
        text=NULL,
        find=FALSE,
        iframe=TRUE,
        nr_of_option=NULL,
        text_of_option=NULL,
        which = NULL){

      Dataset_selector <- get_element(object,type_of_search,value,text,find,iframe,which=which)
      tryCatch({
            selector <- Dataset_selector$findChildElements("class name","selectize-input")[[1]]
            remDr$executeScript("arguments[0].click();", list(selector));
            },error=function(e){
            stop(paste("Element",
                    paste(type_of_search,sep=","),
                    paste(value,sep=","),
                    "could not be clicked"))
      })

      all_options <- Dataset_selector$findChildElements("class name","option")

      if(length(all_options)<1){
        all_options <- Dataset_selector$findChildElements("tag","option")

      }
      if(length(all_options)<1){
        all_options <- append(all_options,
            Dataset_selector$findChildElements("class name","item"))
      }
      if(all_options[[1]]$getElementText()==""){
        object$click()
        all_options <- Dataset_selector$findChildElements("class name","option")

      }
      if(!is.null(nr_of_option)){

        #object$mouseMoveToLocation(x=2,y=2,webElement=all_options[[nr_of_option]])
        #object$click()
        print(get_plain_text(all_options[[nr_of_option]]))
        option_text <- get_plain_text(all_options[[nr_of_option]])
        all_options[[nr_of_option]]$clickElement()

        return(option_text)

      }else if(!is.null(text_of_option)){
        for(i in 1:length(all_options)){
          if(all_options[[i]]$getElementText()[[1]]==text_of_option){
            #object$mouseMoveToLocation(x=2,y=2,webElement=all_options[[i]])
            option_text <- get_plain_text(all_options[[i]])
            print(option_text)
            all_options[[i]]$clickElement()
            found=TRUE

            return(option_text)
          }
        }
        if(!found){
          stop(paste("the option with text:\"",
                  text_of_option,
                  "\" could not be found",
                  sep=""))
        }
      }else{
        stop("please provide either a text of the option or a nr of option")
      }

#      get_option_1 <-  objectremote_driver$findElements("class name","option")[[1]]
#      remDr$mouseMoveToLocation(x=5,y=5,webElement=get_option_1)
    })


get_plain_text <- function(web_element, visibility=TRUE){

  string_of_html <- tryCatch({xpathSApply(
            htmlParse(web_element$getElementAttribute("innerHTML")[[1]],asText=T,trim=T),
            paste0("//text()",
            ifelse(visibility,"[not(ancestor::*[contains(@style, \"display: none\")])]
            [not(ancestor::*[contains(@style, \"display:none\")])]",""),
            "[not(ancestor::script)]
            [not(ancestor::style)]
            [not(ancestor::noscript)]
            [not(ancestor::form)]
              "),
            xmlValue)},error=function(e){NULL})
  trim <- function (x) gsub("^\\s+|\\s+$", "", gsub("\\{\\}","",x))
  string_of_html <- trim(string_of_html)

    string_of_html <- string_of_html[which(string_of_html!="")]

  return(string_of_html)
}

bioWARP_setSafeGeneric(
    "compare_element_by_text",
    function (object, type_of_search,value,...) { standardGeneric("compare_element_by_text") } )

#' Checking webElements for text equality
#'
#' @export
#' @name     compare_element_by_text
#' @details  Go through a list of webElements and check if these contain or match a certain text
#'    string
#' @param    Dataset_selector (\code{list}) A list of \link[RSelenium]{webElement-class} elements
#'    that are representing html web-site elements.
#' @param    text (\code{character}) To search for text within a specific html element this
#'    parameter can be set. It can be inserted to search for example for a button with a specific
#'    text on it. Therefore one would search for an element of the class button and the text "close"
#' @param    find (\code{logical}) Whether to search for the \code{text} parameter within the html
#'    element (TRUE) or to have an exact match (FALSE).
#' @param visibility (\code{logical}) Whether the element can be seen or not
#'
#' @seealso \link{get_element}
#' @author  Sebastian Wolf \email{sebastian.wolf.sw1@roche.com}
compare_element_by_text<-function(Dataset_selector,text,find,visibility=TRUE, ...){
      if(!is.null(text)){

        if(length(Dataset_selector)>0){
          keep_ids <- c()
          for(i in 1:length(Dataset_selector)){

            if(find){
              if(length(
                  grep(text,get_plain_text(Dataset_selector[[i]]))
                  )>0){
                keep_ids <- c(keep_ids,i)
              }
            }else{
              plain.text <- get_plain_text(Dataset_selector[[i]], visibility)
              if(!is.null(plain.text)){

                if(length(plain.text)<2 && length(plain.text)>0){

                  if(Dataset_selector[[i]]$getElementText()==text || plain.text==text){

                    keep_ids <- c(keep_ids,i)
                  }#if == elements_text
                }


              }else{#if null(plain.text)
                if(Dataset_selector[[i]]$getElementText()==text){

                  keep_ids <- c(keep_ids,i)
                }


              }
            }# if find

          }#for

          Dataset_selector <- Dataset_selector[keep_ids]
        }#if length()>0

      }
      return(Dataset_selector)
    }

compare_element_by_css<-function(Dataset_selector,css_properties,css_property_values,...){
  if(length(css_properties)>0 && length(css_properties)==length(css_property_values)){

    if(length(Dataset_selector)>0){
      keep_ids <- c()
      for(i in 1:length(Dataset_selector)){

        check_all_css = sapply(
            1:length(css_properties),function(x){
              css_attribute <- Dataset_selector[[i]]$getElementValueOfCssProperty(
                  css_properties[x])
              css_attribute[[1]] == css_property_values[x]
            }
        )
        if(all(check_all_css)){

          keep_ids <- c(keep_ids,i)
        }#if all check_css

      }#for

      Dataset_selector <- Dataset_selector[keep_ids]
    }#if length()>0

  }
  return(Dataset_selector)
}

bioWARP_setSafeGeneric(
    "get_element",
    function (object, type_of_search,value,...) { standardGeneric("get_element") } )

#' Finding elements by multiple search values within a Web-site
#'
#' @export
#' @name     get_element
#' @param    object   (\code{object}) The \link[RSelenium]{remoteDriver-class} object
#' @param    type_of_search The type of html attribute it should be searched
#'   searched for. "xpath", "css selector", "id", "name",
#'   "tag name", "class name", "link text", "partial link text" are possible entries.
#'   Best working are "id" and "class name". This function allows multiple entries as a character
#'   vector to limit multiple classes or use tag and class as restrictions. These will be restricted
#'   in the inserted order. The \code{value} parameter has to have the same length as this parameter.
#' @param    value (\code{character}) The value that shall match the type_of_search argument. E.g.
#'    type_of_search = "class name" and value="btn" means the html element shall look like
#'    \code{<div class="btn btn-blue"></div>} to be found.
#' @param    text (\code{character}) To search for text within a specific html element this
#'    parameter can be set. It can be inserted to search for example for a button with a specific
#'    text on it. Therefore one would search for an element of the class button and the text "close"
#' @param    find (\code{logical}) Whether to search for the \code{text} parameter within the html
#'    element (TRUE) or to have an exact match (FALSE).
#' @param    iframe (\code{logical}) If this parameter is set to TRUE, the function will proof if it
#'    is inside the right iframe for looking at a Shiny App. It should be set to FALSE if the
#'    web-site to look it is not a Shiny App.
#' @param visibility (\code{logical}) Whether the element can be seen or not
#'
#' @return    Dataset_selector (\code{\link[RSelenium]{webElement-class}}) An RSelenium webElement
#'    will be returned if it can be found. This element can be used for further inspections
#'
#' @details  This function works just for visible elements. Any element that is not visible will be
#'    excluded
#' @seealso  \code{\link[RSelenium]{findElements-method}}, \code{get_element_text}
setMethod("get_element",
    signature = "remoteDriver",
    definition = function(object,
        type_of_search,
        value,
        text=NULL,
        find=FALSE,
        iframe=TRUE,
        visibility=TRUE,
        css_properties=c(),
        css_property_values=c(),
        which = NULL,
        ...){

      # Go to the main RShinyApp data.frame
      if(iframe==TRUE){
        switch_to_right_frame(object)
      }

      object$setImplicitWaitTimeout(as.numeric(10000))

      if(length(type_of_search)!=length(value)){
        stop("type_of_search and value have to have the same length")
      }
      # To check for multiple searches the search has to iterate through a character
      # vector within the type of search. If length<2 then just a RSelenium function is called
      if(length(type_of_search)>0){

        allowed_elements <- c("xpath","tag","class name","css selector","id","value","name","link text")

        if(any(!type_of_search %in% allowed_elements)){
          css_property_index <-
              which(!type_of_search %in% allowed_elements)

          css_properties <- type_of_search[css_property_index]
          css_property_values <- value[css_property_index]

          type_of_search <- type_of_search[which(type_of_search %in% allowed_elements)]
          value <- value[which(type_of_search %in% allowed_elements)]
        }

        # Loop through all the "class", "tag", "id"... restrictions that shall be searched
        # for
        for(i in 1:length(type_of_search)){

          # for the first search use the in-built function of RSelenium
          if(i==1){
            Dataset_selector <- object$findElements(type_of_search[i],value[i])
          # for all other searches a list has to be checked that could come
          # out of the first search
          }else{

            # create an empty list of entry indexes that could match
            keep_ids = c()

            # go trough each element
            for(j in 1:length(Dataset_selector)){

              # Change the search for "class name" to "class"
              if(type_of_search[i]=="class name" || type_of_search[i]=="class"){
                type_of_search[i]<-"class"

                # Split the classes within the "class" attribute
                attribute <- strsplit(
                    Dataset_selector[[j]]$getElementAttribute(
                        type_of_search[i])[[1]],
                        " ")[[1]]
              }else if(type_of_search[i]=="tag" | type_of_search[i]=="tag name"){
                attribute <- Dataset_selector[[j]]$getElementTagName()[[1]]
              }else{
                attribute <- Dataset_selector[[j]]$getElementAttribute(type_of_search[i])[[1]]
              }# IF class search

              # Check if the attribute of the element matches the inserted value and
              # if the html element is visible to the user
              if(value[i] %in% attribute){
                if(visibility){
                    if(Dataset_selector[[j]]$isElementDisplayed()[[1]]){
                        keep_ids <- c(keep_ids,j)
                      }
                }else{
                  keep_ids <- c(keep_ids,j)

                }# if visibility
              }# IF value matches

            }# FOR length Dataset_selector
            Dataset_selector <- Dataset_selector[keep_ids]
          }# IF i==1
        }# FOR type_or_search element
      }else{
        # Just one restriction for the attributes of the webElement is made and it can
        # be searched by the RSelenium function
        Dataset_selector <- object$findElements(type_of_search,value)

      }

      # Check the elements to contain a certain text
      Dataset_selector <- compare_element_by_text(Dataset_selector,text,find, visibility)

      if(visibility && length(Dataset_selector)>0){
        keep_ids = c()
        for(j in 1:length(Dataset_selector)){
          if(Dataset_selector[[j]]$isElementDisplayed()[[1]]){
            keep_ids <- c(keep_ids,j)
          }

        }
        Dataset_selector <- Dataset_selector[keep_ids]
      }
      if(length(css_properties)>0 && length(css_properties)==length(css_property_values)){
        Dataset_selector <- compare_element_by_css(Dataset_selector,
            css_properties,
            css_property_values)
      }
      # Check if less than two elements was matching
      if(length(Dataset_selector)>1){

        # Check if the user told which element of multiple to use
        if(!is.null(which)){
          if(visibility){
            if(Dataset_selector[[which]]$isElementDisplayed()[[1]]){
              return(Dataset_selector[[which]])
            }else{
              stop("no element found due to visibility")
            }
          }else{
            return(Dataset_selector[[which]])
          }
        }else{
          message("ids found:")
          for(j in 1:length(Dataset_selector)){
            message(paste("\t",try(Dataset_selector[[j]]$getElementAttribute("id"))))
          }
          stop("The element selection was not unique")
        }

      # Check if any element was found
      }else{
        if(length(Dataset_selector)<1){
          stop("no element found")
        }else{
          if(visibility){
            if(Dataset_selector[[1]]$isElementDisplayed()[[1]]){
              # Return found element

              return(Dataset_selector[[1]])
            }# if is displayed
          }else{
            return(Dataset_selector[[1]])
          }


        }# if more than 0  elements found


      }# IF less than two elements found

    })
bioWARP_setSafeGeneric(
    "get_element_text",
    function (object, type_of_search,value,...) { standardGeneric("get_element_text") } )

#' Get the text or innerHTML of a html element defined
#'
#' @name     get_element_text
#' @details  Function to search for an html element and return its inner text
#' @export
#' @param    object   (\code{object}) The \code{\link[RSelenium]{remoteDriver-class}} object
#' @param    type_of_search The type of html attribute it should be searched
#'   searched for. "xpath", "css selector", "id", "name",
#'   "tag name", "class name", "link text", "partial link text" are possible entries.
#'   Best working are "id" and "class name". This function allows multiple entries as a character
#'   vector to limit multiple classes or use tag and class as restrictions. These will be restricted
#'   in the inserted order. The \code{value} parameter has to have the same length as this parameter.
#' @param    value (\code{character}) The value that shall match the type_of_search argument. E.g.
#'    type_of_search = "class name" and value="btn" means the html element shall look like
#'    \code{<div class="btn btn-blue"></div>} to be found.
#' @param   response (\code{character}) "text" or "innerHTML" due to if the user wants the whole
#'    HMTL code within the found web element or just the pure text displayed to the UI.
#' @param    iframe (\code{logical}) If this parameter is set to TRUE, the function will proof if it
#'    is inside the right iframe for looking at a Shiny App. It should be set to FALSE if the
#'    web-site to look it is not a Shiny App.
#'
#' @return   (\code{character}) Text within the HTML element or the whole HTML code within the
#'    element
#' @seealso \link{get_element}
#' @author  Sebastian Wolf \email{sebastian.wolf.sw1@roche.com}
setMethod("get_element_text",
    signature = "remoteDriver",
    definition = function(object,
        type_of_search,
        value,
        iframe=T,
        response=c("text","innerHTML"),
        text=NULL,
        find=FALSE,
        which = which,
        ...){

      Dataset_selector <- get_element(object,type_of_search=type_of_search,value=value,iframe=iframe,text=text,find=find, which = which)

      if(response=="text"){
        plain.text <- get_plain_text(Dataset_selector)

        if(!is.null(plain.text)){
          return(plain.text)
        }else{
          return(Dataset_selector$getElementText())
        }

      }else{
        return(Dataset_selector$getElementAttribute("innerHTML"))
      }


    })

bioWARP_setSafeGeneric(
    "get_element_table_as_data_frame",
    function (object, type_of_search,value,...) { standardGeneric("get_element_table_as_data_frame") } )

#' Derive a data.frame from a HTML table that is searched by outer HTML attributes
#'
#' @name     get_element_table_as_data_frame
#' @details  This function allows to search within a HTML page for a specific element that starts
#'    with a table tag. This element is parsed by the XML library into a R data.frame
#'
#' @param    object   (\code{object}) The \code{\link[RSelenium]{remoteDriver-class}} object
#' @param    type_of_search The type of html attribute it should be searched
#'   searched for. "xpath", "css selector", "id", "name",
#'   "tag name", "class name", "link text", "partial link text" are possible entries.
#'   Best working are "id" and "class name". This function allows multiple entries as a character
#'   vector to limit multiple classes or use tag and class as restrictions. These will be restricted
#'   in the inserted order. The \code{value} parameter has to have the same length as this parameter.
#' @param    value (\code{character}) The value that shall match the type_of_search argument. E.g.
#'    type_of_search = "class name" and value="dataTable" means the html element shall look like
#'    \code{<table class="dataTable">...</table>} to be found.
#' @param    iframe (\code{logical}) If this parameter is set to TRUE, the function will proof if it
#'    is inside the right iframe for looking at a Shiny App. It should be set to FALSE if the
#'    web-site to look it is not a Shiny App.
#' @return    (\code{data.frame}) A data.frame with the data found within the HTML table
#' @export
#'
#' @seealso  \link{get_element_text}, \link{get_element}
#' @author  Sebastian Wolf \email{sebastian.wolf.sw1@roche.com}
setMethod("get_element_table_as_data_frame",
  signature = "remoteDriver",
  definition = function(object,type_of_search,value,iframe=T,...){

    # Check if it was searched for an actual html table
    if(!"table"%in%value){
      element <- get_element(object,type_of_search,value,iframe=iframe,...)

      if(!is.null(element)){
        if(element$getElementTagName()[[1]]=="table"){
          the_table <- element$getElementAttribute("innerHTML")
        }else{
          child_table <- element$findChildElements("tag","table")

          if(length(child_table)>1){

            ids_to_keep<-c()

            for(i in 1:length(child_table)){
              if(child_table[[i]]$isElementDisplayed()[[1]]){
                ids_to_keep <- c(ids_to_keep,i)
              }

            }

            if(length(ids_to_keep)>1){
              stop("no unique table")
            }else{
              the_table <- child_table[[ids_to_keep]]$getElementAttribute("innerHTML")
            }


          }else if(length(child_table)<1){
            stop("no element found")
          }else{
            the_table <- child_table[[1]]$getElementAttribute("innerHTML")
          }
        }
      }else{
        stop("no element found")
      }
    }else{
      the_table <- get_element_text(
        object,
        type_of_search=type_of_search,
        value=value,
        iframe=iframe,
        response="innerHTML")
    }

    # Parse the Table by XML
    the_table <- htmlTreeParse(the_table[[1]],asText=T,encoding = "UTF-8")
    the_table <- the_table$children$html$children$body

    # Get the table headers
    headers <- the_table$children[["thead"]]
    column_names <- lapply(headers$children[[1]]$children, function(x) xmlValue(x))

    # Get the table content
    content <- c()
    # For each row
    for(i in 1:length(the_table[["tbody"]]$children))
    {
      table_row <- the_table[["tbody"]]$children[[i]]
      row_content<-c()
      # for each column
      for(j in 1:length(table_row$children)){

        v <- xmlValue(table_row[[j]])

        if(is.null(v)) v2 <- as.character("")
        else if(length(v) == 0) v2 <- as.character("")
        else if(is.na(v)) v2 <- as.character("")
        else v2 <- as.character(v)

        row_content <- c(row_content, v2)
      }

      content <- rbind(content, row_content)
    }
    # Write out the table as a data.frame and delete row.names
    colnames(content) <- as.character(column_names)
    rownames(content) <- NULL

    return(as.data.frame(content,stringsAsFactors=F,check.names  = F))

  })

bioWARP_setSafeGeneric(
    "get_element_image",
    function (object, type_of_search,value,...) { standardGeneric("get_element_image") } )

#' Get the images within an HTML element and write them into temporary files
#'
#' @name     get_element_image
#' @details  Download the image within the <img src=...> tag. If the src contains a base64 string
#'    it is converted into a file instead of just downloading the source.
#'
#' @param    object   (\code{object}) The \code{\link[RSelenium]{remoteDriver-class}} object
#' @param    type_of_search The type of html attribute it should be searched
#'   searched for. "xpath", "css selector", "id", "name",
#'   "tag name", "class name", "link text", "partial link text" are possible entries.
#'   Best working are "id" and "class name". This function allows multiple entries as a character
#'   vector to limit multiple classes or use tag and class as restrictions. These will be restricted
#'   in the inserted order. The \code{value} parameter has to have the same length as this parameter.
#' @param    value (\code{character}) The value that shall match the type_of_search argument. E.g.
#'    type_of_search = "class name" and value="btn" means the html element shall look like
#'    \code{<div class="btn btn-blue">...</div>} to be found.
#' @param    text (\code{character}) To search for text within a specific html element this
#'    parameter can be set. It can be inserted to search for example for a button with a specific
#'    text on it. Therefore one would search for an element of the class button and the text "close"
#' @param    find (\code{logical}) Whether to search for the \code{text} parameter within the html
#'    element (TRUE) or to have an exact match (FALSE).
#' @param    iframe (\code{logical}) If this parameter is set to TRUE, the function will proof if it
#'    is inside the right iframe for looking at a Shiny App. It should be set to FALSE if the
#'    web-site to look it is not a Shiny App.
#'
#' @export
#' @return   (\code{character}) A list of file locations on the hard-drive where the downloaded
#'    images were stored.
#' @seealso  \link{get_element}
#' @author  Sebastian Wolf \email{sebastian.wolf.sw1@roche.com}
setMethod("get_element_image",
    signature = "remoteDriver",
    definition = function(object,type_of_search,value,text=NULL,find=F,iframe=TRUE,output_filename=NULL,...){

      # Get the object defined by the input parameters
      image_parent_object <- get_element(object,type_of_search,value,text,find,iframe)

      # Check if it is already an image
      if(image_parent_object$getElementTagName()[[1]]=="img"){
        image_objects <- image_parent_object
      # if not search for all images within this object
      }else{
        image_objects <- image_parent_object$findChildElements("tag","img")
      }

      if(length(image_objects)<1){
        stop("No image found in the web element searched for")
      }

      # Create an empty list for the output
      list_of_files <- c()

      # Go trough all image_objects found
      for(i in 1:length(image_objects)){

        # Check if the image is visible
        if(image_objects[[i]]$isElementDisplayed()[[1]]){

          # Get their src (source) attribute
          image_link<-image_objects[[i]]$getElementAttribute("src")[[1]]

          # Check if the source is a base64 string
          if(length(grep("base64",image_link))>0){
            if(is.null(output_filename)){
              tmpfile <- tempfile("download_png_",fileext=".png")
              print(strsplit(image_link,"base64,")[[1]][2])
              tmpfile_base64 <- tempfile("download_png_",fileext=".txt")
              writeLines(strsplit(image_link,"base64,")[[1]][2],tmpfile_base64)
              base64::decode(tmpfile_base64, output=tmpfile)
              message(paste("Image stored at",tmpfile))
              list_of_files <- c(list_of_files,tmpfile)
            }else{
              tmpfile_base64 <- tempfile("download_png_",fileext=".txt")
              writeLines(strsplit(image_link,"base64,")[[1]][2],tmpfile_base64)
              base64::decode(tmpfile_base64, output=file(output_filename,"wb"))
              list_of_files <- c(list_of_files,paste(getwd(),output_filename,sep="/"))
            }
          # If the image is a http address download the image with in-built functions
          }else{
            if(is.null(output_filename)){
              tmpfile <- tempfile("download_png_",fileext=".png")
              on.exit(unlink(tmpfile))
              error<-download.file(image_link,basename(image_link),mode="wb")
              print(error)
              message(paste("Image stored at",paste(getwd(),basename(image_link),sep="/")))
              list_of_files <- c(list_of_files,paste(getwd(),basename(image_link),sep="/"))
            }else{
              download.file(image_link,output_filename)
              message(paste("Image stored at",paste(getwd(),output_filename,sep="/")))
              list_of_files <- c(list_of_files,paste(getwd(),output_filename,sep="/"))
            }#if base64
          }# if is displayed
        }# FOR image_objects

      }
      # Close all opened file connections
      closeAllConnections()
      return(list_of_files)
    })


bioWARP_setSafeGeneric(
    "get_element_download_link",
    function (object, type_of_search, value, ...) { standardGeneric("get_element_download_link") } )

#' Get the images within an HTML element and write them into temporary files
#'
#' @name     get_element_image
#' @details  Download the image within the <img src=...> tag. If the src contains a base64 string
#'    it is converted into a file instead of just downloading the source.
#'
#' @param    object   (\code{object}) The \code{\link[RSelenium]{remoteDriver-class}} object
#' @param    type_of_search The type of html attribute it should be searched
#'   searched for. "xpath", "css selector", "id", "name",
#'   "tag name", "class name", "link text", "partial link text" are possible entries.
#'   Best working are "id" and "class name". This function allows multiple entries as a character
#'   vector to limit multiple classes or use tag and class as restrictions. These will be restricted
#'   in the inserted order. The \code{value} parameter has to have the same length as this parameter.
#' @param    value (\code{character}) The value that shall match the type_of_search argument. E.g.
#'    type_of_search = "class name" and value="btn" means the html element shall look like
#'    \code{<div class="btn btn-blue">...</div>} to be found.
#' @param    text (\code{character}) To search for text within a specific html element this
#'    parameter can be set. It can be inserted to search for example for a button with a specific
#'    text on it. Therefore one would search for an element of the class button and the text "close"
#' @param    find (\code{logical}) Whether to search for the \code{text} parameter within the html
#'    element (TRUE) or to have an exact match (FALSE).
#' @param    iframe (\code{logical}) If this parameter is set to TRUE, the function will proof if it
#'    is inside the right iframe for looking at a Shiny App. It should be set to FALSE if the
#'    web-site to look it is not a Shiny App.
#'
#' @export
#' @return   (\code{character}) A list of file locations on the hard-drive where the downloaded
#'    images were stored.
#' @seealso  \link{get_element}
#' @author  Sebastian Wolf \email{sebastian.wolf.sw1@roche.com}
setMethod("get_element_download_link",
    signature = "remoteDriver",
    definition = function(object,type_of_search,value,text=NULL,find=F,iframe=TRUE){

      # Get the object defined by the input parameters
      image_parent_object <- get_element(object,type_of_search,value,text,find,iframe)

      # Check if it is already an image
      if(image_parent_object$getElementTagName()[[1]]=="a"){
        image_objects <- image_parent_object
        # if not search for all images within this object
      }else{
        image_objects <- image_parent_object$findChildElements("css selector","a:link")
      }

      if(length(image_objects)<1){
        stop("No link found in the web element searched for")
      }
      if(length(image_objects)>1){
        stop("No unique link found")
      }

      # Create an empty list for the output
      list_of_files <- c()

      # Close all opened file connections
      return(image_objects$getElementAttribute("href")[[1]])
    })

bioWARP_setSafeGeneric(
    "set_timeout",
    function (object, type_of_search,value,...) { standardGeneric("set_timeout") } )
#' Setting a time out to wait for web-sites
#' Set a time-out in milliseconds
#' @param    object   (\code{object}) The \link[RSelenium]{remoteDriver-class} object
#' @name set_timeout
#' @param time_in_milliseconds (\code{integer}) Number of milliseconds to let the execution pause
setMethod("set_timeout",
    signature = "remoteDriver",
    definition = function(object,time_in_milliseconds=1000){
      Sys.sleep(time_in_milliseconds/1000)
    })

bioWARP_setSafeGeneric(
    "focus_element",
    function (object, type_of_search,value,...) { standardGeneric("focus_element") } )

#' Focus an element by sending an empty key to it
#'
#' @name focus_element
#' @details  This function searches for any element inside a web-site by using the
#'    \link{get_element-method}. Afterwards it focuses this element by sending an empty key to it.
#'    This procedure was found at:
#'       \url{http://stackoverflow.com/questions/11337353/correct-way-to-focus-an-element-in-selenium-webdriver-using-java?answertab=votes#tab-top}
#'     This function ignores the visibility of an object.
#'
#' @param    object   (\code{object}) The \link[RSelenium]{remoteDriver-class} object
#' @param    type_of_search The type of html attribute it should be searched
#'   searched for. "xpath", "css selector", "id", "name",
#'   "tag name", "class name", "link text", "partial link text" are possible entries.
#'   Best working are "id" and "class name". This function allows multiple entries as a character
#'   vector to limit multiple classes or use tag and class as restrictions. These will be restricted
#'   in the inserted order. The \code{value} parameter has to have the same length as this parameter.
#' @param    value (\code{character}) The value that shall match the type_of_search argument. E.g.
#'    type_of_search = "class name" and value="btn" means the html element shall look like
#'    \code{<div class="btn btn-blue"></div>} to be found.
#' @param    text (\code{character}) To search for text within a specific html element this
#'    parameter can be set. It can be inserted to search for example for a button with a specific
#'    text on it. Therefore one would search for an element of the class button and the text "close"
#' @param    find (\code{logical}) Whether to search for the \code{text} parameter within the html
#'    element (TRUE) or to have an exact match (FALSE).
#' @param    iframe (\code{logical}) If this parameter is set to TRUE, the function will proof if it
#'    is inside the right iframe for looking at a Shiny App. It should be set to FALSE if the
#'    web-site to look it is not a Shiny App.
#' @seealso  \link{get_element-method}, \link[RSelenium]{remoteDriver-class}
#' @export
#'
#' @author  Sebastian Wolf \email{sebastian.wolf.sw1@roche.com}
setMethod("focus_element",
    signature = "remoteDriver",
    definition = function(object,type_of_search,value,text=NULL,find=T,iframe=TRUE,...){

      if(find){
        # Get the object to be focused
        element <- get_element(object,type_of_search,value,text,find,iframe,visibility=FALSE)
      }else{
        element <- get_element(object,type_of_search,value,text,find,iframe,visibility=TRUE)
      }

      #element$clickElement()
      remDr$executeScript("arguments[0].focus();", list(element));

    })


bioWARP_setSafeGeneric(
    "change_switch",
    function (object, type_of_search,value,...) { standardGeneric("change_switch") } )

#' Click on a \code{onoffswitch} element
#'
#' @name    change_switch
#' @details This function searches for any element inside a web-site by using the
#'    \link{get_element-method} and looks for any \code{onoffswitch} element that is inside it. This
#'    element will be clicked which leads to a change of the on/off switch.
#'
#' @export
#' @param    object   (\code{object}) The \link[RSelenium]{remoteDriver-class} object
#' @param    type_of_search The type of html attribute it should be searched
#'   searched for. "xpath", "css selector", "id", "name",
#'   "tag name", "class name", "link text", "partial link text" are possible entries.
#'   Best working are "id" and "class name". This function allows multiple entries as a character
#'   vector to limit multiple classes or use tag and class as restrictions. These will be restricted
#'   in the inserted order. The \code{value} parameter has to have the same length as this parameter.
#' @param    value (\code{character}) The value that shall match the type_of_search argument. E.g.
#'    type_of_search = "class name" and value="btn" means the html element shall look like
#'    \code{<div class="btn btn-blue"></div>} to be found.
#' @param    text (\code{character}) To search for text within a specific html element this
#'    parameter can be set. It can be inserted to search for example for a button with a specific
#'    text on it. Therefore one would search for an element of the class button and the text "close"
#' @param    find (\code{logical}) Whether to search for the \code{text} parameter within the html
#'    element (TRUE) or to have an exact match (FALSE).
#' @param    iframe (\code{logical}) If this parameter is set to TRUE, the function will proof if it
#'    is inside the right iframe for looking at a Shiny App. It should be set to FALSE if the
#'    web-site to look it is not a Shiny App.
#' @seealso  \link{get_element-method}, \link[RSelenium]{remoteDriver-class}
#'
#' @author  Sebastian Wolf \email{sebastian.wolf.sw1@roche.com}
setMethod("change_switch",
    signature = "remoteDriver",
    definition = function(object,type_of_search,value,text=NULL,find=F,iframe=TRUE){

      # Get the object to be focused
      element <- get_element(object,type_of_search,value,text,find,iframe)
      if(is.null(element)){
        stop("no element found")
      }
      # Get the switch element within it
      switch <- element$findChildElements("class name",
          "onoffswitch")[[1]]$findChildElements("tag",
          "input")[[1]]

      object$executeScript("arguments[0].focus();", list(switch));

      # click the switch
      object$executeScript("arguments[0].click();", list(switch));
      #$clickElement()

      return_value <- tryCatch({
            if(switch$isElementSelected()[[1]]){
              return("on")
            }else{
              return("off")
            }
          },error=function(e){
            NULL
          })
      return(return_value)
    })

bioWARP_setSafeGeneric(
    "check_box_by_name",
    function (object, name, label_value=NULL, box_value=NULL,...) { standardGeneric("check_box_by_name") } )


#' Click a bioWARP item within the class checkbox
#'
#' @name     check_box_by_name
#' @param    object   (\code{object}) The \code{\link[RSelenium]{remoteDriver-class}} object
#' @param   name (\code{character}) The "name" tag of the checkbox. Mostly its taken as the name
#'    of the form where the checkbox is located. In most cases it can be taken from the DynXML
#'    definition of the checkboxes. It can be e.g. DynXML_Test-Input_Checkbox-Group_in_param02.
#' @param    label_value (\code{character}) This is the label written right next to the checkbox.
#'    This input parameter has to be used really carefully, as a checkbox label does not have to
#'    be unique but, the checkbox itself might be unique. This could result in multiple selected
#'    checkboxes while entering just one checkbox label. This function will \strong{iterate} through
#'    all check boxes with the label entered here.
#' @param    box_value (\code{character}) Each checkbox contains a 'value' attribute. To compare the
#'    value attribute with the user input this parameter can be set. It will be preferred agains the
#'    \code{label_value} parameter. As Shiny mostly names checkboxes just "box01", "box02" ... it is
#'    normally pretty easy to find out this value by the order of boxes.
#' @param    iframe (\code{logical}) If this parameter is set to TRUE, the function will proof if it
#'    is inside the right iframe for looking at a Shiny App. It should be set to FALSE if the
#'    web-site to look it is not a Shiny App.
#' @seealso  \link{get_element-method}, \link[RSelenium]{remoteDriver-class}
#' @export
#' @author  Sebastian Wolf \email{sebastian.wolf.sw1@roche.com}
setMethod("check_box_by_name",
  signature = "remoteDriver",
  definition = function(object,
      name,
      label_value=NULL,
      box_value=NULL,
      iframe=T,...){

    # Move into Shiny App Dataframe
    if(iframe){
      switch_to_right_frame(object)
    }

    # Get all checkboxes on display
    #checkboxes <- remDr$findElements("class name","checkbox")


    # Get the named element, which is the <input name="XXX"> box itself
    #named_elements = remDr$findElements("name",name)

    if(!is.null(label_value)){
      element <- object$findElements("xpath",
          paste0(
              ".//label//span[contains(text(),'",label_value,"')]/../../*"))[[1]]

      if(length(element$findChildElements("name",name))>0){

        object$executeScript("arguments[0].focus();",
            list(element$findChildElements("tag","input")[[1]]))
        remDr$executeScript("arguments[0].click();", list(element$findChildElements("tag","input")[[1]]));
        #element$findChildElements("tag","input")[[1]]$clickElement()

        return(element$findChildElements("tag","input")[[1]]$isElementSelected()[[1]])

      }


    }else if(!is.null(box_value)){
      # Get the box_value and compare it to the box_value inserted by the user

      box <- object$findElements("xpath",
          paste0(
              "//input[@value='",box_value,"']"))[[1]]
      if(box$getElementAttribute("name")==name){
        object$executeScript("arguments[0].focus();",
            list(box))
        #box$clickElement()
        remDr$executeScript("arguments[0].click();", list(box));
        return(box$isElementSelected()[[1]])
      }


    }


  })

bioWARP_setSafeGeneric(
    "drag_drop_input",
    function (object,
        type_of_search=c("id","label"),
        input_text = NULL,
        input_target = NULL,...) { standardGeneric("drag_drop_input") } )

#' Enable Drag and Drop in bioWARP
#'
#' @name     drag_drop_input
#' @details  This function is mainly usable for the bioWARP drag and drop input fields. It does
#'   \strong{not} allow simply dragging and dropping any HTML element, but just bioWARP Drag and
#'   Drop input fields.
#'
#' @param    object   (\code{object}) The \code{\link[RSelenium]{remoteDriver-class}} object
#' @param    type_of_search (\code{character}) Drag and Drop Inputs for Shiny can be found by in two
#'   different ways. First each Drag and Drop field has a heading label. Setting this option to
#'   "label" will search for this label on top of the form. The other option is to search for the
#'   forms id. Therefore the user has to know this unique identifier or how it was defined in the
#'   DynXML used for bioWARP
#' @param    value (\code{character}) The value that shall match the type_of_search argument. E.g.
#'    type_of_search = "id" and value="inputfield2" means the html element shall look like
#'    \code{<div id="inputfield2">...</div>} to be found.
#' @param    input_text (\code{character}) The Drag and Drop form of bioWARP displays the variables
#'    it found in a table as little buttons with the name of the column on top of it. This name plus
#'    the type in brackets as e.g. 'i (numeric)' has to be entered here. This has to be an exact
#'    match.
#' @param    target_text (\code{character}) Each Drag and Drop item from the left hand side of a
#'    drag and drop input form will be moved into a Target Box. On top of this box there is a label
#'    which defines what the target box means. To drop the item into the right target box this
#'    input has to be defined. It has to be an exact match including spaces and special characters.
#' @param    revert (\code{logical}) Sometimes the need to simulate an error could be a valid option.
#'    Therefore any user of this function cannot only drop an item in a target box, but also move
#'    the item back to the big box collecting all items. If this option is set to \code{TRUE} the
#'    item will be moved back from wherever it is in the target boxes to the big collection box at
#'    the start of the Drag and Drop input field.
#' @param    iframe (\code{logical}) If this parameter is set to TRUE, the function will proof if it
#'    is inside the right iframe for looking at a Shiny App. It should be set to FALSE if the
#'    web-site to look it is not a Shiny App.
#' @export
#' @seealso  \link{get_element-method}, \link[RSelenium]{remoteDriver-class}
#'
#' @author  Sebastian Wolf \email{sebastian.wolf.sw1@roche.com}
setMethod("drag_drop_input",
    signature="remoteDriver",
    definition = function(object,
        type_of_search=c("id","label"),
        value,
        input_text = NULL,
        target_text = NULL,
        revert=FALSE,
        iframe=FALSE){

      # Check which type of search was selected and select either by id or by label
      # where the drag and drop search is located
      if(type_of_search=="id"){
        parent <- get_element(object,"id",value,iframe=iframe)
      }else{
        parent <- get_element(object,"class name","dragdrop-input",text=value,find=T,iframe=iframe)
      }

      # Get all items that can be moved
      input_elements <- parent$findChildElements("class name","dragdrop-item")

      # Get all fields where these items can be moved to
      target_elements <- parent$findChildElements("class name",
          "dragdrop-target-container-targets")

      # Check if the user defined the element to be moved
      if(is.null(input_text)){
        stop("Please define an input element to be moved",sep="")
      }

      # Select for the item to move, that was chosen by the user
      input_element<-NULL
      for(i in 1:length(input_elements)){
        if(input_elements[[i]]$getElementText()[[1]]==input_text){
          input_element<-input_elements[[i]]
        }
      }

      if(is.null(input_element)){
        stop(paste("Could not find the input field named \"",input_text,"\"",sep=""))
      }


      # If the user wants to put the item back into the collection
      if(revert){

        # Get the collection container
        container <- parent$findChildElements("class name","dragdrop-source-container")[[1]]


        # Move to the input element, drag it, move to the container, drop it
        object$mouseMoveToLocation(x=5,y=5,input_element)
        object$buttondown()
        object$mouseMoveToLocation(x=2,y=2,container)
        object$buttonup()

        return(c("container",input_elements[[i]]$getElementText()[[1]]))

      }else{

        # Derive the target field webElement by its title which is its whole text split
        # by "\n"
        target_element<-NULL
        for(i in 1:length(target_elements)){
          text_of_element <- strsplit(target_elements[[i]]$getElementText()[[1]],
                        "\n")[[1]][1]

          if(text_of_element==target_text){
            target_element <- target_elements[[i]]
          }
        }
        if(is.null(target_element)){
          stop(paste("Could not find the input field named \"",target_text,"\"",sep=""))
        }

        # Move to the input element, drag it, move to the target field, drop it
        object$mouseMoveToLocation(x=5,y=5,input_element)
        object$buttondown()
        object$mouseMoveToLocation(x=2,y=25,target_element)
        object$buttonup()
        target_text <- strsplit(target_element$getElementText()[[1]],
            "\n")
        target_name <- target_text[[1]][1]

        if(length(target_text[[1]])>1){
          inside_text <- strsplit(target_text[[1]][2],")")[[1]]
          inside_text <- sapply(inside_text,function(x){
                paste0(x,")")})
          names(inside_text) <- NULL
        }else{
          inside_text <- c()
        }

        return(c(target_name,inside_text))
      }



    })


bioWARP_setSafeGeneric(
    "enter_text",
    function (object,
        type_of_search,
        value,
        iframe=T,
        label=NULL,
        text_to_type="",...) { standardGeneric("enter_text") } )

#' Type text into an input field of a web form
#'
#' @name     enter_text
#' @details  Function to type text into an input field
#' @export
#' @param    object   (\code{object}) The \code{\link[RSelenium]{remoteDriver-class}} object
#' @param    type_of_search The type of html attribute it should be searched
#'   searched for. "xpath", "css selector", "id", "name",
#'   "tag name", "class name", "link text", "partial link text" are possible entries.
#'   Best working are "id" and "class name". This function allows multiple entries as a character
#'   vector to limit multiple classes or use tag and class as restrictions. These will be restricted
#'   in the inserted order. The \code{value} parameter has to have the same length as this parameter.
#' @param    value (\code{character}) The value that shall match the type_of_search argument. E.g.
#'    type_of_search = "class name" and value="btn" means the html element shall look like
#'    \code{<div class="btn btn-blue"></div>} to be found.
#' @param   response (\code{character}) "text" or "innerHTML" due to if the user wants the whole
#'    HMTL code within the found web element or just the pure text displayed to the UI.
#' @param    iframe (\code{logical}) If this parameter is set to TRUE, the function will proof if it
#'    is inside the right iframe for looking at a Shiny App. It should be set to FALSE if the
#'    web-site to look it is not a Shiny App.
#' @param text_to_type (\code{character}) The text that shall be entered into the field
#'
#' @return   (\code{character}) Text within the input element after changing it (value)
#' @seealso \link{get_element}
#' @author  Sebastian Wolf \email{sebastian.wolf.sw1@@roche.com}
setMethod("enter_text",
    signature = "remoteDriver",
    definition = function(object,type_of_search,value,iframe=T,text_to_type="",visibility=T,...){

      element <- get_element(object,type_of_search=type_of_search,value=value,iframe=iframe,visibility = visibility)
      if(is.null(element)){
        stop("no element found")
      }

      object$executeScript(
          paste0("arguments[0].style.display = 'block'"),
          list(element))

      if(length(grep("input",element$getElementTagName()[[1]]))>0
      ||  length(grep("textarea",element$getElementTagName()[[1]]))>0 ){
        object$executeScript(
            paste0("arguments[0].setAttribute('value','",
                text_to_type,"');"),
            list(element))

        element$clearElement()
        Sys.sleep(1)
        if(grepl("\\\\return",text_to_type)){
          text_elements<-strsplit(text_to_type,"\\\\return")[[1]]
          if(!text_elements=="\\"){

            for(text in text_elements){
              element$sendKeysToElement(list(text))
              element$sendKeysToElement(list(selKeys$return))
            }
          }
        }else if(grepl("\\\\backspace",text_to_type)){
            text_elements <- strsplit(text_to_type,"\\\\backspace")[[1]]
            if(!text_elements=="\\"){

              for(text in text_elements){
                element$sendKeysToElement(list(text))
                element$sendKeysToElement(list(selKeys$backspace))
              }
            }
        }else if(grepl("\\\\backslash",text_to_type)){
          text_elements <- strsplit(text_to_type,"\\\\backslash")[[1]]


          for(text in text_elements){
            if(!text=="\\"){
              element$sendKeysToElement(list(text))
              element$sendKeysToElement(list(selKeys$alt,selKeys$control,selKeys$backslash))
            }
          }
        }else{

#
#          if(bioWARP_load_value_string(text_to_type,FALSE)!= text_to_type){
#
#            sendKeys <- list(value = list(text_to_type))
##            browser()
#            qpath <- sprintf(
#                "%s/session/%s/element/%s/value",
#                remDr$serverURL
#                    , remDr$sessionInfo[["id"]], element$elementId
#            )
#            print(qpath)
#            element$queryRD(qpath, "POST", qdata = sendKeys)
#          }else{
            element$sendKeysToElement(list( text_to_type))
#          }


        }

      }else{
        stop("no input element selected")
      }
      if(length(grep("input",element$getElementTagName()[[1]]))>0){
        return(element$getElementAttribute("value")[[1]])
      }else{
        return(get_plain_text(element))
      }



    })


bioWARP_setSafeGeneric(
    "navigate_to_window",
    function (object,
        window_nr,...) { standardGeneric("navigate_to_window") } )
#' Change to another window, maybe a popup
#'
#' @name     navigate_to_window
#' @details  Function to change the current working window
#' @export
#' @param    object   (\code{object}) The \code{\link[RSelenium]{remoteDriver-class}} object
#' @param    nr_of_window (\code{integer}) The number of the window that shall be accessed now
#' @seealso \link{init_RSelenium}
#' @author  Sebastian Wolf \email{sebastian.wolf.sw1@@roche.com}
setMethod("navigate_to_window",
    signature = "remoteDriver",
    definition = function(object,window_nr,...){

      all_windows <- object$getWindowHandles()

      object$switchToWindow(all_windows[[window_nr]])

      return(object$getCurrentUrl()[[1]])
    })
