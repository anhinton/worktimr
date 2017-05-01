#' Countdown until stop work
#'
#' Countdown \code{minutes} then show \code{explosion} image
#' fullscreen.
#'
#' @param explosion file path to image file
#' @param minutes numeric
#'
#' @export
workTimer = function(explosion, minutes = 25) {

    ## wait 25 minutes
    endTime = Sys.time() + (minutes * 60)

    ## display mins:secs remaining every second
    while (endTime > Sys.time()) {
        remaining = endTime - Sys.time()
        minutes = floor(as.numeric(remaining, units = "mins"))
        minutes 
        remaining = remaining - minutes
        seconds = floor(as.numeric(remaining, units = "secs"))
        cat("\rtime until explosion:",
            paste(sprintf("%2d", minutes), sprintf("%02d", seconds),
                  sep = ":"))
        utils::flush.console()
        Sys.sleep(1)
    }

    ## it's over!
    cat("\nIT'S OVER!\n")
    system2("eog", c("-f", shQuote(normalizePath(explosion))))
}

#' Prompt to repeat \code{workTimer}
#'
#' Keep prompting to start another batch of work until quit.
#' 
#' @param explosion file path to image file
#' @param minutes numeric
#'
#' @export
promptTimer = function(explosion, minutes = 25) {
    while (TRUE) {
        workTimer(minutes = minutes, explosion = explosion)
        cat("Enter 'g' to go again, or anything else to exit: ")
        enter = readLines(con = "stdin", n = 1)
        if (enter != "g")
            break
    }
}
