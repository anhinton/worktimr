#' Countdown until stop work
#'
#' Countdown \code{minutes} then do something.
#' 
#' What happens when to the time expires is specified by the \code{outcome}
#' argument.
#' \itemize{
#' \item \dQuote{lock} locks the screen
#' \item \dQuote{alarm} plays an alarm sound
#' \item \dQuote{nothing} nothing happens
#' }
#' 
#' If \code{con} is set to an empty character string the output is printed
#' to the console. If a file is named the output is printing to a file,
#' replacing the contents.
#' 
#' The \code{outcome} value \dQuote{alarm} requires that VLC be installed at 
#' \code{C:\\Program Files\\VideoLAN\\VLC\\vlc.exe} on
#' Windows. On Linux it requires \code{vlc} to be on the PATH.
#'
#' @param minutes numeric
#' @param outcome character naming one of "lock", "alarm", or "nothing"
#' @param con character naming output text file
#' @param timerMessage character display before countdown timer
#' @param finalMessage character display when countdown expired
#'
#' @export
workTimer = function(minutes = 25, outcome = "lock", con = "",
                     timerMessage = "time until explosion: ",
                     finalMessage = "IT'S OVER!") {

    ## wait 25 minutes
    endTime = Sys.time() + (minutes * 60)

    ## display mins:secs remaining every second
    while (endTime > Sys.time()) {
        remaining = endTime - Sys.time()
        minutes = floor(as.numeric(remaining, units = "mins"))
        minutes 
        remaining = remaining - minutes
        seconds = floor(as.numeric(remaining, units = "secs"))
        
        output = paste0(
            timerMessage,
            sprintf("%2d", minutes), ":", sprintf("%02d", seconds),
            sep = "")
        
        if (con != "") {
            ## write timer to file
            writeLines(text = output, con = con)
        }
        
        ## print timer message to console
        cat("\r", output, "\r", sep = "")
        
        utils::flush.console()
        Sys.sleep(1)
    }
    
    ## it's over!
    if (con != "") {
        writeLines(text = finalMessage, con = con)
    }
    cat("\n", finalMessage, "\n", sep = "")
    
    utils::flush.console()
    
    systemOs = Sys.info()["sysname"]
    if (outcome == "lock") {
        switch(
            systemOs,
            Darwin = {
                system2(command = "/System/Library/CoreServices/Menu\ Extras/User.menu/Contents/Resources/CGSession",
                        args = "-suspend", wait = FALSE, stdout = FALSE, 
                        stderr = FALSE)
            },
            Windows = {
                system2(command = "rundll32.exe", 
                        args = "user32.dll, LockWorkStation", wait = FALSE,
                        stdout = FALSE, stderr = FALSE)
            },
            Linux = {
                system2(command = "gnome-screensaver-command", args = "-l", 
                        wait = FALSE, stdout = FALSE, stderr = FALSE)
            }
        )
    } else if (outcome == "alarm") {
        switch(
            systemOs,
            Linux = {
                system2(
                    command = "vlc",
                    args = c("--intf", "dummy",
                             system.file("alarm.mp3", package = "worktimr"),
                             "vlc://quit"),
                    wait = FALSE, stdout = FALSE, stderr = FALSE)
            },
            Windows = {
                alarm = system.file("alarm.mp3", package = "worktimr")
                alarm = gsub(pattern = "/", replacement = "\\\\", x = alarm)
                system2(
                    command = "C:/Program Files/VideoLAN/VLC/vlc.exe",
                    args = c("--intf", "dummy",
                             shQuote(alarm, type = "cmd"),
                             "vlc://quit"),
                    invisible = FALSE,
                    wait = FALSE)
            })
    }
}


#' Prompt to repeat \code{workTimer}
#'
#' Keep prompting to start another batch of work until quit.
#' 
#' @param minutes numeric
#' @param outcome character naming one of "lock", "alarm", or "nothing"
#' @param con character naming output text file
#' @param timerMessage character display before countdown timer
#' @param finalMessage character display when countdown expired
#'
#' @export
promptTimer = function(minutes = 25, outcome = "lock", con = "", 
                       timerMessage = "time until explosion: ",
                       finalMessage = "IT'S OVER!") {
    while (TRUE) {
        workTimer(minutes = minutes, outcome = outcome, con = con,
                  timerMessage = timerMessage,
                  finalMessage = finalMessage)
        cat("Enter 'g' to go again, 'e' to exit: ")
        enter = readLines(con = "stdin", n = 1)
        while (enter != "g" & enter != "e") {
            cat("Enter 'g' to go again, 'e' to exit: ")
            enter = readLines(con = "stdin", n = 1)
        }
        switch(
            enter,
            g = next,
            e = break
        )
    }
}
