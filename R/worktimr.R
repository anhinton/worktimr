#' Countdown until stop work
#'
#' Countdown \code{minutes} then lock screen.
#'
#' @param minutes numeric
#' @param lockScreen logical lock the screen when countdown ends
#'
#' @export
workTimer = function(minutes = 25, lockScreen = FALSE) {

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
    
    
    systemOs = Sys.info()["sysname"]
    if (lockScreen) {
        switch(
            systemOs,
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
    } else {
        switch(
            systemOs,
            Windows = shell(
                cmd = paste("start", 
                            system.file("party.jpg", package = "worktimr")),
                wait = FALSE),
            
            Linux = system2(
                command = "see",
                args = system.file("party.jpg", package = "worktimr"),
                wait = FALSE, stdout = FALSE, stderr = FALSE))
    }
}

#' Prompt to repeat \code{workTimer}
#'
#' Keep prompting to start another batch of work until quit.
#' 
#' @param minutes numeric
#' @param lockScreen boolean lock the screen when countdown ends
#'
#' @export
promptTimer = function(minutes = 25, lockScreen = TRUE) {
    while (TRUE) {
        workTimer(minutes = minutes, lockScreen = lockScreen)
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
