#' say
#'
#' @description Summon a whale for a customisable pick-me-up
#'
#' @usage say(what)
#'
#' @param what whatever phrase you want to echo
#'
#'
#' @return Whale shaped message
#' @export
#'
#' @examples
#' say("You're whale-com")

say <-function(what){
  #Defining the whale ASCII
  whale = "\n            ------ \n           %s \n            ------ \n               \\\   \n                \\\  \n                 \\\
     .-'
'--./ /     _.---.
'-,  (__..-`       \\
   \\          .     |
    `,.__.   ,__.--/
     '._/_.'___.-`
"
  #Finding the position of where to print message
  what_pos_start <-
    regexpr('%s', whale)[1] - 1

  what_pos_end <- what_pos_start + 3

  #If what isn't supplied...
  if(missing(what)){
    what <- phrases %>% sample(size = 1) #See here's the pipe!
  }

  #Combining positions, message and whale together
  out <- paste0(substr(whale, 1, what_pos_start), #Top of speech bubble
                what, #User specified message
                substr(whale, what_pos_end, nchar(whale))) #Bottom of speech bubble + whale

  #Return whale message
  message(out)
}
