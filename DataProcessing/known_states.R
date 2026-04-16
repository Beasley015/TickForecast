#' function to supply known states
#' anything coded as unobserved or unknown tick status get NA
#' anything after a death gets absorbing state code
#' 
#' @param ms multi-state capture history matrix
#' @param f vector of time of first capture
#' @param notseen code for unobserved at capture time
#' @param unknown code for unknown tick status
#' @param dead code for mice recovered dead

known_state_ms <- function(ms, f, notseen, unknown, dead){
  state <- ms
  latent.dead <- dead - 1
  state[state == notseen] <- NA
  state[state == unknown] <- NA
  state[state == dead] <- latent.dead
  dead.ind <- which(apply(ms, 1, function(x) any(x == dead)))
  if(length(dead.ind) >= 1){
    for(i in 1:length(dead.ind)){
      n3 <- which(ms[dead.ind[i],] == dead)
      if(n3 < ncol(ms)){
        state[dead.ind[i], (n3+1):ncol(ms)] <- latent.dead + 1
      }
    }
  }
  # state[,1] <- NA
  return(state)
}