match.arg <- function (arg, choices, several.ok = FALSE)
{
  # if no values for parameter 'choices' have been passed
  if (missing(choices)) {
    # gets formal parameters from the environments associated 
    # with functions further up the calling stack 
    formal.args <- formals(sys.function(sysP <- sys.parent()))
    # evaluates the values of formal arguments in the parent environment of the
    #arg.match() functions' calling environment and assigns them as values of 
    # the parameter 'choices'
    choices <- eval(formal.args[[as.character(substitute(arg))]],
                    envir = sys.frame(sysP))
  }
  # if there is no parameter 'arg', the first element of 'choices'
  # vector will be returned
  if (is.null(arg)) return(choices[1L])
  
  # otherwise return an error if the value of parameter 'arg' is no character
  else if(!is.character(arg))
    stop("'arg' must be NULL or a character vector")
  
  # if the logical parameter 'several.ok' is 'FALSE' (only one value for the parameter
  # 'arg' is allowed)
  if (!several.ok) { # most important (default) case:
    ## the arg can be the whole of choices as a default argument.
    
    # if the values of parameters 'arg' and 'choices' identical, return the
    # first element of the parameter 'arg'
    # note: for this case to be valid, length(arg) has to be one
    if(identical(arg, choices)) return(arg[1L])
    
    # if 'arg' consists of more than one value, return an error 
    if(length(arg) > 1L) stop("'arg' must be of length 1")
    
    # if 'arg' consists no values, return an error
  } else if(length(arg) == 0L) stop("'arg' must be of length >= 1")
  
  # if the logical parameter 'several.ok' is 'TRUE':
  
  ## handle each element of arg separately
  # partial matching of arguments: for each element of 'arg' separately, check
  # if there is a partial match with any of values from 'choices'
  # returns a vector of length equal to length(arg)
  # if there is a partial match for arg[j], then i[j] is equal to the position of
  # the value in 'choices' which matches arg[j]; if not, i[j] will be set to zero 
  i <- pmatch(arg, choices, nomatch = 0L, duplicates.ok = TRUE)
  
  # if there is no partial matches, return an error
  if (all(i == 0L))
    stop(gettextf("'arg' should be one of %s",
                  paste(dQuote(choices), collapse = ", ")),
         domain = NA)
  
  # select the positions of partial matches
  i <- i[i > 0L]
  
  # if several matches allowed and there have been actually several matches,
  # return the values of these matches; otherwise, if no several matches allowed
  # return an error message
  if (!several.ok && length(i) > 1)
    stop("there is more than one match in 'match.arg'")
  choices[i]
}


# debugging

make_something <- function(something = c("mess", "cake", "hyuuge mistake")) {
  something <- match.arg(something, several.ok = TRUE)
  message("I made a ", something, ".\n")
}
debugonce(match.arg)
make_something()



