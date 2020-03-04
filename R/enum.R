# REF: https://stackoverflow.com/questions/33838392/enum-like-arguments-in-r

"
How about using a function that defines the enum by returning list(a= 'a', ...)?
You can then either assign the returned vector to a variable and use it in context,
or use the function directly. Either a name or an integer reference will work as an
index, although you have to use the unlist version of the index lookup, [[,
otherwise you get a list with one element.
"

# colorEnum <-
  function() {
  list(BLUE = "BLUE", RED = "RED", BLACK = "BLACK")
}


"You can get the list of names for use in a match, i.e. your function parameter
could be..."

# EnumTest <-
  function(enum = names(colorEnum())) {}

"You can actually abbreviate too, but it must be unique. (If you use RStudio,
since col is a list, it will suggest completions!)"

# col$BLA




"If you want more sophisticated enum handling, you could assign S3 classes to
the thing returned by your enum constructor function and write a small collection
of functions to dispatch on class 'enum'' and allow case-insensitive indexing. You
could also add special functions to work with a specific class, e.g. 'colorEnum';
I have not done that here. Inheritance means the list access methods all still work."

#' Test enum class
#' @export
colorEnum2 <-
  function() {
    structure(
        list(BLUE = "BLUE", RED = "RED", BLACK = "BLACK"),
        class= c("colorEnum2", "enum", "list")
    )
}

# Note, changed example to allow multiple returned values.
# `[.enum` <-
  function(x, i) {
    if ( is.character( i ))
        i <- toupper(i)
    class(x) <- "list"
    names(as.list(x)[i])
}

# `[[.enum` <-
  function(x, i, exact= FALSE) {
    if ( is.character( i ))
        i <- toupper(i)
    class(x) <- "list"
    as.list(x)[[i, exact=exact]]
}

# `$.enum` <-
  function(x, name) {
    x[[name]]
}


# col <- colorEnum2()
# # All these return [1] "RED"
# col$red
# col$r
# col[["red"]]
# col[["r"]]
# col["red"]


# col[c("red", "BLUE")]
# col["r"]


"These override the built in [, [[ and $ functions when the thing being indexed
is of class 'enum', for any 'enum' classed objects. If you need another one, you
just need to define it."

# directionEnum <-
  function() {
  structure(
    list(LEFT = "LEFT", RIGHT = "RIGHT"),
    class= c("directionEnum", "enum", "list")
  )
}




"If you need several enum objects, you could add a factory function enum that
takes a vector of strings and a name and returns an enum object. Most of this is
just validation."

#' Create enum S3 class given a vector of strings and an enum classname
#'
#' Factory function enum that takes a vector of strings and a name and  returns
#'   an enum object. Dispatches on case-insensitive indexing argument list
#' @param enums list of (symbol, value) mappings for enumerable class
#' @param name (optional) name of enumerable class
#' @export
#' @examples
#'
#' col <- enum(c("BLUE", "red", "Black"), name = "TheColors")
#' col$R
#' #> [1] "RED"
#' class(col)
#' #> [1] "TheColors" "enum"      "list"
#'
#' side <- enum(c("left", "right"))
#' side$L
#' #> [1] "LEFT"
#' class(side)
#' #> [1] "enum" "list"
enum <- function(enums, name = NULL) {
  if (length(enums) < 1)
    stop ("Enums may not be empty.")

  enums       <- toupper(as.character(enums))
  uniqueEnums <- unique(enums)

  if (!identical(enums, uniqueEnums))
    stop ("Enums must be unique (ignoring case).")

  validNames <- make.names(enums)
  if (!identical(enums, validNames))
    stop("Enums must be valid R identifiers.")

  enumClass  <- c(name, "enum", "list")
  obj        <- as.list(enums)
  names(obj) <- enums

  structure(obj, class = enumClass)
}



"But now this is starting to look like a package..."

###############################################################################
###############################################################################

"I like to use environments as replacement for enums because you can lock
them to prevent any changes after creation."
# Enum <-
  function(...) {

  ## EDIT: use solution provided in comments to capture the arguments
  values <- sapply(match.call(expand.dots = TRUE)[-1L], deparse)

  stopifnot(identical(unique(values), values))

  res <- setNames(seq_along(values), values)
  res <- as.environment(as.list(res))
  lockEnvironment(res, bindings = TRUE)
  res
}

"Create a new enum like this:"

# FRUITS <- Enum(APPLE, BANANA, MELON)
#
# # We can the access the values:
# FRUITS$APPLE
#
# # But we cannot modify them or create new ones:
#
# FRUITS$APPLE <- 99  # gives error
# FRUITS$NEW <- 88  # gives error


###############################################################################
###############################################################################


# C-like enum
# EnumTest <-
  function(colorEnum = ColorEnum$BLUE) {
  enumArg <- as.character(match.call()[2])
  match.arg(enumArg, stringi::stri_c("ColorEnum$", names(ColorEnum)))
  sprintf("%s: %i",enumArg,colorEnum)
}

# ColorEnum <- list(BLUE = 0L, RED = 1L, BLACK = 2L)




# # enumerations pkg
# match.enum.arg <-
  function(arg, choices) {
  if (missing(choices)) {
    formal.args <- formals(sys.function(sys.parent()))
    choices <- eval(formal.args[[as.character(substitute(arg))]])
  }

  if(identical(arg, choices))
    arg <- choices[[1]][1]    # choose the first value of the first list item

  # extract the integer values of the enum items
  allowed.values <- sapply(choices,function(item) {item[1]})

  if(!is.element(arg, allowed.values))
    stop(paste("'arg' must be one of the values in the 'choices' list:",
               paste(allowed.values, collapse = ", ")))

  return(arg)
}


# color2code(ColorEnum$RED) # use a value from the enum (with auto completion support)
# color2code()              # takes the first color of the ColorEnum
# color2code(3)             # an integer enum value (dirty, just for demonstration)
# color2code(4)             # an invalid number
# # Error in match.enum.arg(enum) :
# #   'arg' must be one of the values in the 'choices' list: 1, 2, 3
