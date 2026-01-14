check_call_plate <- function(prev, new) {
  if (as.character(prev[[1]]) != "generate_96") {
    stop("First argument must be a call to generate_96()")
  }
}

# check is the call last call has fill_scheme
# return either FALSE if no last fill_scheme or the value of fill_scheme
check_last_fill <- function(prev) {
  if (length(prev) < 2) {
    stop("Previous call must have at least one argument")
  }
  # prev$fill == NULL
  if (as.character(prev[[1]]) == "fill_scheme") {
    rlang::call_args(prev)[["fill"]]
  } else {
    NULL
  }
}


# remove the last call
undo_last_call <- function(prev) {
  if (!is.call(prev)) {
    stop("Not a call")
  }
  inner <- prev[[2]]
  if (!is.call(prev)) {
    stop("Inner is not a call, nothing more to do")
  }
  inner
}


# print the expression in a readable format
print_expr <- function(expr) {
  parts <- list()

  # Traverse down the call chain
  while (is.call(expr)) {
    fn <- expr[[1]]
    args <- as.list(expr)[-1]
    parts <- append(parts, list(list(fn = fn, args = args)))
    expr <- args[[1]]
  }

  # Start with the base expression (e.g., 1:10)
  pipe_chain <- deparse(expr)

  # Walk upward and build pipe string
  for (i in rev(seq_along(parts))) {
    fn <- parts[[i]]$fn
    args <- parts[[i]]$args
    args <- args[-1] # remove first argument (piped input)

    # Format remaining arguments (if any)
    if (length(args) > 0) {
      arg_strs <- mapply(
        function(arg, nm) {
          if (nm == "") {
            deparse1(arg)
          } else {
            paste0(nm, " = ", deparse1(arg))
          }
        },
        args,
        names(args),
        SIMPLIFY = TRUE
      )
      arg_text <- paste(arg_strs, collapse = ", ")
      call_str <- paste0(deparse1(fn), "(", arg_text, ")")
    } else {
      call_str <- paste0(deparse1(fn), "()")
    }

    pipe_chain <- paste0(pipe_chain, " |> ", call_str)
  }

  pipe_chain
}
