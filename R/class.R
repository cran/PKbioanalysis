setClass(
    "ChromResBase",
    slots = c(
        metadata = "data.frame",
        peaks = "data.frame",
        transitions = "data.frame",
        compounds = "data.frame",
        vendor = "character",
        pk_metadata = "list"
    )
)

#' Class ChromRes inherits from ChromResBase
#' @noRd
setClass(
    "ChromRes",
    contains = "ChromResBase",
    slots = c(
        runs = "list"
    )
)


setClass(
    "QuantRes",
    slots = c(
        samples_metadata = "data.frame",
        compounds_metadata = "data.frame",
        quanttab = "list",
        linearity = "list",
        suitability = "list",
        resEstim = "list"
    )
)
