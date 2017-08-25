#' @import ggplot2
NULL

#' execute raw ggconf commands
#'
#' @param raw_input A ggconf command
#' @param show_warn Whether to show a warning message
#'                    when ambiguously matched. Default is TRUE.
#' @param batch_mode Default is FALSE.
#'                  If TRUE, the resulted ggplot object is returned.
#' @param as_string Return the resulted ggplot2 object as a string
#'                  not as a ggplot2 object. Default is FALSE.
#' @param show_compiled Show the compiled ggplot2 executable command.
#'                      Default is TRUE.
#'
exec_ggconf <- function(raw_input="",
                        show_warn=TRUE, batch_mode=FALSE,
                        as_string = FALSE, show_compiled=TRUE){
    ggbashenv$const <- define_ggbash_constants()
    ggobj <- ""
    
    cmd <- gsub(" ", "", raw_input)
    cmd <- replace_paren(cmd)
    dbgmsg("Input string:", cmd)
    cmd <- remove_element_whatever(cmd)
    cmd <- paste0(substr(cmd, 1, nchar(cmd)-1), "__ENDOFTOKEN")
    dbgmsg("Input string:", cmd)
    
    if(grepl("theme", raw_input)) {
        if (show_warn)
            ggbashenv$show_amb_warn <- TRUE
        else
            ggbashenv$show_amb_warn <- FALSE
        ggobj <- compile_ggconf(cmd)
        ggobj_verbose <- ggobj
        ggobj <- gsub("ggplot2::", "", ggobj)
        
    } else {
        stop("unknown command is supplied")
    }

    if (is.null(ggobj))
        return(FALSE) # ggobj is NULL when p_error() is called

    if (grepl(GGPLOT2INVALIDTOKEN, ggobj)) {
        message("\nThe built ggplot2 object is :\n  ",
                gsub("\\+ gg", "\\+ \n    gg", ggobj))
        return(FALSE)
    }
    
    built_ggplot2_obj <- eval(parse(text = ggobj_verbose))
    
    if (grepl("ggbash_piped", ggobj)) {
        # ggbash_piped should be internal state (not exposed to users)
        # but removing ggbash_piped causes NOTE in R CMD check...
        ggobj <- rm_piped_dataset(ggobj)
    }
    
    if (show_compiled)
        message("  ", ggobj)
    
    if (batch_mode) {
        if (as_string)
            return(ggobj)
        else
            return(built_ggplot2_obj)
    } else {
        print(built_ggplot2_obj)
    }
    
    return(FALSE)
}



#' an enhanced version of ggplot2::theme()
#'
#' theme2() has an enhanced version of ggplot2::theme() in terms of:
#' 1. no element_(text|line|rect|grob|blank) specification
#' 2. partial match for each configuration (e.g. size by sz)
#'
#' @param ... theme element specification (see examples below)
#' @param as_string return the built theme function call as string.
#'                  Default is FALSE.
#'
#' @examples
#' \dontrun{
#'
#' # all of the following three generate the same plot
#'
#' ggplot(mtcars) + geom_point(aes(wt, hp, color=cyl)) +
#'    theme(text = element_text(size=20, face="bold"),
#'    axis.line = element_line(size=2),
#'    legend.key = element_rect(color="black"))
#'
#' ggplot(mtcars) + geom_point(aes(wt, hp, color=cyl)) +
#'    theme2(text(size=20, face="bold"), axis.line(size=2),
#'           legend.key(color="black"))
#'
#' ggplot(mtcars) + geom_point(aes(wt, hp, color=cyl)) +
#'    theme2(text(sz=20, f="bold"), axis.line(sz=2),
#'           legend.key(c="black"))
#'
#'
#' }
#'
#' @export
theme2 <- function(..., as_string = FALSE){

    elem_list <- as.list(substitute(list(...)))[-1L]

    named_arguments <- names(sapply(match.call(), deparse))[-1]
    if (length(named_arguments) > 0) {
        # input has element_text etc.
        # remove element_text for better syntax sugar
        elem_spec <- gsub("element_(text|rect|line|grob|blank)", "", elem_list)
        elem_str <- paste(paste0(named_arguments, elem_spec), collapse = ",")
    } else {
        elem_str <- paste0(elem_list, collapse=", ")
    }
    # elem_list <- as.list(substitute(match.call()))[-1L]

    input <- paste0("theme(", elem_str, ")")
    dbgmsg("theme2 input: ", input)
    ggstr <-
        exec_ggconf(input, show_warn = FALSE,
                    batch_mode = TRUE, as_string = TRUE,
                    show_compiled = FALSE)
    theme_str <- ggstr
    if (as_string)
        theme_str
    else
        eval(parse(text = theme_str))
}

#' print useful debug advice according to the given error message
#'
#' @param err_message A character returned by \code{stop}
#' @param raw_input A character given to \code{\link{ggbash}} function
#'
advice_on_error <- function(err_message,
                            raw_input="gg iris | p Sepal.W Sepal.L") {
    message(err_message)
    if (grepl("E001", err_message)) {
        # TODO list all data frame and matrices
    } else if (grepl("no such prefix", err_message)) {
        datasetname <- gsub("gg\\s([a-zA-Z0-9]+).*", "\\1", raw_input)
        message("  -- Did you give correct column names, geoms, or aesthetics?")
        show_dataset_column_indices(datasetname)
    }
}

suffix2geom <- function(suffix="point") {
    # all geoms listed in ggplot2 2.1.0 docs
    return(switch(suffix,
            "abline"     = ggplot2::geom_abline(),
            "hline"      = ggplot2::geom_hline(),
            "vline"      = ggplot2::geom_vline(),
            "bar"        = ggplot2::geom_bar(),
            "col"        = ggplot2::geom_col(), # not in document but exists
            "bin2d"      = ggplot2::geom_bin2d(),
            "blank"      = ggplot2::geom_blank(),
            "boxplot"    = ggplot2::geom_boxplot(),
            "contour"    = ggplot2::geom_contour(),
            "count"      = ggplot2::geom_count(),
            "crossbar"   = ggplot2::geom_crossbar(),
            "errorbar"   = ggplot2::geom_errorbar(),
            "linerange"  = ggplot2::geom_linerange(),
            "pointrange" = ggplot2::geom_pointrange(),
            "density"    = ggplot2::geom_density(),
            "density_2d" = ggplot2::geom_density_2d(),
            "density2d"  = ggplot2::geom_density2d(),
            "dotplot"    = ggplot2::geom_dotplot(),
            "errorbarh"  = ggplot2::geom_errorbarh(),
            "freqpoly"   = ggplot2::geom_freqpoly(),
            "histogram"  = ggplot2::geom_histogram(),
            "hex"        = ggplot2::geom_hex(),
            "jitter"     = ggplot2::geom_jitter(),
            "label"      = ggplot2::geom_label(),
            "text"       = ggplot2::geom_text(),
            #"map"        = ggplot2::geom_map(), # FIXME handle map
            "path"       = ggplot2::geom_path(),
            "line"       = ggplot2::geom_line(),
            "step"       = ggplot2::geom_step(),
            "point"      = ggplot2::geom_point(),
            "polygon"    = ggplot2::geom_polygon(),
            "qq"         = ggplot2::geom_qq(),
            "quantile"   = ggplot2::geom_quantile(),
            "raster"     = ggplot2::geom_raster(),
            "rect"       = ggplot2::geom_rect(),
            "tile"       = ggplot2::geom_tile(),
            "ribbon"     = ggplot2::geom_ribbon(),
            "area"       = ggplot2::geom_area(),
            "rug"        = ggplot2::geom_rug(),
            "segment"    = ggplot2::geom_segment(),
            "curve"      = ggplot2::geom_curve(),
            "smooth"     = ggplot2::geom_smooth(),
            "violin"     = ggplot2::geom_violin(),
            # other
            "spoke"      = ggplot2::geom_spoke()
        ))
}

#' convert given ggbash strings into ggplot2 aesthetic specifications
#'
#' @param i An integer of index
#' @param aesv A vector of aesthetics
#' @param must_aesv A vector of required aesthetics
#' @param all_aesv A vector of possible aesthetics.
#' @param colnamev A vector of column names of a dataframe.
#' @param show_warn a flag for printing warning when ambiguous match.
#'                    Default is TRUE.
#'
#' must_aesv and all_aesv are built by
#' \code{\link{get_required_aes}} and
#' \code{\link{get_possible_aes}}, respectively.
#'
#' @export
parse_ggbash_aes <- function(i, aesv, must_aesv, all_aesv,
                             colnamev, show_warn=TRUE){
    if (grepl("=", aesv[i])) {
        before_equal <- gsub("\\s*=.*", "", aesv[i])
    } else {
        # no aes specification like geom_point(aes(my_x, my_y))
        before_equal <- must_aesv[i]

        if (i > length(must_aesv))
            stop("too many unspecified aesthetics. ",
                 "Required aesthetics (in order) are: ",
                 paste0(must_aesv, collapse = ", "))
    }
    after_equal  <- gsub(".*=\\s*", "", aesv[i])

    if (substr(aesv[i],1,1) == "z") {
        # FIXME defaultZproblem - z should not be removed
        before_equal <- "z"
        # knowing "z" is needed for this geom is super hard...
        # must_aesv should contain "z" for geom_contour
        # but should not for geom_point...
    } else {
        if (! before_equal %in% all_aesv)
            before_equal <- all_aesv[find_first_index(before_equal, all_aesv, show_warn)]

    }

    if (grepl("group", before_equal))
        return(paste0(before_equal, "=", after_equal))

    # design decision: column name only by prefix match?
    aftr <- parse_after_equal(after_equal, colnamev, show_warn)
    if (is.null(aftr))
        return(NULL)

    return(paste0(before_equal, "=", aftr))
}

#'  convert given ggbash strings into ggplot2 non-aesthetic (constant) specifications
#'
#' @param non_aes A character of a non-aesthetic key and value pair
#' @param all_aesv A vector of possible aesthetics.
#' @param colnamev A character vector representing column names
#' @param show_warn a flag for printing warning when ambiguous match.
#'                    Default is TRUE.
#'
#' all_aesv are built by \code{\link{get_possible_aes}}.
#' \code{\link{parse_ggbash_aes}}
#'
parse_ggbash_non_aes <- function(non_aes="shape=1", all_aesv,
                                 colnamev, show_warn=TRUE){
    before_equal <- gsub("\\s*=.*", "", non_aes)
    after_equal  <- gsub(".*=\\s*", "", non_aes)

    if (! before_equal %in% all_aesv) # partial match
        before_equal <- all_aesv[find_first_index(before_equal, all_aesv, show_warn)]

    if (length(before_equal) == 0) # no such parameter
        return(NULL)

    after_equal <- parse_after_equal(after_equal, colnamev, show_warn)
    if (is.null(after_equal))
        return(NULL)

    return(paste0(before_equal, "=", after_equal))
}

#' parse symbols after equal sign
#'
#' x=factor(Sepal.W + 1) should be interpreted as x = factor(Sepal.Width + 1).
#'
#' @param after A string after equal sign.
#' @param colnamev A character vector representing column names.
#' @param show_warn Show warning message. Default is TRUE.
#'
#' @importFrom sourcetools tokenize_string
parse_after_equal <- function(
    after="1 + Sepal.W^2*3",
    colnamev = c("Sepal.Width", "Sepal.Length", "Species"), show_warn = TRUE
){
    info <- sourcetools::tokenize_string(after)
    nospace <- info[info$type != "whitespace", ]
    nospace$after <- c(nospace[-1, ]$value, "end")
    nospace$bval  <- c("start", nospace[-nrow(nospace), ]$value)# before value
    # TODO how can I know each symbol is a function?
    # especially func(first, second) arguments.
    not_call <- nospace$type == "symbol" & nospace$after != "("
    not_piped <- nospace$type == "symbol" & ! nospace$bval %in% c("%>%", "%T>%")
    not_special <- ! grepl("\\.\\..*\\.\\.", nospace$value)
    candidates <- nospace[not_call & not_piped & not_special, ]

    if (nrow(candidates) == 0)
        return(after)

    for ( i in 1:nrow(candidates)) {
        index <- find_first_by_prefix(candidates$value[i],
                                        colnamev, show_warn)
        if (is.null(index))
            return(NULL)
        candidates$value[i] <- colnamev[index]
    }

    info[as.numeric(rownames(candidates)), "value"] <- candidates$value

    # handyShortcuts
    info[info$value == "f", "value"] <- "factor"
    info[info$value == "n", "value"] <- "as.numeric"
    # might conflict with dplyr::n()?

    return(paste0(info$value, collapse=""))
}
