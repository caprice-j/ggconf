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
    
    cmd <- replace_marks(raw_input)
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
theme2 <- function(...){

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
    eval(parse(text = theme_str))
}