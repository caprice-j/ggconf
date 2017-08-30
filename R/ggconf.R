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
#'
exec_ggconf <- function(raw_input="",
                        show_warn=TRUE, batch_mode=FALSE,
                        as_string = FALSE, show_compiled=TRUE){
    ggconfenv$const <- define_ggconf_constants()
    ggobj <- ""
    ggconf_dbgmsg("exec_ggconf() receives: ", raw_input)
    
    cmd <- replace_marks(raw_input)
    cmd <- remove_element_whatever(cmd)
    cmd <- paste0(substr(cmd, 1, nchar(cmd)-1), "__ENDOFTOKEN")
    ggconf_dbgmsg("preprocessed string: ", cmd)
    
    if(grepl("theme", raw_input)) {
        if (show_warn)
            ggconfenv$show_amb_warn <- TRUE
        else
            ggconfenv$show_amb_warn <- FALSE
        ggconf_dbgmsg("compile_ggconf receives: ", cmd)
        ggobj <- compile_ggconf(cmd)
        ggobj_verbose <- ggobj
        ggobj <- gsub("ggplot2::", "", ggobj)
        
    } else {
        stop("unknown command is supplied")
    }

    if (is.null(ggobj))
        return(FALSE) # ggobj is NULL when p_error() is called

    if (length(ggobj) == 0)
        return(FALSE)
    
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
#' 
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#' 
#' library(ggplot2)
#' ggplot(mtcars) + geom_point(aes(wt, hp, color=as.factor(cyl))) +
#'   theme2(
#'     text(f="bold", z=24, fmly="Times New Roman"),
#'     pnl.bg(fill="white"),
#'     lgd.box.margin(.2, .2, .2, .2, "cm"),
#'     lgd.box.bg(c="black"),
#'     lgd.key(fill="white"),
#'     lgd.position("bottom"),
#'     lgd.txt(z=rel(.8)),
#'     lgd.title(fmly="Consolas", c="royalblue"),  
#'     axs.title(fmly="Consolas", c="royalblue"),  
#'     axs.title.y(angle=0, vjust=.5),             
#'     axs.txt(z=rel(1.1)),
#'     axs.line(arrow=arrow(type="open", angle=20), z=2),
#'     axs.tick(z=1),                              
#'     axs.tick.len(.5, "cm"),
#'     plt.subttl(f="plain", hjust=1),
#'     plt.margin(.3, .3, .3, .1, "inch")          
#'   )
#'
#' 
#' ggplot(mtcars) + geom_point(aes(wt, hp, color=cyl)) +
#'    theme2(a.txt(family = c("Consolas", "Times")[1]))
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
#'    theme2(txt(sz=20, f="bold"), aline(sz=2), l.key(c="black"))
#' 
#' 
#' 
#' }
#' 
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
    # elem_list <- as.list(substitute(match.call()))[-1L];

    input <- paste0("theme(", elem_str, ")")
    ggconf_dbgmsg("theme2 input: ", input)
    ggstr <-
        exec_ggconf(input, show_warn = FALSE,
                    batch_mode = TRUE, as_string = TRUE,
                    show_compiled = FALSE)
    theme_str <- ggstr
    eval(parse(text = theme_str))
}