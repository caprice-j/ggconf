# CONSTAES : Constant Aesthetics
# CHARAES : Character Aesthetics
GGPLOT2_TOKENS <- c("1_NAME", "CONSTAES", "CHARAES", "0_THEME", "COMMA",
                    "BOOLEAN", "QUOTED", "UNIT", "POUND", "ENDOFTOKEN",
                    "0_FUNC"
                   )

GGPLOT2_LITERALS <- c() # needed?
GGPLOT2INVALIDTOKEN <- " <<INVALID_TOKEN_HERE>> "

# MAYBE-LATER don't know how to pass variables between yacc's production rules
ggconfenv <- new.env() # Note: This is a global variable.

ggregex <- list(
    plus_pipe  = "(\\+|\\|)\\s*",
    quoted     = paste0("('|\\\")",                      # start from a quote
                        "[a-zA-Z0-9\\._\\+\\-\\*\\/\\^ ]+",
                        "('|\\\")"),                      # end by a quote
    booleanaes = paste0("[a-zA-Z_][a-zA-Z_0-9\\.]\\s*=\\s*",
                        "(TRUE|FALSE|true|false|True|False)"),
    boolean    = "^(TRUE|FALSE|T|F|t|f|true|false|True|False)$",
    charaes    = paste0("[a-z]+=('|\\\").*?('|\\\")"),
    constaes   = "[a-z\\.]+=c\\([0-9\\.,\\)]+", # FIXME adhoc for binw=c(.1, .1)
    # Note: ggregex$constaes and t_CONSTAES rules are duplicated
    unit       = "[0-9\\.,]+\\s*['\"]?(cm|in|inch|inches)['\"]?"
)

Ggplot2Lexer <-
    R6::R6Class(
        "Lexer",
        public = list(
            tokens = GGPLOT2_TOKENS,
            literals = GGPLOT2_LITERALS,
            #states = list(c('ggplot')),
            info = "not-used-now",
            # Note: t_(function) defines precedences implicitly
            t_CONSTAES = function(re="[a-z\\.]+\\s*=\\s*-*[0-9\\./\\*-\\+:]*[0-9]", t) {
                ggconf_dbgmsg("  t_CONSTAES: ", t$value)
                if (grepl("^group=", t$value)) {
                    t$type <- "1_NAME"
                    # aes(group=1)
                    return(t)
                }

                # last [0-9] is needed to interpret
                # size=7 + theme as "size=7" and "+ theme"
                return(t) # integers and floats
            },
            # I believe CONSTAES cannot contain +-*/^, because
            # gg iris + point Sepal.W Sepal.L size=4 + smooth colour="red"
            # will be interpreted as
            # LexToken(CHARAES,colour="blue" size=4 + smooth colour="red",1,33)
            # MAYBE LATER default arguments of functions cannot accept
            # global variables as defaults?
            # ggregex$charaes is falsely evaluated as empty string
            t_CHARAES = function(re="[a-z\\.]+\\s*=\\s*('|\\\").*?('|\\\")", t) {
                ggconf_dbgmsg("  t_CHARAES: ", t$value)
                return(t)
            },
            t_0_THEME = function(re="theme\\(", t) { 
                ggconf_dbgmsg("  t_0_THEME: ", t$value)
                t$type <- "0_THEME"; return(t) 
            },
            t_POUND = function(re="#", t) {
                ggconf_dbgmsg("  t_POUND: ", t$value)
                t$type <- "POUND"; return(t) 
            },
            t_COMMA = function(re=",", t) {
                ggconf_dbgmsg("  t_COMMA: ", t$value)
                t$type <- "COMMA"; return(t) 
            },
            t_0_FUNC = function(re="[a-z\\.]+=(arrow|rel|paste|paste0)\\([^#]*\\)", t) {
                # FIXME any function
                ggconf_dbgmsg("  t_0_FUNC: ", t$value)
                t$type <- "0_FUNC"; return(t) 
            },
            t_1_NAME      = function(re="(\\\"|')?[\\.a-zA-Z0-9_\\(\\)\\-][a-zA-Z_0-9\\.,=\\(\\)\\-\\+\\/\\*]*(\\\"|')?(\\s*inches|\\s*inch|\\s*in|\\s*cm)?(\\\"|')?", t) {
                if (grepl("theme\\(", t$value)) {
                    t$type <- "0_THEME"
                    return(t)
                }
                if (t$value == "__ENDOFTOKEN") {
                    ggconf_dbgmsg("  t_ENDOFTOKEN: ", t$value)
                    t$type <- "ENDOFTOKEN"
                    return(t)
                }
                if (grepl(ggregex$constaes, t$value)) {
                    ggconf_dbgmsg("  t_NAME: CONSTAES ", t$value)
                    t$type <- "CONSTAES"
                } else if (grepl(ggregex$boolean, t$value)) {
                    ggconf_dbgmsg("  t_NAME: BOOLEAN ", t$value)
                    t$type <- "BOOLEAN"
                } else if (grepl(ggregex$unit, t$value)) {
                    ggconf_dbgmsg("  t_NAME: UNIT ", t$value)
                    t$type <- "UNIT"
                    # ex. LexToken(UNIT,.20 cm,1,50)
                } else if (grepl(ggregex$quoted, t$value)) {
                    ggconf_dbgmsg("  t_NAME: QUOTED ", t$value)
                    t$type <- "QUOTED"
                } else {
                    ggconf_dbgmsg("  t_NAME: ", t$value)
                }
                return(t)
            },
            #t_LPAREN  = '\\(',
            #t_RPAREN  = '\\)',
            #t_COMMA = ',',
            t_ignore = " \t",
            t_newline = function(re="\\n+", t) {
                t$lexer$lineno <- t$lexer$lineno + nchar(t$value)
                return(NULL)
            },
            t_error = function(t) {
                cat(sprintf("Illegal character '%s'", t$value[1]))
                t$lexer$skip(1)
                return(t)
            }))

#' display ggconf debug message
#'
#' @param ... a sequence of objects passed to message()
#' 
#'
ggconf_dbgmsg <- function(...) {
    if (exists("ggconf_debug"))
        message(...)
}

Ggplot2Parser <-
    R6::R6Class(
        "Parser",
        public = list(
            tokens = GGPLOT2_TOKENS,
            literals = GGPLOT2_LITERALS,
            # Parsing rules
            #precedence = list(),
            # dictionary of names
            names = new.env(hash = TRUE),
            p_expression_func = function(
                    doc="expression : ggproto", p) {
                ggconf_dbgmsg("p_expression_func LAST")
                p$set(1, p$get(2))
            },
            p_ggproto_theme = function(doc="ggproto : theme_init ENDOFTOKEN
                                       | theme_init theme_elem_list ENDOFTOKEN", p) {
                ggconf_dbgmsg("p_ggproto_theme: ", p$get(2), " -- add ) ")
                if (p$length() == 3) {
                    end <- paste0(p$get(2), ")")
                    p$set(1, end)
                } else {
                    p$set(1, paste0(p$get(2), p$get(3), ")"))
                    # p$set(1, ...) means, "combine tokens to generate a string (i.e. p$get(1))"
                }
            },
            p_theme_init = function(doc="theme_init : 0_THEME", p) {
                # initialization
                ggconf_dbgmsg("p_theme_init: ", p$get(2), " -- add (")
                # theme, theme_bw, theme_linedraw, ...
                theme_str <- gsub("\\s|\\+", "", p$get(2))

                # for column name search
                ggconfenv$i_layer <- ggconfenv$i_layer + 1

                p$set(1, paste0("ggplot2::theme("))
            },
            p_theme_elem_list = function(
                doc="theme_elem_list : theme_elem POUND theme_conf_list POUND 
                                | theme_elem POUND theme_conf_list POUND theme_elem_list",
                p) {
                ggconf_dbgmsg("p_theme_elem_list: ", p$get(2), " AND ", p$get(4))
                elem <- p$get(2)
                if (p$length() == 5) {
                    # last configuration
                    #p$set(1, paste0(elem, "(", p$get(3), ")"))
                    p$set(1, paste0(elem, "(", p$get(4), ")"))
                    # close ggplot2::element_*(
                    # MAYBE-LATER "none" is now ("none")
                } else {
                    #if (! ggconfenv$elem_class %in% c("logical", "character"))
                        p$set(1, paste0(elem, "(", p$get(4), "), ", p$get(6)))
                    #else
                    #    p$set(1, paste0(elem, p$get(3), "), ", p$get(4)))
                    # text = element_text(...) , ... so no need to close paren
                }
            },
            p_theme_elem = function(doc="theme_elem : 1_NAME", p) {
                elem_name_partial <- gsub("\\(", "", p$get(2))

                ggconf_dbgmsg("p_theme_elem: ", elem_name_partial)
                tdf <- ggconfenv$const$themedf

                # 'axis.te:' will be 'axis.te'
                #elem_name_partial <- gsub("\\:", "", p$get(2))
                #elem_name_partial <- p$get(2)
                elem_name <- tdf$name[find_first_index(elem_name_partial,
                                                 tdf$name, show_warn = FALSE)]

                # do partial match for theme element
                # (ex. 'legend.t' -> 'legend.text')
                elem_class <- tdf$class[find_first_index(elem_name,
                                                   tdf$name,
                                                   show_warn = FALSE)]

                if (length(elem_class) == 0 || is.na(elem_class)) {
                    errinfo <-
                    list(
                        id = "p_theme_elem:prefix_match",
                        type = paste0("Prefix match for theme element name '",
                                      p$get(2),
                                      "' failed."),
                        raw = p$get(2),
                        input = elem_name_partial,
                        table = tdf$name
                        )
                    show_fixit_diagnostics(errinfo)

                    ggconfenv$error <- TRUE

                    return(p$set(1, GGPLOT2INVALIDTOKEN))

                } else if (length(elem_class) > 1) {
                    message("UNKNOWN ERROR in p_theme_elem: ",
                            paste0(elem_class, collapse = " "))
                    elem_class <- elem_class[1] # What's this error?
                    return(p$set(1, GGPLOT2INVALIDTOKEN))
                }
                
                ggconfenv$elem_class <- elem_class

                if (grepl("^element_|margin", elem_class)) {
                    modifier <- "ggplot2::"
                    function_name <- paste0(modifier, elem_class)
                } else if (elem_class == "unit") {
                    modifier <- "grid::"
                    function_name <- paste0(modifier, elem_class)
                } else if (elem_class %in% c("logical", "character") ){
                    function_name <- ""
                } else {
                    message("ERROR: cannot get correct ",
                            "classes for a theme element: ",
                            paste0(elem_class, collapse = " "))
                    elem_class <- elem_class[1] # What's this error?
                    return(p$set(1, GGPLOT2INVALIDTOKEN))
                }

                p$set(1, paste0(elem_name, " = ", function_name))
            },
            p_theme_conf_list = function(doc="theme_conf_list : CONSTAES
                                         | CHARAES
                                         | QUOTED
                                         | BOOLEAN
                                         | UNIT
                                         | 0_FUNC
                                         | 0_FUNC COMMA theme_conf_list
                                         | CONSTAES COMMA theme_conf_list
                                         | CHARAES COMMA theme_conf_list", p) {
                ggconf_dbgmsg("p_theme_conf_list: ", p$get(2))

                if (! is.null(ggconfenv$error)) {
                    ggconfenv$error <- NULL # FIXME too compicated
                    # FIXME the error in o_theme_elem cannot stop
                    # even if return(p$set(1, GGPLOT2_INVALIDTOKEN)).
                    # It tries to execute this production rule,
                    # so currently early retrun here.
                    # There should be more elegant error handling.
                    return(p$set(1, NULL))
                }

                conf <- p$get(2)

                if (grepl(ggregex$quoted, conf) &&
                    ! grepl("^element_|margin", ggconfenv$elem_class)) {
                    ggconf_dbgmsg("  quoted ", conf, " env$elemclass: ", ggconfenv$elem_class)
                    return(p$set(1, conf))
                } else if (grepl(ggregex$boolean, conf)) {
                    message("  boolean ", conf, " env$elemclass: ", ggconfenv$elem_class)
                    return(p$set(1, conf))
                } else if (grepl(ggregex$unit, conf)) {
                    number <- gsub("[^0-9\\.]", "", conf)
                    this_unit <- gsub("[0-9\\. ]", "", conf)
                    ggconf_dbgmsg("  unit ", conf, " env$elemclass: ", ggconfenv$elem_class)
                    return(p$set(1, paste0(conf))) # e.g. c(2,2,2,2), "inch"
                }

                before_equal <- gsub("=.*", "", conf)
                after_equal  <- gsub(paste0("^", before_equal, "="), "", conf)
                # ggconf_dbgmsg("    before_equal: ", before_equal)
                # ggconf_dbgmsg("     after_equal: ",  after_equal)

                # prefix match
                input <- ggconfenv$elem_class
                tbl <- get_theme_elem_name_conf(input)
                conf_name <- tbl[find_first_index(before_equal,
                                                  tbl, show_warn = FALSE)]

                if (is.na(conf_name)) {
                    errinfo <- list(
                        id = "p_theme_conf_list:partial_match",
                        type = paste0("Partial match for theme ",
                                      "element configuration failed."),
                        input = before_equal,
                        table = tbl
                    )
                    show_fixit_diagnostics(errinfo)
                    return(p$set(1, GGPLOT2INVALIDTOKEN))
                }

                a_conf <- paste0(conf_name, "=", after_equal)
                ggconf_dbgmsg("   p_theme_conf_list->a_conf: ", a_conf)

                if (p$length() == 2) {
                    # FIXME add spaces
                    p$set(1, a_conf)
                } else {
                    p$set(1, paste0(a_conf, ", ", p$get(4)))
                }
            },
            p_error = function(p) {
                if (is.null(p)) {
                    errinfo <- list( id = "p_error:null",
                                     type = "Syntax error at EOF")
                } else {
                    errinfo <- list( id = "p_error:non_null",
                                     type = paste0("Syntax error at \"",
                                                   p$value, "\""))
                }
                show_fixit_diagnostics(errinfo)
            }
            )
        )

#' Display useful debugging info for users
#'
#' @param err A list of error information
#'
#'
show_fixit_diagnostics <- function(
    err = list(
        id = "p_theme_elem:prefix_match",
        type = "Prefix match for theme element name failed.",
        input = "axis.tx:",
        elem_name = "axis.tx",
        elem_table = c("axis.text", "axis.title")
    )
) {
    # MAYBE-LATER Is it possible to get the built entire ggplot object here?
    message("COMPILE ERROR: ", err$type)
    m1 <- function(...) message("  ", ...)
    m2 <- function(...) message("    ", ...)
    m3 <- function(...) message("      ", ...)

    similarv <- get_analogue(err$input, err$table, threshold = 50)$name

    if (err$id == "p_theme_elem:prefix_match") {

        m1("Is your theme element's name correct?")
        m2("The supplied string is \"", err$raw, "\", but")
        m3("maybe: ", paste0(similarv, collapse = ", "))
    } else if (err$id == "p_error:non_null") {
        m1("Did you specify a theme element name?")
        m2(" BAD: theme2(txt(3))")
        m2("GOOD: theme2(txt(size = 3))")
    } else if (err$id == "p_error:null") {

    }
}

#' replace some marks for later ggconf parsing
#'
#' @param input An input string passed from users
#'
replace_marks <- function(input = "theme2(l.txt(size=12))") {
    
    depth <- 0
    strlen <- nchar(input)
    in_double_quote <- FALSE
    in_quote <- FALSE
    
    inputv <- unlist(strsplit(input, ""))
    
    for (i in 1:strlen) {
        char <- substr(input, i, i)
        if (char == "(")
            depth <- depth + 1
        else if (char == ")")
            depth <- depth - 1
        else if (char == "'")
            in_quote <- ifelse(in_quote, FALSE, TRUE)
        else if (char == '"')
            in_double_quote <- ifelse(in_double_quote, FALSE, TRUE)
        
        if (char == " " && (depth < 2 || (!in_double_quote && !in_quote)) ) {
            inputv[i] <- ""
        }
        if ((depth==2 && char == "(") ||
            (depth==1 && char == ")"))
            inputv[i] <- "#"
        if (depth==1 && char == ",")
            inputv[i] <- " "
    }
    return(paste0(inputv, collapse=""))
}

#' remove element_* function calls
#'
#' @param input An input string passed from users
#'
remove_element_whatever <- function(input = "theme(l=element_text(sz=20))") {
    # assume "=" and "element_text" don't have spaces between them
    out <- gsub("=\\s+element_(text|rect|line|grob|blank)", "", input)
    return(out)
}

#' the core function of ggconf
#'
#' @param cmd preprocessed characters
#'
#' compile_ggconf returns a built theme() object as string.
#'
#'
compile_ggconf <- function(cmd = ""){

    lexer <- rly::lex(Ggplot2Lexer)
    parser <- rly::yacc(Ggplot2Parser)

    ggobj <- parser$parse(cmd, lexer)
    ggconf_dbgmsg("ggobj: ", ggobj)
    info <- lexer$instance$info # access internal variables
    return(ggobj)
}

lex  <- rly::lex(Ggplot2Lexer)
yacc <- rly::yacc(Ggplot2Parser)
