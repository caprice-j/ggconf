context("theme2")
library(ggplot2)

test_that("theme2", {

    tdf <- define_ggbash_constants()$themedf
        
    ee(theme2(axis.text(sz=20), text(f="bold")),
       theme(axis.text = element_text(size=20),
             text = element_text(face="bold")))

    ggplot(mtcars) + geom_point(aes(mpg, wt)) +
    theme(axis.text = element_text(size=20, face="bold"),
          axis.line = element_line(color = "black"))
    theme2(axis.text(size=20, face="bold"), axis.line(color="black"))
    
    # spaces under depth 2
    ee(theme2(title(family="Times New Roman")),
       theme(title = element_text(family="Times New Roman")))
    
    ee(theme2(l.bg(fill="red"), l.mgn(.5, .5, .5, .2, "inch")),
       theme(legend.background = element_rect(fill="red"),
             legend.margin = margin(.5, .5, .5, .2, "inch")))
    
    ee(theme2(axis.line(arrow=arrow(angle=30))),
        theme(axis.line = element_line(arrow=arrow())))
    ee(theme2(axis.line(arrow=arrow(angle=30, ends="last", 
                                    type="open", length = unit(.25, "inches")))),
        theme(axis.line = 
                  element_line(arrow=arrow(angle=30, ends="last", 
                                           type="open", 
                                           length = unit(.25, "inches")))))

    ee(theme2(a.txt(z=rel(1.0))),
       theme(axis.text = element_text(size=rel(1.0))))
    
})

test_that("partial-match", {
    
    tdf <- define_ggbash_constants()$themedf
    
    # partial to full
    p2f <- function(p) {
        tdf$name[find_first_index(p, tdf$name, 
                                  show_warn = FALSE, debug = FALSE)]
    }
    
    ee(p2f("l"),  "line")
    ee(p2f("t"),  "text") # not rect
    ee(p2f("r"),  "rect")
    ee(p2f("tl"), "title")
    ee(p2f("a.ratio"), "aspect.ratio")    
    ee(p2f("a.ttl"), "axis.title")        
    ee(p2f("attlx"), "axis.title.x")        
    ee(p2f("atly"), "axis.title.y") 
    ee(p2f("atlxtop"), "axis.title.x.top")        
    ee(p2f("atxt.x.top"), "axis.text.x.top")        
    ee(p2f("a.l"), "axis.line")       
    ee(p2f("leg.bg"), "legend.background")  
    ee(p2f("l.bg"), "legend.background")    
    ee(p2f("l.m"), "legend.margin")    
    ee(p2f("l.s"), "legend.spacing")    
    ee(p2f("l.k"), "legend.key")    
    ee(p2f("lk"), "legend.key")    
    ee(p2f("l.t"), "legend.text")    
    ee(p2f("l.t.a"), "legend.text.align")    
    ee(p2f("l.ttl"), "legend.title")    
    ee(p2f("l.pos"), "legend.position")    
    ee(p2f("l.dir"), "legend.direction")        
    ee(p2f("pnl.bg"), "panel.background")
    ee(p2f("pnl.bd"), "panel.border")
    ee(p2f("plot.bg"), "plot.background")
    ee(p2f("s.txt"), "strip.text")
})

