selectorWidget <- function(options, selected = character(0), title = "Selector",
                           ordersel = FALSE, ordernsel = FALSE, 
                           height = max(10,min(20,length(options)))) {
    
    require(tcltk) || stop("requires the package tcltk")
    
    move <- function(from, to, order) {
        while (length(index <- as.integer(tkcurselection(from)))) {
            item <- tclvalue(tkget(from, index[1]))
            tkinsert(to, "end", item)
            tkdelete(from, index[1])
        }
        tkselection.clear(from, 0, "end")
        if (!missing(order)) {
            newto <- as.character(tkget(to, 0, "end"))
            newto <- intersect(order, newto)
            tkdelete(to, 0, "end")
            tkinsert(to, "end", newto)
        }
    }
    
    ok <- function() {
        selected <<- as.character(tkget(selList, 0, "end"))
        tkdestroy(tt)
    }
    
    tt <- tktoplevel()
    tkwm.title(tt, title)
    tkwm.resizable(tt, 0, 0)
    
    selectorFrame <- tkframe(tt, bd=10)
    
    selFrame <- tkframe(selectorFrame)
    selList <- tklistbox(selFrame, selectmode="extended", height=height,
                         yscrollcommand=function(...) tkset(selScroll, ...))
    selScroll <- tkscrollbar(selFrame, takefocus=0, 
                             command=function(...) tkyview(selList, ...))
    tkgrid(selLabel <- tklabel(selFrame, text="Selected"), columnspan=2)
    tkgrid(selList, selScroll, sticky="ns")
    
    nselFrame <- tkframe(selectorFrame)
    nselList <- tklistbox(nselFrame, selectmode="extended", height=height,
                           yscrollcommand=function(...) tkset(nselScroll, ...))
    nselScroll <- tkscrollbar(nselFrame, takefocus=0, 
                               command=function(...) tkyview(nselList, ...))
    tkgrid(nselLabel <- tklabel(nselFrame, text="Not Selected"), columnspan=2)
    tkgrid(nselList, nselScroll, sticky="ns")
    
    if (ordersel)
        addcmd <- function() move(nselList, selList, options)
    else
        addcmd <- function() move(nselList, selList)
    if (ordernsel)
        removecmd <- function() move(selList, nselList, options)
    else
        removecmd <- function() move(selList, nselList)
    
    moveFrame <- tkframe(selectorFrame)
    addBut <- tkbutton(moveFrame, text="<-", command=addcmd)
    removeBut <- tkbutton(moveFrame, text="->", command=removecmd)
    tkgrid(addBut, pady=5)
    tkgrid(removeBut, padx=10, pady=5)
    
    if (length(selected))
        tkinsert(selList, "end", selected)
    if (length(setdiff(options, selected)))
        tkinsert(nselList, "end", setdiff(options, selected))
    
    tkgrid(selFrame, moveFrame, nselFrame)
    
    buttonFrame <- tkframe(tt)
    cancelBut <- tkbutton(buttonFrame, text="Cancel", command=function() tkdestroy(tt))
    okBut <- tkbutton(buttonFrame, text="OK", default="active", command=ok)
    tkbind(tt, "<Key-Return>", ok)
    tkgrid(cancelBut, okBut, padx=10, pady=10)
    
    tkgrid(selectorFrame)
    tkgrid(buttonFrame, sticky="se")
    
    tkwait.window(tt)
    
    return(selected)
}
