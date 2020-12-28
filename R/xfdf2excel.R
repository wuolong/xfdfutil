
xfdf2excel <- function (dfile) {
    require (xml2)
    if (!file.exists (dfile)) {
        stop (dfile, "does not exisit.\n")
    }
    
    xfdf <- read_xml (dfile)
    annots <- xml_children (xml_children (xfdf)[1])
    
    textboxes <- annots[node.names == "freetext"]
    atext <- do.call ("rbind",
                      lapply (textboxes,
                              function (n) {
                          textnode <- xml_children (xml_children (n)) # body
                          styles <- strsplit (strsplit (xml_attr (textnode, "style"),
                                                        split = ";")[[1]],
                                              split = ":")
                          styles <- data.frame (do.call ("cbind", styles))
                          names (styles) <- gsub ("-", ".", styles[1,])
                          styles <- styles[-1,]
                          cbind (text = xml_text (textnode),
                                 page = xml_attr (n, "page"),
                                 styles,
                                 bg.color = xml_attr (n, "color"),
                                 rect = xml_attr (n, "rect"),
                                 name = xml_attr (n, "name"))
                      }))
    atext <- transform (data.frame (atext),
                        page = as.numeric (page),
                        ## remove extra trailing zeroes
                        rect = gsub ("000($|,)", "\\1", rect))
    rownames (atext) <- NULL
    xlsx::write.xlsx (atext,
                      file = file.path (outdir, gsub ("fdf$", "lsx", basename (dfile)),
                      row.names = FALSE,
                      sheetName = basename (fn))
    return (invisible (atext))
}
