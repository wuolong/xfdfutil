excel2xfdf <- function (xlsxfile) {

    require (xml2)
    require (magrittr)
    
    atext <- xlsx::read.xlsx (xlsxfile, sheetIndex = 1)    
    fdf <- xml_new_root ("xfdf", xmlns = "http://ns.adobe.com/xfdf/",
                     "xml:space" = "preserve") %>%
    xml_add_child ("annots") %>%
    xml_root ()
    fdf.annote <- xml_child (fdf)
    properties <- c("color", "text-align", "font-size", "font-weight",
                    "font-style", "font-family", "font-stretch")
    varnames <- gsub ("-", ".", properties)
    for (i in seq(nrow (atext))) {
        tmp <- atext[i,]
        fdf.annote %>% xml_add_child ("freetext",
                                      color = tmp$bg.color, 
                                      page = tmp$page, rect = tmp$rect, flags = "print")
        fdf.freetext <- xml_child (fdf.annote, i)
        fdf.freetext %>% xml_add_child ("contents-richtext")
        fdf.richtest <- xml_child (fdf.freetext)
        font.style <- paste (properties, tmp[,varnames], sep = ":", collapse = ";")
        fdf.richtest %>% xml_add_child ("body",
                                        tmp$text,
                                        xmlns = "http://www.w3.org/1999/xhtml",
                                        "xmlns:xfa" = "http://www.xfa.org/schema/xfa-data/1.0/",
                                        "xfa:APIVersion" = "Acrobat:11.0.1",
                                        "xfa:spec" = "2.0.2",
                                        style = font.style)
    }
    write_xml (fdf, file = gsub ("lsx$", "fdf", xlsxfile))
    return (invisible (fdf))
}
