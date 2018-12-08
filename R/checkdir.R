checkdir <-
  function(dir, subdir = TRUE, ...) {
    if (missing(dir))
      dir <- tk_choose.dir()

    pdfs <-
      any(grepl("\\.pdf$", list.files(dir, recursive = subdir), ignore.case =
                  TRUE))
    htmls <-
      any(grepl(
        "\\.html?$",
        list.files(dir, recursive = subdir),
        ignore.case = TRUE
      ))

    if (!pdfs & !htmls)
        stop("No PDF or HTML found")

    pdfres <- ifelse(pdfs, checkPDFdir(dir, ...), NULL)
    htmlres <- ifelse(htmls, checkHTMLdir(dir, ...), NULL)

    if (is.null(pdfres) & is.null(htmlres))
      stop("statcheck did not find any results")
    else
      Res <- rbind(pdfres, htmlres)

    class(Res) <- c("statcheck", "data.frame")
    return(Res)

  }
