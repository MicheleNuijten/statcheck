with_scan_dir <- function(code) {
  dir <- tempfile("statcheck-scan-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  code(dir)
}

test_that("directory scans recognize uppercase extensions and ignore backups", {
  with_scan_dir(function(dir) {
    writeLines("<p>t(28) = 2.20, p = .06</p>", file.path(dir, "article.HTML"))
    writeLines("<p>t(28) = 1.00, p = .20</p>", file.path(dir, "backup.html.bak"))
    writeLines("<p>t(28) = 1.00, p = .20</p>", file.path(dir, "not_html"))

    result <- checkHTMLdir(dir, messages = FALSE)
    expect_equal(result$source, "article.HTML")
    expect_equal(checkdir(dir, messages = FALSE), result)
    expect_equal(nrow(checkHTMLdir(dir, extension = FALSE, messages = FALSE)), 3L)

    pdf <- system.file("test_materials/test_dir/Paper1.pdf", package = "statcheck")
    expect_true(file.copy(pdf, file.path(dir, "article.PDF")))
    pdf_result <- checkPDFdir(dir, messages = FALSE)
    expect_equal(unique(pdf_result$source), "article.PDF")
    expect_equal(nrow(checkdir(dir, messages = FALSE)), nrow(pdf_result) + 1L)
  })
})

test_that("mixed directory scans keep results when one format has no statistics", {
  with_scan_dir(function(dir) {
    pdf <- system.file("test_materials/test_dir/Paper2.pdf", package = "statcheck")
    expect_true(file.copy(pdf, file.path(dir, "empty.pdf")))
    html <- file.path(dir, "article.html")
    writeLines("<p>t(28) = 2.20, p = .06</p>", html)

    expect_equal(checkdir(dir, messages = FALSE), checkHTML(html, messages = FALSE))

    pdf <- system.file("test_materials/test_dir/Paper1.pdf", package = "statcheck")
    expect_true(file.copy(pdf, file.path(dir, "empty.pdf"), overwrite = TRUE))
    writeLines("<p>This article has no statistics.</p>", html)
    expect_equal(checkdir(dir, messages = FALSE),
                 checkPDF(file.path(dir, "empty.pdf"), messages = FALSE))
  })
})

test_that("directory scans still report absent files and absent results", {
  with_scan_dir(function(dir) {
    expect_error(checkdir(dir, messages = FALSE), "No PDF or HTML found")
    writeLines("<p>This article has no statistics.</p>", file.path(dir, "empty.htm"))
    expect_error(checkdir(dir, messages = FALSE), "statcheck did not find any results")
  })
})

test_that("repeated HTML basenames retain distinct article identities", {
  with_scan_dir(function(dir) {
    dir.create(file.path(dir, "a"))
    dir.create(file.path(dir, "b"))
    files <- file.path(dir, c("a/article.html", "b/article.html"))
    writeLines("<p>t(28) = 2.20, p = .06</p>", files[1])
    writeLines("<p>t(28) = 1.00, p = .20; another p &lt; .05.</p>", files[2])
    writeLines("<p>t(28) = 2.20, p = .06</p>", file.path(dir, "unique.html"))

    result <- checkHTMLdir(dir, messages = FALSE)
    expect_equal(result$source, c("a/article.html", "b/article.html", "unique.html"))
    expect_equal(result$apa_factor, c(1, .5, 1))
    expect_equal(summary(result)$nr_p_values, c(1L, 1L, 1L, 3L))
    expect_equal(checkdir(dir, messages = FALSE), result)

    direct <- checkHTML(files, messages = FALSE)
    expect_equal(direct$source, normalizePath(files, winslash = "/"))
    expect_equal(direct$apa_factor, c(1, .5))
  })
})

test_that("repeated PDF basenames retain distinct article identities", {
  with_scan_dir(function(dir) {
    dir.create(file.path(dir, "a"))
    dir.create(file.path(dir, "b"))
    files <- file.path(dir, c("a/article.pdf", "b/article.pdf"))
    fixtures <- system.file(paste0("test_materials/test_dir/Paper", c(1, 3), ".pdf"),
                            package = "statcheck")
    expect_true(all(file.copy(fixtures, files)))

    result <- checkPDFdir(dir, messages = FALSE)
    expect_equal(unique(result$source), c("a/article.pdf", "b/article.pdf"))
    expect_equal(nrow(summary(result)), 3L)
    expect_equal(checkdir(dir, messages = FALSE), result)

    writeLines("<p>t(28) = 2.20, p = .06</p>", file.path(dir, "article.html"))
    mixed <- checkdir(dir, messages = FALSE)
    expect_equal(unique(mixed$source), c("a/article.pdf", "b/article.pdf", "article.html"))
    expect_equal(nrow(summary(mixed)), 4L)

    direct <- checkPDF(files, messages = FALSE)
    expect_equal(unique(direct$source), normalizePath(files, winslash = "/"))
  })
})
