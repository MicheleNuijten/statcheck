statcheckReport <-
  function(statcheckOutput,
           outputFileName,
           outputDir) {

    # set working directory to output file in statcheck package library
    currentWD <- getwd()
    on.exit(setwd(currentWD))
    setwd(system.file("inst/rmd", package = "statcheck"))

    # temporarily save statcheck output as RData in the selected working directory
    save(statcheckOutput, file = "statcheckOutput.RData")

    # run the markdown/knitr script
    statcheckReport_template <-
      system.file("rmd/statcheckReport_template.Rmd", package = "statcheck")
    render(statcheckReport_template)

    # save/move the file in/to the specified output directory
    curDir <- system.file("rmd", package = "statcheck")
    file.rename(
      from = paste(curDir, "statcheckReport_template.html", sep = "/"),
      to = paste(outputDir, "/", outputFileName, ".html", sep = "")
    )

    # remove .RData file from package library folder
    file.remove(paste(curDir, "statcheckOutput.RData", sep = "/"))

  }
