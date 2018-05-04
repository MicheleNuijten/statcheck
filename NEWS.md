# Main changes in version 1.3.0

## New features

* **Q-tests**: statcheck is now able to find Q-tests for heterogeneity (in meta-analyses). As always, the Q-tests need to be APA reported. statcheck recognizes general Q-tests, Q-within, and Q-between.
* **HTML reports**: it is now possible to generate nicely formatted HTML reports with statcheck results with the function `statcheckReport()`.

## Bug fixes

* Fixed mistake in error coding. statcheck flagged cases such as "F(1, 138) < 1, p = .812" as inconsistent, and opposite cases as consistent, but it should be the other way around.
* Changed PDF import function so that statcheck can now also handle files saved with double file extensions (e.g., myfile.pdf.pdf or myfile.html.pdf). HT to Nick Brown for pointing out this problem.
* Summary function now gives back the Source names instead of Source numbers
* Recognize minus signs in HTML coded as `&minus`

## Small updates

* Formatted code to improve readability
* Removed text for the help files from the R scripts (the help files are not created automatically anymore, and having all this text in between the R code decreased readability)
