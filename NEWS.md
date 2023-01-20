# statcheck 1.4.0
<!---searched in commits on statcheck repo: `committer-date:2020-03-15..2020-04-30`
and checked out changes in the beta releases --->

## Major external changes
* The variable names from the output of `statcheck()` have changed to increase consistency in style and naming. This means that the variable names in the output of `checkPDF()`, `checkHTML()`, `checkdir()`, `checkPDFdir()`, and `checkHTMLdir()` have also changed.

## Major internal changes
There have been major updates to the internal structure of statcheck. Some of the most notable are:

* The main `statcheck()` function has been significantly shortened by summarizing a lot of repeating actions into general functions, and by moving functions from within `statcheck()` to their own scripts.
* Combine the regular expressions for all separate tests into one main regex and write generic functions to parse the extracted NHST results. In the previous version, a lot of the regexes overlapped, and many actions were performed multiple times throughout the code, which made the script inefficient and error-prone
* Update the way that errors and decision_errors are determined. Now, checking for correct rounding is done within the error function, instead of in a separate function
* Calculate one-tailed p-values more sophisticatedly: instead of simply doing p/2, statcheck now actually calculates a one-tailed p-value based on the surface under one tail of the appropriate distribution. This ensures that for one-tailed p-values, correct rounding is also taken into account
* All documentation and the NAMESPACE are now generated with roxygen2
* The variable names in the output are now based on a file with constants. This makes it easier to update the names in a later stage if necessary, without having to go through every script

## Small updates
* Don't show a message to warn for the potential presence of one-tailed tests and other significance levels. This text was mainly distracting.
* When numbering unnamed sources, add leading zeros to allow for ordering of the data frame 

## Bug fixes
* accurately take correct rounding into account with negative test statistics
* extract punctuation that could signal a wrongly encoded minus sign
* added additional html encodings of mathematical symbols
* ignore result when test value == NA
* don't throw an error when input is NA (e.g.: statcheck(NA))
* test results with multiple comparison signs are no longer extracted (e.g.: "t(38) >= 2.25, p = .03")

# statcheck 1.3.2
<!---searched in commits on statcheck repo: `committer-date:2018-05-28..2020-03-15` --->

## Updates
* Added unit tests for all main statcheck functions
* Make it possible to suppress progress bars and other messages when running statcheck

## Small updates
* In `checkdir()`, add an argument to specify whether or not to also search subdirectories.
* Take case into account for Q-tests, so 

## Bug fixes
* Close connection after reading html file
* In inexactly reported p-values, statcheck only recognized possible one-tailed tests in p < .05, not other numbers. Unclear why. Fixed.
* Don't recognize Kolmogorov-Smirnov test statistic D as a chi-square
* Take case into account for Q-tests to avoid wrongly considering Cohen's q as a heterogeneity test.

# statcheck 1.3.1
<!---searched in commits on statcheck repo: `committer-date:2018-04-05..2018-05-28` --->

## Small updates
* Updates to statcheck report template

## Bug fixes
* Summary function now gives back the Source name (instead of Source number)
* Reset working directory after running statcheckReport()

# statcheck 1.3.0
<!---searched in commits on statcheck repo: `committer-date:2016-12-20..2018-04-05` --->

## New features
* **Q-tests**: statcheck is now able to find Q-tests for heterogeneity (in meta-analyses). As always, the Q-tests need to be APA reported. statcheck recognizes general Q-tests, Q-within, and Q-between.
* **HTML reports**: it is now possible to generate nicely formatted HTML reports with statcheck results with the function `statcheckReport()`.

## Small updates
* Formatted code to improve readability
* Removed text for the help files from the R scripts (the help files are not created automatically anymore, and having all this text in between the R code decreased readability)

## Bug fixes
* Fixed mistake in error coding. statcheck flagged cases such as "F(1, 138) < 1, p = .812" as inconsistent, and opposite cases as consistent, but it should be the other way around.
* Changed PDF import function so that statcheck can now also handle files saved with double file extensions (e.g., myfile.pdf.pdf or myfile.html.pdf). HT to Nick Brown for pointing out this problem.
* Summary function now gives back the Source names instead of Source numbers
* Recognize minus signs in HTML coded as `&minus`
* Fixed bug in `summary.statcheck()` so that it gives back the number of articles instead of the article name

# statcheck 1.2.3
<!---searched in commits on statcheck repo: `committer-date:2016-08-17..2016-12-20` --->

## Bug fixes
* statcheck flagged cases such as "F(1, 138) < 1, p = .812" as inconsistent, and opposite cases as consistent, but it should be the other way around.
* Fix issue with reading html in a Linux environment by using `useBytes = TRUE`

# statcheck 1.2.2
<!---searched in commits on statcheck repo: `committer-date:2016-08-16..2016-08-17` --->

## Small internal updates
* Updated documentation
* Small updates in NAMESPACE to pass R CMD check

# statcheck 1.2.1
<!---searched in commits on statcheck repo: `committer-date:2016-06-16..2016-08-16` --->

## Small internal updates
* Import packages instead of Depending on them
* Using `message()` instead of `cat()`

# statcheck 1.2.0
<!---searched in commits on statcheck repo: `committer-date:2015-11-12..2016-06-16` --->

## New features
* Make it optional to count p = .000 as an Error

## Small updates
* Adapted plot function based on John Sakaluk's code. statcheck can now plot in APA style.
* Removed CopyPaste test; this function checked if the same string of results was reported multiple times in a paper or text and flagged it as a possible copy-paste error. However, this function wasn't very useful and therefore removed.
* Added axis limits to plot function so they won't get cut off when the re are no p-values > .5.

## Bug fixes
* Updated regex for chi-square, so that it doesn't match t, F, or r with a subscript
* statcheck sometimes read t-tests in old PDFs as correlations, resulting in correlations >1. This caused an Error in statcheck, but is now ignored.
* In old PDFs "F(1, X) = Y" gets converted by pdftotext to "F(l, X) = Y". If this happens, convert "l" back into a "1". Thanks to Erika Salomon for pointing this out to me.

# statcheck 1.0.2
<!---searched in commits on statcheck repo: `committer-date:2014-07-01..2015-11-12` --->

## New features
* Add option to choose whether or not to count p == alpha as significant or not
* Show pop-up window to select files for `checkPDF()` and `checkHTML()`

## Small updates
* Improve search for subscripts in html
* Also find chi2 with thousand separators in N
* In the automated one-tailed test, search more specifically of "*one*-sided"", instead of "sided", etc.
* Also plot ns statistics

## Bug fixes
* Fixed bugs in determining correct rounding
* If a result is not an error, it can also not be a decision error (this happened in some cases when reported p = .05)

# statcheck 1.0.0
<!---searched in commits on statcheck repo: `committer-date:2012-07-30..2014-07-01` --->

## New features
* Added `diagnose()` to guess a probable cause for an error
* Recognize z-tests
* Calculate the APA factor for each article

## Small updates
* Small updates to the regular expressions
* Recognize number of decimals to distinguish between p = .04 and p = .040
* Also extract and parse negative test statistics
* Also recognize inexact test statistics
* Also recognize values with thousand separators
* Also detect result reported as ns.
* Also recognize p-values in scientific notation
* Recognize more types of spacing in HTML
* Also accept .htm files instead of only .html
* Search for one-tailed tests in text
* Make it optional to count one-tailed tests as errors or not
* Add an option to assume that all tests are one-sided
* Add warnings for possible different significance levels
* Add the option to also search subdirectories
* Chris Hartgerink added inline documentation

## Bug fixes
* Fixed bugs in extracting df for chi-square
* Fixed bug in plotting inexact p-values
* Fixed some bugs in identifying errors
* Better recognition of rounding errors
* Better recognition of p < 0
* Fixed bug so that statcheck doesn't crash if no results are extracted
* Don't read t *space* (df) as a chi-square
* Added Wald tests but removed them again as they were too buggy

## Other updates
* Michele became maintainer of the package

# statcheck 0.1.0
<!---searched in commits on statcheck repo: `committer-date:2012-03-27..2012-07-30` --->

## New features
* plot.statcheck() to plot statcheck object
* Include search for correlations and chi-square
* Search HTML files
* Search entire directories

## Small updates
* Add progress bars

## Bug fixes
* Fixed some bugs in summary function (na.rm = TRUE)

## Other updates
* Michele Nuijten added as co-author

# statcheck 0.0.1
* First version
