# statcheck

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/statcheck)](https://cran.r-project.org/package=statcheck)

## What is statcheck?

`statcheck` is a free, open source R package that can be used to automatically extract statistical null-hypothesis significant testing (NHST) results from articles and recompute the *p*-values based on the reported test statistic and degrees of freedom to detect possible inconsistencies. 

`statcheck` is mainly useful for:

1. **Self-checks**: you can use `statcheck` to make sure your manuscript doesn't contain copy-paste errors or other inconsistencies before you submit it to a journal.
2. **Peer review**: editors and reviewers can use `statcheck` to check submitted manuscripts for statistical inconsistencies. They can ask authors for a correction or clarification before publishing a manuscript.
3. **Research**: `statcheck` can be used to automatically extract statistical test results from articles that can then be analyzed. You can for instance investigate whether you can predict statistical inconsistencies (see e.g., [Nuijten et al., 2017](https://www.collabra.org/article/10.1525/collabra.102/)), or use it to analyze p-value distributions (see e.g., [Hartgerink et al., 2016](https://peerj.com/articles/1935/)).

## How does statcheck work?

The algorithm behind `statcheck` consists of four basic steps:

1. **Convert** pdf and html articles to plain text files.
2. **Search** the text for instances of NHST results. Specifically, `statcheck` can recognize *t*-tests, *F*-tests, correlations, *z*-tests, $\chi^2$-tests, and Q-tests (from meta-analyses) if they are reported completely (test statistic, degrees of freedom, and *p*-value) and in APA style.
3. **Recompute** the *p*-value using the reported test statistic and degrees of freedom.
4. **Compare** the reported and recomputed *p*-value. If the reported *p*-value does not match the computed one, the result is marked as an *inconsistency* (`Error` in the output). If the reported *p*-value is significant and the computed is not, or vice versa, the result is marked as a *gross inconsistency* (`DecisionError` in the output).

`statcheck` takes into account correct rounding of the test statistic, and has the option to take into account one-tailed testing. See the [manual](http://rpubs.com/michelenuijten/statcheckmanual) for details.

## Installation and use

For detailed information about installing and using `statcheck`, see the [manual on RPubs](http://rpubs.com/michelenuijten/statcheckmanual).

[statcheck.io](http://statcheck.io/) is a web-based interface for statcheck.

## Main changes in version 1.3.0 compared to 1.2.2

The latest statcheck version includes 2 cool new features:

* **Q-tests**: statcheck is now able to find Q-tests for heterogeneity (in meta-analyses). As always, the Q-tests need to be APA reported. statcheck recognizes general Q-tests, Q-within, and Q-between.
* **HTML reports**: it is now possible to generate nicely formatted HTML reports with statcheck results with the function `statcheckReport()`.


