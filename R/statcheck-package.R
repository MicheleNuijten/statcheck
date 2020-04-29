#' statcheck: Extract statistics from articles and recompute p-values
#' 
#' The package \code{statcheck} can extract Null Hypothesis Significance Test
#' (NHST) results from articles (or plain text) and recomputes p-values to check 
#' whether a reported NHST result is internally consistent or not. 
#' 
#' \code{statcheck} can be used for multiple purposes, including:
#' 
#' \itemize{
#'     \item \strong{Self-checks}: you can use statcheck to make sure your 
#'     manuscript doesn't contain copy-paste errors or other inconsistencies 
#'     before you submit it to a journal.
#'     \item \strong{Peer review}: editors and reviewers can use statcheck to 
#'     check submitted manuscripts for statistical inconsistencies. They can ask 
#'     authors for a correction or clarification before publishing a manuscript.
#'     \item \strong{Research}: statcheck can be used to automatically extract 
#'     statistical test results from articles that can then be analyzed. You can 
#'     for instance investigate whether you can predict statistical 
#'     inconsistencies (see e.g., 
#'     \href{https://www.collabra.org/article/10.1525/collabra.102/}{Nuijten et al., 2017}), 
#'     or use it to analyze p-value distributions (see e.g., 
#'     \href{https://peerj.com/articles/1935/}{Hartgerink et al., 2016}).
#' }
#' 
#' @section Using statcheck on a string of text:
#' The most basic usage of \code{statcheck} is to directly extract NHST results 
#' and check for inconsistencies in a string of text. See 
#' \code{\link{statcheck}} for details and an example of how to do this.
#' 
#' @section Using statcheck on an article:
#' Another option is to run \code{statcheck} on an article (PDF or HTML). This 
#' is a useful option if you want to check for inconsistencies in a single 
#' article (e.g., as a final check before you submit it). Depending on whether
#' you want to check an article in HTML or PDF, you can use 
#' \code{\link{checkHTML}} or \code{\link{checkPDF}}, respectively. Note: it is
#' recommended to check articles in HTML, as converting PDF files to plain text
#' sometimes results in some conversion errors.
#' 
#' @section Using statcheck on a folder of articles:
#' Finally, it is possible to run \code{statcheck} on an entire folder of 
#' articles. This is often useful for meta-research. To do so, you can use
#' \code{\link{checkPDFdir}} to check all PDF articles in a folder, 
#' \code{\link{checkHTMLdir}} to check all PDF articles in a folder, and 
#' \code{\link{checkdir}} to check both PDF and HTML articles in a folder.
#' 
#' @section Accuracy of the algorithm in detecting inconsistencies:
#' It is important to note that \code{statcheck} is not perfect. Its performance
#' in detecting NHST results depends on the type-setting and reporting style of 
#' an article and can vary widely. However, \code{statcheck} performs well in 
#' classifying the retrieved statistics in different consistency categories. We 
#' found that statcheck’s sensitivity (true positive rate) and specificity (true 
#' negative rate) were high: between 85.3% and 100%, and between 96.0% and 100%, 
#' respectively, depending on the assumptions and settings. The overall accuracy 
#' of statcheck ranged from 96.2% to 99.9%. More details on the validity study
#' can be found in \href{https://psyarxiv.com/tcxaj/}{Nuijten et al., 2017}.
#' 
#' @section Manual: 
#' Details on what statcheck can and cannot do, and how to install the package
#' and the necessary program Xpdf can be found in the 
#' \href{https://rpubs.com/michelenuijten/statcheckmanual}{online manual}.
#' 
#' @section Web app:
#' \code{statcheck} is also available as a free, online web app at 
#' \url{http://statcheck.io}. 
#' 
#' @references 
#' Hartgerink, C. H. J., Van Aert, R. C. M., Nuijten, M. B., Wicherts, J. M., 
#' Van Assen, M. A. L. M. (2016). Distributions of p-values smaller than .05 in 
#' psychology: What is going on? \emph{PeerJ}, \emph{4}, e1935. 
#' doi: 10.7717/peerj.1935
#' 
#' Nuijten, M. B., Van Assen, M. A. L. M., Hartgerink, C. H. J., Epskamp, S., & 
#' Wicherts, J. M. (2017). The validity of the tool "statcheck" in discovering 
#' statistical reporting inconsistencies. \emph{Preprint retrieved from 
#' https://psyarxiv.com/tcxaj/.}

#' @keywords internal
#' @aliases {statcheck}-package
"_PACKAGE"

#' @name statcheck


# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL
