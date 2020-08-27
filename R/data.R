#' @title Example vis-NIRS and reference dataset
#' @name ikeogu.2017
#' @description The `ikeogu.2017` data set contains raw vis-NIRS scans, total
#'   carotenoid content, and cassava root dry matter content (using the oven
#'   method) from the 2017 PLOS One paper by Ikeogu et al. This dataset contains
#'   a subset of the original scans and reference values from the supplementary
#'   files of the paper.
#' `ikeogu.2017` is a `data.frame` that contains the following columns:
#' \itemize{
#'   \item study.name = Name of the study as described in Ikeogu et al. (2017).
#'   \item sample.id = Unique identifier for each individual root sample
#'   \item DMC.oven = Cassava root dry matter content, the percentage of dry weight relative to fresh weight of a sample after oven drying.
#'   \item TCC = Total carotenoid content (\eqn{\mu g/g}, unknown whether on a fresh or dry weight basis) as measured by
#'   high performance liquid chromatography
#'   \item X350:X2500 = spectral reflectance measured with the QualitySpec Trek: S-10016 vis-NIR spectrometer.
#' Each cell represents the mean of 150 scans on a single root at a single wavelength.
#' }
#' @author Original authors: Ikeogu, U.N., F. Davrieux, D. Dufour, H. Ceballos,
#'   C.N. Egesi, and J. Jannink. Reformatted by Jenna Hershberger.
#' @references Ikeogu, U.N., F. Davrieux, D. Dufour, H. Ceballos, C.N. Egesi, et
#'   al. 2017. Rapid analyses of dry matter content and carotenoids in fresh
#'   cassava roots using a portable visible and near infrared spectrometer
#'   (Vis/NIRS). PLOS One 12(12): 1–17. doi: 10.1371/journal.pone.0188918.
#'
#' @examples
#' library(magrittr)
#' library(dplyr)
#' library(tidyr)
#' library(ggplot2)
#' data(ikeogu.2017)
#' ikeogu.2017[1:10,1:10]
#' ikeogu.2017 %>% dplyr::select(-starts_with("X")) %>% dplyr::group_by(study.name) %>%
#'     tidyr::gather(trait, value, c(DMC.oven:TCC), na.rm = TRUE) %>%
#'     ggplot2::ggplot(aes(x = study.name, y = value, fill = study.name)) +
#'         facet_wrap(~ trait, scales = 'free_y', nrow = 2) +
#'         geom_boxplot()

"ikeogu.2017"
