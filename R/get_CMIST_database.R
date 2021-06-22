#' Get the CMIST data from the online portal
#'
#' @importFrom magrittr %>%
#' @return
#' @export
#'
#' @examples
#' CMISTdata <- get_CMIST_database()
get_CMIST_database <- function(){
  table <- 'https://www.bio.gc.ca/science/monitoring-monitorage/cmist/data-donnees-en.php?ASM_YEAR=-1&PRJ_ID=-1&COU_ID_1=-1&RG_ID=-1&ASA_STUDY_AREA=-1&MWT_ID=-1&BOW_ID=-1&ASA_SOUTHERN_LATITUDE=&ASA_NORTHERN_LATITUDE=&ASA_WESTERN_LONGITUDE=&ASA_EASTERN_LONGITUDE=&SPC_GENUS-SPC_SPECIES=-1&TAX_ID=-1&ASU_ADJ_RISK_SCORE=#results' %>%
    rvest::read_html()


    rvest::html_table(table)[[1]] %>%
    dplyr::mutate(Data=gsub("SummaryCMIST AssessmentMonth: ","",Data)) %>%
    tidyr::separate(Data,into=c("ASM_MONTH","Data"),sep = " Year: ") %>%
    tidyr::separate(Data,into=c("ASM_YEAR","Data"),sep = "Assessment AreaCountry: ") %>%
    tidyr::separate(Data,into=c("Country","Data"),sep = "DFO Region: ") %>%
    tidyr::separate(Data,into=c("Region","Data"),sep = "Study Area: ") %>%
    tidyr::separate(Data,into=c("ASA_STUDY_AREA","Data"),sep = "Water Type: ") %>%
    tidyr::separate(Data,into=c("Water_Type","Data"),sep = "Body of Water: ") %>%
    tidyr::separate(Data,into=c("Body_of_Water","Data"),sep = "Latitude Max: ") %>%
    tidyr::separate(Data,into=c("ASA_NORTHERN_LATITUDE","Data"),sep = "Latitude Min: ") %>%
    tidyr::separate(Data,into=c("ASA_SOUTHERN_LATITUDE","Data"),sep = "Longitude Min: ") %>%
    tidyr::separate(Data,into=c("ASA_WESTERN_LONGITUDE","Data"),sep = "Longitude Max: ") %>%
    tidyr::separate(Data,into=c("ASA_EASTERN_LONGITUDE","Data"),sep = "Assessment SpeciesAphiaID: ") %>%
    tidyr::separate(Data,into=c("SPC_WORMS_APHIAID","Data"),sep = "Taxonomic Serial No.: ") %>%
    tidyr::separate(Data,into=c("SPC_ITIS_TSN","Data"),sep = "Taxonomic Group: ") %>%
    tidyr::separate(Data,into=c("Taxonomic_Group","Data"),sep = "Common name: ") %>%
    tidyr::separate(Data,into=c("SPC_COMMON_1","Data"),sep = "Common name 2: ") %>%
    tidyr::separate(Data,into=c("SPC_COMMON_2","Data"),sep = "Common name 3: ") %>%
    tidyr::separate(Data,into=c("SPC_COMMON_3","Data"),sep = "Kingdom: ") %>%
    tidyr::separate(Data,into=c("SPC_KINGDOM","Data"),sep = "Phylum: ") %>%
    tidyr::separate(Data,into=c("SPC_PHYLUM","Data"),sep = "Subphylum: ") %>%
    tidyr::separate(Data,into=c("SPC_SUBPHYLUM","Data"),sep = "Class: ") %>%
    tidyr::separate(Data,into=c("SPC_CLASS","Data"),sep = "Order: ") %>%
    tidyr::separate(Data,into=c("SPC_ORDER","Data"),sep = "Family: ") %>%
    tidyr::separate(Data,into=c("SPC_FAMILY","Data"),sep = "Genus: ") %>%
    tidyr::separate(Data,into=c("SPC_GENUS","Data"),sep = "Species: ") %>%
    tidyr::separate(Data,into=c("SPC_SPECIES","Data"),sep = "Taxon Notes: ") %>%
    tidyr::separate(Data,into=c("SPC_TAXONOMIC_NOTES","Data"),sep = "Risk ScoreLikelihood of invasion: ") %>%
    tidyr::separate(Data,into=c("ASU_RAW_LIKELIHOOD_INVASION","Data"),sep = "Impact of invasion: ") %>%
    tidyr::separate(Data,into=c("ASU_RAW_IMPACT_INVASION","Data"),sep = "Mean risk score: ") %>%
    tidyr::separate(Data,into=c("ASU_RAW_MEAN_RISK_SCORE","Data"),sep = "Adjusted risk score: ") %>%
    tidyr::separate(Data,into=c("ASU_ADJ_RISK_SCORE","Data"),sep = "Lower confidence limit: ") %>%
    tidyr::separate(Data,into=c("ASU_ADJ_LOW_CONFIDENCE_LIMIT","Data"),sep = "Upper confidence limit: ") %>%
    tidyr::separate(Data,into=c("ASU_ADJ_UPPER_CONFIDENCE_LIMIT","Data"),sep = "ResponsesQ1") %>%
    tidyr::separate(Data,into=paste0("QST_QUESTION.",1:17),sep="Q") %>%

    tidyr::separate(QST_QUESTION.1,into=c("QST_QUESTION.1","QRS_RISK_SCORE.1"),sep = "Risk Score: ") %>%
    tidyr::separate(QRS_RISK_SCORE.1,into=c("QRS_RISK_SCORE.1","UNC_UNCERTAINTY_SCORE.1"),sep = "Uncertainty score: ") %>%
    tidyr::separate(QRS_RISK_SCORE.1,into=c("QRS_RISK_SCORE.1","QRS_DESCRIPTION.1"),sep = " - ")  %>%
    tidyr::separate(UNC_UNCERTAINTY_SCORE.1,into=c("UNC_UNCERTAINTY_SCORE.1","UNC_UNCERTAINTY.1"),sep = " - ") %>%

    tidyr::separate(QST_QUESTION.2,into=c("QST_QUESTION.2","QRS_RISK_SCORE.2"),sep = "Risk Score: ") %>%
    tidyr::separate(QRS_RISK_SCORE.2,into=c("QRS_RISK_SCORE.2","UNC_UNCERTAINTY_SCORE.2"),sep = "Uncertainty score: ") %>%
    tidyr::separate(QRS_RISK_SCORE.2,into=c("QRS_RISK_SCORE.2","QRS_DESCRIPTION.2"),sep = " - ")  %>%
    tidyr::separate(UNC_UNCERTAINTY_SCORE.2,into=c("UNC_UNCERTAINTY_SCORE.2","UNC_UNCERTAINTY.2"),sep = " - ") %>%

    tidyr::separate(QST_QUESTION.3,into=c("QST_QUESTION.3","QRS_RISK_SCORE.3"),sep = "Risk Score: ") %>%
    tidyr::separate(QRS_RISK_SCORE.3,into=c("QRS_RISK_SCORE.3","UNC_UNCERTAINTY_SCORE.3"),sep = "Uncertainty score: ") %>%
    tidyr::separate(QRS_RISK_SCORE.3,into=c("QRS_RISK_SCORE.3","QRS_DESCRIPTION.3"),sep = " - ")  %>%
    tidyr::separate(UNC_UNCERTAINTY_SCORE.3,into=c("UNC_UNCERTAINTY_SCORE.3","UNC_UNCERTAINTY.3"),sep = " - ") %>%

    tidyr::separate(QST_QUESTION.4,into=c("QST_QUESTION.4","QRS_RISK_SCORE.4"),sep = "Risk Score: ") %>%
    tidyr::separate(QRS_RISK_SCORE.4,into=c("QRS_RISK_SCORE.4","UNC_UNCERTAINTY_SCORE.4"),sep = "Uncertainty score: ") %>%
    tidyr::separate(QRS_RISK_SCORE.4,into=c("QRS_RISK_SCORE.4","QRS_DESCRIPTION.4"),sep = " - ")  %>%
    tidyr::separate(UNC_UNCERTAINTY_SCORE.4,into=c("UNC_UNCERTAINTY_SCORE.4","UNC_UNCERTAINTY.4"),sep = " - ") %>%

    tidyr::separate(QST_QUESTION.5,into=c("QST_QUESTION.5","QRS_RISK_SCORE.5"),sep = "Risk Score: ") %>%
    tidyr::separate(QRS_RISK_SCORE.5,into=c("QRS_RISK_SCORE.5","UNC_UNCERTAINTY_SCORE.5"),sep = "Uncertainty score: ") %>%
    tidyr::separate(QRS_RISK_SCORE.5,into=c("QRS_RISK_SCORE.5","QRS_DESCRIPTION.5"),sep = " - ")  %>%
    tidyr::separate(UNC_UNCERTAINTY_SCORE.5,into=c("UNC_UNCERTAINTY_SCORE.5","UNC_UNCERTAINTY.5"),sep = " - ") %>%

    tidyr::separate(QST_QUESTION.6,into=c("QST_QUESTION.6","QRS_RISK_SCORE.6"),sep = "Risk Score: ") %>%
    tidyr::separate(QRS_RISK_SCORE.6,into=c("QRS_RISK_SCORE.6","UNC_UNCERTAINTY_SCORE.6"),sep = "Uncertainty score: ") %>%
    tidyr::separate(QRS_RISK_SCORE.6,into=c("QRS_RISK_SCORE.6","QRS_DESCRIPTION.6"),sep = " - ")  %>%
    tidyr::separate(UNC_UNCERTAINTY_SCORE.6,into=c("UNC_UNCERTAINTY_SCORE.6","UNC_UNCERTAINTY.6"),sep = " - ") %>%

    tidyr::separate(QST_QUESTION.7,into=c("QST_QUESTION.7","QRS_RISK_SCORE.7"),sep = "Risk Score: ") %>%
    tidyr::separate(QRS_RISK_SCORE.7,into=c("QRS_RISK_SCORE.7","UNC_UNCERTAINTY_SCORE.7"),sep = "Uncertainty score: ") %>%
    tidyr::separate(QRS_RISK_SCORE.7,into=c("QRS_RISK_SCORE.7","QRS_DESCRIPTION.7"),sep = " - ")  %>%
    tidyr::separate(UNC_UNCERTAINTY_SCORE.7,into=c("UNC_UNCERTAINTY_SCORE.7","UNC_UNCERTAINTY.7"),sep = " - ") %>%

    tidyr::separate(QST_QUESTION.8,into=c("QST_QUESTION.8","QRS_RISK_SCORE.8"),sep = "Risk Score: ") %>%
    tidyr::separate(QRS_RISK_SCORE.8,into=c("QRS_RISK_SCORE.8","UNC_UNCERTAINTY_SCORE.8"),sep = "Uncertainty score: ") %>%
    tidyr::separate(QRS_RISK_SCORE.8,into=c("QRS_RISK_SCORE.8","QRS_DESCRIPTION.8"),sep = " - ")  %>%
    tidyr::separate(UNC_UNCERTAINTY_SCORE.8,into=c("UNC_UNCERTAINTY_SCORE.8","UNC_UNCERTAINTY.8"),sep = " - ") %>%

    tidyr::separate(QST_QUESTION.9,into=c("QST_QUESTION.9","QRS_RISK_SCORE.9"),sep = "Risk Score: ") %>%
    tidyr::separate(QRS_RISK_SCORE.9,into=c("QRS_RISK_SCORE.9","UNC_UNCERTAINTY_SCORE.9"),sep = "Uncertainty score: ") %>%
    tidyr::separate(QRS_RISK_SCORE.9,into=c("QRS_RISK_SCORE.9","QRS_DESCRIPTION.9"),sep = " - ")  %>%
    tidyr::separate(UNC_UNCERTAINTY_SCORE.9,into=c("UNC_UNCERTAINTY_SCORE.9","UNC_UNCERTAINTY.9"),sep = " - ") %>%

    tidyr::separate(QST_QUESTION.10,into=c("QST_QUESTION.10","QRS_RISK_SCORE.10"),sep = "Risk Score: ") %>%
    tidyr::separate(QRS_RISK_SCORE.10,into=c("QRS_RISK_SCORE.10","UNC_UNCERTAINTY_SCORE.10"),sep = "Uncertainty score: ") %>%
    tidyr::separate(QRS_RISK_SCORE.10,into=c("QRS_RISK_SCORE.10","QRS_DESCRIPTION.10"),sep = " - ")  %>%
    tidyr::separate(UNC_UNCERTAINTY_SCORE.10,into=c("UNC_UNCERTAINTY_SCORE.10","UNC_UNCERTAINTY.10"),sep = " - ") %>%

    tidyr::separate(QST_QUESTION.11,into=c("QST_QUESTION.11","QRS_RISK_SCORE.11"),sep = "Risk Score: ") %>%
    tidyr::separate(QRS_RISK_SCORE.11,into=c("QRS_RISK_SCORE.11","UNC_UNCERTAINTY_SCORE.11"),sep = "Uncertainty score: ") %>%
    tidyr::separate(QRS_RISK_SCORE.11,into=c("QRS_RISK_SCORE.11","QRS_DESCRIPTION.11"),sep = " - ")  %>%
    tidyr::separate(UNC_UNCERTAINTY_SCORE.11,into=c("UNC_UNCERTAINTY_SCORE.11","UNC_UNCERTAINTY.11"),sep = " - ") %>%

    tidyr::separate(QST_QUESTION.12,into=c("QST_QUESTION.12","QRS_RISK_SCORE.12"),sep = "Risk Score: ") %>%
    tidyr::separate(QRS_RISK_SCORE.12,into=c("QRS_RISK_SCORE.12","UNC_UNCERTAINTY_SCORE.12"),sep = "Uncertainty score: ") %>%
    tidyr::separate(QRS_RISK_SCORE.12,into=c("QRS_RISK_SCORE.12","QRS_DESCRIPTION.12"),sep = " - ")  %>%
    tidyr::separate(UNC_UNCERTAINTY_SCORE.12,into=c("UNC_UNCERTAINTY_SCORE.12","UNC_UNCERTAINTY.12"),sep = " - ") %>%

    tidyr::separate(QST_QUESTION.13,into=c("QST_QUESTION.13","QRS_RISK_SCORE.13"),sep = "Risk Score: ") %>%
    tidyr::separate(QRS_RISK_SCORE.13,into=c("QRS_RISK_SCORE.13","UNC_UNCERTAINTY_SCORE.13"),sep = "Uncertainty score: ") %>%
    tidyr::separate(QRS_RISK_SCORE.13,into=c("QRS_RISK_SCORE.13","QRS_DESCRIPTION.13"),sep = " - ")  %>%
    tidyr::separate(UNC_UNCERTAINTY_SCORE.13,into=c("UNC_UNCERTAINTY_SCORE.13","UNC_UNCERTAINTY.13"),sep = " - ") %>%

    tidyr::separate(QST_QUESTION.14,into=c("QST_QUESTION.14","QRS_RISK_SCORE.14"),sep = "Risk Score: ") %>%
    tidyr::separate(QRS_RISK_SCORE.14,into=c("QRS_RISK_SCORE.14","UNC_UNCERTAINTY_SCORE.14"),sep = "Uncertainty score: ") %>%
    tidyr::separate(QRS_RISK_SCORE.14,into=c("QRS_RISK_SCORE.14","QRS_DESCRIPTION.14"),sep = " - ")  %>%
    tidyr::separate(UNC_UNCERTAINTY_SCORE.14,into=c("UNC_UNCERTAINTY_SCORE.14","UNC_UNCERTAINTY.14"),sep = " - ") %>%

    tidyr::separate(QST_QUESTION.15,into=c("QST_QUESTION.15","QRS_RISK_SCORE.15"),sep = "Risk Score: ") %>%
    tidyr::separate(QRS_RISK_SCORE.15,into=c("QRS_RISK_SCORE.15","UNC_UNCERTAINTY_SCORE.15"),sep = "Uncertainty score: ") %>%
    tidyr::separate(QRS_RISK_SCORE.15,into=c("QRS_RISK_SCORE.15","QRS_DESCRIPTION.15"),sep = " - ")  %>%
    tidyr::separate(UNC_UNCERTAINTY_SCORE.15,into=c("UNC_UNCERTAINTY_SCORE.15","UNC_UNCERTAINTY.15"),sep = " - ") %>%

    tidyr::separate(QST_QUESTION.16,into=c("QST_QUESTION.16","QRS_RISK_SCORE.16"),sep = "Risk Score: ") %>%
    tidyr::separate(QRS_RISK_SCORE.16,into=c("QRS_RISK_SCORE.16","UNC_UNCERTAINTY_SCORE.16"),sep = "Uncertainty score: ") %>%
    tidyr::separate(QRS_RISK_SCORE.16,into=c("QRS_RISK_SCORE.16","QRS_DESCRIPTION.16"),sep = " - ")  %>%
    tidyr::separate(UNC_UNCERTAINTY_SCORE.16,into=c("UNC_UNCERTAINTY_SCORE.16","UNC_UNCERTAINTY.16"),sep = " - ") %>%

    tidyr::separate(QST_QUESTION.17,into=c("QST_QUESTION.17","QRS_RISK_SCORE.17"),sep = "Risk Score: ") %>%
    tidyr::separate(QRS_RISK_SCORE.17,into=c("QRS_RISK_SCORE.17","UNC_UNCERTAINTY_SCORE.17"),sep = "Uncertainty score: ") %>%
    tidyr::separate(QRS_RISK_SCORE.17,into=c("QRS_RISK_SCORE.17","QRS_DESCRIPTION.17"),sep = " - ")  %>%
    tidyr::separate(UNC_UNCERTAINTY_SCORE.17,into=c("UNC_UNCERTAINTY_SCORE.17","UNC_UNCERTAINTY.17"),sep = " - ")
}


