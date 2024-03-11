# Script for running playOmics experiment
my_experiment_name <- "test_run"

# Create logger
log_path <- paste0(getwd(), my_experiment_name, "_logs.txt")
logger::log_appender(logger::appender_file(log_path))
logger::log_threshold(logger::DEBUG)
logger::log_info("Experiment '{my_experiment_name}' started")

# load playOmics
library(playOmics)

# Load the additional required libraries
library(tidyverse)
library(readxl)

clinical_data <-
  read_delim("TCGA-BRCA/Human__TCGA_BRCA__MS__Clinical__Clinical__01_28_2016__BI__Clinical__Firehose.tsi", na = c("NA", "NA,NA")) %>%
  data.table::transpose(keep.names = "ID", make.names = "attrib_name") %>%
  select(-"overallsurvival") %>%
  mutate_at(.vars = c("years_to_birth", "overall_survival", "number_of_lymph_nodes", "Tumor_purity"), as.numeric)


clinical_data <-
  clinical_data %>%
  filter(histological_type %in% c("infiltratingductalcarcinoma", "infiltratinglobularcarcinoma")) %>%
  # increase readability
  mutate(histological_type = case_when(
    histological_type == "infiltratinglobularcarcinoma" ~ "lobular",
    histological_type == "infiltratingductalcarcinoma" ~ "ductal"
  )) %>%
  select(ID, histological_type)

proteome <-
  read_delim("TCGA-BRCA/Human__TCGA_BRCA__MDA__RPPA__MDA_RPPA__01_28_2016__BI__Gene__Firehose_RPPA.cct", na = c("NA", "NA,NA"), show_col_types = F) %>%
  data.table::transpose(keep.names = "ID", make.names = "attrib_name") %>%
  mutate_at(vars(-ID), as.numeric)

methylation <-
  read_delim("TCGA-BRCA/Human__TCGA_BRCA__JHU_USC__Methylation__Meth450__01_28_2016__BI__Gene__Firehose_Methylation_Prepocessor.cct", na = c("NA", "NA,NA"), show_col_types = F) %>%
  data.table::transpose(keep.names = "ID", make.names = "attrib_name") %>%
  mutate_at(vars(-ID), as.numeric)

miRNA <-
  read_delim("TCGA-BRCA/Human__TCGA_BRCA__BDGSC__miRNASeq__HS_miR__01_28_2016__BI__Gene__Firehose_RPKM_log2.cct", na = c("NA", "NA,NA"), show_col_types = F) %>%
  data.table::transpose(keep.names = "ID", make.names = "attrib_name") %>%
  mutate_at(vars(-ID), as.numeric)

mutation <-
  read_delim("TCGA-BRCA/Human__TCGA_BRCA__WUSM__Mutation__GAIIx__01_28_2016__BI__Gene__Firehose_MutSig2CV.cbt", na = c("NA", "NA,NA"), show_col_types = F) %>%
  data.table::transpose(keep.names = "ID", make.names = "attrib_name") %>%
  mutate_at(vars(-ID), as.numeric)

RNAseq <-
  read_delim("TCGA-BRCA/Human__TCGA_BRCA__UNC__RNAseq__HiSeq_RNA__01_28_2016__BI__Gene__Firehose_RSEM_log2.cct", na = c("NA", "NA,NA"), show_col_types = F) %>%
  data.table::transpose(keep.names = "ID", make.names = "attrib_name") %>%
  mutate_at(vars(-ID), as.numeric)

SCNV_log_ratio <-
  read_delim("TCGA-BRCA/Human__TCGA_BRCA__BI__SCNA__SNP_6.0__01_28_2016__BI__Gene__Firehose_GISTIC2.cct", na = c("NA", "NA,NA"), show_col_types = F) %>%
  data.table::transpose(keep.names = "ID", make.names = "attrib_name") %>%
  mutate_at(vars(-ID), as.numeric)

BRCA_data <- connect_datasets(clinical_data, proteome, methylation, miRNA, mutation, RNAseq, SCNV_log_ratio,
                              remove_original_data = TRUE
)

my_target <- 
  define_target(
    phenotype_df_name = "clinical_data",
    id_variable_name = "ID",
    target_variable_name = "histological_type",
    positive_class_name = "lobular"
  )

BRCA_data_splitted <-
  split_data_into_train_test(BRCA_data, prop = 8 / 10, target = my_target)
rm(BRCA_data)

data_prepared <-
  prepare_data_for_modelling(
    data = BRCA_data_splitted$train_data, 
    target = my_target, 
    remove_correlated_features = F
  )

test_data_prepared <-
  prepare_data_for_modelling(
    BRCA_data_splitted$test_data, 
    target = my_target, 
    remove_correlated_features = F)

rm(BRCA_data_splitted)

data_filtered <-
  nested_filtering(
    data = data_prepared,
    target = my_target,
    filter_name = "auc",
    cutoff_method = "top_n",
    cutoff_treshold = 5,
    n_fold = 5,
    n_threads = 5
  )
rm(data_prepared)

my_models <-
  create_multiple_models(
    experiment_name = my_experiment_name,
    train_data = data_filtered,
    test_data = test_data_prepared,
    target = my_target,
    n_min = 2,
    n_max = 3,
    trim_models = TRUE,
    trim_metric = "train_mcc",
    trim_threshold = 0.3,
    # single model settings
    validation_method = "cv",
    n_prop = NULL,
    n_repeats = 5,
    log_experiment = TRUE,
    explain = TRUE,
    # configuration
    n_cores = 5,
    directory = here::here()
  )

logger::log_info("Experiment '{my_experiment_name}' ended")
gc()
logger::log_appender(logger::appender_console)
