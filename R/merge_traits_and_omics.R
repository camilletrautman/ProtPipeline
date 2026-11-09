#' Merge traits and omics data
#'
#' Merges a demographics data frame with an omics dataset, automatically
#' detecting whether subject IDs appear as the omics column names, row names,
#' or in a dedicated ID column. Useful for downstream analyses.
#'
#' @param omics A data.table (preferable) or data.frame containing omics data (each column corresponding to a sample and each row a feature, such as protein, transcript, etc.)
#' @param demographics A data.table (preferable) or data.frame containing demographics or traits data (e.g., confounders and exposure info, subject ID info)
#' @param subject_ids A character string identifying the column name associated with sample ids used to connect the correct omics sample to the demographics data. Defaults to `"guid"`.
#'
#' @return A data.table with one row per subject found in both datasets, containing the demographic variables followed by the omics measurements.
#'
#' @export
#'
#' @examples
#' # Simulate a small demographics table
#' demographics <- data.frame(
#'   guid = c("A1", "A2", "A3"),
#'   age = c(34, 51, 47),
#'   sex = c("F", "M", "F")
#' )
#'
#' # Case 1: subject IDs are the omics column names (features x subjects)
#' omics_cols <- matrix(
#'   rnorm(12), nrow = 4,
#'   dimnames = list(paste0("protein", 1:4), c("A1", "A2", "A3"))
#' )
#' merge_traits_and_omics(omics_cols, demographics)
#'
#' # Case 2: subject IDs are the omics row names (subjects x features)
#' omics_rows <- as.data.frame(t(omics_cols))
#' merge_traits_and_omics(omics_rows, demographics)
#'
#' # Case 3: subject IDs are in a column of the omics data
#' omics_idcol <- cbind(guid = rownames(omics_rows), omics_rows)
#' merge_traits_and_omics(omics_idcol, demographics)

merge_traits_and_omics <- function(
  omics,
  demographics,
  subject_ids = "guid"
) {
  if (!subject_ids %in% colnames(demographics)) {
    stop("Column '", subject_ids, "' not found in demographics.")
  }

  # detect the orientiation of the data ----
  real_ids <- demographics[[subject_ids]]

  ids_in_colnames <- sum(real_ids %in% colnames(omics))
  ids_in_rownames <- sum(real_ids %in% rownames(omics))
  ids_in_id_col <- if (subject_ids %in% colnames(omics)) {
    sum(real_ids %in% omics[[subject_ids]])
  } else {
    0
  }

  if (ids_in_colnames == 0 & ids_in_rownames == 0 & ids_in_id_col == 0) {
    stop(
      "No matching subject IDs from the demographic dataset were found in the omics dataset."
    )
  }

  if (subject_ids %in% colnames(omics)) {
    message(
      "Column '",
      subject_ids,
      "' was detected in omics dataset. Merging demographics and omics datasets..."
    )
    dat <- merge(
      data.table::as.data.table(demographics),
      data.table::as.data.table(omics),
      by = subject_ids
    )
  } else if (ids_in_colnames > ids_in_rownames) {
    message(
      "Subject IDs detected in omics dataset columns. Merging demographics and omics datasets..."
    )
    omics_t <- data.table::as.data.table(t(omics), keep.rownames = TRUE)
    colnames(omics_t)[1] <- subject_ids
    dat <- merge(
      data.table::as.data.table(demographics),
      data.table::as.data.table(omics_t),
      by = subject_ids
    )
  } else {
    message(
      "Subject IDs detected in omics dataset rows. Merging demographics and omics datasets..."
    )
    omics_df <- omics |>
      tibble::rownames_to_column(subject_ids)
    dat <- merge(
      data.table::as.data.table(demographics),
      data.table::as.data.table(omics_df),
      by = subject_ids
    )
  }

  message(
    "Merging complete."
  )
  return(dat)
}
