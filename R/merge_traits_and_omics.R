#' Merge traits and omics data for use with analysis functions (e.g., MLR.R)

#' @param omics A data.table (preferable) or data.frame containing omics data (each column corresponding to a sample and each row a feature, such as protein, transcript, etc.)
#' @param demographics A data.table (preferable) or data.frame containing demographics or traits data (e.g., confounders and exposure info)
#' @param subject_ids A character string naming the column name associated with sample ids -- typically GUIDs for the Huels lab -- used to connect the correct omics sample to the demographics data
#'
#' @examples
#'
#' @export

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
