valid_titles <- function(rgs) {
  all_aes_in_mappings <- all_aesthetics(rgs)
  for (titled_aes in names(rgs$titles)) {
    if (!(titled_aes %in% all_aes_in_mappings)) {
      unformatted_msg <- paste(
        "Error: title provided for aesthetic not found",
        "in any layer's aesthetic mapping: %s."
      )
      errmsg <- sprintf(unformatted_msg, titled_aes)
      stop(errmsg)
    }
  }
}
