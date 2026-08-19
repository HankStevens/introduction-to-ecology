extract_cited_refs <- function(bib_files, qmd_pattern = "\\.qmd$", 
                               out = "local_refs.bib") {
  
  # Get all cited keys from .qmd files
  qmds <- list.files(".", pattern = qmd_pattern, full.names = TRUE)
  text <- unlist(lapply(qmds, readLines, warn = FALSE))
  
  # Extract keys - stop at ], ), ;, ,, ., space, or end of string
  raw_keys <- unique(trimws(unlist(regmatches(text, 
                                              gregexpr("(?<=@)[A-Za-z][A-Za-z0-9_:./-]+", text, perl=TRUE)))))
  
  # Strip trailing punctuation
  raw_keys <- gsub("[\\]\\)\\.;,]+$", "", raw_keys, perl=TRUE)
  
  # Remove Quarto cross-references (fig-, tbl-, eq-, sec-)
  raw_keys <- raw_keys[!grepl("^(fig|tbl|eq|sec|lst|thm|def|lem)-", raw_keys)]
  
  # Remove anything with a dot suggesting a URL or file path
  raw_keys <- raw_keys[!grepl("\\.(edu|com|org|png|pdf|jpg)", raw_keys)]
  
  raw_keys <- unique(raw_keys)
  cat("Citation keys found:", length(raw_keys), "\n")
  print(sort(raw_keys))
  
  # Extract matching entries from each bib file
  entries <- character()
  for (bib in bib_files) {
    raw <- readLines(bib, warn = FALSE)
    starts <- grep("^@", raw)
    ends <- c(starts[-1] - 1, length(raw))
    for (i in seq_along(starts)) {
      block <- raw[starts[i]:ends[i]]
      key <- regmatches(block[1], regexpr("(?<=\\{)[^,]+", block[1], perl=TRUE))
      if (length(key) > 0 && trimws(key) %in% raw_keys) {
        entries <- c(entries, block, "")
      }
    }
  }
  
  writeLines(entries, out)
  message("Wrote ", sum(grepl("^@", entries)), " entries to ", out)
}

extract_cited_refs(
  bib_files = c("/Users/stevenmh/tidy.bib", 
                "/Users/stevenmh/MyLibrary_zotero.bib"),
  out = "local_refs.bib"
)