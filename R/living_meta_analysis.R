#' Living Systematic Review and Meta-Analysis System
#'
#' Automated system for maintaining living systematic reviews with:
#' - Automated literature monitoring (PubMed, Crossref, OpenAlex)
#' - Automatic study screening via AI
#' - Automated data extraction
#' - Continuous meta-analysis updates
#' - Change detection and alerts
#' - Version control and provenance tracking
#'
#' @name living_meta_analysis
NULL

#' Initialize Living Meta-Analysis
#'
#' Sets up a living systematic review project with automated monitoring
#' and updating capabilities.
#'
#' @param project_name Name for the living review project
#' @param search_query Search query for literature databases
#' @param databases Databases to monitor (default: c("pubmed", "crossref"))
#' @param update_frequency Update frequency: "daily", "weekly", "monthly"
#' @param screening_threshold AI screening confidence threshold (0-1)
#' @param output_dir Directory for project files
#'
#' @return Living meta-analysis project object
#'
#' @export
#' @examples
#' \dontrun{
#' # Initialize living review
#' living_ma <- init_living_meta_analysis(
#'   project_name = "Aspirin_CVD_Prevention",
#'   search_query = "aspirin AND cardiovascular AND prevention",
#'   databases = c("pubmed", "crossref"),
#'   update_frequency = "weekly"
#' )
#'
#' # Run initial search and screening
#' living_ma <- update_living_review(living_ma)
#'
#' # Check for updates (can be automated via cron/scheduler)
#' living_ma <- check_for_updates(living_ma)
#' }
init_living_meta_analysis <- function(project_name,
                                     search_query,
                                     databases = c("pubmed", "crossref"),
                                     update_frequency = "weekly",
                                     screening_threshold = 0.7,
                                     output_dir = file.path(getwd(), project_name)) {

  cat("╔══════════════════════════════════════════════════════════════╗\n")
  cat("║   LIVING META-ANALYSIS INITIALIZATION                       ║\n")
  cat("╚══════════════════════════════════════════════════════════════╝\n\n")

  cat(sprintf("Project: %s\n", project_name))
  cat(sprintf("Search Query: %s\n", search_query))
  cat(sprintf("Databases: %s\n", paste(databases, collapse = ", ")))
  cat(sprintf("Update Frequency: %s\n", update_frequency))
  cat(sprintf("Output Directory: %s\n\n", output_dir))

  # Create project directory structure
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  dirs <- c("searches", "screening", "extracted_data", "meta_analyses",
           "reports", "logs", "versions")

  for (d in dirs) {
    dir_path <- file.path(output_dir, d)
    if (!dir.exists(dir_path)) {
      dir.create(dir_path)
    }
  }

  cat("✓ Project directory structure created\n")

  # Initialize project metadata
  project <- list(
    metadata = list(
      project_name = project_name,
      created = Sys.time(),
      last_updated = Sys.time(),
      version = 1,
      search_query = search_query,
      databases = databases,
      update_frequency = update_frequency,
      screening_threshold = screening_threshold
    ),
    directories = list(
      root = output_dir,
      searches = file.path(output_dir, "searches"),
      screening = file.path(output_dir, "screening"),
      extracted_data = file.path(output_dir, "extracted_data"),
      meta_analyses = file.path(output_dir, "meta_analyses"),
      reports = file.path(output_dir, "reports"),
      logs = file.path(output_dir, "logs"),
      versions = file.path(output_dir, "versions")
    ),
    included_studies = data.frame(),
    excluded_studies = data.frame(),
    pending_screening = data.frame(),
    current_meta_analysis = NULL,
    update_history = list()
  )

  class(project) <- c("living_meta_analysis", "list")

  # Save project
  save_living_project(project)

  cat("✓ Living meta-analysis project initialized!\n\n")
  cat("Next steps:\n")
  cat("  1. Run update_living_review() to perform initial search\n")
  cat("  2. Review screening results\n")
  cat("  3. Extract data from included studies\n")
  cat("  4. Run meta-analysis\n")
  cat("  5. Schedule regular updates\n\n")

  return(project)
}

#' Update Living Review
#'
#' Searches literature databases for new studies, screens them, and updates
#' the meta-analysis if new includable studies are found.
#'
#' @param project Living meta-analysis project object
#' @param force_search Force new search even if recently updated
#'
#' @return Updated project object
#'
#' @export
update_living_review <- function(project, force_search = FALSE) {

  cat("\n╔══════════════════════════════════════════════════════════════╗\n")
  cat("║   LIVING REVIEW UPDATE                                       ║\n")
  cat("╚══════════════════════════════════════════════════════════════╝\n\n")

  update_start <- Sys.time()

  # Check if update needed
  if (!force_search) {
    time_since_update <- difftime(Sys.time(), project$metadata$last_updated, units = "hours")

    update_interval <- switch(project$metadata$update_frequency,
      "daily" = 24,
      "weekly" = 168,
      "monthly" = 720,
      168  # default to weekly
    )

    if (time_since_update < update_interval) {
      cat(sprintf("⏰ Last update was %.1f hours ago.\n", as.numeric(time_since_update)))
      cat(sprintf("   Next scheduled update in %.1f hours.\n",
                 update_interval - as.numeric(time_since_update)))
      cat("   Use force_search=TRUE to update anyway.\n\n")
      return(project)
    }
  }

  # STEP 1: Search databases
  cat("STEP 1: Searching databases...\n")
  new_citations <- search_databases(
    query = project$metadata$search_query,
    databases = project$metadata$databases,
    last_search_date = project$metadata$last_updated
  )

  cat(sprintf("✓ Found %d new citations\n", nrow(new_citations)))

  if (nrow(new_citations) == 0) {
    cat("  No new studies found. Meta-analysis unchanged.\n")
    project$metadata$last_updated <- Sys.time()
    save_living_project(project)
    return(project)
  }

  # Remove duplicates (already screened)
  all_screened <- rbind(
    project$included_studies[, c("title", "doi"), drop = FALSE],
    project$excluded_studies[, c("title", "doi"), drop = FALSE]
  )

  new_citations$is_duplicate <- new_citations$doi %in% all_screened$doi |
                               new_citations$title %in% all_screened$title

  truly_new <- new_citations[!new_citations$is_duplicate, ]

  cat(sprintf("  After deduplication: %d truly new citations\n\n", nrow(truly_new)))

  if (nrow(truly_new) == 0) {
    project$metadata$last_updated <- Sys.time()
    save_living_project(project)
    return(project)
  }

  # STEP 2: AI-powered screening
  cat("STEP 2: AI-powered screening...\n")
  screening_results <- ai_screen_studies(
    citations = truly_new,
    inclusion_criteria = project$metadata$inclusion_criteria,
    threshold = project$metadata$screening_threshold
  )

  n_include <- sum(screening_results$decision == "INCLUDE")
  n_exclude <- sum(screening_results$decision == "EXCLUDE")
  n_uncertain <- sum(screening_results$decision == "UNCERTAIN")

  cat(sprintf("✓ Screening complete:\n"))
  cat(sprintf("  Include: %d\n", n_include))
  cat(sprintf("  Exclude: %d\n", n_exclude))
  cat(sprintf("  Uncertain (needs manual review): %d\n\n", n_uncertain))

  # Add to appropriate lists
  project$included_studies <- rbind(
    project$included_studies,
    screening_results[screening_results$decision == "INCLUDE", ]
  )

  project$excluded_studies <- rbind(
    project$excluded_studies,
    screening_results[screening_results$decision == "EXCLUDE", ]
  )

  project$pending_screening <- rbind(
    project$pending_screening,
    screening_results[screening_results$decision == "UNCERTAIN", ]
  )

  # STEP 3: Automated data extraction (for included studies)
  if (n_include > 0) {
    cat("STEP 3: Automated data extraction...\n")

    new_data <- ai_extract_data(
      studies = screening_results[screening_results$decision == "INCLUDE", ],
      extraction_template = project$metadata$extraction_template
    )

    cat(sprintf("✓ Extracted data from %d new studies\n\n", n_include))

    # Combine with existing data
    if (!is.null(project$extracted_data)) {
      project$extracted_data <- rbind(project$extracted_data, new_data)
    } else {
      project$extracted_data <- new_data
    }
  }

  # STEP 4: Update meta-analysis
  if (n_include > 0 && nrow(project$extracted_data) >= 3) {
    cat("STEP 4: Updating meta-analysis...\n")

    previous_result <- project$current_meta_analysis

    new_result <- cbamm_fast(project$extracted_data, verbose = FALSE)

    # Detect meaningful changes
    change_detected <- FALSE
    change_summary <- list()

    if (!is.null(previous_result)) {
      # Effect size change
      effect_change <- abs(new_result$estimate - previous_result$estimate)
      change_summary$effect_change <- effect_change

      # CI change
      prev_ci_width <- previous_result$ci_upper - previous_result$ci_lower
      new_ci_width <- new_result$ci_upper - new_result$ci_lower
      ci_precision_gain <- (prev_ci_width - new_ci_width) / prev_ci_width
      change_summary$ci_precision_gain <- ci_precision_gain

      # Significance change
      prev_sig <- previous_result$p_value < 0.05
      new_sig <- new_result$p_value < 0.05
      significance_changed <- prev_sig != new_sig
      change_summary$significance_changed <- significance_changed

      # Heterogeneity change
      I2_change <- abs(new_result$I2 - previous_result$I2)
      change_summary$I2_change <- I2_change

      # Determine if changes are meaningful
      if (effect_change > 0.1 || ci_precision_gain > 0.1 ||
          significance_changed || I2_change > 10) {
        change_detected <- TRUE
      }
    } else {
      change_detected <- TRUE  # First meta-analysis
      change_summary$first_analysis <- TRUE
    }

    project$current_meta_analysis <- new_result

    cat("✓ Meta-analysis updated\n")
    cat(sprintf("  Pooled Effect: %.3f (95%% CI: %.3f to %.3f)\n",
               new_result$estimate, new_result$ci_lower, new_result$ci_upper))
    cat(sprintf("  I² = %.1f%%, p %s\n",
               new_result$I2,
               if(new_result$p_value < 0.001) "< 0.001" else sprintf("= %.3f", new_result$p_value)))

    if (change_detected) {
      cat("\n⚠️  MEANINGFUL CHANGE DETECTED!\n")
      if (!is.null(previous_result)) {
        cat(sprintf("  Effect size change: %.3f → %.3f (Δ = %.3f)\n",
                   previous_result$estimate, new_result$estimate,
                   change_summary$effect_change))

        if (change_summary$significance_changed) {
          cat("  ⚠️  Statistical significance changed!\n")
        }

        if (change_summary$ci_precision_gain > 0.1) {
          cat(sprintf("  Precision improved by %.1f%%\n",
                     100 * change_summary$ci_precision_gain))
        }
      }

      # Generate alert/report
      generate_update_alert(project, change_summary)
    }

    cat("\n")
  }

  # STEP 5: Version control
  project$metadata$version <- project$metadata$version + 1
  project$metadata$last_updated <- Sys.time()

  # Save version snapshot
  save_version_snapshot(project)

  # Add to update history
  update_record <- list(
    timestamp = Sys.time(),
    version = project$metadata$version,
    new_citations = nrow(new_citations),
    new_includes = n_include,
    new_excludes = n_exclude,
    pending_review = n_uncertain,
    change_detected = change_detected,
    duration = difftime(Sys.time(), update_start, units = "secs")
  )

  project$update_history[[length(project$update_history) + 1]] <- update_record

  # Save project
  save_living_project(project)

  cat("╔══════════════════════════════════════════════════════════════╗\n")
  cat("║   UPDATE COMPLETE                                            ║\n")
  cat("╚══════════════════════════════════════════════════════════════╝\n\n")

  cat(sprintf("Version: %d\n", project$metadata$version))
  cat(sprintf("Duration: %.1f seconds\n", as.numeric(update_record$duration)))
  cat(sprintf("Total included studies: %d\n", nrow(project$included_studies)))
  cat(sprintf("Total excluded studies: %d\n", nrow(project$excluded_studies)))
  cat(sprintf("Pending manual review: %d\n\n", nrow(project$pending_screening)))

  return(project)
}

#' Generate Living Review Report
#'
#' Generates a comprehensive report for the current state of the living review.
#'
#' @param project Living meta-analysis project
#' @param format Report format: "html", "pdf", "word"
#'
#' @return Path to generated report
#'
#' @export
generate_living_report <- function(project, format = "html") {

  cat("Generating living review report...\n")

  # Create report content
  report <- list()

  # Summary section
  report$summary <- sprintf("
# Living Systematic Review Report

**Project**: %s
**Version**: %d
**Last Updated**: %s
**Total Studies**: %d

## Current Meta-Analysis Results

- **Pooled Effect**: %.3f (95%% CI: %.3f to %.3f)
- **P-value**: %s
- **Heterogeneity (I²)**: %.1f%%
- **Between-study variance (τ²)**: %.3f

## Study Flow

- **Total Citations Screened**: %d
- **Included**: %d
- **Excluded**: %d
- **Pending Review**: %d

  ",
    project$metadata$project_name,
    project$metadata$version,
    format(project$metadata$last_updated, "%Y-%m-%d %H:%M"),
    nrow(project$included_studies),
    project$current_meta_analysis$estimate,
    project$current_meta_analysis$ci_lower,
    project$current_meta_analysis$ci_upper,
    if(project$current_meta_analysis$p_value < 0.001) "< 0.001" else
      sprintf("%.4f", project$current_meta_analysis$p_value),
    project$current_meta_analysis$I2,
    project$current_meta_analysis$tau2,
    nrow(project$included_studies) + nrow(project$excluded_studies) + nrow(project$pending_screening),
    nrow(project$included_studies),
    nrow(project$excluded_studies),
    nrow(project$pending_screening)
  )

  # Update history
  report$history <- "\n## Update History\n\n"
  for (i in seq_along(project$update_history)) {
    update <- project$update_history[[i]]
    report$history <- paste0(report$history, sprintf(
      "**Version %d** (%s): %d new citations, %d included, %d excluded%s\n\n",
      update$version,
      format(update$timestamp, "%Y-%m-%d"),
      update$new_citations,
      update$new_includes,
      update$new_excludes,
      if(update$change_detected) " - **Meaningful change detected**" else ""
    ))
  }

  # Combine
  report_text <- paste(report, collapse = "\n")

  # Save report
  report_file <- file.path(
    project$directories$reports,
    sprintf("living_review_report_v%d.%s",
           project$metadata$version,
           if(format == "html") "html" else if(format == "pdf") "pdf" else "docx")
  )

  if (format == "html") {
    html_content <- markdown::markdownToHTML(text = report_text, fragment.only = FALSE)
    writeLines(html_content, report_file)
  } else {
    writeLines(report_text, report_file)
  }

  cat(sprintf("✓ Report generated: %s\n", report_file))

  return(report_file)
}

#==============================================================================
# HELPER FUNCTIONS
#==============================================================================

#' @keywords internal
search_databases <- function(query, databases, last_search_date = NULL) {
  all_results <- data.frame()

  for (db in databases) {
    cat(sprintf("  Searching %s...\n", db))

    results <- switch(db,
      "pubmed" = search_pubmed(query, last_search_date),
      "crossref" = search_crossref(query, last_search_date),
      "openalex" = search_openalex(query, last_search_date),
      data.frame()  # Unknown database
    )

    if (nrow(results) > 0) {
      results$database <- db
      all_results <- rbind(all_results, results)
    }

    cat(sprintf("    Found %d results\n", nrow(results)))
  }

  return(all_results)
}

#' @keywords internal
search_pubmed <- function(query, since_date = NULL) {
  if (!requireNamespace("rentrez", quietly = TRUE)) {
    warning("rentrez package not available. Install with: install.packages('rentrez')")
    return(data.frame(
      title = character(),
      abstract = character(),
      authors = character(),
      year = integer(),
      doi = character(),
      pmid = character(),
      stringsAsFactors = FALSE
    ))
  }

  # Build date filter if provided
  date_filter <- ""
  if (!is.null(since_date)) {
    date_str <- format(since_date, "%Y/%m/%d")
    date_filter <- paste0(" AND ", date_str, "[PDAT]:3000[PDAT]")
  }

  full_query <- paste0(query, date_filter)

  tryCatch({
    # Search PubMed
    search_result <- rentrez::entrez_search(
      db = "pubmed",
      term = full_query,
      retmax = 1000,
      use_history = TRUE
    )

    if (search_result$count == 0) {
      return(data.frame(
        title = character(),
        abstract = character(),
        authors = character(),
        year = integer(),
        doi = character(),
        pmid = character(),
        stringsAsFactors = FALSE
      ))
    }

    # Fetch details in batches
    results <- data.frame()
    batch_size <- 100

    for (start in seq(1, min(search_result$count, 1000), by = batch_size)) {
      fetch_result <- rentrez::entrez_fetch(
        db = "pubmed",
        web_history = search_result$web_history,
        rettype = "xml",
        retmax = batch_size,
        retstart = start - 1
      )

      # Parse XML
      parsed <- parse_pubmed_xml(fetch_result)
      results <- rbind(results, parsed)

      Sys.sleep(0.5)  # Rate limiting
    }

    return(results)

  }, error = function(e) {
    warning(sprintf("PubMed search error: %s", e$message))
    return(data.frame(
      title = character(),
      abstract = character(),
      authors = character(),
      year = integer(),
      doi = character(),
      pmid = character(),
      stringsAsFactors = FALSE
    ))
  })
}

#' @keywords internal
parse_pubmed_xml <- function(xml_string) {
  if (!requireNamespace("xml2", quietly = TRUE)) {
    return(data.frame())
  }

  tryCatch({
    xml_doc <- xml2::read_xml(xml_string)
    articles <- xml2::xml_find_all(xml_doc, ".//PubmedArticle")

    results <- lapply(articles, function(article) {
      # Extract title
      title <- xml2::xml_text(xml2::xml_find_first(article, ".//ArticleTitle"))

      # Extract abstract
      abstract_nodes <- xml2::xml_find_all(article, ".//AbstractText")
      abstract <- paste(xml2::xml_text(abstract_nodes), collapse = " ")

      # Extract authors
      author_nodes <- xml2::xml_find_all(article, ".//Author")
      authors <- sapply(author_nodes, function(auth) {
        lastname <- xml2::xml_text(xml2::xml_find_first(auth, ".//LastName"))
        firstname <- xml2::xml_text(xml2::xml_find_first(auth, ".//ForeName"))
        paste(firstname, lastname)
      })
      authors <- paste(authors, collapse = "; ")

      # Extract year
      year_node <- xml2::xml_find_first(article, ".//PubDate/Year")
      year <- as.integer(xml2::xml_text(year_node))

      # Extract DOI
      doi_node <- xml2::xml_find_first(article, ".//ArticleId[@IdType='doi']")
      doi <- xml2::xml_text(doi_node)

      # Extract PMID
      pmid_node <- xml2::xml_find_first(article, ".//PMID")
      pmid <- xml2::xml_text(pmid_node)

      data.frame(
        title = ifelse(length(title) > 0, title, NA),
        abstract = ifelse(length(abstract) > 0, abstract, NA),
        authors = ifelse(length(authors) > 0, authors, NA),
        year = ifelse(length(year) > 0 && !is.na(year), year, NA),
        doi = ifelse(length(doi) > 0, doi, NA),
        pmid = ifelse(length(pmid) > 0, pmid, NA),
        stringsAsFactors = FALSE
      )
    })

    do.call(rbind, results)

  }, error = function(e) {
    return(data.frame())
  })
}

#' @keywords internal
search_crossref <- function(query, since_date = NULL) {
  if (!requireNamespace("rcrossref", quietly = TRUE)) {
    warning("rcrossref package not available. Install with: install.packages('rcrossref')")
    return(data.frame(
      title = character(),
      abstract = character(),
      authors = character(),
      year = integer(),
      doi = character(),
      stringsAsFactors = FALSE
    ))
  }

  tryCatch({
    # Build filter
    filter_args <- list()
    if (!is.null(since_date)) {
      filter_args$from_pub_date <- format(since_date, "%Y-%m-%d")
    }

    # Search Crossref
    search_result <- rcrossref::cr_works(
      query = query,
      filter = filter_args,
      limit = 1000
    )

    if (is.null(search_result$data) || nrow(search_result$data) == 0) {
      return(data.frame(
        title = character(),
        abstract = character(),
        authors = character(),
        year = integer(),
        doi = character(),
        stringsAsFactors = FALSE
      ))
    }

    data <- search_result$data

    # Extract and format results
    results <- data.frame(
      title = data$title,
      abstract = data$abstract,
      authors = sapply(data$author, function(x) {
        if (is.null(x) || length(x) == 0) return(NA)
        if (is.list(x)) {
          auths <- sapply(x, function(a) paste(a$given, a$family))
          return(paste(auths, collapse = "; "))
        }
        return(NA)
      }),
      year = as.integer(data$published.print.date.parts),
      doi = data$doi,
      stringsAsFactors = FALSE
    )

    return(results)

  }, error = function(e) {
    warning(sprintf("Crossref search error: %s", e$message))
    return(data.frame(
      title = character(),
      abstract = character(),
      authors = character(),
      year = integer(),
      doi = character(),
      stringsAsFactors = FALSE
    ))
  })
}

#' @keywords internal
search_openalex <- function(query, since_date = NULL) {
  if (!requireNamespace("openalexR", quietly = TRUE)) {
    warning("openalexR package not available. Install with: install.packages('openalexR')")
    return(data.frame(
      title = character(),
      abstract = character(),
      authors = character(),
      year = integer(),
      doi = character(),
      openalex_id = character(),
      stringsAsFactors = FALSE
    ))
  }

  tryCatch({
    # Build filter string
    filter_str <- NULL
    if (!is.null(since_date)) {
      filter_str <- paste0("from_publication_date:", format(since_date, "%Y-%m-%d"))
    }

    # Search OpenAlex
    search_result <- openalexR::oa_fetch(
      entity = "works",
      search = query,
      filter = filter_str,
      count_only = FALSE,
      verbose = FALSE
    )

    if (is.null(search_result) || nrow(search_result) == 0) {
      return(data.frame(
        title = character(),
        abstract = character(),
        authors = character(),
        year = integer(),
        doi = character(),
        openalex_id = character(),
        stringsAsFactors = FALSE
      ))
    }

    # Extract and format results
    results <- data.frame(
      title = search_result$display_name,
      abstract = ifelse("abstract_inverted_index" %in% names(search_result),
                       sapply(search_result$abstract_inverted_index, reconstruct_abstract),
                       NA),
      authors = sapply(search_result$author, function(x) {
        if (is.null(x) || length(x) == 0) return(NA)
        paste(x$au_display_name, collapse = "; ")
      }),
      year = as.integer(search_result$publication_year),
      doi = search_result$doi,
      openalex_id = search_result$id,
      stringsAsFactors = FALSE
    )

    return(results)

  }, error = function(e) {
    warning(sprintf("OpenAlex search error: %s", e$message))
    return(data.frame(
      title = character(),
      abstract = character(),
      authors = character(),
      year = integer(),
      doi = character(),
      openalex_id = character(),
      stringsAsFactors = FALSE
    ))
  })
}

#' @keywords internal
reconstruct_abstract <- function(inverted_index) {
  if (is.null(inverted_index) || length(inverted_index) == 0) {
    return(NA)
  }

  tryCatch({
    # Reconstruct abstract from inverted index
    words <- character()
    for (word in names(inverted_index)) {
      positions <- inverted_index[[word]]
      for (pos in positions) {
        words[pos] <- word
      }
    }
    return(paste(words, collapse = " "))
  }, error = function(e) {
    return(NA)
  })
}

#' @keywords internal
ai_screen_studies <- function(citations, inclusion_criteria = NULL, threshold = 0.7) {
  cat("  AI-powered screening in progress...\n")

  # Initialize decision columns
  citations$decision <- "UNCERTAIN"
  citations$confidence <- 0.5
  citations$rationale <- "Manual review recommended"

  # Check if LLM is available
  llm_available <- check_llm_availability()

  if (!llm_available) {
    warning("No LLM available. All studies marked for manual review.")
    return(citations)
  }

  # Default inclusion criteria if not provided
  if (is.null(inclusion_criteria)) {
    inclusion_criteria <- "Original research studies with quantitative outcomes suitable for meta-analysis"
  }

  # Screen each citation
  for (i in 1:nrow(citations)) {
    if (i %% 10 == 0) {
      cat(sprintf("    Screened %d/%d studies...\n", i, nrow(citations)))
    }

    tryCatch({
      # Build prompt for LLM
      prompt <- sprintf("
You are screening a study for systematic review inclusion.

INCLUSION CRITERIA:
%s

STUDY INFORMATION:
Title: %s
Abstract: %s
Year: %s

TASK:
Determine if this study meets the inclusion criteria.
Provide:
1. Decision: INCLUDE, EXCLUDE, or UNCERTAIN
2. Confidence: 0-1 (how confident are you?)
3. Brief rationale (one sentence)

Format your response as:
DECISION: [INCLUDE/EXCLUDE/UNCERTAIN]
CONFIDENCE: [0.0-1.0]
RATIONALE: [Brief explanation]
      ",
        inclusion_criteria,
        citations$title[i],
        substr(citations$abstract[i], 1, 1000),  # Limit abstract length
        citations$year[i]
      )

      # Call LLM
      llm_response <- call_llm_screening(prompt)

      # Parse response
      decision_match <- regmatches(llm_response, regexpr("DECISION:\\s*(INCLUDE|EXCLUDE|UNCERTAIN)", llm_response))
      confidence_match <- regmatches(llm_response, regexpr("CONFIDENCE:\\s*([0-9.]+)", llm_response))
      rationale_match <- regmatches(llm_response, regexpr("RATIONALE:\\s*(.+?)($|\n)", llm_response))

      if (length(decision_match) > 0) {
        decision <- gsub("DECISION:\\s*", "", decision_match[1])
        citations$decision[i] <- decision
      }

      if (length(confidence_match) > 0) {
        confidence <- as.numeric(gsub("CONFIDENCE:\\s*", "", confidence_match[1]))
        citations$confidence[i] <- confidence
      }

      if (length(rationale_match) > 0) {
        rationale <- gsub("RATIONALE:\\s*", "", rationale_match[1])
        citations$rationale[i] <- rationale
      }

      # Apply threshold
      if (citations$confidence[i] < threshold) {
        citations$decision[i] <- "UNCERTAIN"
      }

      Sys.sleep(0.2)  # Rate limiting

    }, error = function(e) {
      # Keep default values on error
    })
  }

  return(citations)
}

#' @keywords internal
check_llm_availability <- function() {
  # Check for Gemini API key
  gemini_key <- Sys.getenv("GEMINI_API_KEY")
  if (nchar(gemini_key) > 0) {
    return(TRUE)
  }

  # Check for Ollama
  if (check_ollama_available()) {
    return(TRUE)
  }

  return(FALSE)
}

#' @keywords internal
check_ollama_available <- function() {
  tryCatch({
    # Try to connect to Ollama
    if (requireNamespace("httr", quietly = TRUE)) {
      response <- httr::GET("http://localhost:11434/api/tags", timeout = 2)
      return(httr::status_code(response) == 200)
    }
    return(FALSE)
  }, error = function(e) {
    return(FALSE)
  })
}

#' @keywords internal
call_llm_screening <- function(prompt) {
  # Try Gemini first
  gemini_key <- Sys.getenv("GEMINI_API_KEY")

  if (nchar(gemini_key) > 0) {
    return(call_gemini_api(prompt, gemini_key))
  }

  # Fallback to Ollama
  if (check_ollama_available()) {
    return(call_ollama_api(prompt))
  }

  # No LLM available
  return("DECISION: UNCERTAIN\nCONFIDENCE: 0.5\nRATIONALE: No LLM available for screening")
}

#' @keywords internal
call_gemini_api <- function(prompt, api_key) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("httr package required")
  }

  tryCatch({
    url <- sprintf("https://generativelanguage.googleapis.com/v1beta/models/gemini-pro:generateContent?key=%s", api_key)

    body <- list(
      contents = list(
        list(
          parts = list(
            list(text = prompt)
          )
        )
      )
    )

    response <- httr::POST(
      url,
      body = jsonlite::toJSON(body, auto_unbox = TRUE),
      httr::content_type_json(),
      httr::timeout(30)
    )

    if (httr::status_code(response) == 200) {
      content <- httr::content(response, "parsed")
      return(content$candidates[[1]]$content$parts[[1]]$text)
    } else {
      return("DECISION: UNCERTAIN\nCONFIDENCE: 0.5\nRATIONALE: API error")
    }
  }, error = function(e) {
    return("DECISION: UNCERTAIN\nCONFIDENCE: 0.5\nRATIONALE: API call failed")
  })
}

#' @keywords internal
call_ollama_api <- function(prompt) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("httr package required")
  }

  tryCatch({
    body <- list(
      model = "llama3",
      prompt = prompt,
      stream = FALSE
    )

    response <- httr::POST(
      "http://localhost:11434/api/generate",
      body = jsonlite::toJSON(body, auto_unbox = TRUE),
      httr::content_type_json(),
      httr::timeout(60)
    )

    if (httr::status_code(response) == 200) {
      content <- httr::content(response, "parsed")
      return(content$response)
    } else {
      return("DECISION: UNCERTAIN\nCONFIDENCE: 0.5\nRATIONALE: Ollama error")
    }
  }, error = function(e) {
    return("DECISION: UNCERTAIN\nCONFIDENCE: 0.5\nRATIONALE: Ollama call failed")
  })
}

#' @keywords internal
ai_extract_data <- function(studies, extraction_template = NULL) {
  cat("  AI-powered data extraction in progress...\n")

  # Check LLM availability
  llm_available <- check_llm_availability()

  if (!llm_available) {
    warning("No LLM available. Creating placeholder data entries.")
    return(data.frame(
      study = studies$title,
      effect = NA,
      se = NA,
      n = NA,
      stringsAsFactors = FALSE
    ))
  }

  # Default extraction template
  if (is.null(extraction_template)) {
    extraction_template <- "Extract: sample size, mean difference or effect size, standard error or confidence interval"
  }

  # Extract data from each study
  extracted_data <- list()

  for (i in 1:nrow(studies)) {
    if (i %% 5 == 0) {
      cat(sprintf("    Extracted %d/%d studies...\n", i, nrow(studies)))
    }

    tryCatch({
      prompt <- sprintf("
You are extracting data from a research study for meta-analysis.

EXTRACTION TEMPLATE:
%s

STUDY INFORMATION:
Title: %s
Abstract: %s

TASK:
Extract the requested data from this study.
If exact data is not available in the abstract, indicate that full text review is needed.

Format your response as:
EFFECT_SIZE: [numeric value or 'NEEDS_FULL_TEXT']
STANDARD_ERROR: [numeric value or 'NEEDS_FULL_TEXT']
SAMPLE_SIZE: [integer or 'NEEDS_FULL_TEXT']
NOTES: [Any relevant notes]
      ",
        extraction_template,
        studies$title[i],
        substr(studies$abstract[i], 1, 1500)
      )

      llm_response <- call_llm_screening(prompt)

      # Parse response
      effect_match <- regmatches(llm_response, regexpr("EFFECT_SIZE:\\s*([0-9.-]+|NEEDS_FULL_TEXT)", llm_response))
      se_match <- regmatches(llm_response, regexpr("STANDARD_ERROR:\\s*([0-9.-]+|NEEDS_FULL_TEXT)", llm_response))
      n_match <- regmatches(llm_response, regexpr("SAMPLE_SIZE:\\s*([0-9]+|NEEDS_FULL_TEXT)", llm_response))
      notes_match <- regmatches(llm_response, regexpr("NOTES:\\s*(.+?)($|\n)", llm_response))

      effect <- NA
      se <- NA
      n <- NA
      notes <- "Requires manual review"

      if (length(effect_match) > 0) {
        effect_str <- gsub("EFFECT_SIZE:\\s*", "", effect_match[1])
        if (effect_str != "NEEDS_FULL_TEXT") {
          effect <- as.numeric(effect_str)
        }
      }

      if (length(se_match) > 0) {
        se_str <- gsub("STANDARD_ERROR:\\s*", "", se_match[1])
        if (se_str != "NEEDS_FULL_TEXT") {
          se <- as.numeric(se_str)
        }
      }

      if (length(n_match) > 0) {
        n_str <- gsub("SAMPLE_SIZE:\\s*", "", n_match[1])
        if (n_str != "NEEDS_FULL_TEXT") {
          n <- as.integer(n_str)
        }
      }

      if (length(notes_match) > 0) {
        notes <- gsub("NOTES:\\s*", "", notes_match[1])
      }

      extracted_data[[i]] <- data.frame(
        study = studies$title[i],
        effect = effect,
        se = se,
        n = n,
        extraction_notes = notes,
        stringsAsFactors = FALSE
      )

      Sys.sleep(0.3)  # Rate limiting

    }, error = function(e) {
      extracted_data[[i]] <- data.frame(
        study = studies$title[i],
        effect = NA,
        se = NA,
        n = NA,
        extraction_notes = "Extraction failed",
        stringsAsFactors = FALSE
      )
    })
  }

  return(do.call(rbind, extracted_data))
}

#' @keywords internal
save_living_project <- function(project) {
  project_file <- file.path(project$directories$root, "project.rds")
  saveRDS(project, project_file)
}

#' @keywords internal
save_version_snapshot <- function(project) {
  version_file <- file.path(
    project$directories$versions,
    sprintf("version_%d.rds", project$metadata$version)
  )
  saveRDS(project, version_file)
}

#' @keywords internal
generate_update_alert <- function(project, changes) {
  alert_file <- file.path(
    project$directories$reports,
    sprintf("alert_v%d.txt", project$metadata$version)
  )

  alert_text <- sprintf("
LIVING REVIEW UPDATE ALERT
=========================

Project: %s
Version: %d
Date: %s

MEANINGFUL CHANGES DETECTED

%s

Review the full report for details.
  ",
    project$metadata$project_name,
    project$metadata$version,
    Sys.time(),
    paste(capture.output(print(changes)), collapse = "\n")
  )

  writeLines(alert_text, alert_file)

  cat(sprintf("✓ Alert generated: %s\n", alert_file))
}

#' Schedule Automatic Updates
#'
#' Sets up automatic scheduled updates for a living review project.
#'
#' @param project Living meta-analysis project
#' @param scheduler Scheduling system: "cron" (Unix/Mac) or "taskscheduler" (Windows)
#'
#' @return Instructions for scheduling
#'
#' @export
schedule_living_updates <- function(project, scheduler = "cron") {

  script_content <- sprintf('
library(cbamm)

# Load project
project <- readRDS("%s")

# Run update
project <- update_living_review(project)

# Generate report if changes detected
if (length(project$update_history) > 0) {
  last_update <- project$update_history[[length(project$update_history)]]
  if (last_update$change_detected) {
    generate_living_report(project, format = "html")
  }
}
  ',
    file.path(project$directories$root, "project.rds")
  )

  script_file <- file.path(project$directories$root, "auto_update.R")
  writeLines(script_content, script_file)

  cat("Automatic update script created:", script_file, "\n\n")

  if (scheduler == "cron") {
    cat("To schedule updates on Unix/Mac, add to crontab:\n\n")

    schedule_cmd <- switch(project$metadata$update_frequency,
      "daily" = "0 9 * * *",
      "weekly" = "0 9 * * 1",
      "monthly" = "0 9 1 * *",
      "0 9 * * 1"  # Default weekly
    )

    cat(sprintf("%s Rscript %s\n\n", schedule_cmd, script_file))
    cat("Use: crontab -e\n")

  } else if (scheduler == "taskscheduler") {
    cat("To schedule updates on Windows:\n\n")
    cat("1. Open Task Scheduler\n")
    cat("2. Create Basic Task\n")
    cat("3. Set trigger based on update_frequency\n")
    cat(sprintf("4. Action: Start program - Rscript %s\n\n", script_file))
  }
}
