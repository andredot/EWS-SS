#!/usr/bin/env Rscript
# ==============================================================================
# DEPRIVATION INDEX - Lombardy municipalities + Milan health districts
# ==============================================================================
#
# Builds an area deprivation index from the ISTAT 2023 permanent census
# (section-level data) and assigns each unit to a deprivation decile, so that
# vaccination coverage can be compared across deciles (the WHO approach to
# monitoring socioeconomic inequality when individual deprivation is not
# available: give each resident the deprivation of the area they live in).
#
# Units of analysis
#   - every Lombard municipality except Milan, as a whole
#   - Milan split into its 9 health districts (which coincide with the 9
#     Municipi), built by summing the census sections of each district
#
# Output: one row per unit with
#   codice_comune  ISTAT municipality code (6 digits, character). For Milan's
#                  districts an invented code that starts with Milan's code:
#                  "015146" + 2-digit district number, e.g. "01514601".
#   nome_comune    municipality name; for districts the district name
#   indice_deprivazione  sum of the standardised indicators (higher = more
#                  deprived)
#   decile         1 = least deprived ... 10 = most deprived
#   popolazione    resident population (needed to weight coverage by decile)
#
# Run from the project root:   Rscript scripts/deprivation_index.R
# or in R:                     source("scripts/deprivation_index.R")
#
# Required packages: dplyr, readr, readxl, stringr, tibble (all in renv.lock).
# The shapefile is NOT needed: Milan's sections are assigned to districts via
# the sub-municipal area code (ASC) carried by the census table, or via an
# optional crosswalk file.
#
# ------------------------------------------------------------------------------
# METHOD
# ------------------------------------------------------------------------------
# Permanent-census reformulation of the Italian Deprivation Index (Caranci et
# al. 2010; Rosano et al. 2020), same implementation as
# github.com/andredot/atlaspm/blob/master/R/deprivation.R. Four indicators, all
# oriented so that higher = more disadvantaged:
#
#   edu_low  share of the population aged 9+ with at most lower-secondary
#            education                      (P86 + P87 + P88) / P83
#   nonemp   share of the 15-64 population not employed
#                                          (sum(P17:P26) - P101) / sum(P17:P26)
#   foreign  share of foreign residents     ST1 / P1
#   crowd    occupants per occupied dwelling  P1 / A2
#
# This is a proxy, not the validated 2011 five-indicator index: home ownership
# and single-parent families are not released by the permanent census.
#
# Steps
#   1. Sections are summed to municipality (Milan as a whole). This set of
#      municipalities is the REFERENCE distribution: the mean and SD of each
#      indicator are taken across municipalities (unweighted, as in the
#      original index).
#   2. Sections are summed to the units of analysis (municipalities outside
#      Milan, districts inside). Each indicator is standardised with the
#      reference mean/SD from step 1 and the four z-scores are summed.
#      Using municipalities as the reference keeps the scale homogeneous: the
#      9 districts are scored on it, but do not define it.
#   3. Units are ranked by score and split into deciles. By default the
#      deciles are POPULATION-WEIGHTED: each holds ~10% of the Lombard
#      population, which is what the WHO approach needs (coverage in the
#      poorest 10% of residents vs the richest 10%). With unweighted deciles
#      the ~1,500 municipalities would be split into equal counts, and the
#      population per decile would range from a few tens of thousands
#      (small mountain municipalities) to well over a million.
#      A unit is never split across deciles, so shares are approximately, not
#      exactly, 10%.
#
# ------------------------------------------------------------------------------
# INPUT
# ------------------------------------------------------------------------------
# ISTAT "Dati per sezioni di censimento - Censimento permanente 2023", file for
# Lombardy (R03_..._2023_sezioni.xlsx or .csv), downloadable from
# https://esploradati.istat.it/databrowser/#/it/censpop/categories/IT1,Z1200CPA,1.0/SUB_MUN_DATA/PCENS2023
# One row per census section (2021 sections). Wide format with the variable
# codes listed in CENSUS_VARS below. CHECK THEM against the TRACCIATO file
# shipped with the data before trusting the result: if a code points at the
# wrong column the arithmetic still runs and the index is silently wrong (the
# plausibility check at the end is there to catch exactly that).
#
# If the file covers more than Lombardy (e.g. national), set REGION_CODE so
# only Lombardy is kept: the reference distribution and the deciles are both
# meant to be regional.
# ==============================================================================


# Configuration ----------------------------------------------------------------

CONFIG <- list(
  # ISTAT 2023 section-level census table (.xlsx or .csv)
  census_file = "data/istat/R03_indicatori_2023_sezioni.xlsx",
  census_sheet = 1,

  # Keep only this region (ISTAT 2-digit code, Lombardy = "03"). Used only if
  # the table has a region column; set to NULL to keep every row.
  region_code = "03",

  # Optional crosswalk section -> Milan district, CSV with two columns:
  #   sez_id      census section identifier, same coding as the census table
  #   distretto   district number (1-9)
  # When NULL, the ASC (sub-municipal area) column of the census table is used:
  # in Milan the ASC are the 9 Municipi, which coincide with the 9 districts.
  milano_xwalk_file = NULL,

  milano_procom = "015146",

  # District number -> name. Edit the names to the official ones if needed.
  milano_distretti = tibble::tibble(
    distretto = 1:9,
    nome      = paste("Milano - Distretto", 1:9)
  ),

  # "population" (each decile ~10% of residents) or "units" (each decile ~10%
  # of the units)
  decile_weighting = "population",

  out_file = "output/indice_deprivazione_lombardia_2023.csv",
  out_file_detail = "output/indice_deprivazione_lombardia_2023_dettaglio.csv"
)

# Census variable codes (2023 permanent census). See header.
CENSUS_VARS <- list(
  pop       = "P1",                       # resident population
  pop_1564  = paste0("P", 17:26),         # age bands 15-19 ... 60-64
  edu_den   = "P83",                      # population aged 9+ (education)
  edu_low   = c("P86", "P87", "P88"),     # at most lower-secondary education
  employed  = "P101",                     # employed
  foreign   = "ST1",                      # foreign residents
  dwellings = "A2"                        # occupied dwellings
)

# Accepted names for the identifier columns. ISTAT has changed them across
# releases, so the first match is used and reported.
ID_CANDIDATES <- list(
  procom = c("PROCOM", "PRO_COM", "PRO_COM_T", "CODICE_COMUNE", "COD_COM"),
  comune = c("COMUNE", "DENOMINAZIONE", "NOME_COMUNE", "COMUNE21", "DEN_COM"),
  sez_id = c("SEZ21_ID", "SEZ2021", "SEZ21", "IDSEZ", "SEZ_ID", "SEZ2011",
             "SEZ", "NSEZ", "SEZIONE", "COD_SEZ"),
  asc    = c("CODASC", "COD_ASC", "ASC", "ASC21", "COD_ASC21", "ASC21_ID"),
  region = c("CODREG", "COD_REG", "REGIONE_COD")
)


# Helpers ----------------------------------------------------------------------

#' Return the first column of `df` whose name matches one of `candidates`
#' (case-insensitive). Stops with an informative message if `required` and
#' nothing matches, otherwise returns NULL.
find_col <- function(df, candidates, what, required = TRUE) {
  nm  <- names(df)
  hit <- nm[match(toupper(candidates), toupper(nm))]
  hit <- hit[!is.na(hit)]
  if (length(hit)) {
    message("  ", what, ": column '", hit[1], "'")
    return(hit[1])
  }
  if (required) {
    stop("No column for ", what, " found in the census table.\n",
         "  Tried: ", paste(candidates, collapse = ", "), "\n",
         "  Available: ", paste(nm, collapse = ", "), call. = FALSE)
  }
  NULL
}

#' Counts as numeric. Census files can carry blanks, "-" or decimal commas.
to_num <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "-", "NA", "..")] <- NA
  suppressWarnings(as.numeric(gsub(",", ".", x, fixed = TRUE)))
}

#' ISTAT municipality code as a 6-digit character string
pad_procom <- function(x) {
  x <- gsub("\\D", "", as.character(x))
  stringr::str_pad(x, width = 6, side = "left", pad = "0")
}

#' Deciles of `score` with optional weights (1 = lowest score).
#'
#' Units are sorted by score and each is placed by the midpoint of its share
#' of the cumulative weight: decile = floor(10 * midpoint) + 1. With unit
#' weights this is the usual equal-count split; with population weights each
#' decile holds ~10% of the population. NA scores stay NA.
weighted_decile <- function(score, w = rep(1, length(score)), n = 10L) {
  out <- rep(NA_integer_, length(score))
  ok  <- which(!is.na(score) & !is.na(w) & w > 0)
  if (!length(ok)) return(out)
  o   <- ok[order(score[ok])]
  cw  <- cumsum(w[o])
  mid <- (cw - w[o] / 2) / sum(w[o])
  out[o] <- pmin(n, as.integer(floor(mid * n)) + 1L)
  out
}


# Import -----------------------------------------------------------------------

#' Read the ISTAT 2023 section-level census table and return a tidy tibble:
#' sez_id, procom, comune, asc, plus the numeric count columns in CENSUS_VARS.
read_census_sections <- function(path, sheet = 1, vars = CENSUS_VARS,
                                 region_code = NULL) {
  if (!file.exists(path)) {
    stop("Census file not found: ", path, "\n",
         "  Download the 2023 section data for Lombardy from ISTAT and set ",
         "CONFIG$census_file.", call. = FALSE)
  }
  message("Reading ", path)

  ext <- tolower(tools::file_ext(path))
  raw <- if (ext %in% c("xlsx", "xls")) {
    readxl::read_excel(path, sheet = sheet, col_types = "text")
  } else if (ext %in% c("csv", "txt")) {
    # ISTAT csv files are usually ';'-separated and latin1-encoded
    first <- readLines(path, n = 1, warn = FALSE)
    delim <- if (lengths(regmatches(first, gregexpr(";", first))) >
                 lengths(regmatches(first, gregexpr(",", first)))) ";" else ","
    readr::read_delim(path, delim = delim,
                      col_types = readr::cols(.default = "c"),
                      locale = readr::locale(encoding = "latin1"),
                      progress = FALSE)
  } else {
    stop("Unsupported file type: .", ext, call. = FALSE)
  }

  # Trim header whitespace and, if present, a UTF-8 BOM on the first name
  names(raw) <- trimws(sub("^﻿", "", names(raw)))

  col_procom <- find_col(raw, ID_CANDIDATES$procom, "municipality code")
  col_comune <- find_col(raw, ID_CANDIDATES$comune, "municipality name",
                         required = FALSE)
  col_sez    <- find_col(raw, ID_CANDIDATES$sez_id, "section id",
                         required = FALSE)
  col_asc    <- find_col(raw, ID_CANDIDATES$asc, "sub-municipal area (ASC)",
                         required = FALSE)
  col_reg    <- find_col(raw, ID_CANDIDATES$region, "region code",
                         required = FALSE)

  count_cols <- unique(unlist(vars, use.names = FALSE))
  missing <- setdiff(count_cols, names(raw))
  if (length(missing)) {
    stop("Census variables not found: ", paste(missing, collapse = ", "),
         ".\n  Check CENSUS_VARS against the TRACCIATO of the release.",
         call. = FALSE)
  }

  if (!is.null(region_code) && !is.null(col_reg)) {
    reg  <- stringr::str_pad(gsub("\\D", "", raw[[col_reg]]), 2, pad = "0")
    keep <- reg == region_code
    message("  Keeping region ", region_code, ": ", sum(keep, na.rm = TRUE),
            " of ", nrow(raw), " sections")
    raw <- raw[!is.na(keep) & keep, , drop = FALSE]
  }

  out <- tibble::tibble(
    sez_id = if (is.null(col_sez)) NA_character_ else trimws(raw[[col_sez]]),
    procom = pad_procom(raw[[col_procom]]),
    comune = if (is.null(col_comune)) NA_character_ else trimws(raw[[col_comune]]),
    asc    = if (is.null(col_asc)) NA_character_ else trimws(raw[[col_asc]])
  )
  for (v in count_cols) out[[v]] <- to_num(raw[[v]])

  message("  ", nrow(out), " sections, ", dplyr::n_distinct(out$procom),
          " municipalities")
  out
}


# Units of analysis ------------------------------------------------------------

#' Assign each section to its unit of analysis.
#'
#' Outside Milan the unit is the municipality. Inside Milan it is the health
#' district, taken from `xwalk` if given, else from the ASC column. Milan
#' sections that cannot be assigned (e.g. ISTAT's fictitious "888888x"
#' sections for residents with no fixed address) are reported and dropped
#' from the district units; they still count towards Milan in the reference
#' distribution.
assign_units <- function(sez,
                         milano_procom = CONFIG$milano_procom,
                         distretti = CONFIG$milano_distretti,
                         xwalk = NULL) {

  in_mi <- sez$procom == milano_procom
  sez$distretto <- NA_integer_

  if (any(in_mi)) {
    if (!is.null(xwalk)) {
      if (all(is.na(sez$sez_id))) {
        stop("A Milan crosswalk was given but the census table has no section ",
             "id column to join it on.", call. = FALSE)
      }
      xw <- dplyr::distinct(xwalk, sez_id = trimws(as.character(sez_id)),
                            distretto = as.integer(distretto))
      dup <- xw$sez_id[duplicated(xw$sez_id)]
      if (length(dup)) {
        stop("Crosswalk assigns ", length(unique(dup)), " section(s) to more ",
             "than one district, e.g. ", dup[1], call. = FALSE)
      }
      sez$distretto[in_mi] <- xw$distretto[match(sez$sez_id[in_mi], xw$sez_id)]
      source_msg <- "crosswalk file"
    } else {
      if (all(is.na(sez$asc[in_mi]))) {
        stop("Milan's sections carry no ASC code, so they cannot be assigned ",
             "to districts. Provide CONFIG$milano_xwalk_file (sez_id -> ",
             "distretto).", call. = FALSE)
      }
      # ASC codes can come as "1", "01" or with a prefix; take the trailing
      # number and then require it to be a known district.
      sez$distretto[in_mi] <- suppressWarnings(
        as.integer(stringr::str_extract(sez$asc[in_mi], "\\d+$"))
      )
      source_msg <- "ASC column"
    }

    bad <- in_mi & !is.na(sez$distretto) & !sez$distretto %in% distretti$distretto
    if (any(bad)) {
      stop("Milan sections have district values not in CONFIG$milano_distretti: ",
           paste(utils::head(sort(unique(sez$distretto[bad])), 20),
                 collapse = ", "),
           "\n  Check how the ASC/district is coded and adjust.", call. = FALSE)
    }

    unassigned <- in_mi & is.na(sez$distretto)
    if (any(unassigned)) {
      message("  Milan: ", sum(unassigned), " section(s) with no district (",
              formatC(sum(sez[[CENSUS_VARS$pop]][unassigned], na.rm = TRUE),
                      format = "d", big.mark = ".", decimal.mark = ","),
              " residents) left out of the district units")
    }
    message("  Milan sections assigned to districts via ", source_msg, ": ",
            paste(sprintf("%d=%d", as.integer(names(table(sez$distretto[in_mi]))),
                          as.integer(table(sez$distretto[in_mi]))),
                  collapse = ", "))

    missing_d <- setdiff(distretti$distretto, sez$distretto[in_mi])
    if (length(missing_d)) {
      warning("No section assigned to Milan district(s) ",
              paste(missing_d, collapse = ", "), call. = FALSE)
    }
  }

  # Municipality names: one per code (sections may disagree on spelling)
  names_lu <- sez |>
    dplyr::filter(!is.na(comune)) |>
    dplyr::count(procom, comune) |>
    dplyr::arrange(procom, dplyr::desc(n)) |>
    dplyr::distinct(procom, .keep_all = TRUE)

  sez |>
    dplyr::mutate(
      unit_code = dplyr::if_else(
        in_mi, paste0(milano_procom, sprintf("%02d", distretto)), procom),
      unit_name = dplyr::if_else(
        in_mi,
        distretti$nome[match(distretto, distretti$distretto)],
        names_lu$comune[match(procom, names_lu$procom)]),
      unit_code = dplyr::if_else(in_mi & is.na(distretto), NA_character_,
                                 unit_code)
    )
}


# Indicators -------------------------------------------------------------------

#' Sum census counts by `group` and compute the four indicators.
#'
#' One function for both the reference municipalities and the units of
#' analysis, so the same arithmetic is provably applied to both.
#'
#' Zero denominators give NA (not Inf, which would poison mean and SD).
#' Values outside the substantively possible range (proportions outside
#' [0, 1], more than 8 occupants per dwelling) mean a broken denominator, not
#' an extreme area: they are clamped and reported.
deprivation_indicators <- function(sez, group, vars = CENSUS_VARS,
                                   bounds = list(edu_low = c(0, 1),
                                                 nonemp  = c(0, 1),
                                                 foreign = c(0, 1),
                                                 crowd   = c(1, 8))) {
  count_cols <- unique(unlist(vars, use.names = FALSE))

  agg <- sez |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group))) |>
    dplyr::summarise(dplyr::across(dplyr::all_of(count_cols),
                                   ~ sum(.x, na.rm = TRUE)),
                     .groups = "drop")

  safe <- function(num, den) ifelse(den > 0, num / den, NA_real_)
  rs   <- function(cols) rowSums(agg[, cols, drop = FALSE], na.rm = TRUE)

  pop_1564 <- rs(vars$pop_1564)

  out <- dplyr::mutate(
    agg[, group, drop = FALSE],
    popolazione = agg[[vars$pop]],
    edu_low = safe(rs(vars$edu_low), agg[[vars$edu_den]]),
    nonemp  = safe(pop_1564 - agg[[vars$employed]], pop_1564),
    foreign = safe(agg[[vars$foreign]], agg[[vars$pop]]),
    crowd   = safe(agg[[vars$pop]], agg[[vars$dwellings]])
  )

  for (v in names(bounds)) {
    lo  <- bounds[[v]][1]; hi <- bounds[[v]][2]
    bad <- !is.na(out[[v]]) & (out[[v]] < lo | out[[v]] > hi)
    if (any(bad)) {
      warning(sum(bad), " value(s) of `", v, "` outside [", lo, ", ", hi,
              "] clamped (units: ",
              paste(utils::head(out[[group[1]]][bad], 5), collapse = ", "),
              if (sum(bad) > 5) ", ..." else "",
              "). This points to a broken denominator; check those units.",
              call. = FALSE)
    }
    out[[v]] <- pmin(pmax(out[[v]], lo), hi)
  }
  out
}


# Index ------------------------------------------------------------------------

#' Build the deprivation index for municipalities + Milan districts.
#'
#' @param sez Output of assign_units().
#' @param weighting "population" or "units" (see header).
#' @return Tibble with codice_comune, nome_comune, indice_deprivazione,
#'   decile, popolazione and the four raw indicators. The reference means/SDs
#'   are attached as attribute "reference".
build_deprivation_index <- function(sez, weighting = c("population", "units")) {
  weighting <- match.arg(weighting)
  ind <- c("edu_low", "nonemp", "foreign", "crowd")

  # 1. Reference distribution: all municipalities, Milan as a whole
  ref_units <- deprivation_indicators(sez, "procom")
  ref <- vapply(ind, function(v) c(mean = mean(ref_units[[v]], na.rm = TRUE),
                                   sd   = stats::sd(ref_units[[v]], na.rm = TRUE)),
                numeric(2))
  if (any(!is.finite(ref["sd", ])) || any(ref["sd", ] == 0)) {
    stop("An indicator has zero or undefined SD across municipalities.",
         call. = FALSE)
  }
  message("Reference distribution: ", nrow(ref_units), " municipalities")

  # 2. Units of analysis: municipalities outside Milan + Milan districts
  units <- sez |>
    dplyr::filter(!is.na(unit_code)) |>
    deprivation_indicators(c("unit_code", "unit_name"))

  z <- vapply(ind, function(v) (units[[v]] - ref["mean", v]) / ref["sd", v],
              numeric(nrow(units)))
  if (is.null(dim(z))) z <- matrix(z, nrow = 1, dimnames = list(NULL, ind))
  units$indice_deprivazione <- rowSums(z)   # NA if any indicator is NA

  # 3. Deciles (1 = least deprived)
  w <- if (weighting == "population") units$popolazione else rep(1, nrow(units))
  units$decile <- weighted_decile(units$indice_deprivazione, w)

  out <- units |>
    dplyr::transmute(
      codice_comune = unit_code,
      nome_comune   = unit_name,
      indice_deprivazione,
      decile,
      popolazione,
      edu_low, nonemp, foreign, crowd
    ) |>
    dplyr::arrange(codice_comune)

  n_na <- sum(is.na(out$indice_deprivazione))
  if (n_na) {
    warning(n_na, " unit(s) have no score (zero denominator): ",
            paste(utils::head(out$codice_comune[is.na(out$indice_deprivazione)],
                              10), collapse = ", "), call. = FALSE)
  }

  attr(out, "reference") <- ref
  attr(out, "weighting") <- weighting
  out
}


# Checks -----------------------------------------------------------------------

#' Plausibility checks. A census code pointing at the wrong column produces no
#' error, only implausible magnitudes, so compare the medians with known
#' Italian figures and look for a single unit dominating the scale.
check_deprivation <- function(di,
                              bounds = list(edu_low = c(0.25, 0.75),
                                            nonemp  = c(0.20, 0.60),
                                            foreign = c(0.01, 0.40),
                                            crowd   = c(1.5, 3.5))) {
  problems <- character(0)
  for (v in names(bounds)) {
    med <- stats::median(di[[v]], na.rm = TRUE)
    if (!is.na(med) && (med < bounds[[v]][1] || med > bounds[[v]][2])) {
      problems <- c(problems, sprintf("  %s: median %.3f outside %.2f-%.2f",
                                      v, med, bounds[[v]][1], bounds[[v]][2]))
    }
  }

  s   <- di$indice_deprivazione
  mad <- stats::mad(s, na.rm = TRUE)
  if (is.finite(mad) && mad > 0) {
    far <- abs(s - stats::median(s, na.rm = TRUE)) / mad
    if (max(far, na.rm = TRUE) > 20) {
      i <- which.max(far)
      problems <- c(problems, sprintf(
        "  indice_deprivazione: %s (%s) is %.0f MADs from the median",
        di$codice_comune[i], di$nome_comune[i], far[i]))
    }
  }

  if (length(problems)) {
    warning("Implausible deprivation indicators:\n",
            paste(problems, collapse = "\n"),
            "\n  Check CENSUS_VARS against the ISTAT TRACCIATO.", call. = FALSE)
  }

  # Population and number of units per decile
  summ <- di |>
    dplyr::filter(!is.na(decile)) |>
    dplyr::group_by(decile) |>
    dplyr::summarise(
      n_unita     = dplyr::n(),
      popolazione = sum(popolazione),
      indice_min  = min(indice_deprivazione),
      indice_max  = max(indice_deprivazione),
      .groups = "drop"
    ) |>
    dplyr::mutate(quota_pop = popolazione / sum(popolazione))

  invisible(summ)
}


# Main -------------------------------------------------------------------------

main <- function(config = CONFIG) {
  sez <- read_census_sections(config$census_file, config$census_sheet,
                              region_code = config$region_code)

  xwalk <- NULL
  if (!is.null(config$milano_xwalk_file)) {
    xwalk <- readr::read_csv(config$milano_xwalk_file,
                             col_types = readr::cols(.default = "c"))
  }
  sez <- assign_units(sez, config$milano_procom, config$milano_distretti, xwalk)

  di   <- build_deprivation_index(sez, weighting = config$decile_weighting)
  summ <- check_deprivation(di)

  message("\nUnits: ", nrow(di), " (", sum(startsWith(di$codice_comune,
          config$milano_procom)), " Milan districts)")
  message("Deciles (", config$decile_weighting, "-weighted):")
  print(summ, n = Inf)

  dir.create(dirname(config$out_file), showWarnings = FALSE, recursive = TRUE)
  out <- dplyr::mutate(di, dplyr::across(
    c(indice_deprivazione, edu_low, nonemp, foreign, crowd), ~ round(.x, 4)))
  readr::write_csv(
    dplyr::select(out, codice_comune, nome_comune, indice_deprivazione,
                  decile, popolazione),
    config$out_file, na = ""
  )
  readr::write_csv(out, config$out_file_detail, na = "")
  message("\nWritten: ", config$out_file, "\n         ", config$out_file_detail)

  invisible(di)
}

# Skipped when sourced with DEPRIV_SKIP_MAIN=1 (used to load the functions only)
if (Sys.getenv("DEPRIV_SKIP_MAIN") != "1") {
  deprivation_index <- main()
}
