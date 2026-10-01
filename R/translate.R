# Pattern-based SAS <-> Python translators for the learner studio.
# These are teaching translators, not a full language compiler.
# Unrecognised syntax is preserved as a review comment.

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x

blank_if_null <- function(x) if (is.null(x)) "" else x

# ---------------------------------------------------------------------------
# Shared helpers
# ---------------------------------------------------------------------------

strip_sas_comments <- function(text) {
  # Replace /* */ block comments and * ; comments with placeholders we can restore.
  chars <- strsplit(text, "", fixed = TRUE)[[1]]
  n <- length(chars)
  if (n == 0) return(list(text = text, comments = character()))
  out <- character(n)
  comments <- character()
  i <- 1L
  in_sq <- FALSE
  in_dq <- FALSE
  while (i <= n) {
    ch <- chars[i]
    nxt <- if (i < n) chars[i + 1L] else ""
    if (!in_dq && ch == "'" && !in_sq) { in_sq <- TRUE; out[i] <- ch; i <- i + 1L; next }
    if (in_sq && ch == "'") {
      if (nxt == "'") { out[i] <- ch; out[i + 1L] <- nxt; i <- i + 2L; next }
      in_sq <- FALSE; out[i] <- ch; i <- i + 1L; next
    }
    if (!in_sq && ch == '"' && !in_dq) { in_dq <- TRUE; out[i] <- ch; i <- i + 1L; next }
    if (in_dq && ch == '"') {
      if (nxt == '"') { out[i] <- ch; out[i + 1L] <- nxt; i <- i + 2L; next }
      in_dq <- FALSE; out[i] <- ch; i <- i + 1L; next
    }
    if (!in_sq && !in_dq && ch == "/" && nxt == "*") {
      j <- i + 2L
      while (j < n && !(chars[j] == "*" && chars[j + 1L] == "/")) j <- j + 1L
      end <- min(n, j + 1L)
      body <- paste(chars[i:end], collapse = "")
      comments <- c(comments, body)
      token <- sprintf(" __COMMENT_%d__ ", length(comments))
      out[i] <- token
      if (i + 1L <= n) out[(i + 1L):end] <- ""
      i <- end + 1L
      next
    }
    out[i] <- ch
    i <- i + 1L
  }
  list(text = paste(out, collapse = ""), comments = comments)
}

restore_comment_tokens <- function(text, comments, style = c("python", "sas")) {
  style <- match.arg(style)
  if (!length(comments)) return(text)
  for (k in seq_along(comments)) {
    raw <- comments[k]
    inner <- gsub("^/\\*|\\*/$", "", raw)
    inner <- trimws(inner)
    if (style == "python") {
      repl <- paste(sprintf("# %s", strsplit(inner, "\n", fixed = TRUE)[[1]]), collapse = "\n")
    } else {
      repl <- raw
    }
    text <- gsub(sprintf("__COMMENT_%d__", k), repl, text, fixed = TRUE)
  }
  text
}

split_sas_statements <- function(text) {
  chars <- strsplit(text, "", fixed = TRUE)[[1]]
  n <- length(chars)
  parts <- character()
  buf <- character()
  in_sq <- FALSE
  in_dq <- FALSE
  i <- 1L
  while (i <= n) {
    ch <- chars[i]
    nxt <- if (i < n) chars[i + 1L] else ""
    buf <- c(buf, ch)
    if (!in_dq && ch == "'") {
      if (in_sq && nxt == "'") { buf <- c(buf, nxt); i <- i + 2L; next }
      in_sq <- !in_sq
    } else if (!in_sq && ch == '"') {
      if (in_dq && nxt == '"') { buf <- c(buf, nxt); i <- i + 2L; next }
      in_dq <- !in_dq
    } else if (!in_sq && !in_dq && ch == ";") {
      stmt <- trimws(paste(buf, collapse = ""))
      if (nzchar(stmt)) parts <- c(parts, stmt)
      buf <- character()
    }
    i <- i + 1L
  }
  tail <- trimws(paste(buf, collapse = ""))
  if (nzchar(tail)) parts <- c(parts, tail)
  parts
}

unquote <- function(x) {
  x <- trimws(x)
  gsub('^["\']|["\']$', "", x)
}

sas_name <- function(x) {
  x <- trimws(x)
  x <- sub("^work\\.", "", x, ignore.case = TRUE)
  gsub("[^A-Za-z0-9_.]", "", x)
}

py_ident <- function(x) {
  x <- sas_name(x)
  if (!nzchar(x)) return("df")
  x
}

split_vars <- function(x) {
  x <- trimws(x)
  if (!nzchar(x)) return(character())
  parts <- strsplit(x, "[,\\s]+")[[1]]
  parts[nzchar(parts)]
}

py_list <- function(vars) {
  if (!length(vars)) return("[]")
  paste0("[", paste(sprintf('"%s"', vars), collapse = ", "), "]")
}

note_item <- function(msg) msg

# ---------------------------------------------------------------------------
# SAS -> Python
# ---------------------------------------------------------------------------

translate_data_step <- function(statements) {
  notes <- character()
  header <- statements[1]
  body <- statements[-1]
  body <- body[!grepl("^(run|quit)\\s*;?$", body, ignore.case = TRUE)]

  m <- regexec("^data\\s+([^;(\\s]+)", header, ignore.case = TRUE)
  mm <- regmatches(header, m)[[1]]
  out_name <- if (length(mm) >= 2) py_ident(mm[2]) else "out"
  if (tolower(out_name) %in% c("_null_", "null")) {
    notes <- c(notes, "DATA _NULL_ has no DataFrame equivalent. Translated as a Python loop sketch — review it.")
    out_name <- "result"
  }

  set_stmt <- body[grepl("^set\\s+", body, ignore.case = TRUE)]
  merge_stmt <- body[grepl("^merge\\s+", body, ignore.case = TRUE)]
  infile_stmt <- body[grepl("^infile\\s+", body, ignore.case = TRUE)]
  input_stmt <- body[grepl("^input\\s+", body, ignore.case = TRUE)]

  lines <- character()
  src <- NULL

  if (length(set_stmt)) {
    sm <- regexec("^set\\s+([^;(]+)", set_stmt[1], ignore.case = TRUE)
    smm <- regmatches(set_stmt[1], sm)[[1]]
    src <- py_ident(trimws(smm[2]))
    lines <- c(lines, sprintf("%s = %s.copy()", out_name, src))
    if (grepl("\\(", set_stmt[1])) {
      notes <- c(notes, "Dataset options on SET (WHERE/KEEP/DROP/RENAME/OBS) were not fully applied. Check the SET statement.")
    }
  } else if (length(merge_stmt)) {
    mmr <- regexec("^merge\\s+(.+?)(?:\\s+\\(|;|$)", merge_stmt[1], ignore.case = TRUE)
    mmrm <- regmatches(merge_stmt[1], mmr)[[1]]
    tables <- split_vars(if (length(mmrm) >= 2) mmrm[2] else "")
    by_stmt <- body[grepl("^by\\s+", body, ignore.case = TRUE)]
    keys <- character()
    if (length(by_stmt)) {
      keys <- split_vars(sub("^by\\s+", "", by_stmt[1], ignore.case = TRUE))
    }
    if (length(tables) >= 2) {
      left <- py_ident(tables[1])
      acc <- left
      tmp_lines <- character()
      for (k in seq(2, length(tables))) {
        right <- py_ident(tables[k])
        dest <- if (k == length(tables)) out_name else sprintf("_m%d", k)
        how <- "outer"
        indicator <- ""
        if (any(grepl("^in\\s*=", body, ignore.case = TRUE)) || grepl("\\bin\\s*=", merge_stmt[1], ignore.case = TRUE)) {
          indicator <- ", indicator=True"
          notes <- c(notes, "SAS IN= flags became a pandas merge indicator column. Map _merge values yourself (left_only/right_only/both).")
        }
        if (!length(keys)) {
          notes <- c(notes, "MERGE without BY is a SAS positional match-merge, not a pandas join. A key-based outer merge was emitted — this is not equivalent.")
          tmp_lines <- c(tmp_lines, sprintf("%s = %s.merge(%s, how='%s'%s)  # REVIEW: no BY key", dest, acc, right, how, indicator))
        } else {
          tmp_lines <- c(tmp_lines, sprintf("%s = %s.merge(%s, on=%s, how='%s'%s)", dest, acc, right, py_list(keys), how, indicator))
        }
        acc <- dest
      }
      lines <- c(lines, tmp_lines)
      notes <- c(notes, "SAS MERGE defaults to a many-to-many match-merge. pandas merge is a SQL-style join. Confirm cardinality.")
    }
    body <- body[!grepl("^(merge|by)\\s+", body, ignore.case = TRUE)]
  } else if (length(infile_stmt)) {
    fm <- regexec("infile\\s+['\"]?([^'\";\\s]+)", infile_stmt[1], ignore.case = TRUE)
    fmm <- regmatches(infile_stmt[1], fm)[[1]]
    path <- if (length(fmm) >= 2) unquote(fmm[2]) else "data.csv"
    delim <- if (grepl("dlm\\s*=\\s*['\"],", infile_stmt[1], ignore.case = TRUE)) "," else NULL
    if (is.null(delim) && grepl("dsd", infile_stmt[1], ignore.case = TRUE)) delim <- ","
    reader <- if (!is.null(delim) && delim == ",") {
      sprintf('pd.read_csv("%s")', path)
    } else {
      sprintf('pd.read_csv("%s", sep="\\s+", engine="python")', path)
    }
    lines <- c(lines, sprintf("%s = %s", out_name, reader))
    notes <- c(notes, "INFILE/INPUT was approximated with pandas.read_csv. Informats, column pointers, and fixed-width layouts need a manual read_fwf.")
    body <- body[!grepl("^(infile|input)\\s+", body, ignore.case = TRUE)]
  } else {
    lines <- c(lines, sprintf("%s = pd.DataFrame()", out_name))
    notes <- c(notes, "DATA step without SET/MERGE/INFILE started from an empty DataFrame.")
  }

  keep_vars <- character()
  drop_vars <- character()
  where_expr <- NULL

  for (stmt in body) {
    st <- trimws(stmt)
    st <- sub(";\\s*$", "", st)
    low <- tolower(st)
    if (low %in% c("run", "quit")) next
    if (grepl("^set\\s+", low)) next
    if (grepl("^/\\*", st) || grepl("^__COMMENT_", st) || grepl("^#", st)) {
      lines <- c(lines, st)
      next
    }
    if (grepl("^keep\\s+", low)) {
      keep_vars <- split_vars(sub("^keep\\s+", "", st, ignore.case = TRUE))
      next
    }
    if (grepl("^drop\\s+", low)) {
      drop_vars <- split_vars(sub("^drop\\s+", "", st, ignore.case = TRUE))
      next
    }
    if (grepl("^where\\s+", low)) {
      where_expr <- sub("^where\\s+", "", st, ignore.case = TRUE)
      lines <- c(lines, sprintf("%s = %s.query(\"%s\")", out_name, out_name, py_expr(where_expr)))
      notes <- c(notes, "WHERE was passed to DataFrame.query. SAS missing-value logic may differ.")
      next
    }
    if (grepl("^rename\\s+", low)) {
      pairs <- sub("^rename\\s+", "", st, ignore.case = TRUE)
      maps <- parse_rename(pairs)
      if (length(maps)) {
        lines <- c(lines, sprintf("%s = %s.rename(columns=%s)", out_name, out_name, py_dict(maps)))
      }
      next
    }
    if (grepl("^length\\s+", low) || grepl("^format\\s+", low) || grepl("^informat\\s+", low) || grepl("^label\\s+", low) || grepl("^attrib\\s+", low)) {
      lines <- c(lines, sprintf("# SAS declaration not applied automatically: %s", st))
      notes <- c(notes, sprintf("Declaration skipped (no direct pandas equivalent): %s", st))
      next
    }
    if (grepl("^if\\s+.+=\\s*then\\s+delete", low) || grepl("^if\\s+.+=\\s*then\\s+do;\\s*delete", low)) {
      cond <- sub("^if\\s+", "", st, ignore.case = TRUE)
      cond <- sub("\\s+then\\s+delete.*$", "", cond, ignore.case = TRUE)
      lines <- c(lines, sprintf("%s = %s.loc[~( %s )].copy()", out_name, out_name, py_mask(cond, out_name)))
      next
    }
    if (grepl("^if\\s+.+=\\s*then\\s+output", low)) {
      notes <- c(notes, "Conditional OUTPUT was not split into multiple frames. Review control flow.")
      lines <- c(lines, sprintf("# REVIEW output control: %s", st))
      next
    }
    if (grepl("^if\\s+", low) && grepl("\\bthen\\b", low)) {
      lines <- c(lines, translate_if(st, out_name))
      next
    }
    if (grepl("^else\\s+if\\s+", low) || grepl("^else\\s+", low)) {
      lines <- c(lines, sprintf("# REVIEW else-branch (attach to previous np.where/np.select): %s", st))
      notes <- c(notes, "ELSE / ELSE IF was not chained automatically. Prefer np.select for multi-branch recodes.")
      next
    }
    if (grepl("^do\\s+", low) || low %in% c("end", "end.")) {
      lines <- c(lines, sprintf("# SAS DO-block boundary: %s", st))
      notes <- c(notes, "DO/END groups are not translated as Python blocks. Flatten the logic or rewrite as a function.")
      next
    }
    if (grepl("=", st, fixed = TRUE) && !grepl("^(proc|data|set|merge)\\s", low)) {
      parts <- strsplit(st, "=", fixed = TRUE)[[1]]
      lhs <- trimws(parts[1])
      rhs <- trimws(paste(parts[-1], collapse = "="))
      if (grepl("^[A-Za-z_][A-Za-z0-9_]*$", lhs)) {
        lines <- c(lines, sprintf('%s["%s"] = %s', out_name, lhs, py_expr(rhs, out_name)))
        next
      }
    }
    lines <- c(lines, sprintf("# REVIEW unparsed DATA step statement: %s", st))
    notes <- c(notes, sprintf("Unparsed DATA step statement: %s", st))
  }

  if (length(drop_vars)) {
    lines <- c(lines, sprintf("%s = %s.drop(columns=%s)", out_name, out_name, py_list(drop_vars)))
  }
  if (length(keep_vars)) {
    lines <- c(lines, sprintf("%s = %s.loc[:, %s].copy()", out_name, out_name, py_list(keep_vars)))
  }
  list(code = lines, notes = unique(notes))
}

parse_rename <- function(text) {
  pairs <- strsplit(text, "\\s+")[[1]]
  maps <- character()
  for (p in pairs) {
    if (!grepl("=", p, fixed = TRUE)) next
    bits <- strsplit(p, "=", fixed = TRUE)[[1]]
    if (length(bits) == 2) maps[trimws(bits[2])] <- trimws(bits[1])
  }
  maps
}

py_dict <- function(named) {
  paste0("{", paste(sprintf('"%s": "%s"', names(named), named), collapse = ", "), "}")
}

py_expr <- function(expr, df = NULL) {
  e <- trimws(expr)
  e <- gsub("\\s+", " ", e)
  e <- gsub("\\bne\\b", "!=", e, ignore.case = TRUE)
  e <- gsub("\\beq\\b", "==", e, ignore.case = TRUE)
  e <- gsub("\\bgt\\b", ">", e, ignore.case = TRUE)
  e <- gsub("\\blt\\b", "<", e, ignore.case = TRUE)
  e <- gsub("\\bge\\b", ">=", e, ignore.case = TRUE)
  e <- gsub("\\ble\\b", "<=", e, ignore.case = TRUE)
  e <- gsub("\\^=", "!=", e)
  e <- gsub("\\bnot\\s*=", "!=", e, ignore.case = TRUE)
  e <- gsub("\\band\\b", "&", e, ignore.case = TRUE)
  e <- gsub("\\bor\\b", "|", e, ignore.case = TRUE)
  e <- gsub("\\**", "**", e, fixed = TRUE)
  e <- gsub("(?i)\\bmissing\\(([^)]+)\\)", "\\1.isna()", e, perl = TRUE)
  e <- gsub("(?i)\\bnmiss\\(([^)]+)\\)", "\\1.isna().sum()", e, perl = TRUE)
  e <- gsub("(?i)\\bsum\\(([^)]+)\\)", "np.nansum([\\1])", e, perl = TRUE)
  e <- gsub("(?i)\\bmean\\(([^)]+)\\)", "np.nanmean([\\1])", e, perl = TRUE)
  e <- gsub("(?i)\\bint\\(([^)]+)\\)", "np.trunc(\\1)", e, perl = TRUE)
  e <- gsub("(?i)\\bround\\(([^),]+),\\s*([^)]+)\\)", "np.round(\\1, \\2)", e, perl = TRUE)
  e <- gsub("(?i)\\bupcase\\(([^)]+)\\)", "\\1.str.upper()", e, perl = TRUE)
  e <- gsub("(?i)\\blowcase\\(([^)]+)\\)", "\\1.str.lower()", e, perl = TRUE)
  e <- gsub("(?i)\\bstrip\\(([^)]+)\\)", "\\1.str.strip()", e, perl = TRUE)
  e <- gsub("(?i)\\bsubstr\\(([^,]+),\\s*([^,]+),\\s*([^)]+)\\)", "\\1.str.slice(int(\\2)-1, int(\\2)-1+int(\\3))", e, perl = TRUE)
  e <- gsub("(?i)\\bcats\\(([^)]+)\\)", "(\\1).astype(str).agg(''.join, axis=1)", e, perl = TRUE)
  e <- gsub("(?i)\\btoday\\(\\)", "pd.Timestamp.today().normalize()", e, perl = TRUE)
  # A bare SAS missing dot, not a decimal and not a method/name separator.
  e <- gsub("(^|[^A-Za-z0-9_])\\.(?![0-9A-Za-z_])", "\\1np.nan", e, perl = TRUE)
  e
}

py_mask <- function(cond, df) {
  e <- py_expr(cond, df)
  # Qualify bare names lightly: leave as query-like expression using backticks if needed.
  e
}

translate_if <- function(st, df) {
  # if <cond> then <assign or action>
  m <- regexec("^if\\s+(.+?)\\s+then\\s+(.+)$", st, ignore.case = TRUE)
  mm <- regmatches(st, m)[[1]]
  if (length(mm) < 3) return(sprintf("# REVIEW if: %s", st))
  cond <- mm[2]
  action <- trimws(mm[3])
  action <- sub(";\\s*$", "", action)
  if (grepl("=", action, fixed = TRUE) && !grepl("(?i)^(call|output|delete|do)\\b", action, perl = TRUE)) {
    bits <- strsplit(action, "=", fixed = TRUE)[[1]]
    lhs <- trimws(bits[1])
    rhs <- trimws(paste(bits[-1], collapse = "="))
    if (grepl("^[A-Za-z_][A-Za-z0-9_]*$", lhs)) {
      return(sprintf(
        '%s["%s"] = np.where(%s, %s, %s["%s"] if "%s" in %s.columns else np.nan)',
        df, lhs, py_mask(cond, df), py_expr(rhs, df), df, lhs, lhs, df
      ))
    }
  }
  if (grepl("^(?i)delete$", action)) {
    return(sprintf("%s = %s.loc[~( %s )].copy()", df, df, py_mask(cond, df)))
  }
  sprintf("# REVIEW if-action: %s", st)
}

translate_proc <- function(statements) {
  notes <- character()
  header <- statements[1]
  rest <- statements[-1]
  rest <- rest[!grepl("^(run|quit)\\s*;?$", rest, ignore.case = TRUE)]
  kind <- tolower(sub("^proc\\s+([A-Za-z0-9_]+).*", "\\1", header, ignore.case = TRUE))
  data_m <- regexec("data\\s*=\\s*([^\\s;]+)", header, ignore.case = TRUE)
  data_mm <- regmatches(header, data_m)[[1]]
  data_name <- if (length(data_mm) >= 2) py_ident(data_mm[2]) else "df"
  out_m <- regexec("out\\s*=\\s*([^\\s;]+)", paste(c(header, rest), collapse = " "), ignore.case = TRUE)
  out_mm <- regmatches(paste(c(header, rest), collapse = " "), out_m)[[1]]
  out_name <- if (length(out_mm) >= 2) py_ident(out_mm[2]) else NULL

  opt_text <- paste(rest, collapse = "\n")

  if (kind == "import") {
    fm <- regexec("datafile\\s*=\\s*(\"[^\"]+\"|'[^']+'|\\S+)", header, ignore.case = TRUE)
    fmm <- regmatches(header, fm)[[1]]
    if (length(fmm) < 2) {
      fm <- regexec("datafile\\s*=\\s*(\"[^\"]+\"|'[^']+'|\\S+)", opt_text, ignore.case = TRUE)
      fmm <- regmatches(opt_text, fm)[[1]]
    }
    path <- if (length(fmm) >= 2) unquote(fmm[2]) else "file.csv"
    dbms <- tolower(sub(".*dbms\\s*=\\s*([A-Za-z0-9]+).*", "\\1", paste(header, opt_text), ignore.case = TRUE))
    dest <- out_name %||% "imported"
    reader <- if (dbms %in% c("xlsx", "excel")) {
      sprintf('pd.read_excel("%s")', path)
    } else if (dbms %in% c("tab", "dlm")) {
      sprintf('pd.read_csv("%s", sep="\\t")', path)
    } else {
      sprintf('pd.read_csv("%s")', path)
    }
    notes <- c(notes, "PROC IMPORT guessing (types, guessingrows) is not reproduced. Pass dtype/parse_dates if SAS guessed differently.")
    return(list(code = sprintf("%s = %s", dest, reader), notes = notes))
  }

  if (kind == "export") {
    fm <- regexec("outfile\\s*=\\s*(\"[^\"]+\"|'[^']+'|\\S+)", paste(header, opt_text), ignore.case = TRUE)
    fmm <- regmatches(paste(header, opt_text), fm)[[1]]
    path <- if (length(fmm) >= 2) unquote(fmm[2]) else "export.csv"
    dbms <- tolower(sub(".*dbms\\s*=\\s*([A-Za-z0-9]+).*", "\\1", paste(header, opt_text), ignore.case = TRUE))
    writer <- if (dbms %in% c("xlsx", "excel")) {
      sprintf('%s.to_excel("%s", index=False)', data_name, path)
    } else {
      sprintf('%s.to_csv("%s", index=False)', data_name, path)
    }
    return(list(code = writer, notes = "PROC EXPORT labels/formats are not written to the file."))
  }

  if (kind == "print") {
    obs <- NULL
    om <- regexec("obs\\s*=\\s*([0-9]+)", header, ignore.case = TRUE)
    omm <- regmatches(header, om)[[1]]
    if (length(omm) >= 2) obs <- omm[2]
    vars <- character()
    vm <- rest[grepl("^var\\s+", rest, ignore.case = TRUE)]
    if (length(vm)) vars <- split_vars(sub("^var\\s+", "", vm[1], ignore.case = TRUE))
    expr <- data_name
    if (length(vars)) expr <- sprintf("%s.loc[:, %s]", data_name, py_list(vars))
    if (!is.null(obs)) expr <- sprintf("%s.head(%s)", expr, obs)
    return(list(code = expr, notes = "PROC PRINT display options (labels, formats, BY pages) are not reproduced."))
  }

  if (kind == "sort") {
    by_stmt <- rest[grepl("^by\\s+", rest, ignore.case = TRUE)]
    keys <- character()
    ascending <- logical()
    if (length(by_stmt)) {
      raw <- sub("^by\\s+", "", by_stmt[1], ignore.case = TRUE)
      tokens <- strsplit(trimws(raw), "\\s+")[[1]]
      desc <- FALSE
      for (tok in tokens) {
        if (tolower(tok) == "descending") { desc <- TRUE; next }
        keys <- c(keys, tok)
        ascending <- c(ascending, !desc)
        desc <- FALSE
      }
    }
    dest <- out_name %||% data_name
    nodup <- grepl("nodupkey|nodup", header, ignore.case = TRUE) || any(grepl("nodupkey|nodup", rest, ignore.case = TRUE))
    if (!length(keys)) {
      return(list(code = sprintf("%s = %s.copy()  # REVIEW: PROC SORT without BY", dest, data_name),
                  notes = "PROC SORT had no BY list."))
    }
    line <- sprintf("%s = %s.sort_values(%s, ascending=%s)", dest, data_name, py_list(keys), py_bool_list(ascending))
    if (nodup) {
      line <- paste0(line, sprintf("\n%s = %s.drop_duplicates(subset=%s, keep='first')", dest, dest, py_list(keys)))
      notes <- c(notes, "NODUPKEY keeps the first row per BY key after sorting. Confirm that matches the SAS order you need.")
    }
    return(list(code = line, notes = notes))
  }

  if (kind %in% c("means", "summary", "univariate")) {
    class_stmt <- rest[grepl("^class\\s+", rest, ignore.case = TRUE)]
    var_stmt <- rest[grepl("^var\\s+", rest, ignore.case = TRUE)]
    class_vars <- if (length(class_stmt)) split_vars(sub("^class\\s+", "", class_stmt[1], ignore.case = TRUE)) else character()
    var_vars <- if (length(var_stmt)) split_vars(sub("^var\\s+", "", var_stmt[1], ignore.case = TRUE)) else character()
    stats <- parse_means_stats(header)
    if (!length(stats)) stats <- c("count", "mean", "std", "min", "max")
    dest <- out_name %||% "summary"
    target <- if (length(var_vars)) sprintf("%s.loc[:, %s]", data_name, py_list(var_vars)) else data_name
    if (length(class_vars)) {
      line <- sprintf("%s = %s.groupby(%s, dropna=False).agg(%s).reset_index()", dest, target, py_list(class_vars), py_list(stats))
    } else {
      line <- sprintf("%s = %s.agg(%s)", dest, target, py_list(stats))
    }
    notes <- c(notes, "SAS PROC MEANS names and NMISS differ from pandas agg labels. N excludes missing; pandas count does too for numeric agg.")
    if (kind == "univariate") notes <- c(notes, "PROC UNIVARIATE moments/plots are only roughly covered by describe().")
    return(list(code = line, notes = notes))
  }

  if (kind == "freq") {
    tables <- rest[grepl("^tables\\s+", rest, ignore.case = TRUE)]
    if (!length(tables)) {
      return(list(code = sprintf("%s.value_counts()", data_name), notes = "PROC FREQ without TABLES."))
    }
    spec <- sub("^tables\\s+", "", tables[1], ignore.case = TRUE)
    spec <- sub("\\s*/.*$", "", spec)
    dims <- strsplit(spec, "\\*")[[1]]
    dims <- trimws(dims)
    dest <- out_name %||% "freq"
    if (length(dims) == 1) {
      line <- sprintf("%s = %s[%s].value_counts(dropna=False).rename_axis(%s).reset_index(name='COUNT')", dest, data_name, sprintf('"%s"', dims[1]), sprintf('"%s"', dims[1]))
    } else {
      line <- sprintf("%s = pd.crosstab(%s, dropna=False)", dest, paste(sprintf('%s["%s"]', data_name, dims), collapse = ", "))
    }
    notes <- c(notes, "PROC FREQ percent/row/col options are not all reproduced. Add normalize= to crosstab or value_counts if you need shares.")
    return(list(code = line, notes = notes))
  }

  if (kind == "sql") {
    sql <- paste(rest, collapse = "\n")
    sql <- gsub("\\s+", " ", sql)
    return(translate_proc_sql(sql, notes))
  }

  if (kind == "transpose") {
    return(list(
      code = sprintf("%s = %s.melt(id_vars=%s)  # REVIEW: PROC TRANSPOSE var/by/id mapping", out_name %||% "transposed", data_name, "[]"),
      notes = "PROC TRANSPOSE was sketched with melt. ID, BY, and VAR need a manual pivot/melt."
    ))
  }

  if (kind == "contents") {
    return(list(
      code = sprintf("pd.DataFrame({'column': %s.columns, 'dtype': %s.dtypes.astype(str)})", data_name, data_name),
      notes = "PROC CONTENTS metadata (labels, formats, engine) is not on a pandas DataFrame unless you stored it yourself."
    ))
  }

  if (kind == "datasets") {
    return(list(
      code = "# PROC DATASETS library management has no pandas equivalent.\n# Use pathlib / os to delete files, or del to drop a DataFrame.",
      notes = "PROC DATASETS is session/library administration, not a DataFrame transform."
    ))
  }

  list(
    code = sprintf("# REVIEW unsupported procedure: %s\n# %s", kind, paste(statements, collapse = " | ")),
    notes = sprintf("PROC %s is not in the learner translator yet.", toupper(kind))
  )
}

parse_means_stats <- function(header) {
  known <- c("n", "mean", "median", "std", "min", "max", "sum", "var", "nmiss", "p1", "p5", "p10", "p25", "p50", "p75", "p90", "p95", "p99")
  map <- c(n = "count", mean = "mean", median = "median", std = "std", min = "min", max = "max",
           sum = "sum", var = "var", nmiss = "count", p50 = "median", p25 = "quantile", p75 = "quantile")
  tokens <- strsplit(tolower(header), "[^a-z0-9]+")[[1]]
  hit <- tokens[tokens %in% known]
  if (!length(hit)) return(character())
  out <- unique(unname(map[hit]))
  out[!is.na(out)]
}

py_bool_list <- function(x) {
  paste0("[", paste(ifelse(x, "True", "False"), collapse = ", "), "]")
}

translate_proc_sql <- function(sql, notes) {
  notes <- c(notes, "PROC SQL was translated heuristically. Joins, subqueries, and calculated columns often need a manual rewrite.")
  create <- regexec("create\\s+table\\s+([^\\s]+)\\s+as\\s+(.+)$", sql, ignore.case = TRUE)
  cm <- regmatches(sql, create)[[1]]
  dest <- "sql_out"
  body <- sql
  if (length(cm) >= 3) {
    dest <- py_ident(cm[2])
    body <- cm[3]
  }
  sel <- regexec("select\\s+(.+?)\\s+from\\s+([^\\s]+)(.*)$", body, ignore.case = TRUE)
  sm <- regmatches(body, sel)[[1]]
  if (length(sm) < 3) {
    return(list(code = sprintf("# REVIEW PROC SQL:\n# %s", sql), notes = notes))
  }
  select_list <- sm[2]
  table <- py_ident(sm[3])
  tail <- if (length(sm) >= 4) sm[4] else ""
  where <- sub(".*\\bwhere\\s+(.+?)(\\s+group\\s+by|\\s+order\\s+by|\\s+having|$)", "\\1", tail, ignore.case = TRUE)
  has_where <- grepl("\\bwhere\\b", tail, ignore.case = TRUE)
  group <- sub(".*\\bgroup\\s+by\\s+(.+?)(\\s+having|\\s+order\\s+by|$)", "\\1", tail, ignore.case = TRUE)
  has_group <- grepl("\\bgroup\\s+by\\b", tail, ignore.case = TRUE)
  order <- sub(".*\\border\\s+by\\s+(.+)$", "\\1", tail, ignore.case = TRUE)
  has_order <- grepl("\\border\\s+by\\b", tail, ignore.case = TRUE)

  lines <- character()
  cur <- table
  if (has_where && nzchar(trimws(where)) && !grepl("^group\\s+by", where, ignore.case = TRUE)) {
    lines <- c(lines, sprintf('%s = %s.query("%s")', dest, cur, py_expr(trimws(where))))
    cur <- dest
  }
  if (has_group) {
    keys <- split_vars(group)
    lines <- c(lines, sprintf("%s = %s.groupby(%s, dropna=False).agg('mean').reset_index()  # REVIEW aggregates in SELECT", dest, cur, py_list(keys)))
    notes <- c(notes, sprintf("SELECT list was not mapped column-by-column: %s", select_list))
    cur <- dest
  } else if (trimws(tolower(select_list)) != "*") {
    cols <- split_vars(gsub("\\bas\\s+\\w+", "", select_list, ignore.case = TRUE))
    cols <- cols[!tolower(cols) %in% c("as", "distinct")]
    if (length(cols) && !any(grepl("\\(", cols))) {
      lines <- c(lines, sprintf("%s = %s.loc[:, %s].copy()", dest, cur, py_list(cols)))
      cur <- dest
    } else {
      lines <- c(lines, sprintf("%s = %s.copy()  # REVIEW select list: %s", dest, cur, select_list))
      notes <- c(notes, "SELECT expressions (functions, aliases) were not rewritten.")
    }
  } else {
    lines <- c(lines, sprintf("%s = %s.copy()", dest, cur))
  }
  if (has_order) {
    desc <- grepl("desc", order, ignore.case = TRUE)
    keys <- split_vars(gsub("(?i)\\basc\\b|\\bdesc\\b", "", order, perl = TRUE))
    lines <- c(lines, sprintf("%s = %s.sort_values(%s, ascending=%s)", dest, dest, py_list(keys), if (desc) "False" else "True"))
  }
  list(code = paste(lines, collapse = "\n"), notes = unique(notes))
}

sas_to_python <- function(code, add_imports = TRUE) {
  if (!nzchar(trimws(code))) {
    return(list(code = "", notes = "Paste SAS code first.", warnings = character()))
  }
  stripped <- strip_sas_comments(code)
  statements <- split_sas_statements(stripped$text)
  if (!length(statements)) {
    return(list(code = "", notes = "No SAS statements found.", warnings = character()))
  }
  blocks <- list()
  cur <- character()
  for (stmt in statements) {
    cur <- c(cur, stmt)
    if (grepl("^(run|quit)\\s*;?$", stmt, ignore.case = TRUE) || grepl("^(data|proc)\\s+", stmt, ignore.case = TRUE) && length(cur) > 1) {
      # If a new DATA/PROC starts, close previous unterminated block.
      if (grepl("^(data|proc)\\s+", stmt, ignore.case = TRUE) && length(cur) > 1) {
        prev <- cur[-length(cur)]
        blocks[[length(blocks) + 1L]] <- prev
        cur <- stmt
      }
    }
    if (grepl("^(run|quit)\\s*;?$", stmt, ignore.case = TRUE)) {
      blocks[[length(blocks) + 1L]] <- cur
      cur <- character()
    }
  }
  if (length(cur)) blocks[[length(blocks) + 1L]] <- cur

  chunks <- character()
  notes <- character()
  for (block in blocks) {
    head <- block[1]
    if (grepl("^data\\s+", head, ignore.case = TRUE)) {
      res <- translate_data_step(block)
    } else if (grepl("^proc\\s+", head, ignore.case = TRUE)) {
      res <- translate_proc(block)
    } else if (grepl("^libname\\s+", head, ignore.case = TRUE)) {
      res <- list(code = sprintf("# LIBNAME is a library binding, not a DataFrame.\n# %s\n# Point pandas readers at that folder instead.", head),
                  notes = "LIBNAME was not translated. Use a path in read_csv/read_sas.")
    } else if (grepl("^options\\s+", head, ignore.case = TRUE)) {
      res <- list(code = sprintf("# SAS session option (no pandas equivalent): %s", head),
                  notes = "OPTIONS is session configuration.")
    } else if (grepl("^%macro|^%mend|^%", head, ignore.case = TRUE)) {
      res <- list(code = sprintf("# SAS macro statement. Rewrite as a Python function.\n# %s", head),
                  notes = "Macros do not map 1:1. Wrap repeated logic in a def function.")
    } else {
      res <- list(code = sprintf("# REVIEW: %s", head), notes = sprintf("Unparsed statement: %s", head))
    }
    piece <- if (length(res$code) > 1) paste(res$code, collapse = "\n") else res$code
    chunks <- c(chunks, piece)
    notes <- c(notes, res$notes)
  }
  body <- paste(chunks, collapse = "\n\n")
  body <- restore_comment_tokens(body, stripped$comments, "python")
  header <- if (isTRUE(add_imports)) {
    paste(
      "# Translated by SASPy Learner — pattern based, review before use.",
      "import pandas as pd",
      "import numpy as np",
      "",
      sep = "\n"
    )
  } else {
    "# Translated by SASPy Learner — pattern based, review before use.\n"
  }
  list(
    code = paste0(header, body, "\n"),
    notes = unique(notes[nzchar(notes)]),
    warnings = character()
  )
}

# ---------------------------------------------------------------------------
# Python -> SAS
# ---------------------------------------------------------------------------

python_to_sas <- function(code) {
  if (!nzchar(trimws(code))) {
    return(list(code = "", notes = "Paste Python code first.", warnings = character()))
  }
  notes <- character()
  text <- gsub("\r\n", "\n", code)
  # Drop import lines
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  kept <- lines[!grepl("^\\s*(import |from )", lines)]
  # Join implicit continuations ending with backslash or open paren roughly by collapsing method chains that end with .
  blob <- paste(kept, collapse = "\n")
  # Split on newlines but recombine lines that are only continuation (start with . or are indented method)
  raw_lines <- strsplit(blob, "\n", fixed = TRUE)[[1]]
  stmts <- character()
  buf <- ""
  for (ln in raw_lines) {
    if (!nzchar(trimws(ln)) && !nzchar(buf)) next
    if (grepl("^\\s*#", ln) && !nzchar(buf)) {
      stmts <- c(stmts, ln)
      next
    }
    buf <- if (nzchar(buf)) paste(buf, trimws(ln), sep = " ") else ln
    if (!grepl("\\($|\\.$|,\\s*$|\\\\\\s*$", trimws(ln))) {
      stmts <- c(stmts, trimws(buf))
      buf <- ""
    }
  }
  if (nzchar(buf)) stmts <- c(stmts, trimws(buf))

  out <- character()
  for (st in stmts) {
    if (grepl("^\\s*#", st)) {
      out <- c(out, paste0("/* ", sub("^\\s*#\\s*", "", st), " */"))
      next
    }
    hit <- translate_py_statement(st)
    out <- c(out, hit$code)
    notes <- c(notes, hit$notes)
  }
  list(
    code = paste(c("/* Translated by SASPy Learner — pattern based, review before use. */", out), collapse = "\n"),
    notes = unique(notes[nzchar(notes)]),
    warnings = character()
  )
}

translate_py_statement <- function(st) {
  notes <- character()
  s <- trimws(st)
  s1 <- gsub("\\s+", " ", s)

  m <- regexec("^([A-Za-z_][A-Za-z0-9_]*)\\s*=\\s*pd\\.read_csv\\(\\s*['\"]([^'\"]+)['\"](.*)\\)\\s*$", s1)
  mm <- regmatches(s1, m)[[1]]
  if (length(mm) >= 3) {
    return(list(code = sprintf('proc import datafile="%s" out=work.%s dbms=csv replace;\n  getnames=yes;\nrun;', mm[3], mm[2]),
                notes = "read_csv options (sep, dtype, parse_dates) were not copied into PROC IMPORT."))
  }
  m <- regexec("^([A-Za-z_][A-Za-z0-9_]*)\\s*=\\s*pd\\.read_excel\\(\\s*['\"]([^'\"]+)['\"](.*)\\)\\s*$", s1)
  mm <- regmatches(s1, m)[[1]]
  if (length(mm) >= 3) {
    return(list(code = sprintf('proc import datafile="%s" out=work.%s dbms=xlsx replace;\n  getnames=yes;\nrun;', mm[3], mm[2]),
                notes = "Sheet name and dtype from read_excel were not translated."))
  }
  m <- regexec("^([A-Za-z_][A-Za-z0-9_]*)\\s*=\\s*pd\\.read_sas\\(\\s*['\"]([^'\"]+)['\"](.*)\\)\\s*$", s1)
  mm <- regmatches(s1, m)[[1]]
  if (length(mm) >= 3) {
    return(list(code = sprintf('/* pandas read_sas — point a LIBNAME at the folder and SET the member */\nlibname in sas7bdat "%s";\n/* REVIEW member name */', dirname_guess(mm[3])),
                notes = "read_sas needs a LIBNAME plus the member name inside the file."))
  }

  m <- regexec("^([A-Za-z_][A-Za-z0-9_]*)\\.to_csv\\(\\s*['\"]([^'\"]+)['\"](.*)\\)\\s*$", s1)
  mm <- regmatches(s1, m)[[1]]
  if (length(mm) >= 3) {
    return(list(code = sprintf('proc export data=work.%s outfile="%s" dbms=csv replace;\nrun;', mm[2], mm[3]), notes = character()))
  }
  m <- regexec("^([A-Za-z_][A-Za-z0-9_]*)\\.to_excel\\(\\s*['\"]([^'\"]+)['\"](.*)\\)\\s*$", s1)
  mm <- regmatches(s1, m)[[1]]
  if (length(mm) >= 3) {
    return(list(code = sprintf('proc export data=work.%s outfile="%s" dbms=xlsx replace;\nrun;', mm[2], mm[3]), notes = character()))
  }

  m <- regexec("^([A-Za-z_][A-Za-z0-9_]*)\\s*=\\s*([A-Za-z_][A-Za-z0-9_]*)\\.copy\\(\\s*\\)\\s*$", s1)
  mm <- regmatches(s1, m)[[1]]
  if (length(mm) >= 3) {
    return(list(code = sprintf("data work.%s;\n  set work.%s;\nrun;", mm[2], mm[3]), notes = character()))
  }

  m <- regexec("^([A-Za-z_][A-Za-z0-9_]*)\\s*=\\s*([A-Za-z_][A-Za-z0-9_]*)\\.head\\(\\s*([0-9]+)\\s*\\)\\s*$", s1)
  mm <- regmatches(s1, m)[[1]]
  if (length(mm) >= 4) {
    return(list(code = sprintf("data work.%s;\n  set work.%s(obs=%s);\nrun;", mm[2], mm[3], mm[4]), notes = character()))
  }

  m <- regexec("^([A-Za-z_][A-Za-z0-9_]*)\\s*=\\s*([A-Za-z_][A-Za-z0-9_]*)\\.query\\(\\s*['\"](.+)['\"]\\s*\\)\\s*$", s1)
  mm <- regmatches(s1, m)[[1]]
  if (length(mm) >= 4) {
    return(list(code = sprintf("data work.%s;\n  set work.%s;\n  where %s;\nrun;", mm[2], mm[3], sas_expr(mm[4])),
                notes = "DataFrame.query strings can contain Python operators. The WHERE clause may need quotes and SAS mnemonics checked."))
  }

  m <- regexec("^([A-Za-z_][A-Za-z0-9_]*)\\s*=\\s*([A-Za-z_][A-Za-z0-9_]*)\\.sort_values\\(\\s*\\[([^\\]]+)\\](.*)\\)\\s*$", s1)
  mm <- regmatches(s1, m)[[1]]
  if (length(mm) >= 4) {
    keys <- extract_strings(mm[4])
    asc <- !grepl("ascending\\s*=\\s*False", mm[5], ignore.case = TRUE)
    by <- if (asc) paste(keys, collapse = " ") else paste("descending", keys, collapse = " ")
    if (!asc && length(keys) > 1 && grepl("ascending\\s*=\\s*\\[", mm[5])) {
      notes <- c(notes, "Mixed ascending lists were flattened. Check DESCENDING on each BY variable.")
    }
    return(list(code = sprintf("proc sort data=work.%s out=work.%s;\n  by %s;\nrun;", mm[3], mm[2], by), notes = notes))
  }

  m <- regexec("^([A-Za-z_][A-Za-z0-9_]*)\\s*=\\s*([A-Za-z_][A-Za-z0-9_]*)\\.drop_duplicates\\((.*)\\)\\s*$", s1)
  mm <- regmatches(s1, m)[[1]]
  if (length(mm) >= 3) {
    keys <- extract_strings(mm[4])
    by <- if (length(keys)) paste(keys, collapse = " ") else "_ALL_"
    return(list(code = sprintf("proc sort data=work.%s out=work.%s nodupkey;\n  by %s;\nrun;", mm[3], mm[2], by),
                notes = "drop_duplicates became PROC SORT NODUPKEY. Subset columns and keep= may differ."))
  }

  m <- regexec("^([A-Za-z_][A-Za-z0-9_]*)\\s*=\\s*([A-Za-z_][A-Za-z0-9_]*)\\.drop\\(columns\\s*=\\s*\\[([^\\]]+)\\](.*)\\)\\s*$", s1)
  mm <- regmatches(s1, m)[[1]]
  if (length(mm) >= 4) {
    cols <- extract_strings(mm[4])
    return(list(code = sprintf("data work.%s;\n  set work.%s;\n  drop %s;\nrun;", mm[2], mm[3], paste(cols, collapse = " ")), notes = character()))
  }

  m <- regexec("^([A-Za-z_][A-Za-z0-9_]*)\\s*=\\s*([A-Za-z_][A-Za-z0-9_]*)\\.rename\\(columns\\s*=\\s*\\{([^}]+)\\}(.*)\\)\\s*$", s1)
  mm <- regmatches(s1, m)[[1]]
  if (length(mm) >= 4) {
    pairs <- parse_py_dict(mm[4])
    ren <- paste(sprintf("%s=%s", names(pairs), pairs), collapse = " ")
    return(list(code = sprintf("data work.%s;\n  set work.%s;\n  rename %s;\nrun;", mm[2], mm[3], ren),
                notes = "pandas rename is new=old in the dict; SAS RENAME is old=new. The mapping was flipped."))
  }

  m <- regexec('^([A-Za-z_][A-Za-z0-9_]*)\\[\\s*[\'"]([^\'"]+)[\'"]\\s*\\]\\s*=\\s*(.+)$', s1)
  mm <- regmatches(s1, m)[[1]]
  if (length(mm) >= 4) {
    rhs <- mm[4]
    if (grepl("^np\\.where\\(", rhs)) {
      wm <- regexec("^np\\.where\\(\\s*(.+),\\s*(.+),\\s*(.+)\\)\\s*$", rhs)
      wmm <- regmatches(rhs, wm)[[1]]
      if (length(wmm) >= 4) {
        return(list(
          code = sprintf("data work.%s;\n  set work.%s;\n  if %s then %s = %s;\n  else %s = %s;\nrun;", mm[2], mm[2], sas_expr(wmm[2]), mm[3], sas_expr(wmm[3]), mm[3], sas_expr(wmm[4])),
          notes = "np.where else-branch may reference the same column; SAS PDV logic can differ if the column is new."
        ))
      }
    }
    return(list(
      code = sprintf("data work.%s;\n  set work.%s;\n  %s = %s;\nrun;", mm[2], mm[2], mm[3], sas_expr(rhs)),
      notes = character()
    ))
  }

  m <- regexec("^([A-Za-z_][A-Za-z0-9_]*)\\s*=\\s*([A-Za-z_][A-Za-z0-9_]*)\\.merge\\(\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*,(.*)\\)\\s*$", s1)
  mm <- regmatches(s1, m)[[1]]
  if (length(mm) >= 4) {
    keys <- extract_strings(mm[5])
    how <- sub('.*how\\s*=\\s*[\'"]([A-Za-z]+)[\'"].*', "\\1", mm[5])
    if (identical(how, mm[5])) how <- "inner"
    by <- if (length(keys)) paste(keys, collapse = " ") else "/* REVIEW key */ id"
    extra <- ""
    if (tolower(how) == "left") extra <- "  /* pandas how='left': consider IN= flags and a subsetting IF */\n"
    if (tolower(how) %in% c("inner", "right", "outer", "left")) {
      notes <- c(notes, sprintf("pandas how='%s' is not the same as a SAS MERGE. A MATCH-MERGE was emitted; filter with IN= for inner/left joins.", how))
    }
    return(list(
      code = sprintf("data work.%s;\n  merge work.%s work.%s;\n  by %s;\n%srun;", mm[2], mm[3], mm[4], by, extra),
      notes = notes
    ))
  }

  m <- regexec("^([A-Za-z_][A-Za-z0-9_]*)\\s*=\\s*([A-Za-z_][A-Za-z0-9_]*)\\.groupby\\(\\s*\\[([^\\]]+)\\](.*)\\.agg\\(\\s*\\[([^\\]]+)\\](.*)\\)\\s*(?:\\.reset_index\\(\\s*\\))?\\s*$", s1)
  mm <- regmatches(s1, m)[[1]]
  if (length(mm) >= 4) {
    keys <- extract_strings(mm[4])
    stats <- extract_strings(mm[6])
    sas_stats <- py_stats_to_sas(stats)
    return(list(
      code = sprintf("proc means data=work.%s %s noprint;\n  class %s;\n  output out=work.%s %s;\nrun;", mm[3], sas_stats, paste(keys, collapse = " "), mm[2], paste(sprintf("%s=", sas_stats_keywords(stats)), collapse = " ")),
      notes = "groupby.agg column selection was not fully recovered. Add a VAR statement."
    ))
  }

  m <- regexec("^([A-Za-z_][A-Za-z0-9_]*)\\s*=\\s*([A-Za-z_][A-Za-z0-9_]*)\\.groupby\\(\\s*['\"]([^'\"]+)['\"]\\s*\\)\\[\\s*['\"]([^'\"]+)['\"]\\s*\\]\\.([A-Za-z_]+)\\(\\s*\\)", s1)
  mm <- regmatches(s1, m)[[1]]
  if (length(mm) >= 6) {
    return(list(
      code = sprintf("proc means data=work.%s %s;\n  class %s;\n  var %s;\n  output out=work.%s %s=;\nrun;", mm[3], py_stats_to_sas(mm[6]), mm[4], mm[5], mm[2], py_stats_to_sas(mm[6])),
      notes = character()
    ))
  }

  m <- regexec("^([A-Za-z_][A-Za-z0-9_]*)\\s*=\\s*([A-Za-z_][A-Za-z0-9_]*)\\[\\s*['\"]([^'\"]+)['\"]\\s*\\]\\.value_counts\\((.*)\\)", s1)
  mm <- regmatches(s1, m)[[1]]
  if (length(mm) >= 4) {
    return(list(code = sprintf("proc freq data=work.%s;\n  tables %s / missing;\nrun;", mm[3], mm[4]), notes = "value_counts result table shape differs from PROC FREQ listing."))
  }

  m <- regexec("^([A-Za-z_][A-Za-z0-9_]*)\\s*=\\s*pd\\.crosstab\\(\\s*([A-Za-z_][A-Za-z0-9_]*)\\[\\s*['\"]([^'\"]+)['\"]\\s*\\]\\s*,\\s*\\2\\[\\s*['\"]([^'\"]+)['\"]\\s*\\](.*)\\)", s1)
  mm <- regmatches(s1, m)[[1]]
  if (length(mm) >= 5) {
    return(list(code = sprintf("proc freq data=work.%s;\n  tables %s * %s / missing;\nrun;", mm[3], mm[4], mm[5]), notes = character()))
  }

  m <- regexec("^([A-Za-z_][A-Za-z0-9_]*)\\s*=\\s*([A-Za-z_][A-Za-z0-9_]*)\\.describe\\(\\s*\\)\\s*$", s1)
  mm <- regmatches(s1, m)[[1]]
  if (length(mm) >= 3) {
    return(list(code = sprintf("proc means data=work.%s n mean std min p25 median p75 max;\nrun;", mm[3]), notes = character()))
  }

  m <- regexec("^([A-Za-z_][A-Za-z0-9_]*)\\s*=\\s*([A-Za-z_][A-Za-z0-9_]*)\\.fillna\\(\\s*(.+)\\)\\s*$", s1)
  mm <- regmatches(s1, m)[[1]]
  if (length(mm) >= 4) {
    return(list(
      code = sprintf("data work.%s;\n  set work.%s;\n  /* REVIEW: fillna(%s) — apply to the columns you mean */\n  array nums[*] _numeric_;\n  do i = 1 to dim(nums);\n    if missing(nums[i]) then nums[i] = %s;\n  end;\n  drop i;\nrun;", mm[2], mm[3], mm[4], sas_expr(mm[4])),
      notes = "fillna on a subset of columns should become a targeted IF MISSING assignment, not necessarily an array over all numeric columns."
    ))
  }

  m <- regexec("^([A-Za-z_][A-Za-z0-9_]*)\\s*=\\s*([A-Za-z_][A-Za-z0-9_]*)\\.loc\\[([^\\]]+)\\]\\s*$", s1)
  mm <- regmatches(s1, m)[[1]]
  if (length(mm) >= 4) {
    return(list(
      code = sprintf("data work.%s;\n  set work.%s;\n  if %s;\nrun;", mm[2], mm[3], sas_expr(mm[4])),
      notes = ".loc masks were treated as a subsetting IF. Column selectors inside loc need a KEEP/DROP."
    ))
  }

  list(
    code = sprintf("/* REVIEW unparsed Python: %s */", s),
    notes = sprintf("Unparsed Python statement: %s", s)
  )
}

dirname_guess <- function(path) {
  sub("[/\\\\][^/\\\\]+$", "", path)
}

extract_strings <- function(text) {
  m <- gregexpr("['\"]([A-Za-z_][A-Za-z0-9_]*)['\"]", text, perl = TRUE)
  hit <- regmatches(text, m)[[1]]
  gsub("['\"]", "", hit)
}

parse_py_dict <- function(text) {
  # "new": "old"  -> we need old=new for SAS, so names are old, values are new? 
  # pandas {"old": "new"} means rename old to new.
  m <- gregexpr("['\"]([A-Za-z_][A-Za-z0-9_]*)['\"]\\s*:\\s*['\"]([A-Za-z_][A-Za-z0-9_]*)['\"]", text, perl = TRUE)
  hit <- regmatches(text, m)[[1]]
  old <- gsub("['\"]([A-Za-z_][A-Za-z0-9_]*)['\"]\\s*:\\s*['\"]([A-Za-z_][A-Za-z0-9_]*)['\"]", "\\1", hit)
  new <- gsub("['\"]([A-Za-z_][A-Za-z0-9_]*)['\"]\\s*:\\s*['\"]([A-Za-z_][A-Za-z0-9_]*)['\"]", "\\2", hit)
  stats::setNames(new, old)
}

py_stats_to_sas <- function(stats) {
  map <- c(count = "n", mean = "mean", median = "median", std = "std", min = "min", max = "max", sum = "sum", var = "var")
  out <- unname(map[tolower(stats)])
  out <- out[!is.na(out)]
  if (!length(out)) "mean" else paste(unique(out), collapse = " ")
}

sas_stats_keywords <- function(stats) {
  strsplit(py_stats_to_sas(stats), " ")[[1]]
}

sas_expr <- function(expr) {
  e <- trimws(expr)
  e <- gsub("\\bTrue\\b", "1", e)
  e <- gsub("\\bFalse\\b", "0", e)
  e <- gsub("\\bNone\\b|\\bnp\\.nan\\b|\\bpd\\.NA\\b", ".", e)
  e <- gsub("(?i)\\.isna\\(\\)", " = .", e, perl = TRUE)
  e <- gsub("(?i)\\.notna\\(\\)", " ne .", e, perl = TRUE)
  e <- gsub("==", "=", e, fixed = TRUE)
  e <- gsub("!=", "ne", e, fixed = TRUE)
  e <- gsub(">=", "ge", e, fixed = TRUE)
  e <- gsub("<=", "le", e, fixed = TRUE)
  e <- gsub("&", "and", e, fixed = TRUE)
  e <- gsub("|", "or", e, fixed = TRUE)
  e <- gsub("\\*\\*", "**", e)
  e <- gsub("\\bdf\\[['\"]([^'\"]+)['\"]\\]", "\\1", e)
  e <- gsub("\\b[A-Za-z_][A-Za-z0-9_]*\\[['\"]([^'\"]+)['\"]\\]", "\\1", e)
  e
}
