## step 1: reading in the R file ##
read_r_file <- function(path) {
  if (!file.exists(path)) {
    stop("File does not exist.")
  }
  
  code <- readLines(path, warn = FALSE)
  return(code)
}
#setwd("C:/Users/Owner/Downloads/STAT 5430/project")
#code_lines <- read_r_file("example_r_file.R")
#print(code_lines)

# Content Translators:
#I/O
translate_io <- function(line) {
  
  # read.csv()
  if (grepl("read\\.csv", line)) {
    line <- gsub("read\\.csv", "pd.read_csv", line)
    return(line)
  }
  
  # write.csv()
  if (grepl("write\\.csv", line)) {
    # convert to df.to_csv()
    return(
      sub("write\\.csv\\((.*?),\\s*(.*?)\\)",
          "\\1.to_csv(\\2, index=False)",
          line)
    )
  }
  
  return(line)
}


# Expressions: 
  translate_expression <- function(line) {
    original <- line
    
    # Remove leading/trailing whitespace
    line <- trimws(line)
    
     # Arithmetic Operators
    # power: ^ → **
    line <- gsub("\\^", "**", line)
    
    #logical operators
    # AND, OR, NOT
    line <- gsub("(?<=[^<>=!])&(?![&])", " and ", line, perl = TRUE)
    line <- gsub("(?<=[^<>=!])\\|(?![|])", " or ", line, perl = TRUE)
    line <- gsub("!", "not ", line, perl = TRUE)
    
    # Boolean Values
    line <- gsub("\\bTRUE\\b", "True", line)
    line <- gsub("\\bFALSE\\b", "False", line)
    
    ## null / NA
    line <- gsub("\\bNULL\\b", "None", line)
    line <- gsub("\\bNA\\b", "None", line)
    

    #Comparisons (==, != stay same)
    
    # Sequence Operators (STAT 1601 common)
    # 1:10 → range(1, 11)
    line <- gsub("(\\b[0-9]+):(\\b[0-9]+)",
                 "range(\\1, \\2 + 1)", line)
    
    # Subsetting $
    # x$col → x["col"]
    line <- gsub("\\$([A-Za-z0-9_]+)",
                 "[\"\\1\"]", line)
    
    # Clean extra spaces
    line <- gsub("\\s+", " ", line)
    line <- trimws(line)
    
    return(line)
  }

### Operators: 
translate_operators <- function(line) {
  original <- line
  line <- trimws(line)
    
    # power outputs
    # R: x ^ y → Python: x ** y
  line <- gsub("\\^", "**", line)
    
    #logical operators 
    # & → and   (but NOT &&)
  line <- gsub("(?<=[^&])&(?!&)", " and ", line, perl = TRUE)
    
    # | → or    (but NOT ||)
  line <- gsub("(?<=[^|])\\|(?!\\|)", " or ", line, perl = TRUE)
    
    # ! → not   (only when standalone)
    # R allows: !x, !(x > 3), etc.
  line <- gsub("!\\s*", "not ", line)
    
    # boolean constants 
  line <- gsub("\\bTRUE\\b", "True", line)
  line <- gsub("\\bFALSE\\b", "False", line)
    
    # NULL / NA
  line <- gsub("\\bNULL\\b", "None", line)
  line <- gsub("\\bNA\\b", "None", line)
    
    # clean spacing

  line <- gsub("\\s+", " ", line)
  line <- trimws(line)
  
  return(line)
}



# Assignments
translate_assignment <- function(line) {
  # if the line contains an assignment operator
  if (grepl("<-", line)) {
    
    # "<-" becomes "="
    python_line <- gsub("<-", "=", line)
    
    # removes the spaces 
    python_line <- trimws(python_line)
    
    return(python_line)
  }
  
  return(NULL)
}

### Vector 
translate_vector <- function(line) {
  # handle either "<-" or "=" before c(
  if (grepl("(<-|=)\\s*c\\(", line)) {
    
    # variable name before <- or =
    var_name <- trimws(sub("(<-|=).*", "", line))
    
    # contents of c(...)
    values <- sub(".*c\\((.*)\\).*", "\\1", line)
    
    # python list
    python_line <- paste0(var_name, " = [", values, "]")
    
    return(python_line)
  }
  
  return(NULL) # if not a vector line
}
# loop through the entire file 
vector_translations <- lapply(code_lines, translate_vector)

# remove lines that are not vectors
vector_translations <- vector_translations[!sapply(vector_translations, is.null)]

# print(vector_translations) "nums = [1, 2, 3, 4, 5]"

## Lists
# Detect start of list(
is_list_start <- function(line) {
  grepl("list\\s*\\(", line)
}

# Convert an inner list element: name = value
convert_list_element <- function(line) {
  clean <- trimws(line)
  
  # Remove trailing comma if present
  clean <- sub(",$", "", clean)
  
  # Must contain "=" to be a named list element
  if (!grepl("=", clean)) return(NULL)
  
  parts <- strsplit(clean, "=")[[1]]
  name <- trimws(parts[1])
  value <- trimws(parts[2])
  
  # Convert to Python dict syntax
  python <- paste0('"', name, '": ', value)
  
  return(python)
}

# Main translator for list(...)
translate_list <- function(lines, start_index) {
  
  python_output <- c("{")
  
  i <- start_index + 1
  
  # loop until closing ")"
  while (!grepl("^\\)", trimws(lines[i]))) {
    
    raw_line <- lines[i]
    
    # ignore completely blank lines
    if (nchar(trimws(raw_line)) == 0) {
      python_output <- c(python_output, "")
      i <- i + 1
      next
    }
    
    # Try converting the line
    converted <- convert_list_element(raw_line)
    
    if (!is.null(converted)) {
      python_output <- c(python_output, paste0("    ", converted, ","))
    } else {
      # Fallback: keep raw content (e.g., nested list, nested df)
      python_output <- c(python_output, paste0("    ", raw_line))
    }
    
    i <- i + 1
  }
  
  # Remove trailing comma
  python_output[length(python_output)] <-
    sub(",$", "", python_output[length(python_output)])
  
  # Close dictionary
  python_output <- c(python_output, "}")
  
  return(list(output = python_output, next_index = i))
}


#Structural translators:

# Data frames 
# start of a data.frame(
is_data_frame_start <- function(line) {
  grepl("data\\.frame\\s*\\(", line)
}

# Convert a single column line: name = value
convert_df_column <- function(line) {
  clean <- trimws(line)
  
  # remove trailing comma
  clean <- sub(",$", "", clean)
  
  # must contain "="
  if (!grepl("=", clean)) return(NULL)
  
  parts <- strsplit(clean, "=")[[1]]
  col_name <- trimws(parts[1])
  rhs <- trimws(parts[2])
  
  # convert vector in RHS
  if (grepl("c\\(", rhs)) {
    rhs <- sub("c\\((.*)\\)", "[\\1]", rhs)
  }
  
  return(paste0('"', col_name, '": ', rhs))
}


# Main translator for an entire data.frame block
translate_data_frame <- function(lines, start_index) {
  
  python_output <- c("pd.DataFrame({")
  
  i <- start_index + 1
  
  while (!grepl("^\\)", trimws(lines[i]))) {
    
    raw_line <- lines[i]
    inner <- trimws(raw_line)
    
    # skip blank lines
    if (nchar(inner) == 0) {
      python_output <- c(python_output, "")
      i <- i + 1
      next
    }
    
    # attempt column conversion
    converted <- convert_df_column(inner)
    
    if (!is.null(converted)) {
      
      #### ----- NESTED TRANSLATORS ----- ####
      
      # vector on RHS
      if (grepl("c\\(", converted)) {
        converted <- translate_vector(converted)
      }
      
      # ---- ONLY SAFE TO SPLIT IF STRING ----
      if (is.character(converted) && grepl(":", converted)) {
        
        split <- strsplit(converted, ":")[[1]]
        left <- split[1]
        value <- trimws(split[2])
        
        # translate only the RHS
        value <- translate_operators(value)
        value <- translate_expression(value)
        
        converted <- paste0(left, ": ", value)
      }
      
      python_output <- c(python_output, paste0("    ", converted, ","))
      
    } else {
      # not a column line → keep raw
      python_output <- c(python_output, paste0("    ", raw_line))
    }
    
    i <- i + 1
  }
  
  # clean trailing comma
  python_output[length(python_output)] <-
    sub(",$", "", python_output[length(python_output)])
  
  python_output <- c(python_output, "})")
  
  return(list(output = python_output, next_index = i))
}



#Function:


# Detect the start of an R function
is_function_start <- function(line) {
  grepl("<-\\s*function\\s*\\(", line)
}

# Extract function name and argument list
extract_function_parts <- function(line) {
  clean <- gsub("\\s+", " ", trimws(line))
  
  # name before "<-"
  func_name <- sub("\\s*<-.*", "", clean)
  
  # everything inside function(...)
  args <- sub(".*function\\s*\\((.*)\\).*", "\\1", clean)
  
  return(list(name = func_name, args = args))
}

# Main function translator:
translate_function <- function(lines, start_index) {
  
  start_line <- lines[start_index]
  parts <- extract_function_parts(start_line)
  
  python_output <- c()
  
  # Python function header
  python_output <- c(
    python_output,
    paste0("def ", parts$name, "(", parts$args, "):")
  )
  
  i <- start_index + 1
  
  # Loop until we hit closing "}"
  while (!grepl("^\\}", trimws(lines[i]))) {
    
    raw_line <- lines[i]
    inner <- trimws(raw_line)
    
    # skip blank lines
    if (nchar(inner) == 0) {
      python_output <- c(python_output, "")
      i <- i + 1
      next
    }
    
    # Start with the raw inner line
    new_line <- inner
    
    #### CONTENT TRANSLATORS 
    
    # assignment
    assign_out <- translate_assignment(new_line)
    if (!is.null(assign_out)) new_line <- assign_out
    
    # vector translation
    vec_out <- translate_vector(new_line)
    if (!is.null(vec_out)) new_line <- vec_out
    
    # operators
    new_line <- translate_operators(new_line)
    
    # expressions
    new_line <- translate_expression(new_line)
    
    # I/O
    new_line <- translate_io(new_line)
    
    
    # indent Python body line
    python_output <- c(python_output, paste0("    ", new_line))
    
    i <- i + 1
  }
  
  # Return translated function + last index in source lines
  return(list(output = python_output, next_index = i))
}

#FOR LOOPS

# Detect start of a for-loop
is_for_loop_start <- function(line) {
  grepl("^\\s*for\\s*\\(", line)
}

# Extract variable + sequence from:  for (i in 1:10) {
extract_for_parts <- function(line) {
  clean <- trimws(line)
  
  # grab everything inside parentheses
  inside <- sub("for\\s*\\((.*)\\).*", "\\1", clean)
  
  # split "i in something"
  pieces <- strsplit(inside, "\\s+in\\s+")[[1]]
  
  var <- trimws(pieces[1])
  seq <- trimws(pieces[2])
  
  return(list(var = var, seq = seq))
}

# Main for-loop translator
translate_for_loop <- function(lines, start_index) {
  
  start_line <- lines[start_index]
  parts <- extract_for_parts(start_line)
  
  python_output <- c(
    paste0("for ", parts$var, " in ", parts$seq, ":")
  )
  
  i <- start_index + 1
  
  while (!grepl("^\\}", trimws(lines[i]))) {
    
    raw_line <- lines[i]
    inner <- trimws(raw_line)
    
    if (nchar(inner) == 0) {
      # blank line
      python_output <- c(python_output, "")
      i <- i + 1
      next
    }
    
    new_line <- inner
    
    # Content translators 
    
    # assignment
    assign_out <- translate_assignment(new_line)
    if (!is.null(assign_out)) new_line <- assign_out
    
    # vector
    vec_out <- translate_vector(new_line)
    if (!is.null(vec_out)) new_line <- vec_out
    
    # operators
    new_line <- translate_operators(new_line)
    
    # expressions
    new_line <- translate_expression(new_line)
    
    # I/O (read.csv, write.csv)
    new_line <- translate_io(new_line)
    
    # indent the loop line
    python_output <- c(python_output, paste0("    ", new_line))
    
    i <- i + 1
  }
  
  return(list(output = python_output, next_index = i))
}

# While loops
# Detect start of while-loop
is_while_loop_start <- function(line) {
  grepl("^\\s*while\\s*\\(", line)
}

# Extract the condition from: while (condition) {
extract_while_condition <- function(line) {
  clean <- trimws(line)
  # Extract whatever is inside parentheses
  cond <- sub("while\\s*\\((.*)\\).*", "\\1", clean)
  return(trimws(cond))
}

# Main while-loop translator
translate_while_loop <- function(lines, start_index) {
  
  start_line <- lines[start_index]
  cond <- extract_while_condition(start_line)
  
  # Python header
  python_output <- c(
    paste0("while ", cond, ":")
  )
  
  i <- start_index + 1
  
  # Read until closing brace "}"
  while (!grepl("^\\}", trimws(lines[i]))) {
    
    raw_line <- lines[i]
    inner <- trimws(raw_line)
    
    # blank line inside loop
    if (nchar(inner) == 0) {
      python_output <- c(python_output, "")
      i <- i + 1
      next
    }
    
    new_line <- inner
    
    # Content translators
    
    # Assignments
    assign_out <- translate_assignment(new_line)
    if (!is.null(assign_out)) new_line <- assign_out
    
    # Vector translation
    vec_out <- translate_vector(new_line)
    if (!is.null(vec_out)) new_line <- vec_out
    
    # Operators
    new_line <- translate_operators(new_line)
    
    # Expressions
    new_line <- translate_expression(new_line)
    
    # I/O
    new_line <- translate_io(new_line)
    
    # indent inside while-loop
    python_output <- c(python_output, paste0("    ", new_line))
    
    i <- i + 1
  }
  
  # Return structured block and next index
  return(list(output = python_output, next_index = i))
}

# If else

# Detect patterns
is_if_start <- function(line) {
  grepl("^\\s*if\\s*\\(", line)
}

is_elseif_start <- function(line) {
  grepl("^\\s*else if\\s*\\(", line)
}

is_else_start <- function(line) {
  grepl("^\\s*else\\s*\\{", line)
}

# Extract condition from if(...) or else if(...)
extract_condition <- function(line) {
  cond <- sub(".*\\((.*)\\).*", "\\1", trimws(line))
  return(trimws(cond))
}

# Main translators
translate_if_block <- function(lines, start_index) {
  
  python_output <- c()
  i <- start_index
  
# IF block: 
  if (is_if_start(lines[i])) {
    cond <- extract_condition(lines[i])
    python_output <- c(python_output, paste0("if ", cond, ":"))
  }
  i <- i + 1
  
  # Now handle the body of the IF until we reach a "}"
  while (!grepl("^\\}", trimws(lines[i]))) {
    
    raw_line <- lines[i]
    inner <- trimws(raw_line)
    
    if (nchar(inner) == 0) {
      python_output <- c(python_output, "")
      i <- i + 1
      next
    }
    
    new_line <- inner
    
  #content translators 
    assign_out <- translate_assignment(new_line)
    if (!is.null(assign_out)) new_line <- assign_out
    
    vec_out <- translate_vector(new_line)
    if (!is.null(vec_out)) new_line <- vec_out
    
    new_line <- translate_operators(new_line)
    new_line <- translate_expression(new_line)
    new_line <- translate_io(new_line)
    
    python_output <- c(python_output, paste0("    ", new_line))
    i <- i + 1
  }
  
  # Now i is at the "}" closing the IF
  i <- i + 1
  
  # else if blocks
  while (i <= length(lines) && is_elseif_start(lines[i])) {
    
    cond <- extract_condition(lines[i])
    python_output <- c(python_output, paste0("elif ", cond, ":"))
    
    i <- i + 1
    
    # process elseif body
    while (!grepl("^\\}", trimws(lines[i]))) {
      raw_line <- lines[i]
      inner <- trimws(raw_line)
      
      if (nchar(inner) == 0) {
        python_output <- c(python_output, "")
        i <- i + 1
        next
      }
      
      new_line <- inner
      
      assign_out <- translate_assignment(new_line)
      if (!is.null(assign_out)) new_line <- assign_out
      
      vec_out <- translate_vector(new_line)
      if (!is.null(vec_out)) new_line <- vec_out
      
      new_line <- translate_operators(new_line)
      new_line <- translate_expression(new_line)
      new_line <- translate_io(new_line)
      
      python_output <- c(python_output, paste0("    ", new_line))
      
      i <- i + 1
    }
    
    i <- i + 1  # move past "}"
  }
  
  # else block 
  if (i <= length(lines) && is_else_start(lines[i])) {
    
    python_output <- c(python_output, "else:")
    
    i <- i + 1
    
    # process else block body
    while (!grepl("^\\}", trimws(lines[i]))) {
      raw_line <- lines[i]
      inner <- trimws(raw_line)
      
      if (nchar(inner) == 0) {
        python_output <- c(python_output, "")
        i <- i + 1
        next
      }
      
      new_line <- inner
      
      assign_out <- translate_assignment(new_line)
      if (!is.null(assign_out)) new_line <- assign_out
      
      vec_out <- translate_vector(new_line)
      if (!is.null(vec_out)) new_line <- vec_out
      
      new_line <- translate_operators(new_line)
      new_line <- translate_expression(new_line)
      new_line <- translate_io(new_line)
      
      python_output <- c(python_output, paste0("    ", new_line))
      
      i <- i + 1
    }
    
    i <- i + 1  # move after closing "}"
  }
  
  # return block 
  return(list(output = python_output, next_index = i))
}

# TIDYVERSE 
# Detect pipeline
is_pipeline_line <- function(line) {
  grepl("%>%", line)
}

collect_pipeline_block <- function(lines, start_index) {
  block <- c(lines[start_index])
  i <- start_index + 1
  
  while (i <= length(lines)) {
    stripped <- trimws(lines[i])
    
    # keep lines that still have %>% OR are indented filter/select/mutate/etc
    if (grepl("%>%", stripped) ||
        grepl("^(filter|select|mutate|arrange|summarize|group_by)\\(", stripped)) {
      block <- c(block, lines[i])
      i <- i + 1
    } else {
      break
    }
  }
  
  return(list(block = block, go = i))
}


# Split tidyverse pipeline into list of steps
split_pipeline <- function(line) {
  parts <- strsplit(line, "%>%")[[1]]
  trimws(parts)
}

# Extract the name of the first dataframe (leftmost part)
get_pipeline_df_name <- function(step) {
  trimws(step)
}

# Helper: translate filter()
translate_filter <- function(df_name, args) {
  cond <- trimws(args)
  
  # VERY simple pattern: col <operator> rest  (e.g., age > 23)
  m <- regexec("^([A-Za-z_][A-Za-z0-9_]*)\\s*(.*)$", cond)
  reg <- regmatches(cond, m)[[1]]
  
  if (length(reg) >= 3) {
    col <- reg[2]
    rest <- reg[3]
    cond_py <- paste0(df_name, "[\"", col, "\"]", rest)
  } else {
    cond_py <- cond
  }
  
  paste0(df_name, " = ", df_name, "[", cond_py, "]")
}


# Helper: translate select()
translate_select <- function(df_name, args) {
  # split columns
  cols <- trimws(unlist(strsplit(args, ",")))
  cols_python <- paste0('"', cols, '"', collapse = ", ")
  paste0(df_name, " = ", df_name, "[[", cols_python, "]]")
}

# Helper: translate arrange()
translate_arrange <- function(df_name, args) {
  args <- translate_expression(args)
  
  if (grepl("^desc\\(", args)) {
    col <- sub("desc\\((.*)\\)", "\\1", args)
    return(paste0(df_name, " = ", df_name, ".sort_values(by=\"", col, "\", ascending=FALSE)"))
  }
  
  paste0(df_name, " = ", df_name, ".sort_values(by=\"", args, "\")")
}

# Helper: translate mutate()
translate_mutate <- function(df_name, args) {
  assignments <- unlist(strsplit(args, ","))
  output <- c()
  
  for (assign in assignments) {
    parts <- strsplit(assign, "=")[[1]]
    name <- trimws(parts[1])
    expr <- translate_expression(trimws(parts[2]))
    expr <- translate_operators(expr)
    
    output <- c(output,
                paste0(df_name, "[\"", name, "\"] = ", expr))
  }
  
  return(output)
}

# Helper: translate summarize()
translate_summarize <- function(df_name, args) {
  parts <- strsplit(args, "=")[[1]]
  new_name <- trimws(parts[1])
  expr <- trimws(parts[2])
  
  expr <- translate_expression(expr)
  expr <- translate_operators(expr)
  
  paste0(new_name, " = ", expr)
}



translate_pipeline_block <- function(block_lines) {
  full <- paste(block_lines, collapse = " ")
  full <- gsub("\n", " ", full)
  full <- gsub("\\s+", " ", full)
  translate_pipeline(full)
}

parse_pipeline_lhs <- function(step) {
  parts <- strsplit(step, "<-")[[1]]
  if (length(parts) == 2) {
    target <- trimws(parts[1])
    df_name <- trimws(parts[2])
  } else {
    target <- trimws(step)
    df_name <- target
  }
  list(target = target, df = df_name)
}

# Main Pipeline Translator
translate_pipeline <- function(line) {
  steps <- split_pipeline(line)
  
  # first step contains df_filtered <- df
  lhs <- parse_pipeline_lhs(steps[1])
  target <- lhs$target      # df_filtered
  base_df <- lhs$df         # df
  
  # transform target data frame
  df_name <- target
  
  python_output <- c()
  
  # first, copy base df into target (so we don't overwrite original df)
  if (base_df != target) {
    python_output <- c(python_output,
                       paste0(target, " = ", base_df))
  }
  
  for (i in 2:length(steps)) {
    step <- steps[i]
    
    # filter()
    if (grepl("^filter\\(", step)) {
      args <- sub("filter\\((.*)\\)", "\\1", step)
      python_output <- c(python_output,
                         translate_filter(df_name, args))
    }
    
    # select()
    else if (grepl("^select\\(", step)) {
      args <- sub("select\\((.*)\\)", "\\1", step)
      python_output <- c(python_output,
                         translate_select(df_name, args))
    }
    
    # arrange()
    else if (grepl("^arrange\\(", step)) {
      args <- sub("arrange\\((.*)\\)", "\\1", step)
      python_output <- c(python_output,
                         translate_arrange(df_name, args))
    }
    
    # mutate()
    else if (grepl("^mutate\\(", step)) {
      args <- sub("mutate\\((.*)\\)", "\\1", step)
      python_output <- c(python_output,
                         translate_mutate(df_name, args))
    }
    
    # summarize()
    else if (grepl("^summarize\\(", step)) {
      args <- sub("summarize\\((.*)\\)", "\\1", step)
      python_output <- c(python_output,
                         translate_summarize(df_name, args))
    }
    
    # group_by() - optional
    else if (grepl("^group_by\\(", step)) {
      col <- sub("group_by\\((.*)\\)", "\\1", step)
      col <- trimws(col)
      python_output <- c(
        python_output,
        paste0(df_name, " = ", df_name, ".groupby(\"", col, "\")")
      )
    }
    
    else {
      python_output <- c(python_output,
                         paste0("# Unrecognized pipeline verb: ", step))
    }
  }
  
  return(python_output)
}


#GGPLOT 
# Detect the start of a ggplot block
is_ggplot_start <- function(line) {
  grepl("^\\s*ggplot\\s*\\(", line)
}

extract_ggplot_parts <- function(line) {
  clean <- trimws(line)
  clean <- gsub("\\+\\s*$", "", clean)
  
  # extract inside ggplot(...)
  inside <- sub("ggplot\\s*\\((.*)\\)", "\\1", clean)
  
  parts <- strsplit(inside, ",")[[1]]
  data <- trimws(parts[1])
  
  # extract aes(...)
  aes_raw <- gsub("\\+\\s*$", "", parts[2])
  aes_inside <- sub("aes\\s*\\((.*)\\)", "\\1", aes_raw)
  
  # process aes key/value arguments
  aes_pairs <- strsplit(aes_inside, ",")[[1]]
  aes_list <- list()
  
  for (pair in aes_pairs) {
    keyval <- strsplit(pair, "=")[[1]]
    key <- trimws(keyval[1])
    val <- trimws(keyval[2])
    val <- gsub("[^A-Za-z0-9_]", "", val)  # strip +, ), (, spaces
    aes_list[[key]] <- val
  }
  
  return(list(data = data, aes = aes_list))
}


# Convert common geoms to matplotlib commands
translate_geom <- function(geom_line, data, aes) {
  g <- trimws(geom_line)
  
  ### geom_point()
  if (grepl("^\\s*\\+?\\s*geom_point", g)) {
    return(
      paste0("plt.scatter(", data, "[\"", aes$x, "\"], ",
             data, "[\"", aes$y, "\"])")
    )
  }
  
  ### geom_histogram()
  if (grepl("^\\s*\\+?\\s*geom_histogram", g)) {
    return(
      paste0("plt.hist(", data, "[\"", aes$x, "\"], bins=30)")
    )
  }
  
  ### geom_line()
  if (grepl("^\\s*\\+?\\s*geom_line", g)) {
    return(
      paste0("plt.plot(", data, "[\"", aes$x, "\"], ",
             data, "[\"", aes$y, "\"])")
    )
  }
  
  ### geom_bar()
  if (grepl("^\\s*\\+?\\s*geom_bar", g)) {
    return(
      paste0("plt.bar(", data, "[\"", aes$x, "\"], ",
             data, "[\"", aes$y, "\"])")
    )
  }
  
  ### geom_smooth(method="lm")
  if (grepl("^\\s*\\+?\\s*geom_smooth", g)) {
    return("# geom_smooth() → complex translation, placeholder")
  }
  
  ### anything unrecognized → return as a comment 
  return(paste0("# Unrecognized ggplot layer: ", g))
}


### MAIN ggplot translator
translate_ggplot <- function(lines, start_index) {
  
  start_line <- lines[start_index]
  parts <- extract_ggplot_parts(start_line)
  
  data <- parts$data
  aes <- parts$aes
  
  python_output <- c()
  
  # Always import matplotlib
  python_output <- c(python_output, "import matplotlib.pyplot as plt")
  
  # No plot drawn yet – that happens in layers
  
  i <- start_index + 1
  
  # Loop through all geom_* layers (with or without '+')
  while (i <= length(lines) && grepl("geom_", lines[i])) {
    
    geom_line <- trimws(lines[i])
    
    layer_py <- translate_geom(geom_line, data, aes)
    python_output <- c(python_output, layer_py)
    
    i <- i + 1
  }
  
  
  # Add a final show() call
  python_output <- c(python_output, "plt.show()")
  
  # return output + last processed index
  return(list(output = python_output, next_index = i - 1))
}



#### Main loop
translate_file <- function(lines) {
  
  translated <- c()
  i <- 1
  
  while (i <= length(lines)) {
    line <- lines[i]
    stripped <- trimws(line)
    
    #skipping blank lines
    if (nchar(stripped) == 0) {
      translated <- c(translated, "")
      i <- i + 1
      next
    }
    
    if (grepl("^library\\(tidyverse\\)", stripped)) {
      # comment out the original R line
      translated <- c(translated, paste0("# ", stripped))
      # add pandas import
      translated <- c(translated, "import pandas as pd")
      i <- i + 1
      next
    }
    #structural translators:
    
    #fucntions: 

    if (is_function_start(stripped)) {
      result <- translate_function(lines, i)
      translated <- c(translated, result$output)
      i <- result$next_index + 1
      next
    }
    
#forloops
    if (is_for_loop_start(stripped)) {
      result <- translate_for_loop(lines, i)
      translated <- c(translated, result$output)
      i <- result$next_index + 1
      next
    }
    
#while loops
        if (is_while_loop_start(stripped)) {
      result <- translate_while_loop(lines, i)
      translated <- c(translated, result$output)
      i <- result$next_index + 1
      next
    }
    
#if/ifelse/ else
        if (is_if_start(stripped)) {
      result <- translate_if_block(lines, i)
      translated <- c(translated, result$output)
      i <- result$next_index + 1
      next
    }

    if (is_pipeline_line(stripped)) {
      collected <- collect_pipeline_block(lines, i)
      py <- translate_pipeline_block(collected$block)
      translated <- c(translated, py)
      i <- collected$go
      next
    }
    
    
    
    
#ggplot
    if (is_ggplot_start(stripped)) {
      result <- translate_ggplot(lines, i)
      translated <- c(translated, result$output)
      i <- result$next_index + 1
      next
    }
    
# data frames
    if (is_data_frame_start(stripped)) {
      
      # Extract variable name before '<-'
      var_name <- trimws(sub("<-.*", "", stripped))
      
      result <- translate_data_frame(lines, i)
      
      # Add assignment: df = pd.DataFrame({...})
      python_df <- result$output
      python_df[1] <- paste0(var_name, " = ", python_df[1])
      
      translated <- c(translated, python_df)
      i <- result$next_index + 1
      next
    }
    
#list strucutres 
    if (is_list_start(stripped)) {
      result <- translate_list(lines, i)
      translated <- c(translated, result$output)
      i <- result$next_index + 1
      next
    }
    
    
# content level translators: 

    new_line <- line
    
    
    ## assignment (<-)
    assign_out <- translate_assignment(new_line)
    if (!is.null(assign_out)) new_line <- assign_out
    
    ## vector (c(...))
    vec_out <- translate_vector(new_line)
    if (!is.null(vec_out)) new_line <- vec_out
    
    ## operators
    new_line <- translate_operators(new_line)
    
    ## expressions
    new_line <- translate_expression(new_line)
    
    ## I/O (read.csv, write.csv)
    new_line <- translate_io(new_line)
    
    
    #### 4. SEND PROCESSED LINE OUT
    translated <- c(translated, new_line)
    i <- i + 1
  }
  
  return(translated)
}

translate_r_to_python_file <- function(input_r_file, output_python_file) {
  
  # Make sure input exists
  if (!file.exists(input_r_file)) {
    stop("Input R file does not exist.")
  }
  
  # Ensure output has .py extension
  if (!grepl("\\.py$", output_python_file)) {
    output_python_file <- paste0(output_python_file, ".py")
  }
  
  # 1. Read R file
  r_lines <- readLines(input_r_file, warn = FALSE)
  
  # 2. Translate using your translator
  python_lines <- translate_file(r_lines)
  
  # 3. Write to an actual .py file
  writeLines(python_lines, output_python_file)
  
  message("Success! Python file created at: ", output_python_file)
}



## Implement 
code_lines <- read_r_file("example_r_file.R")
py <- translate_file(code_lines)
cat(py, sep="\n")

translate_r_to_python_file("example_r_file.R", "example_r_file.py")

