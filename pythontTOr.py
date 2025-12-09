import os
import re
from typing import List, Tuple

def read_file(path: str) -> List[str]:
    if not os.path.exists(path):
        raise FileNotFoundError(f"File does not exist: {path}")
    with open(path, "r") as f:
        return f.readlines()


# ------------------------------------------------------------
# Language Detection
# ------------------------------------------------------------

def detect_language(line: str) -> str:
    stripped = line.strip()

    if stripped == "":
        return "other"
    if stripped.startswith("#"):
        return "other"

    # R heuristics
    if "<-" in stripped:
        return "r"
    if stripped.startswith("library(") or stripped.startswith("require("):
        return "r"
    if "ggplot(" in stripped or "tidyverse" in stripped:
        return "r"
    if re.match(r"^[A-Za-z0-9_.]+\s*<-\s*function\(", stripped):
        return "r"

    # Python heuristics
    if stripped.startswith("def "):
        return "python"
    if stripped.startswith("for ") or stripped.startswith("while "):
        return "python"
    if stripped.startswith("if ") or stripped.startswith("elif ") or stripped.startswith("else:"):
        return "python"
    if stripped.startswith("import ") or stripped.startswith("from "):
        return "python"
    if re.match(r"^[A-Za-z0-9_.]+\s*=", stripped):
        return "python"
    if "pd." in stripped or "plt." in stripped:
        return "python"
    return "other"

# helper functions

def get_indent(line: str) -> int:
    return len(line) - len(line.lstrip(" "))

def replace_first_equals_with_arrow(line: str) -> str:
    # only if it looks like an assignment at start of line.
    if re.match(r"^\s*[A-Za-z_][A-Za-z0-9_.]*\s*=", line):
        return line.replace("=", " <-", 1)
    return line

def translate_bool_and_none(line: str) -> str:
    line = re.sub(r"\bTrue\b", "TRUE", line)
    line = re.sub(r"\bFalse\b", "FALSE", line)
    line = re.sub(r"\bNone\b", "NULL", line)
    return line

def translate_logical_ops(line: str) -> str:
    # Use word boundaries to avoid partial matches
    line = re.sub(r"\band\b", "&", line)
    line = re.sub(r"\bor\b", "|", line)
    line = re.sub(r"\bnot\b", "!", line)
    return line

def translate_power(line: str) -> str:
    return line.replace("**", "^")

def translate_lists_to_c(line: str) -> str:
    # translate bare [1, 2, 3] into c(1, 2, 3)
    # avoid subsetting like df[ ... ]
    if re.search(r"[A-Za-z0-9_]\s*\[", line):
        return line
    # replace single bracketed lists
    def repl(match):
        inner = match.group(1)
        return f"c({inner})"
    return re.sub(r"\[([^\[\]]+)\]", repl, line)

def translate_imports(line: str) -> List[str]:
    """
    Convert Python imports to tidyverse / ggplot2.
    Returns a list of output lines (or empty list if removing import).
    """
    stripped = line.strip()
    # pandas -> tidyverse
    if "import pandas" in stripped or "from pandas" in stripped:
        return ["library(tidyverse)"]
    # matplotlib -> ggplot2
    if "import matplotlib" in stripped or "from matplotlib" in stripped:
        return ["library(ggplot2)"]
    # numpy
    if "import numpy" in stripped or "from numpy" in stripped:
        return []  # remove numpy imports
    # generic python imports → remove
    if stripped.startswith("import ") or stripped.startswith("from "):
        return []

    return [line.rstrip("\n")]

# pandas DataFrame dict literal -> data.frame
def translate_dataframe_block(lines: List[str], start_idx: int) -> Tuple[List[str], int]:
    # translate a block starting with `pd.DataFrame({` and ending with `})` into an R data.frame(...) expression.
    # returns (translated_lines, next_index_after_block).
    indent = get_indent(lines[start_idx])
    collected = []
    i = start_idx

    # collect until we see a line containing '})'
    while i < len(lines):
        collected.append(lines[i])
        if "})" in lines[i]:
            break
        i += 1

    # extract inner content between { and }
    inner_lines = []
    for j, l in enumerate(collected):
        s = l.strip()
        if j == 0:
            # remove up to '{'
            brace_pos = s.find("{")
            if brace_pos != -1:
                s = s[brace_pos + 1:]
        if j == len(collected) - 1:
            # remove after '}'
            brace_pos = s.rfind("}")
            if brace_pos != -1:
                s = s[:brace_pos]
        inner_lines.append(s)

    # parse lines
    col_specs = []
    for raw in inner_lines:
        s = raw.strip().rstrip(",")
        if not s:
            continue
        m = re.match(r'"([^"]+)"\s*:\s*(\[.*\])', s)
        if not m:
            col_specs.append(f"# could not parse column: {raw.strip()}")
            continue
        col = m.group(1)
        arr = m.group(2)
        arr_r = arr.replace("[", "c(").replace("]", ")")
        col_specs.append(f"{col} = {arr_r}")

    r_lines = []
    base_indent = " " * indent
    if len(col_specs) == 0:
        r_lines.append(base_indent + "data.frame()")
    else:
        r_lines.append(base_indent + "data.frame(")
        for k, spec in enumerate(col_specs):
            comma = "," if k < len(col_specs) - 1 else ""
            r_lines.append(base_indent + "    " + spec + comma)
        r_lines.append(base_indent + ")")

    return r_lines, i + 1

# pandas / indexing translation (simple patterns)

def translate_pandas_indexing(line: str) -> str:
    """
    Handle patterns like:
      df_filtered = df_filtered[df_filtered["age"] > 23]
      df_filtered = df_filtered[["name", "age"]]
    into base R:
      df_filtered <- df_filtered[df_filtered$age > 23, ]
      df_filtered <- df_filtered[c("name", "age")]
    """
    stripped = line.strip()
    m = re.match(r"^([A-Za-z_][A-Za-z0-9_]*)\s*=\s*\1\[(.+)\]\s*$", stripped)
    if not m:
        return line

    var = m.group(1)
    inner = m.group(2).strip()

    # column selection with double brackets
    if inner.startswith("[") and inner.endswith("]"):
        inner2 = inner[1:-1].strip()
        # remove outer [] and keep as c(...)
        inner2 = inner2.replace("[", "c(").replace("]", ")")
        new_line = f"{var} <- {var}[{inner2}]"
        return re.sub(r"^.*$", new_line, line)

    pattern = rf"{var}\[\"([^\"]+)\"\]"
    inner = re.sub(pattern, rf"{var}$\1", inner)
    new_line = f"{var} <- {var}[{inner}, ]"
    indent = " " * get_indent(line)
    return indent + new_line

# matplotlib -> ggplot2

def translate_matplotlib_hist(line: str) -> str:
    # Translate plt.hist(df_filtered["age"], bins=30) to ggplot(df_filtered, aes(x = age)) + geom_histogram(bins = 30)
    indent = " " * get_indent(line)
    stripped = line.strip()

    m = re.match(r"plt\.hist\((.+),\s*bins\s*=\s*([0-9]+)\s*\)", stripped)
    if not m:
        return line
    arg = m.group(1).strip()
    bins = m.group(2)

    # parse df["col"]
    m2 = re.match(r'([A-Za-z_][A-Za-z0-9_]*)\s*\["([^"]+)"\s*\]', arg)
    if not m2:
        return indent + "# TODO: translate matplotlib hist manually\n" + line

    df_name = m2.group(1)
    col_name = m2.group(2)
    r1 = f'{indent}ggplot({df_name}, aes(x = {col_name})) +'
    r2 = f'{indent}    geom_histogram(bins = {bins})'
    return r1 + "\n" + r2



# per-line Python translation
def translate_python_simple(line: str) -> str:
    # booleans / None
    line = translate_bool_and_none(line)
    # logical ops
    line = translate_logical_ops(line)
    # power
    line = translate_power(line)
    # lists
    line = translate_lists_to_c(line)
    # len(...) -> length(...)
    line = line.replace("len(", "length(")
    return line

# structural translation: def/if/for/while/else with braces
def translate_structural_mixed(lines: List[str]) -> List[str]:
    r_lines: List[str] = []
    indent_stack: List[int] = []

    i = 0
    n = len(lines)

    while i < n:
        line = lines[i]
        lang = detect_language(line)
        stripped = line.strip()
        current_indent = get_indent(line)

        while indent_stack and current_indent < indent_stack[-1]:
            indent_stack.pop()
            r_lines.append(" " * (indent_stack[-1] if indent_stack else 0) + "}")

        # r lines: pass through unchanged
        if lang == "r":
            r_lines.append(line.rstrip("\n"))
            i += 1
            continue

        # "other" lines: pass unchanged, except some matplotlib cleanup
        if lang == "other":
            if stripped.startswith("import ") or stripped.startswith("from "):
                new_lines = translate_imports(line)
                for nl in new_lines:
                    r_lines.append(nl)
                i += 1
                continue
            if stripped.startswith("plt.show()"):
                i += 1
                continue
            r_lines.append(line.rstrip("\n"))
            i += 1
            continue

        if stripped.startswith("import ") or stripped.startswith("from "):
            new_lines = translate_imports(line)
            for nl in new_lines:
                r_lines.append(nl)
            i += 1
            continue

        # handle DataFrame block first
        if "pd.DataFrame({" in stripped:
            new_block, next_idx = translate_dataframe_block(lines, i)
            r_lines.extend(new_block)
            i = next_idx
            continue

        # structural headers
        if stripped.startswith("def "):
            m = re.match(r"def\s+([A-Za-z_][A-Za-z0-9_]*)\((.*)\):", stripped)
            if m:
                func_name = m.group(1)
                args = m.group(2)
                new_line = f"{func_name} <- function({args}) {{"
                r_lines.append(" " * current_indent + new_line)
                indent_stack.append(current_indent + 4)
                i += 1
                continue

        if stripped.startswith("if "):
            cond = stripped[3:].rstrip(":").strip()
            cond = translate_python_simple(cond)
            new_line = f"if ({cond}) {{"
            r_lines.append(" " * current_indent + new_line)
            indent_stack.append(current_indent + 4)
            i += 1
            continue

        if stripped.startswith("elif "):
            cond = stripped[5:].rstrip(":").strip()
            cond = translate_python_simple(cond)
            new_line = f"else if ({cond}) {{"
            r_lines.append(" " * current_indent + new_line)
            indent_stack.append(current_indent + 4)
            i += 1
            continue

        if stripped.startswith("else:"):
            new_line = "else {"
            r_lines.append(" " * current_indent + new_line)
            indent_stack.append(current_indent + 4)
            i += 1
            continue

        if stripped.startswith("for "):
            # for i in seq:
            content = stripped[4:].rstrip(":")
            parts = content.split(" in ")
            if len(parts) == 2:
                var = parts[0].strip()
                seq = parts[1].strip()
                # translate simple range(...) to a:b
                m = re.match(r"range\(\s*([0-9]+)\s*,\s*([0-9]+)\s*\)", seq)
                if m:
                    start = int(m.group(1))
                    end = int(m.group(2)) - 1
                    seq = f"{start}:{end}"
                new_line = f"for ({var} in {seq}) {{"
                r_lines.append(" " * current_indent + new_line)
                indent_stack.append(current_indent + 4)
                i += 1
                continue

        if stripped.startswith("while "):
            cond = stripped[6:].rstrip(":").strip()
            cond = translate_python_simple(cond)
            new_line = f"while ({cond}) {{"
            r_lines.append(" " * current_indent + new_line)
            indent_stack.append(current_indent + 4)
            i += 1
            continue

        # non-header Python line
        # first try matplotlib hist
        if "plt.hist(" in stripped:
            hist_translated = translate_matplotlib_hist(line)
            for sub_line in hist_translated.split("\n"):
                r_lines.append(sub_line.rstrip("\n"))
            i += 1
            continue

        if stripped.startswith("plt.show()"):
            i += 1
            continue

        # pandas indexing patterns
        if re.match(r"^\s*[A-Za-z_][A-Za-z0-9_]*\s*=\s*[A-Za-z_][A-Za-z0-9_]*\[", stripped):
            new_line = translate_pandas_indexing(line)
            new_line = translate_python_simple(new_line)
            r_lines.append(new_line.rstrip("\n"))
            i += 1
            continue

        new_line = line.rstrip("\n")
        new_line = replace_first_equals_with_arrow(new_line)
        new_line = translate_python_simple(new_line)
        r_lines.append(new_line)
        i += 1

    # close remaining Python blocks
    while indent_stack:
        indent_stack.pop()
        r_lines.append(" " * (indent_stack[-1] if indent_stack else 0) + "}")

    return r_lines

# main :)

def translate_file(input_path: str) -> List[str]:
    lines = read_file(input_path)
    return translate_structural_mixed(lines)

def main():
    input_file = "example_python_file.py"    # change this to the file you want to translate
    output_file = "translated.R"
    
    if not os.path.exists(input_file):
        print(f"Error: {input_file} not found. Set file in main().")
        return

    r_lines = translate_file(input_file)
    with open(output_file, "w") as f:
        for line in r_lines:
            f.write(line + "\n")

if __name__ == "__main__":
    main()
