# NPP Project: SQL Reader

This Haskell project is a command-line tool for running simple SQL-like queries on CSV files.

## 🧠 Overview

This tool is designed to:
- Inspect CSV files and infer column types.
- Accept simple SQL-like queries.
- Execute validated queries on the data.
- Export the results to a new file.

It uses:
- [`cassava`](https://hackage.haskell.org/package/cassava) for efficient CSV parsing.
- [`megaparsec`](https://hackage.haskell.org/package/megaparsec) for parsing SQL-like queries.

## Pipeline

1. **Argument Parsing**  
   Detects if user provided just CSV path (interactive mode) or full auto mode with CSV + query + output.

2. **CSV Loading**  
   Reads and validates CSV using `cassava`.

3. **Data Preview**  
   Prints column names, 2–3 sample rows, and infers column types.

4. **Query Handling**  
   Parses SQL-like query with `megaparsec`.  
   Validates column names and type compatibility.

5. **Execution**  
   Applies the query to data, prints sample output.

6. **Export**  
   Saves result to user-specified path.

## Example Usage

Interactive mode:
```bash
$ ./npp-sql-reader ./data/employees.csv
```
