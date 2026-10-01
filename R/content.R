# Learner guide, cheat sheet, and loadable examples.

guide_modules <- function() {
  list(
    list(
      id = "model",
      title = "1. Mental model",
      summary = "SAS thinks in a program data vector that walks rows. pandas thinks in columns (Series) on a DataFrame.",
      sas = "data out;\n  set in;\n  bmi = weight / (height**2);\nrun;",
      python = "out = in.copy()\nout[\"bmi\"] = out[\"weight\"] / (out[\"height\"] ** 2)",
      tips = c(
        "A DATA step compiles, then executes once per row. An assignment in pandas is usually a whole-column vector operation.",
        "work. is the SAS temporary library. A Python name is just a variable in memory until you write it.",
        "Row order is part of the SAS contract. pandas may not preserve order unless you sort or pass kind='stable'."
      )
    ),
    list(
      id = "io",
      title = "2. Reading and writing",
      summary = "PROC IMPORT guesses. pandas also guesses, but you should pass dtype and parse_dates on purpose.",
      sas = "proc import datafile=\"patients.csv\" out=work.patients dbms=csv replace;\n  getnames=yes;\nrun;\n\nproc export data=work.patients outfile=\"out.csv\" dbms=csv replace;\nrun;",
      python = "patients = pd.read_csv(\"patients.csv\")\npatients.to_csv(\"out.csv\", index=False)",
      tips = c(
        "SAS dates are often numeric with a format. pandas may import them as strings until you parse them.",
        "read_sas() can open .sas7bdat directly when you do not need a full SAS session.",
        "index=False stops pandas from writing the row index as a column SAS will treat as data."
      )
    ),
    list(
      id = "filter",
      title = "3. Filtering rows",
      summary = "WHERE and subsetting IF both drop rows. In pandas, prefer loc or query and assign the result.",
      sas = "data adults;\n  set patients;\n  where age ge 18 and missing(death_dt);\nrun;",
      python = "adults = patients.query(\"age >= 18 and death_dt.isna()\")\n# or\nadults = patients.loc[(patients[\"age\"] >= 18) & patients[\"death_dt\"].isna()].copy()",
      tips = c(
        "SAS missing is smaller than any number, so where age < 0 keeps missing ages. pandas NA fails the comparison and is dropped.",
        "Always .copy() after a filter if you will add columns, or you can hit SettingWithCopyWarning.",
        "ge/le/eq/ne are the same ideas as >=, <=, ==, !=."
      )
    ),
    list(
      id = "recode",
      title = "4. Conditions and recodes",
      summary = "IF/THEN writes the program data vector. np.where and np.select write a column.",
      sas = "data out;\n  set in;\n  if score >= 90 then grade = \"A\";\n  else if score >= 80 then grade = \"B\";\n  else grade = \"C\";\nrun;",
      python = "out = in.copy()\nout[\"grade\"] = np.select(\n    [out[\"score\"] >= 90, out[\"score\"] >= 80],\n    [\"A\", \"B\"],\n    default=\"C\",\n)",
      tips = c(
        "A long ELSE IF chain is np.select, not nested np.where.",
        "SAS character comparisons are case-sensitive and padded. Use .str.strip() and an explicit case rule in Python.",
        "Creating a new variable mid-step is normal in SAS. In pandas, the column exists for every row after the assignment."
      )
    ),
    list(
      id = "sort",
      title = "5. Sorting",
      summary = "PROC SORT replaces the data set (or writes OUT=). sort_values returns a new object unless inplace=True.",
      sas = "proc sort data=patients out=patients_s;\n  by site descending enrolled_dt;\nrun;",
      python = "patients_s = patients.sort_values(\n    [\"site\", \"enrolled_dt\"],\n    ascending=[True, False],\n)",
      tips = c(
        "NODUPKEY is drop_duplicates(subset=keys, keep='first') after the sort.",
        "SAS BY requires sorted data. pandas groupby does not.",
        "descending applies only to the next BY variable."
      )
    ),
    list(
      id = "means",
      title = "6. Summaries",
      summary = "PROC MEANS / SUMMARY collapse rows. groupby.agg is the same idea with different column names.",
      sas = "proc means data=patients n mean std min max;\n  class site sex;\n  var age bmi;\n  output out=stats mean= std= / autoname;\nrun;",
      python = "stats = (\n    patients.groupby([\"site\", \"sex\"], dropna=False)[[\"age\", \"bmi\"]]\n    .agg([\"count\", \"mean\", \"std\", \"min\", \"max\"])\n    .reset_index()\n)",
      tips = c(
        "CLASS missing groups are kept in SAS if you ask. groupby drops NA keys unless dropna=False.",
        "The automatic _TYPE_ / _FREQ_ columns from OUTPUT OUT= have no default pandas equivalent.",
        "N in PROC MEANS excludes missing. pandas count does the same; use size for row counts including NA."
      )
    ),
    list(
      id = "freq",
      title = "7. Frequencies",
      summary = "PROC FREQ is value_counts for one variable and crosstab for a star-separated table.",
      sas = "proc freq data=patients;\n  tables site * sex / missing nocol norow nopercent;\nrun;",
      python = "pd.crosstab(patients[\"site\"], patients[\"sex\"], dropna=False)",
      tips = c(
        "Add normalize='all' (or 'index' / 'columns') when you need the percent options.",
        "MISSING on the TABLES statement corresponds to dropna=False."
      )
    ),
    list(
      id = "merge",
      title = "8. Merge vs join",
      summary = "This is the most common place SAS programmers get a silent wrong answer in pandas.",
      sas = "data both;\n  merge demo(in=a) labs(in=b);\n  by subject;\n  if a and b;\nrun;",
      python = "both = demo.merge(labs, on=\"subject\", how=\"inner\")",
      tips = c(
        "SAS MERGE without BY matches by row number. Never replace that with DataFrame.merge.",
        "Many-to-many SAS match-merge can fan out differently from a SQL-style pandas merge. Check row counts.",
        "IN= flags are the indicator= column, or how='left' / how='inner'."
      )
    ),
    list(
      id = "sql",
      title = "9. PROC SQL",
      summary = "If you already think in SQL, pandas is optional. duckdb and pandasql can run the SQL more directly.",
      sas = "proc sql;\n  create table site_mean as\n  select site, mean(age) as mean_age\n  from patients\n  group by site;\nquit;",
      python = "site_mean = (\n    patients.groupby(\"site\", as_index=False)[\"age\"]\n    .mean()\n    .rename(columns={\"age\": \"mean_age\"})\n)",
      tips = c(
        "Calculated aliases in PROC SQL can be reused later in the same query. pandas cannot do that inside one expression.",
        "For a faithful translation, keep the SQL and run it with duckdb.query(sql).df()."
      )
    ),
    list(
      id = "missing",
      title = "10. Missing values",
      summary = "SAS has one missing for numeric (.) and blank for character. pandas has NA, NaN, NaT, and None.",
      sas = "if missing(result) then result = 0;",
      python = "df[\"result\"] = df[\"result\"].fillna(0)",
      tips = c(
        "Numeric missing sorts first in SAS. It sorts last in pandas unless you use na_position.",
        "Propagating missing is the SAS default for many math expressions. numpy can differ with nansum vs sum.",
        "Do not translate a bare SAS dot inside a number (3.14) as missing."
      )
    ),
    list(
      id = "dates",
      title = "11. Dates",
      summary = "A SAS date is days since 1 Jan 1960. A pandas timestamp is nanoseconds since the Unix epoch, shown as a date.",
      sas = "age_years = yrdif(dob, today(), 'age');",
      python = "age_years = (pd.Timestamp.today().normalize() - df[\"dob\"]).dt.days / 365.25",
      tips = c(
        "INTNX and INTCK have no one-line pandas twin. Use DateOffset and check end-of-month rules.",
        "Formats such as date9. are display rules. In pandas, keep a datetime dtype and format only when you export."
      )
    ),
    list(
      id = "macro",
      title = "12. Macros vs functions",
      summary = "A macro pastes text before SAS compiles. A Python function runs values.",
      sas = "%macro topn(data, var, n);\n  proc sort data=&data out=_s;\n    by descending &var;\n  run;\n  data top;\n    set _s(obs=&n);\n  run;\n%mend;",
      python = "def topn(data, var, n):\n    return data.sort_values(var, ascending=False).head(n)",
      tips = c(
        "Do not build Python with string pasting just because the SAS version used &var.",
        "Call execute and macro variables become ordinary arguments."
      )
    )
  )
}

cheat_sheet <- function() {
  data.frame(
    topic = c(
      "Read CSV", "Write CSV", "Read Excel", "Copy table", "Row filter",
      "Keep columns", "Drop columns", "Rename", "Sort", "Unique keys",
      "Means by group", "Frequency", "Cross-tab", "Inner join", "Left join",
      "New column", "Recode", "Fill missing", "First 10 rows", "Describe",
      "SQL aggregate", "Library", "Macro", "Contents"
    ),
    sas = c(
      "PROC IMPORT ... DBMS=CSV",
      "PROC EXPORT ... DBMS=CSV",
      "PROC IMPORT ... DBMS=XLSX",
      "DATA out; SET in; RUN;",
      "WHERE / subsetting IF",
      "KEEP var1 var2;",
      "DROP var1;",
      "RENAME old=new;",
      "PROC SORT; BY a descending b;",
      "PROC SORT NODUPKEY; BY id;",
      "PROC MEANS; CLASS g; VAR x;",
      "PROC FREQ; TABLES x;",
      "PROC FREQ; TABLES a*b;",
      "MERGE + IF a AND b;",
      "MERGE + IF a;",
      "y = x * 2;",
      "IF/THEN/ELSE",
      "IF MISSING(x) THEN x = 0;",
      "SET in(OBS=10);",
      "PROC MEANS N MEAN STD MIN MAX;",
      "PROC SQL GROUP BY",
      "LIBNAME",
      "%MACRO / %MEND",
      "PROC CONTENTS"
    ),
    python = c(
      "pd.read_csv(path)",
      "df.to_csv(path, index=False)",
      "pd.read_excel(path)",
      "out = in.copy()",
      "df.query(...) or df.loc[mask]",
      "df.loc[:, ['var1','var2']]",
      "df.drop(columns=['var1'])",
      "df.rename(columns={'old':'new'})",
      "sort_values(['a','b'], ascending=[True, False])",
      "drop_duplicates(subset=['id'])",
      "groupby('g')['x'].agg([...])",
      "df['x'].value_counts(dropna=False)",
      "pd.crosstab(df['a'], df['b'])",
      "a.merge(b, on='id', how='inner')",
      "a.merge(b, on='id', how='left')",
      "df['y'] = df['x'] * 2",
      "np.where / np.select",
      "df['x'] = df['x'].fillna(0)",
      "df.head(10)",
      "df.describe()",
      "groupby(...).agg(...)",
      "a folder path in the reader",
      "def function(...)",
      "df.dtypes / df.columns"
    ),
    watch = c(
      "Type guessing differs",
      "Do not write the index",
      "Sheet name is not guessed the same way",
      "copy() avoids later view bugs",
      "SAS missing passes some numeric filters",
      "Order of columns follows the KEEP list",
      "errors='ignore' has no SAS twin",
      "Dict direction is old -> new",
      "Descending binds to the next key only",
      "Sort first if you care which row survives",
      "N vs size; NA groups",
      "Percent options are normalize=",
      "Margins are margins=True",
      "Not a DATA step MERGE",
      "Use indicator= to mimic IN=",
      "Vectorised, not row-by-row",
      "Multi-branch needs np.select",
      "Fill value type must match",
      "OBS= is not the same as FIRSTOBS",
      "Percentiles are labelled differently",
      "Alias reuse is SQL-only",
      "No engine or libref",
      "Functions take values, macros paste text",
      "Labels and formats are not dtypes"
    ),
    stringsAsFactors = FALSE
  )
}

sas_examples <- function() {
  list(
    "Import a CSV" = 'proc import datafile="patients.csv" out=work.patients dbms=csv replace;\n  getnames=yes;\nrun;',
    "Filter and compute" = "data adults;\n  set patients;\n  where age ge 18;\n  bmi = weight / (height ** 2);\n  if bmi ge 30 then obese = 1;\n  else obese = 0;\n  keep subject age bmi obese;\nrun;",
    "Sort descending" = "proc sort data=patients out=patients_s nodupkey;\n  by site descending enrolled;\nrun;",
    "Group means" = "proc means data=patients n mean std min max;\n  class site sex;\n  var age bmi;\n  output out=stats mean=;\nrun;",
    "Cross-tab" = "proc freq data=patients;\n  tables site * sex / missing;\nrun;",
    "Match-merge" = "data both;\n  merge demo labs;\n  by subject;\nrun;",
    "Simple SQL" = "proc sql;\n  create table site_mean as\n  select site, mean(age) as mean_age\n  from patients\n  where age ge 18\n  group by site\n  order by site;\nquit;",
    "Export" = 'proc export data=adults outfile="adults.csv" dbms=csv replace;\nrun;'
  )
}

python_examples <- function() {
  list(
    "Import a CSV" = 'patients = pd.read_csv("patients.csv")',
    "Filter and compute" = 'adults = patients.query("age >= 18").copy()\nadults["bmi"] = adults["weight"] / (adults["height"] ** 2)\nadults["obese"] = np.where(adults["bmi"] >= 30, 1, 0)\nadults = adults.loc[:, ["subject", "age", "bmi", "obese"]].copy()',
    "Sort descending" = 'patients_s = patients.sort_values(["site", "enrolled"], ascending=[True, False])\npatients_s = patients_s.drop_duplicates(subset=["site"])',
    "Group means" = 'stats = patients.groupby(["site", "sex"], dropna=False)[["age", "bmi"]].agg(["count", "mean", "std", "min", "max"]).reset_index()',
    "Cross-tab" = 'freq = pd.crosstab(patients["site"], patients["sex"], dropna=False)',
    "Inner join" = 'both = demo.merge(labs, on="subject", how="inner")',
    "Fill missing" = 'patients = patients.fillna(0)',
    "Export" = 'adults.to_csv("adults.csv", index=False)'
  )
}
