## R CMD check results

0 errors | 0 warnings | 1 notes

* This is a new release.


# COMMENT
Pls omit the redundant " The 'oRaklE' package is designed "

Is there some reference about the method you can add in the Description
field in the form Authors (year) <doi:10.....>?

# ANSWER
I included 2 studies in the description file. Additionally, I am planning to submit a detailed paper regarding the package and the applied method to a journal for statistical software.

# COMMENT

\dontrun{} should only be used if the example really cannot be executed
(e.g. because of missing additional software, missing API keys, ...) by
the user. That's why wrapping examples in \dontrun{} adds the comment
("# Not run:") as a warning for the user. Does not seem necessary.
Please replace \dontrun with \donttest.
Please unwrap the examples if they are executable in < 5 sec, or replace
dontrun{} with \donttest{}.
-> full_forecast.Rd

# ANSWER

I changed dontrun{} to donttest{}. Because this function calls all the other functions of the library after another an actual example run would take too long. Therefore I just provided a simple output example and kept the function calls to show how the different variables affect the output.

# COMMENT

You write information messages to the console that cannot be easily
suppressed.
It is more R like to generate objects that can be used to extract the
information a user is interested in, and then print() that object.
Instead of print()/cat() rather use message()/warning() or
if(verbose)cat(..) (or maybe stop()) if you really have to write text to
the console. (except for print, summary, interactive functions)

# ANSWER

Good point. I added a verbose option with default = FALSE.

# COMMENT

Please ensure that your functions do not write by default or in your
examples/vignettes/tests in the user's home filespace (including the
package directory and getwd()). This is not allowed by CRAN policies.
Please omit any default path in writing functions. In your
examples/vignettes/tests you can write to tempdir().
For more details: 

# ANSWER

The functions will write to a tmpdir by default now and only write to a folder specified by the user,
if the user gives active consent.

# COMMENT

Please do not set a seed to a specific number within a function. 

# ANSWER

I added a rdm_seed input variable which allows for setting of a random seed and is set to a random number
by default.
