# Guidelines for rsystrade code

Follow [tidyverse style guide](https://style.tidyverse.org/index.html) with the 
  following exceptions.
  
## GitHub

NEVER commit to stable branch.
Only ever change stable by merging from other branches.

NEVER commit to dev branch.
Only ever change dev by merging from other branches.

-   All work is collected in the dev branch and tagged with a major.minor.patch.dev number.
-   When dev branch reaches a stage suitable for a stable release, the dev branch is merged into the stable branch and tagged with a major.minor.patch number.
-   To work on a new feature or bug, create a temporary branch from the dev branch.
  -   When the feature has been implemented or the bug has been fixed:
    -   Merge the branch with the dev branch
    -   Delete the branch.
-   To hotfix a bug in the stable release, create a temporary hotfix branch.
  -   When the bug has been fixed:
    -   Merge the branch with the stable and dev branches.
    -   Delete the hotfix branch.
-   Possibly work in a release-* branch to touch up just before release.

### Branching with Git Flow

| subversion | on branch | branch to | as subversion |
| -- | -- | -- | -- |
| `major.minor.patch` |  `stable` | `dev` | `major.minor.patch.dev` |
|  `major.minor.patch` |  `stable` | `hotfix-<*>` | `major.minor.patch-<*>` |
| `major.minor.patch.dev` | `dev` | `feature-<topic-*>` | `major.minor.patch.feature-<topic-*>` |
| `major.minor.patch.dev` | `dev` | `bug-<topic-*>` | `major.minor.patch.bug-<topic-*>` |
|  |  |  |  |


### Merging with Git Flow

| subversion | on branch | merge to | as subversion |
| -- | -- | -- | -- |
| `major.minor.patch.dev` | `dev` | `stable` | `major.minor.patch` |
| `major.minor.patch.dev` | `dev` | `feature-<topic-*>` | `major.minor.patch.feature-<topic-*>` |
| `major.minor.patch.dev` | `dev` | `bug-<topic-*>` | `major.minor.patch.feature-<topic-*>` |
| `major.minor.patch.feature-<topic-*>` | `feature-topic-*` | `dev` | `major.minor.patch.dev` |
| `major.minor.patch.bug-<topic-*>` | `bug-<topic-*>` | `dev` | `major.minor.patch.dev` |
| `major.minor.patch-<*>` | `hotfix-<*>` | `stable` | `major.minor.(patch+1)` |
| `major.minor.patch-<*>` | `hotfix-<*>` | `dev` | `major.minor.(patch+1).dev` |

## Comments
-   Use `## ` (double hash plus a single space) for comments.  
-   Use `#` (single hash) for commenting out code.
-   `#'` At the beginning of a line indicates that the line is part of a comment
  that will be turned into a help file by `roxygen2::roxygenise()`.

## Function names
**rsystrade** aspires to follow the convention, that function names should 
  include a verb. However, because most of what **rsystrade** does is 
  calculations, there is one exception to this guideline:
-   The name of an R function that implements a *math function* begins with 
    `f_`. 
-   Here `f` at the beginning of function name represents the verb `calculate`.
-   Instead of `calculate_normalization_factor()` we use 
    `f_normalization_factor()`.
-   In other words: Functions with a name that begin with `f_` perform 
    *calculations*.
-   Note: Not all functions that perform calculations have names that begin with
    `f_`. If the function doesn't explicitly implement a formula, the function
    name will typically begin with `calculate`.
-   As a mnemonic you might think of "formula", "function" or "find". I think of 
    the math symbol for a function, because these calculations all implement 
    some sort of formula. Visually the f at the beginning seems intuitive to me, 
    when I skim the code, since it's an *R function* that

General thoughts:  
-   Best
  -   Strive for verbs as function names - but: This is hard to do in practice.
  -   Example: ...um... can't find any in **rsystrade** at the time of writing...
-   Good
  -   Verb first, then other words in lower case, connected by underscores.
  -   Example: 
  ```R
  apply_entering_rule
  ```
-   Less ideal (but often any alternatives come out contrived):
  -   The function name actually describes the output. The problem here is, that
    this is typically the name of the variable we want to assign the output to.
    A typical example is `mean()`. `calculate_mean()` would just be silly.
  -   So when assigning output from such a function to a variable, I recommend to
    end the name of the variable with an underscore.
  -   Example: 
  ```R
  mean_ <- mean()
  ```
  -   I try to avoid abbreviations in variable/function names, but sometimes I 
    make exceptions to avoid very long variable names. There is not clear rule
    here. For instance I use `f_inst_div_mult` instead of 
    `calculate_instrument_diversification_multiplier`. I also use
    `get_unique_inst_paths_from_expanded_algos_list`, which is almost as long,
    so... again, no clear line here.
