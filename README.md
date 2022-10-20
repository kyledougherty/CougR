# CougR

## Sourcing R Function From Github:

-   install.packages("devtools")
-   devtools::source_url("<https://raw.githubusercontent.com/kyledougherty/CougR/main/a_LoCoH_HR_Function.R>")
    -   To get the URL of any function:

        -   Navigate to function on github

        -   Click "Raw" at top of code window.

        -   Copy URL.

## Python

-   To use any of the python functions, you will need to install a version of Python 3: <https://www.python.org/downloads/>

    -   To verify installation:

        -   Open a command line terminal.

        -   Run: python \--version (you may need to swap "py" for "python" depending on configuration)

    -   After installing Python, check that Pip is installed:

        -   Open a command line terminal.

        -   Run: pip \--version

        -   If pip is installed, you should see some specifications regarding which version you have installed. If not, run the following commands in your terminal:

            -   curl <https://bootstrap.pypa.io/get-pip.py> -o get-pip.py

            -   py get-pip.py (again may need to swap "py" for "python")

        -   Then re-reun pip \--version to verify installation.

    -   After confirming installation of Python and pip, install CougR package.

        -   Open command line terminal.

        -   Run: pip install git+<https://github.com/kyledougherty/CougR>

    -   To access functions in a Quarto (.qmd) file:

        -   Insert a new python chunk (insert new chunk and change the {r} to {python}).

        -   Use the from and import functions (similar to library in R).

            -   Ex: To access functions contained in ee_utilities.py, you would run the following:

                -   from CougR import ee_utilities

                -   You could then access the local_shp_to_ee function using ee_utilities.local_shp_to_ee()
