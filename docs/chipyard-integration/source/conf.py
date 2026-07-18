# Configuration file for the Sphinx documentation builder.

# -- Project information -----------------------------------------------------

project = 'OPERA-DSP Chipyard Integration'
copyright = '2026, OPERA-DSP'
author = 'OPERA-PLATFORM'


# -- General configuration ---------------------------------------------------

extensions = []
templates_path = ['_templates']
exclude_patterns = []


# -- Options for HTML output -------------------------------------------------

html_theme = 'sphinx_rtd_theme'
html_static_path = ['_static']
html_css_files = ['set_width.css']


# -- Options for LaTeX output -------------------------------------------------

# This is a short, screen-oriented integration guide. Avoid the manual class's
# default two-sided/open-right layout, which inserts blank verso pages before
# the table of contents and chapters.
latex_elements = {
    'extraclassoptions': 'openany,oneside',
}
