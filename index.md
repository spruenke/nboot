## Welcome to the nboot Package

This R-package was designed for researchers and practioners using statistical methods on small samples. Often, bootstrap statistics can provide convenient solutions to small sample problems, however, in some cases they are either hard to set up or computationally complex (at least regarding computation time). Furthermore, one may not spend time on reading how to actually implement a bootstrap version for a specific method. 

nboot offers a range of methods in a bootstrapped versions, each offering different bootstrap methods to choose from. Also, the number of bootstrap iterations can be chosen. Computations are mostly done in C++ using the Rcpp integration for R, granting high speed and precision. 

The current version of the package is 0.3.0.

## Installing the Package

Currently, there is no version available on CRAN. To download and install the Package, make sure you have the latest versions of R and Rtools. Install the package by using `devtools::install_github("spruenke/nboot")`. If you are using a windows machine, please make sure Rtools is installed: https://cran.r-project.org/bin/windows/Rtools/history.html

## Template content

You can use the [editor on GitHub](https://github.com/spruenke/nboot/edit/gh-pages/index.md) to maintain and preview the content for your website in Markdown files.

Whenever you commit to this repository, GitHub Pages will run [Jekyll](https://jekyllrb.com/) to rebuild the pages in your site, from the content in your Markdown files.

### Markdown

Markdown is a lightweight and easy-to-use syntax for styling your writing. It includes conventions for

```markdown
Syntax highlighted code block

# Header 1
## Header 2
### Header 3

- Bulleted
- List

1. Numbered
2. List

**Bold** and _Italic_ and `Code` text

[Link](url) and ![Image](src)
```

For more details see [GitHub Flavored Markdown](https://guides.github.com/features/mastering-markdown/).

### Jekyll Themes

Your Pages site will use the layout and styles from the Jekyll theme you have selected in your [repository settings](https://github.com/spruenke/nboot/settings). The name of this theme is saved in the Jekyll `_config.yml` configuration file.

### Support or Contact

Having trouble with Pages? Check out our [documentation](https://docs.github.com/categories/github-pages-basics/) or [contact support](https://github.com/contact) and we’ll help you sort it out.
