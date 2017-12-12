# shiny-rmarkdown

This is a simple app for rendering RMarkdown documents from within a shiny runtime.
A sample workflow is provided by default, but you can edit as required.
The document can be rendered within the app as html, or output as pdf, docx, or standalone html.

## Basic Use

To use the app, enter RMarkdown code on the left-hand side textbox.
The markdown will render on the fly on the right-hand side.

To prevent repeated rendering from slowing down your user experience, you can toggle the "Update continuously" checkbox.
This will prevent continous rendering.
