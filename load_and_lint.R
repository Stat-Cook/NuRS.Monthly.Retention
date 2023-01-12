devtools::document()
devtools::load_all()

globalVariables(c("%>%", "%like%", ":="))

styler:::style_active_pkg()
lintr::lint_package()
