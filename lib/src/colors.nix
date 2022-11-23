_: {
  inverseSchemeType = type:
    if ("dark" == type)
    then "light"
    else if ("light" == type)
    then "dark"
    else throw "Unsupported color scheme type '${type}' specified.";
}
