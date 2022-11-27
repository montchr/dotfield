if vim.g.vscode then
  -- VSCode extension
else
  vim.g["aniseed#env"] = {module = "cdom.init"}
end
