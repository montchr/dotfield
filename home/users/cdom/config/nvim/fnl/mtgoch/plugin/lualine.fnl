(module mtgoch.plugin.lualine)

(let [(ok? lualine) (pcall require :lualine)]
  (when ok?
    (lualine.setup)))
