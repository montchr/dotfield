(module mtgoch.plugin.ale
  {autoload {nvim aniseed.nvim}})

(set nvim.g.ale_linters
  {:clojure [:clj-kondo :joker]})

