(module mtgoch.plugin
  {autoload {a aniseed.core
             packer packer}})

(defn- safe-require-plugin-config [name]
  "Safely require a module under the mtgoch.plugin.* prefix. Will catch errors
  and print them while continuing execution, allowing other plugins to load
  even if one configuration module is broken."
  (let [(ok? val-or-err) (pcall require (.. "mtgoch.plugin." name))]
    (when (not ok?)
      (print (.. "Plugin config error: " val-or-err)))))

(defn req [name]
  "A shortcut to building a require string for your plugin
  configuration. Intended for use with packer's config or setup
  configuration options. Will prefix the name with `mtgoch.plugin.`
  before requiring."
  (.. "require('mtgoch.plugin." name "')"))

(defn use [...]
  "Iterates through the arguments as pairs and calls packer's use function for
  each of them. Works around Fennel not liking mixed associative and sequential
  tables as well.

  This is just a helper / syntax sugar function to make interacting with packer
  a little more concise."
  (let [pkgs [...]]
    (packer.startup
      (fn [use]
        (for [i 1 (a.count pkgs) 2]
          (let [name (. pkgs i)
                opts (. pkgs (+ i 1))]
            (-?> (. opts :mod) (safe-require-plugin-config))
            (use (a.assoc opts 1 name)))))))

  nil)
