# List all dotfield features and their aspects (nixos/home)

mod nixos

src-dir := justfile_directory() / "src"

[no-cd]
@_cache-global-list scope:
  rg --only-matching --no-filename --type nix --replace '$1' \
    'dotfield\.aspects\.([^.]+)\.{{ scope }}' \
    {{ src-dir }}/features \
  | sort \
  | uniq \
  > /tmp/dotfield-global-aspects-{{ scope }}

[no-cd]
@_cache-user-list user scope:
  rg --only-matching --no-filename --type nix --replace '$1' \
    'dotfield\.users\.{{ user }}\.aspects\.([^.]+)\.{{ scope }}' \
    {{ src-dir }}/users/{{ user }}/features \
  | sort \
  | uniq \
  > /tmp/dotfield-user-{{ user }}-aspects-{{ scope }}

[no-cd]
list-global-aspects-by-scope scope: (_cache-global-list scope)
  @cat /tmp/dotfield-global-aspects-{{ scope }}

[no-cd]
list-user-aspects-by-scope user scope: (_cache-user-list user scope)
  @cat /tmp/dotfield-user-{{ user }}-aspects-{{ scope }}

# [no-cd]
# list-user-aspects scope:

# [no-cd]
# list-multi-scoped: (list-by-scope "nixos") (list-by-scope "home")
#   comm -12 <(just -q features::list nixos) <(just -q features::list home)

# List all unique dotfield.features values with their aspects
list-dotfield-features:
  @echo "=== dotfield.features.<name> patterns ==="
  @echo
  @echo "Features with nixos aspect only:"
  @rg "dotfield\.features\.[^.]+\.nixos" src --type nix -o | \
    sed -E 's/.*dotfield\.features\.([^.]+)\.nixos.*/\1/' | \
    sort -u > /tmp/dotfield-features-nixos
  @rg "dotfield\.features\.[^.]+\.home" src --type nix -o | \
    sed -E 's/.*dotfield\.features\.([^.]+)\.home.*/\1/' | \
    sort -u > /tmp/dotfield-features-home
  @comm -23 /tmp/dotfield-features-nixos /tmp/dotfield-features-home | sed 's/^/  /'
  @echo
  @echo "Features with home aspect only:"
  @comm -13 /tmp/dotfield-features-nixos /tmp/dotfield-features-home | sed 's/^/  /'
  @echo
  @echo "Features with both nixos and home aspects:"
  @comm -12 /tmp/dotfield-features-nixos /tmp/dotfield-features-home | sed 's/^/  /'
  @echo
  @echo "=== dotfield.users.cdom.features.<name> patterns ==="
  @echo
  @echo "User features with nixos aspect only:"
  @rg "dotfield\.users\.cdom\.features\.[^.]+\.nixos" src --type nix -o | \
    sed -E 's/.*dotfield\.users\.cdom\.features\.([^.]+)\.nixos.*/\1/' | \
    sort -u > /tmp/cdom-features-nixos
  @rg "dotfield\.users\.cdom\.features\.[^.]+\.home" src --type nix -o | \
    sed -E 's/.*dotfield\.users\.cdom\.features\.([^.]+)\.home.*/\1/' | \
    sort -u > /tmp/cdom-features-home
  @comm -23 /tmp/cdom-features-nixos /tmp/cdom-features-home | sed 's/^/  /'
  @echo
  @echo "User features with home aspect only:"
  @comm -13 /tmp/cdom-features-nixos /tmp/cdom-features-home | sed 's/^/  /'
  @echo
  @echo "User features with both nixos and home aspects:"
  @comm -12 /tmp/cdom-features-nixos /tmp/cdom-features-home | sed 's/^/  /'
  @rm -f /tmp/dotfield-features-* /tmp/cdom-features-*

# List all dotfield.features values (simple list)
list-features-simple:
  @echo "All dotfield.features values:"
  @rg "dotfield\.features\.[^.]+\.(nixos|home)" src --type nix -o | \
    sed -E 's/.*dotfield\.features\.([^.]+)\.(nixos|home).*/\1/' | \
    sort -u | sed 's/^/  /'
  @echo
  @echo "All dotfield.users.cdom.features values:"
  @rg "dotfield\.users\.cdom\.features\.[^.]+\.(nixos|home)" src --type nix -o | \
    sed -E 's/.*dotfield\.users\.cdom\.features\.([^.]+)\.(nixos|home).*/\1/' | \
    sort -u | sed 's/^/  /'
