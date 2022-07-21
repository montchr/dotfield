overlay: final: prev:
if prev == null || (prev.isFakePkgs or false)
then {}
else overlay final prev
##: sources:
# - https://github.com/divnix/digga/issues/464#issuecomment-1154974631
# - https://github.com/linyinfeng/dotfiles/blob/074aade622fb28ba759c4c7f9e3f4c461230bbd8/lib/overlay-null-protector.nix

