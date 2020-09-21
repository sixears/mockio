#!/run/current-system/sw/bin/nix-shell
#!nix-shell -i bash -p cabal2nix

set -eu -o pipefail

echo cabal2nix . '>' cabal.nix
cabal2nix . > cabal.nix
for i in $( find proto -type f -name \*.hs -printf '%P\n' ); do
  echo nix-shell --command '${env_replace}' env-replace.nix '<' proto/$i '>' src/$i
  nix-shell --command '${env_replace}' env-replace.nix < proto/$i > src/$i
done
