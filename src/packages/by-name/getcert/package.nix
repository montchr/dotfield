# Display x509 certificates for an HTTPS URL
# Source: https://git.j3s.sh/dotfiles/blob/main/bin/getcert
{ openssl, writeShellApplication }:

writeShellApplication {
  name = "getcert";
  runtimeInputs = [ openssl ];

  text = ''
    url="$1"
    parsed_url=$(printf "%s" "$url" | sed 's|https://||g')

    printf '\n' \
    | openssl s_client -connect "$parsed_url":443 -showcerts \
    | openssl x509 -noout -text
  '';
}
