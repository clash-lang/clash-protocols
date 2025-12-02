#!/bin/bash

set -uo pipefail

mkdir -p ~/.config/attic
cat <<EOF > ~/.config/attic/config.toml
default-server = "public"

[servers.public]
endpoint = "http://192.168.102.136:9200"
token = "$ATTIC_AUTH_TOKEN"
EOF
