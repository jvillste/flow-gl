#!/usr/bin/env bash

LEIN_DIR="$HOME/.lein"
PROFILES_FILE="$LEIN_DIR/profiles.clj"

mkdir -p "$LEIN_DIR"

if [ -f "$PROFILES_FILE" ]; then
  echo "profiles.clj already exists. Skipping." >&2
  exit 1
fi

echo "Creating $PROFILES_FILE..."

cat > "$PROFILES_FILE" <<'EOF'
{:user
 {:repositories
  {"local-maven" {:url "file:///host-maven-repository"
                  :checksum :ignore}}}}
EOF

echo ""
echo "Done. Run 'lein deps' to resolve dependencies from it."
