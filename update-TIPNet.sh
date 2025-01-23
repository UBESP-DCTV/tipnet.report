#!/bin/bash
# Utilizzo: ./update-TIPNet.sh [test|production] [target-make]
# # Aggiornamento manuale completo
# ./update-TIPNet.sh test update
#
# # Forzare solo aggiornamento dati
# ./update-TIPNet.sh production update-data
set -euo pipefail

ENV_TYPE="${1:-test}"
TARGET="${2:-update}"

case $ENV_TYPE in
  test) APP_NAME="TIPNet_test" ;;
  production) APP_NAME="TIPNet" ;;
  *) echo "Usage: $0 [test|production] [target]"; exit 1 ;;
esac

echo "🚀 Starting update ($TARGET) for: $ENV_TYPE"
make -C /srv/shiny-server/$APP_NAME APP_NAME=$APP_NAME $TARGET
